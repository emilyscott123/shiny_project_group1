library(shiny)
library(seqinr)
library(Biostrings)

ui<-fluidPage(
  
  # Input() functions,
  helpText("Be sure to enter only DNA sequences containing the base pairs
           A, T, C, and G for this particular Shiny App. When you click 'Submit Sequence', 
           an analysis of the sequence will be displayed, including the total number
           of base pairs, the percentage of each base pair type, and the amino acid 
           sequence that the DNA codes for."),
  
  textInput( inputId = "seq", 
             label = "Enter your FASTA sequence here:", 
             value = "", 
             width = NULL, placeholder = NULL ),
  
  # Addition of a submit-type button so user can decide when they are finish entering a sequence
  submitButton("Submit Sequence", icon = NULL, width = NULL ),
  
  # Output() functions
  textOutput(outputId = "error"),
  plotOutput(outputId = "bar"),
  textOutput(outputId = "seq"),
  textOutput(outputId = "length"),
  textOutput(outputId =  "base"),
  textOutput(outputId = "codons"),
  textOutput(outputId = "amino_acids")
  
  )

server <- function(input, output)
{
  #Produce an error message if the sequence does not make sense
  output$error <- renderText({
                              A <- s2c(input$seq)
                              if (any((A != "A") & (A != "T") & (A != "G") & (A != "C"))) {
                                print("You need to enter only FASTA formatted DNA base pairs A, T, G, and C, please try again")
                              } else {
                                  print("Thank you!")
                                }
                              
  })
  
  seq_input <- eventReactive(input$Submit, {runif(input$seq)})
  #seq_input <- reactiveVal('')
  #observeEvent(input$seq,{seq_input()})
  
  # Count the number of base pairs entered by the user 
  output$length <- renderPrint({nchar(seq_input)
    if(seq_input() == '')
    {
      ""
    }else{
      nchar(seq_input())
    }
  })
  
  # Count the number of each base pair (A, T, C, G) entered by the user
  
  output$base <- renderPrint(
    {
      if(seq_input() == '')
      {
        ""
      }else{
        #base_numbers <- table(seq_input)
        base_numbers <- strsplit(seq_input, "")
        base_numbers <- unlist(base_numbers)
        print(sum(base_numbers =="A"))
        print(sum(base_numbers =="T"))
        print(sum(base_numbers =="G"))
        print(sum(base_numbers =="C"))
      }
    })
  # for (base in seq_input)
  #{
  # A <- s2c(seq_input)
  # A_sum <- sum(A == "A")
  #T_sum <- sum(A == "T")
  #G_sum <- sum(A == "G")
  #C_sum <- sum(A == "C")
  # } 
  # print(A_sum)
  #print(T_sum)
  #print(G_sum)
  # print(C_sum)
  
  # Will display the sequence entered by the user
  output$seq <- renderPrint({toupper(input$seq)})
  
  # Code to create a pie chart of the bases
  output$bar <- renderPlot({  
                            basLabel <- c("A", "T", "G", "C")
                            A <- s2c(input$seq)
                            A_sum <- sum(A == "A")
                            T_sum <- sum(A == "T")
                            G_sum <- sum(A == "G")
                            C_sum <- sum(A == "C")
                            seq_sum<-sum(A_sum, T_sum, G_sum, C_sum)
                            percent <- cbind(A_percent <- round(A_sum/seq_sum*100),
                                             T_percent <- round(T_sum/seq_sum*100),
                                             G_percent <- round(G_sum/seq_sum*100),
                                             C_percent <- round(C_sum/seq_sum*100))
                            basLabel <- paste(basLabel, percent, "%") #add % to labels
                            print(barplot(height = percent,
                                          beside = TRUE,
                                          width = 1, 
                                          legend.text = basLabel,
                                          col = c("red", "blue", "yellow", "black"),
                                          args.legend = list(x="topleft"),
                                          ylab = "Percentage (%)",
                                          border = "dark blue",
                                          main = "Bar Graph of Bases"))
    
  })
  
  # Code for splitting into codons
  # Splits up sequences into groups of three base pairs (i.e. codons)
  output$codons <- renderText({
    
    # Installed the "seqinr" package to split sequence up into codons.
    input <- s2c(input$seq) # Store user input sequence as a variable # MDG for example to aa
    # s2c is a utility function used to convert string into characters
    sequence <- splitseq(seq= input, frame = 0 , word= 3)
  })
  # Code for assigning contains to an amino acid; code for putting together aa sequence
  
  output$amino_acids <- renderText({
    
    input <- s2c(input$seq)
    sequence <- splitseq(seq= input, frame = 0 , word= 3)
    amino_acid <- getGeneticCode()(sequence)
    print(amino_acid)
    # get aa_sequence
  } )
  
  # output$amino_acids <- renderText({
  #   
  #     get_aa_sequence <- function(sequence= sequence){
  #     
  #     amino_acid_code_codon_chart <- {
  #       'TTT'='F'; 'TTC'='F'; 'TTA'='L'; 'TTG'='L'; 'TCT'='S';
  #       'TCC'='S'; 'TCA'='S'; 'TCG'='S'; 'TAT'='Y'; 'TAC'='Y';
  #       'TGT'='C'; 'TGC'='C'; 'TGG'='W'; 'CTT'='L'; 'CTC'='L';
  #       'CTA'='L'; 'CTG'='L'; 'CCT'='P'; 'CCC'='P'; 'CCA'='P';
  #       'CCG'='P'; 'CAT'='H'; 'CAC'='H'; 'CAA'='Q'; 'CAG'='Q';
  #       'CGT'='R'; 'CGC'='R'; 'CGA'='R'; 'CGG'='R'; 'ATT'='I';
  #       'ATC'='I'; 'ATA'='I'; 'ATG'='M'; 'ACT'='T'; 'ACC'='T';
  #       'ACA'='T'; 'ACG'='T'; 'AAT'='N'; 'AAC'='N'; 'AAA'='K';
  #       'AAG'='K'; 'AGT'='S'; 'AGC'='S'; 'AGA'='R'; 'AGG'='R';
  #       'GTT'='V'; 'GTC'='V'; 'GTA'='V'; 'GTG'='V'; 'GCT'='A';
  #       'GCC'='A'; 'GCA'='A'; 'GCG'='A'; 'GAT'='D'; 'GAC'='D';
  #       'GAA'='E'; 'GAG'='E'; 'GGT'='G'; 'GGC'='G'; 'GGA'='G';
  #       'GGG'='G'}
  #     
  #     stop_codons <- list('TAA', 'TAG', 'TGA')
  #     
  #     aa_sequence <- " " # Initialize variable to store amino acids as they are added to the sequence
  #     for (item in sequence)
  #     {
  #       
  #       
  #       aa_sequence <- aa_sequence + amino_acid_code_codon_chart.get(item)
  #       print(aa_sequence)
  #       
  #     }
  #   }
  #   
  # }
  # )

  
}    


shinyApp(ui = ui, server = server)