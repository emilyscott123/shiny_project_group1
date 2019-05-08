library(shiny)
library(seqinr)

ui<-fluidPage(
  
  helpText("Welcome to Quick DNA Sequence Analysis! Be sure to enter only DNA sequences containing the base pairs
           A, T, C, and G for this particular Shiny App. When you click 'Submit Sequence', 
           an analysis of the sequence will be displayed, including the total number
           of base pairs, the percentage of each base pair type, and the amino acid 
           sequence that the DNA codes for. Be warned, your sequence will be striped of remaining bases if 
           sequence provided is not divisible by three."),
  
  # Input() functions,
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  textInput( inputId = "seq", 
             label = "Enter your FASTA sequence here:", 
             value = "", 
             width = NULL, placeholder = NULL ),
  
  # Addition of a submit-type button so user can decide when they are finish entering a sequence
  actionButton("button","Submit"),
  
  # Output() functions
  plotOutput(outputId = "bar"),
  textOutput(outputId = "seq"),
  textOutput(outputId = "length"),
  textOutput(outputId =  "base"),
  textOutput(outputId = "codons"),
  textOutput(outputId = "amino_acids")
  
  )

server <- function(input, output)
{
  seq_input <- reactiveVal('')
  base_input <- reactiveVal(0)
  observeEvent(input$button,{(
    
    seq_input(
      toupper(input$seq),
      tmp<-c("A", "T", "G", "C"), # Creates list of A,T,G,C
      tmp <- table(tmp), # Makes into a table with four different columns A, T, G, C
      tmp[]<-0, # Sets all base values to zero 
      base_numbers <- strsplit(seq_input, ""), # Splits up input sequence into individual characters
      base_numbers <- unlist(base_numbers), # Unlists the base_numbers variable
      sum_A <- sum(base_numbers =="A"), # Calculate the number of adenine (A)
      sum_T <- sum(base_numbers =="T"), # Calculate the number of thymine (T)
      sum_G <- sum(base_numbers =="G"), # Calculate the number of guanine (G)
      sum_C <- sum(base_numbers =="C"), # Calculate the number of cytosine (C)
      tmp[] <- c(sum_A, sum_C, sum_G, sum_T), # Replace O's with new values
      tmp_per<-tmp/(sum(tmp))*100,
      TMP<-c("Number", tmp),
      tmp_PER<-c("Percent", tmp_per),
      tmp_total<-rbind((TMP), (tmp_PER))
    )
  )
    
    
    base_input(
      #the percentages of base pairs shown in graph
      percent <- (tmp_per),
      
      #the label for the bar graph
      bases <- c("A", "T", "G", "C"),
      basLabel <- paste(bases, tmp_per, "%")
    )
  })
  
  # Will display the sequence entered by the user
  output$seq <- renderPrint({
    
    print(paste("This is the sequence you entered:" , seq_input()))
    
    })
  
  # Count the number of base pairs entered by the user 
  output$length <- renderPrint({
    
    print(paste("The number of bases in the sequence entered is:", nchar(seq_input())))
    if(seq_input() == '')
    {
      print("NA")
    }else{
      nchar(seq_input())
    }
  })
  
  # Count the number of each base pair (A, T, C, G) entered by the user
  
  #Produce an error message if the sequence does not make sense
  output$error <- renderText({
    A <- ((seq_input()))
    if (any((A != "A") & (A != "T") & (A != "G") & (A != "C"))) {
      print("You need to enter only FASTA formatted DNA base pairs A, T, G, and C, please try again")
    } else {
      print("Thank you!")
    }
    
  })
  
  output$base <- renderPrint(
      {
        if(seq_input() == '')
        {
          ''
        }else{
          #base_numbers <- strsplit(seq_input, "")
          #base_numbers <- unlist(base_numbers)
            #sum_A <- sum(base_numbers =="A")
            #print(paste("The number of adenine in the sequence is:" , sum_A))
            #print(paste("The number of thymine in the sequence is:", sum_T))
            #print(paste("The number of guanine in the sequence is:", sum_G))
            #print(paste("The number of guanine in the sequence is:", sum_C))
            #print(sum(base_numbers =="C"))
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
  
  # Code to create a bar graph of the bases
  output$bar <- renderPlot({  
                            basLabel <- c("A", "T", "G", "C")
                            A <- s2c(seq_input())
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
                                          ylab = "Percentage (%) of Bases",
                                          border = "dark blue",
                                          main = "Bar Graph of Bases in DNA Sequence"))
    
  })
  
  # Will display the sequence entered by the user
  output$seq <- renderPrint({seq_input()})
  
  # Code for splitting into codons
  # Splits up sequences into groups of three base pairs (i.e. codons)
  output$codons <- renderText({
    
    # Installed the "seqinr" package to split sequence up into codons.
    input <- s2c(toupper(seq_input())) # Store user input sequence as a variable # MDG for example to aa
    # s2c is a utility function used to convert string into characters
    sequence <- splitseq(seq= input, frame = 0 , word= 3)
  })
  # Code for assigning contains to an amino acid; code for putting together aa sequence
  
  output$amino_acids <- renderText({
    
    input <- s2c(seq_input())
    sequence <- splitseq(seq= input, frame = 0 , word= 3)
    amino_acid <- getGeneticCode()[(sequence)]
    print(amino_acid)
 } )
  
 
}    


shinyApp(ui = ui, server = server)