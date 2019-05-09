library(shiny)
library(seqinr)
library(Biostrings)
library(magrittr)

ui<-fluidPage(
  
  helpText("Welcome to Quick DNA Sequence Analysis! Be sure to enter only DNA sequences containing the base pairs
           A, T, C, and G for this particular Shiny App. When you click 'Submit Sequence', 
           an analysis of the sequence will be displayed, including the total number
           of base pairs, the percentage of each base pair type, and the amino acid 
           sequence that the DNA codes for. Here are some things to be aware of when entering a sequence:
           
           The end of your sequence will be striped if it doesn't contain a number of base pairs divisible by three.

           The number of characters is in the ENTIRE sequence, not just those that make up codons."),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  textInput( inputId = "seq", 
             label = "Enter your FASTA sequence here:", 
             value = "", 
             width = NULL, placeholder = NULL ),
  
  # Addition of a submit-type button so user can decide when they are finish entering a sequence
  #submitButton("Submit Sequence", icon = NULL, width = NULL ),
  actionButton("button", "Submit", icon = NULL), 
  
  # Output() functions
  textOutput(outputId = "error"),
  plotOutput(outputId = "bar"),
  textOutput(outputId = "seq"),
  textOutput(outputId = "codons"),
  textOutput(outputId = "amino_acids"),
  textOutput(outputId = "length"),
  textOutput(outputId = "codon_count"),
  tableOutput(outputId =  "base")
  
  )

server <- function(input, output)
{

  #empty string
  seq_input <- reactiveVal('')
  
  #empty table
  base_input<- reactiveVal({
    #table with ATGC
    tmp<-table(c("A",  "T",  "G", "C"))
    #fill in table with 0's
    tmp[]<-0
    tmp
  })
  
  #when the button is pushed update seq_input and base_input
  observeEvent(input$button, {
    
    #process input$seq - do anything else to new_seq_input that you want to here
    new_seq_input<- (toupper(input$seq))
    
    #change the stored value of seq_input
    seq_input(new_seq_input)
    
    #more processing of input$seq - do anything else to new_base_input here
    new_base_input<- strsplit(new_seq_input,"")%>%
                     unlist()%>%
                     table()
    tmp_per<-(round((new_base_input/sum(new_base_input))*100))
    TMP<-c("Number", new_base_input)
    tmp_PER<-c("Percent", tmp_per)
    new_base_input <-(rbind((TMP), (tmp_PER)))

    
    #change stored value of base_input
    base_input(new_base_input)
    
  }, ignoreInit=TRUE)
  
  
  ###ERROR
  #Produce an error message if the sequence does not make sense
  output$error <- renderText({
    ba <- ((seq_input()))
    ba <- s2c(ba)
    if (all((ba == "A") | (ba=="T") | (ba=="G") | ( ba == "C"))) {   
      print("Thank you! Enjoy your results!")
    } else {
      error=TRUE
      print("You need to enter only FASTA formatted DNA base pairs A, T, G, and C, please try again")
      }
  })
  
 #if (error != TRUE){
   ###SEQUENCE
   # Will display the sequence entered by the user
   output$seq <- renderPrint({print(paste0("This is the DNA sequence entered: " , seq_input(), "."))})
          # if (error = TRUE){
          #   print("There is an error")
          # }
          # else{(seq_input())}
          # })
        
    ###NUMBER BASE PAIRS
    # Count the number of base pairs entered by the user 
    output$length <- renderPrint({   
          if(seq_input() == '')
          {
            "0"
          }else{
            print(paste("The number of bases in the ENTIRE sequence is:" ,  nchar(seq_input()) , "."))
          }
        })
        
    ###BAR GRAPH
    # Code to create a bar graph of the bases
    output$bar <- renderPlot({  
                                  seqq<-s2c(seq_input())
                                  as<-sum(seqq=="A")
                                  ts<-sum(seqq=="T")
                                  gs<-sum(seqq=="G")
                                  cs<-sum(seqq=="C")
                                  total<-sum(as,ts,gs,cs)
                                  percent<-c(as, ts, gs, cs)
                                  percent<-round((percent/total)*100)
                                  bases <- c("A", "T", "G", "C")
                                  basLabel <- paste(bases, percent, "%")
                                  print(barplot(height = percent,
                                                beside = TRUE,
                                                width = 1, 
                                                legend.text = basLabel,
                                                col = c("red", "blue", "yellow", "black"),
                                                args.legend = list(x="bottomright"),
                                                ylab = "Percentage (%)",
                                                xlab= "Bases",
                                                border = "dark blue",
                                                main = "Bar Graph of Bases"))
        })
        
    ###BASES TABLE
    # Count the number of each base pair (A, T, C, G) entered by the user
    output$base <- renderTable(
          {
            base_input()
          }) 
        
    ###CODONS
    # Code for splitting into codons
    # Splits up sequences into groups of three base pairs (i.e. codons)
    output$codons <- renderText({
          
          # Installed the "seqinr" package to split sequence up into codons.
          input <- (s2c(seq_input())) # Store user input sequence as a variable # MDG for example to aa
          # s2c is a utility function used to convert string into characters
          sequence <- splitseq(seq= input, frame = 0 , word= 3)
          print(sequence)
        })
    
    ###CODON CHARACTER COUNT
    
    output$codon_count <- renderText({
      
      input <- (s2c(seq_input())) # Store user input sequence as a variable # MDG for example to aa
      # s2c is a utility function used to convert string into characters
      sequence <- splitseq(seq= input, frame = 0 , word= 3)
      sequence_codon_char <- nchar(sequence)
      print(paste("The number of bases that is found in the CODONS ONLY is:" , sum(sequence_codon_char), "."))
    })
        
    ###AMINO ACID
    # Code for assigning contains to an amino acid; code for putting together aa sequence
    output$amino_acids <- renderText({
          
          input <- (s2c(seq_input()))
          sequence <- splitseq(seq= input, frame = 0 , word= 3)
          amino_acid <- getGeneticCode()[(sequence)]
          print(amino_acid)
        } )
        
 #}else{break}      
}    



shinyApp(ui = ui, server = server)