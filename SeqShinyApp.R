library(shiny)
library(plotrix)
library(seqinr)

ui<-fluidPage(
  
  helpText("Welcome to Quick DNA Sequence Analysis! Be sure to enter only DNA sequences containing the base pairs
           A, T, C, and G for this particular Shiny App. When you click 'Submit', 
           an analysis of the sequence will be displayed, including the total number
           of base pairs, the percentage of each base pair type, and the amino acid 
           sequence that the DNA codes for."),
  
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
  plotOutput(outputId = "bar"),
  textOutput(outputId = "seq"),
  textOutput(outputId = "length"),
  textOutput(outputId =  "base"),
  textOutput(outputId = "codons"),
  textOutput(outputId = "amino_acids")
  
  )

server <- function(input, output)
{
  
  # Count the number of base pairs entered by the user 
  output$length <- renderPrint({nchar(input$seq)})
  
  # Count the number of each base pair (A, T, C, G) entered by the user
  
  output$base <- renderPrint(
    {
      for (base in input$seq)
      {
        A <- s2c(input$seq)
        A_sum <- sum(A == "A")
        T_sum <- sum(T == "T")
        G_sum <- sum(G == "G")
        C_sum <- sum(C == "C")
        slices <- c(A_sum, T_sum, G_sum, C_sum)
        basLabel <- c("A", "T", "G", "C")
        baspct <- round(slices/sum(slices)*100)
        basLabel <- paste(basLabel, baspct) #add percent to base labels
        basLabel <- paste(basLabel, "%", sep ="") #add % to labels
        
        
        
      } 
      print( A_sum)
      print( T_sum)
      print( G_sum)
      print( C_sum)
      
    }
  )
  # Will display the sequence entered by the user
  output$seq <- renderPrint({input$seq})
  
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
    
      get_aa_sequence()
 } )
  #Practice Pie Chart
  seq = cbind("A", "T", "G", "C", "T")
  seq_sum <- 5
  A_sum <- sum(seq==("A"))
  T_sum <- sum(seq==("T"))
  G_sum <- sum(seq==("G"))
  C_sum <- sum(seq==("C"))
  slices <- c(A, T, G, C)
  basLabel <- c("A", "T", "G", "C")
  baspct <- round(slices/sum(slices)*100)
  
  #Practice Bar Graph
  basLabel <- c("A", "T", "G", "C")
  percent <- cbind(A_percent <- (A_sum/seq_sum*100),
                   T_percent <- (T_sum/seq_sum*100),
                   G_percent <- (G_sum/seq_sum*100),
                   C_percent <- (C_sum/seq_sum*100))
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
  
}    


shinyApp(ui = ui, server = server)