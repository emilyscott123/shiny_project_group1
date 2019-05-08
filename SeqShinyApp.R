library(shiny)
library(seqinr)
library(Biostrings)
library(magrittr)

ui<-fluidPage(
  
  # Input() functions,
  helpText("Be sure to enter only DNA sequences containing the base pairs
           A, T, C, and G for this particular Shiny App. When you click 'Submit Sequence', 
           an analysis of the sequence will be displayed, including the total number
           of base pairs, the percentage of each base pair type, and the amino acid 
           sequence that the DNA codes for. Be warned, your sequence will be striped if
           if it doesn't contain a number of base pairs divisible by three."),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  textInput( inputId = "seq", 
             label = "Enter your FASTA sequence here:", 
             value = "", 
             width = NULL, placeholder = NULL ),
  
  # Addition of a submit-type button so user can decide when they are finish entering a sequence
  #submitButton("Submit Sequence", icon = NULL, width = NULL ),
  actionButton("button", "Submit", icon = icon("refresh")), 
  
  # Output() functions
  textOutput(outputId = "error"),
  plotOutput(outputId = "bar"),
  textOutput(outputId = "seq"),
  textOutput(outputId = "length"),
  tableOutput(outputId =  "base"),
  textOutput(outputId = "codons"),
  textOutput(outputId = "amino_acids")
  
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
    tmp_per<-(round((base_input()/sum(base_input()))*100))
    TMP<-c("Number", base_input())
    tmp_PER<-c("Percent", tmp_per)
    tmp_total<-table(rbind((TMP), (tmp_PER)))
    #new_base_input <-
    
    #change stored value of base_input
    base_input(new_base_input)
    
  }, ignoreInit=TRUE)
    
    # seq_input(
    #   (toupper(input$seq)),
    #   tmp<-c("A", "T", "G", "C"),
    #   tmp<- table(tmp),
    #   tmp[]<-0,
    #   tmp<-(seq_input),
    #   tmp_per<-tmp/sum(tmp),
    #   TMP<-c("Number", tmp),
    #   tmp_PER<-c("Percent", tmp_per),
    #   tmp_total<-rbind((TMP), (tmp_PER))
    # )
    
    
    
    # base_input(
    #   #the percentages of base pairs shown in graph
    #   percent <- (tmp_per),
    #   
    #   #the label for the bar graph
    #   bases <- c("A", "T", "G", "C"),
    #   basLabel <- paste(bases, tmp_per, "%")
    #    )
    # })
  

  
  ###ERROR
  #Produce an error message if the sequence does not make sense
  output$error <- renderText({
    ba <- ((seq_input()))
    ba <- s2c(ba)
    ca <- c(A|T|G|C)
    if (any (all(any(ba == "A") | any(ba=="T") | any(ba=="G") | any( ba == "C")))) {   #(any(ba != "A") & any(ba != "T") & any(ba != "G") & any(ba != "C"))) {
      print("Thank you!")
      
    } else {
      print("You need to enter only FASTA formatted DNA base pairs A, T, G, and C, please try again")
      error=TRUE
      
      }
    
  })
  
  
  
        ###SEQUENCE
        # Will display the sequence entered by the user
        output$seq <- renderPrint({(seq_input())})
          # if (error = TRUE){
          #   print("There is an error")
          # }
          # else{(seq_input())}
          # })
        
        ###NUMBER BASE PAIRS
        # Count the number of base pairs entered by the user 
        output$length <- renderPrint({nchar(seq_input())
          if(seq_input() == '')
          {
            "0"
          }else{
            nchar(seq_input())
          }
        })
        
        ###BAR GRAPH
        # Code to create a bar graph of the bases
        output$bar <- renderPlot({  
                                  tmp_per<-(round((base_input()/sum(base_input()))*100))
                                  TMP<-c("Number", base_input())
                                  tmp_PER<-c("Percent", tmp_per)
                                  tmp_total<-table(rbind((TMP), (tmp_PER)))
                                  bases <- c("A", "T", "G", "C")
                                  basLabel <- paste(bases, tmp_per, "%")
                                  print(barplot(height = tmp_per,
                                                beside = TRUE,
                                                width = 1, 
                                                legend.text = basLabel,
                                                col = c("red", "blue", "yellow", "black"),
                                                args.legend = list(x="bottomright"),
                                                ylab = "Percentage (%)",
                                                border = "dark blue",
                                                main = "Bar Graph of Bases"))
          
        })
        
        ###BASES TABLE
        # Count the number of each base pair (A, T, C, G) entered by the user
        output$base <- renderTable(
          {
            
            (tmp_total, rownames= ( "Number", "Percent"), colnames=("A", "T", "G", "C"))
            #tmp_per<-(round((base_input()/sum(base_input()))*100))
            
            #TMP<-c("Number", base_input())
            #print(TMP)
            #tmp_PER<-c("Percent", tmp_per)
            #print(tmp_PER)
            #tmp_total<-(table((TMP), (tmp_PER)))
            #print(tmp_total)
            #print((tmp_total))
            #print(base_input)
            # if(seq_input() == '')
            # {
            #   "0"
            # }else{
            #   #base_numbers <- table(seq_input)
            #   base_numbers <- strsplit(seq_input(), "")
            #   base_numbers <- unlist(base_numbers)
            #   print(tmp_total)
            #   #print(sum(base_numbers =="A"))
            #   #print(sum(base_numbers =="T"))
            #   #print(sum(base_numbers =="G"))
            #   #print(sum(base_numbers =="C"))
            # }
          })
        
        ###CODONS
        # Code for splitting into codons
        # Splits up sequences into groups of three base pairs (i.e. codons)
        output$codons <- renderText({
          
          # Installed the "seqinr" package to split sequence up into codons.
          input <- ((seq_input())) # Store user input sequence as a variable # MDG for example to aa
          # s2c is a utility function used to convert string into characters
          sequence <- splitseq(seq= input, frame = 0 , word= 3)
        })
        
        ###AMINO ACID
        # Code for assigning contains to an amino acid; code for putting together aa sequence
        output$amino_acids <- renderText({
          
          input <- (toupper(seq_input()))
          sequence <- splitseq(seq= input, frame = 0 , word= 3)
          amino_acid <- getGeneticCode()(sequence)
          print(amino_acid)
          # get aa_sequence
        } )
        
        
      }    


shinyApp(ui = ui, server = server)