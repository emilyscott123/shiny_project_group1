library(shiny)
library(plotrix)

ui<-fluidPage(
  
  # Input() functions,
  textInput( inputId = "seq", 
             label = "Enter your FASTA sequence here:", 
             value = "Enter FASTA sequence...", 
             width = NULL, placeholder = NULL )
  ,
  

  #Practice, I don't think we actually need this in our code. We need to calculate the seq length in our code for out users.
  sliderInput(inputId = "inputSeq", 
              label = "How long is your sequence?", 
              value = 25, min = 1, max = 100)
  
  ,
  # Output() functions
  plotOutput(outputId = "pie"),
  output$seq,
  output$inputSeq
  
)


server <- function(input, output){
  output$pie <- renderPlot({pie3D(slices, labels = basLabel, explode= 0.1, main = "Pie Chart of Bases")})
}
shinyApp(ui = ui, server = server)



#Practice Pie Chart
seq = cbind("A", "T", "G", "C")
A <- sum(seq==("A"))
T <- sum(seq==("T"))
G <- sum(seq==("G"))
C <- sum(seq==("C"))
slices <- c(A, T, G, C)
basLabel <- c("A", "T", "G", "C")
baspct <- round(slices/sum(slices)*100)
basLabel <- paste(basLabel, baspct) #add percent to base labels
basLabel <- paste(basLabel, "%", sep ="") #add % to labels
print(pie3D(slices, labels = basLabel, explode= 0.1, main = "Pie Chart of Bases"))

      