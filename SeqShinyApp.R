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
  textInput( inputId = "seq", 
             label = "Enter your FASTA sequence here:", 
             value = "Enter FASTA sequence...", 
             width = NULL, placeholder = NULL ),
  
  # Addition of a submit-type button so user can decide when they are finish entering a sequence
  submitButton("Submit"),
  
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
  
  seq_input <- reactiveVal("")
  observeEvent("Submit",{seq_input()})
  # Count the number of base pairs entered by the user 
  output$length <- renderPrint({
    if(seq_input() == '')
    {
      ""
    }else{
      nchar(seq_input())
    }
  })
  
  # Code for splitting into codons
  # Splits up sequences into groups of three base pairs (i.e. codons)
  output$codons <- renderText({
    
    # Installed the "seqinr" package to split sequence up into codons.
    input <- s2c(input$seq) # Store user input sequence as a variable # MDG for example to aa
    # s2c is a utility function used to convert string into characters
    sequence <- splitseq(seq= input, frame = 0 , word= 3)
  })
  # Count the number of each base pair (A, T, C, G) entered by the user
  
  output$base <- renderPrint(
  {
    if(seq_input() == '')
    {
      ""
    }else{
      base_numbers <- table(seq_input)
      strsplit(base_numbers, '' ) %>%
      unlist(seq_input)
    }
  #   for (base in input$seq)
  #   {
  #     A <- s2c(input$seq)
  #     A_sum <- sum(A == "A")
  #     T_sum <- sum(A == "T")
  #     G_sum <- sum(A == "G")
  #     C_sum <- sum(A == "C")
  #     
  #   } 
  #     print(A_sum)
  #     print(T_sum)
  #     print(G_sum)
  #     print(C_sum)
   }
)
  # Will display the sequence entered by the user
  output$seq <- renderPrint({input$seq})
  
  # Code to create a pie chart of the bases
  output$bar <- renderPlot({pie3D(slices, labels = basLabel, explode= 0.1, main = "Pie Chart of Bases")})
  
  # Code for assigning contains to an amino acid; code for putting together aa sequence
    
  output$amino_acids <- renderText({

get_aa_sequence <- function(sequence= sequence){
    
    amino_acid_code_codon_chart <- {
      'TTT'='F'; 'TTC'='F'; 'TTA'='L'; 'TTG'='L'; 'TCT'='S';
      'TCC'='S'; 'TCA'='S'; 'TCG'='S'; 'TAT'='Y'; 'TAC'='Y';
      'TGT'='C'; 'TGC'='C'; 'TGG'='W'; 'CTT'='L'; 'CTC'='L';
      'CTA'='L'; 'CTG'='L'; 'CCT'='P'; 'CCC'='P'; 'CCA'='P';
      'CCG'='P'; 'CAT'='H'; 'CAC'='H'; 'CAA'='Q'; 'CAG'='Q';
      'CGT'='R'; 'CGC'='R'; 'CGA'='R'; 'CGG'='R'; 'ATT'='I';
      'ATC'='I'; 'ATA'='I'; 'ATG'='M'; 'ACT'='T'; 'ACC'='T';
      'ACA'='T'; 'ACG'='T'; 'AAT'='N'; 'AAC'='N'; 'AAA'='K';
      'AAG'='K'; 'AGT'='S'; 'AGC'='S'; 'AGA'='R'; 'AGG'='R';
      'GTT'='V'; 'GTC'='V'; 'GTA'='V'; 'GTG'='V'; 'GCT'='A';
      'GCC'='A'; 'GCA'='A'; 'GCG'='A'; 'GAT'='D'; 'GAC'='D';
      'GAA'='E'; 'GAG'='E'; 'GGT'='G'; 'GGC'='G'; 'GGA'='G';
      'GGG'='G'}
    
    stop_codons <- list('TAA', 'TAG', 'TGA')
    
    aa_sequence <- " " # Initialize variable to store amino acids as they are added to the sequence
    for (item in sequence)
    {
      
      
      aa_sequence <- aa_sequence + amino_acid_code_codon_chart.get(item)
      print(aa_sequence)
      
    }
  }
    
}
)
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

}    


shinyApp(ui = ui, server = server)