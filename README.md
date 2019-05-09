# Final BIFX551 Project : Quick DNA Sequence Analysis
# shiny_project_group1
This is an app that is designed to help make DNA analysis simpler. The user need only input a DNA sequence such as "ATGGTACCTTCA" and the app will break down the sequence and provide the user with the following information pertaining to the sequence entered:

1) The sequence entered by the user.
2) The total number of bases in the sequence entered.
3) The total number of bases found in the codons of the sequence.
4) The codons of the sequence entered. Note that base pairs are stripped off the end of the sequence if it is not divisible by three.
5) The amino acid sequence translated from the codons the sequence was broken down into.
6) The number and percentage of each base pair found in the ENTIRE sequence entered presented as a table. 
7) The percentage of each base pair displayed as a bar graph.

## Installation

```r
require(shiny)
require(seqinr)
require(Biostrings)
require(magrittr)
install_github('BIFX551-Spring-2019/shiny_project_group1')

```

## Basic usage of the App

Enter a DNA Sequence and click the "Submit" button:
```r
print("Enter a FASTA sequence:")
seq_input <- "ATGTGAGTGAGTGA"
ba <- (seq_input)
ba <- s2c(ba)

    if (all((ba == "A") | (ba=="T") | (ba=="G") | ( ba == "C"))) {   
      print("Thank you! Enjoy your results!")
    } else {
      error=TRUE
      print("You need to enter only FASTA formatted DNA base pairs A, T, G, and C, please try again")
    }
    
## The message: "Thank you! Enjoy your results!" will be displayed to the user.    
```

The sequence will be displayed:

```r
seq_input <- "ATGTGAGTGAGTGA"
print(paste0("This is the DNA sequence entered: " , seq_input, "."))

## This is the DNA sequence entered: "ATGTGAGTGAGTGA."
```

The number of bases in the ENTIRE sequence will be provided:

```r
seq_input <- "ATGTGAGTGAGTGA"
print(paste("The number of bases in the ENTIRE sequence is:" ,  nchar(seq_input) , "."))

## The number of bases in the ENTIRE sequence is: 14.

```

The number of bases in JUST THE CODONS will be provided:

```r
seq_input <- "ATGTGAGTGAGTGA"
input <- (s2c(seq_input)) # Store user input sequence as a variable 
# s2c is a utility function used to convert string into characters
sequence <- splitseq(seq= input, frame = 0 , word= 3)
sequence_codon_char <- nchar(sequence)
print(paste("The number of bases that is found in the CODONS ONLY is:" , sum(sequence_codon_char), "."))

## The number of bases that is found in the CODONS ONLY is: 12.

```

The codons of the sequences will be given as an output, in addition to those codons translated to an amino acid sequence:

```r
#Code to break up the sequence into codons

seq_input <- "ATGTGAGTGAGTGA"

#Installed the "seqinr" package to split sequence up into codons.
input <- (s2c(seq_input)) # Store user input sequence as a variable 
# s2c is a utility function used to convert string into characters
sequence <- splitseq(seq= input, frame = 0 , word= 3)
print(sequence)

# Code to translate the codons into amino acids

input <- (s2c(seq_input))
sequence <- splitseq(seq= input, frame = 0 , word= 3)
amino_acid <- getGeneticCode()[(sequence)]
print(amino_acid)

## [1]"ATG" "TGA" "GTG" "AGT" # Sequence broken up into codons
## "M" "*" "V" "S" # Codons translated into their amino acid sequence

```

The number of each base pair in the entire sequence will be given as both a number and a percent in a table:

```r
seq_input <- "ATGTGAGTGAGTGA"

base_input<- 
    #table with ATGC
    tmp<-table(c("A",  "T",  "G", "C"))
    #fill in table with 0's
    tmp[]<-0
    tmp

new_base_input<- strsplit(seq_input,"")%>%
                     unlist()%>%
                     table()
    tmp_per<-(round((new_base_input/sum(new_base_input))*100))
    TMP<-c("Number", new_base_input)
    tmp_PER<-c("Percent", tmp_per)
    new_base_input <-(rbind((TMP), (tmp_PER)))

    
#change stored value of base_input
base_input(new_base_input)    
print(base_input)


## V1	     A 	  G	  T
Number	   4	  6	  4
Percent	  29   43  29

```

Base pair percentages will be illustrated in a bar graph:
```r
seq_input <- "ATGTGAGTGAGTGA"
   
seqq<-s2c(seq_input))
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

## The output will be a bar graph of the bases found in the sequence entered. 
```
