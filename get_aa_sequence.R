seq <- c("AGGGCTTGCCACGAATTGTGA")

get_aa_sequence <- function(seq)
{
  
  amino_acid_code_codon_chart <- c(
    'TTT'='F', 'TTC'='F', 'TTA'='L', 'TTG'='L', 'TCT'='S',
    'TCC'='S', 'TCA'='S', 'TCG'='S', 'TAT'='Y', 'TAC'='Y',
    'TGT'='C', 'TGC'='C', 'TGG'='W', 'CTT'='L', 'CTC'='L',
    'CTA'='L', 'CTG'='L', 'CCT'='P', 'CCC'='P', 'CCA'='P',
    'CCG'='P', 'CAT'='H', 'CAC'='H', 'CAA'='Q', 'CAG'='Q',
    'CGT'='R', 'CGC'='R', 'CGA'='R', 'CGG'='R', 'ATT'='I',
    'ATC'='I', 'ATA'='I', 'ATG'='M', 'ACT'='T', 'ACC'='T',
    'ACA'='T', 'ACG'='T', 'AAT'='N', 'AAC'='N', 'AAA'='K',
    'AAG'='K', 'AGT'='S', 'AGC'='S', 'AGA'='R', 'AGG'='R',
    'GTT'='V', 'GTC'='V', 'GTA'='V', 'GTG'='V', 'GCT'='A',
    'GCC'='A', 'GCA'='A', 'GCG'='A', 'GAT'='D', 'GAC'='D',
    'GAA'='E', 'GAG'='E', 'GGT'='G', 'GGC'='G', 'GGA'='G',
    'GGG'='G', 'TAA'='Stop', 'TAG'='Stop', 'TGA'="Stop")
  
  
  
  split(seq,as.numeric(gl(length(seq),3,length(seq))))  %>%
     unlist()
  
  for (item in sequence)
  {
    
    
    aa_sequence <- amino_acid_code_codon_chart[aa_sequence]
    print(aa_sequence)
    
  }
}


lappend <- function(aa_sequence, amino_acids ) {
  lst[[length(aa_sequence, amino_acids)]] <- obj
  return(aa_sequence)
} 