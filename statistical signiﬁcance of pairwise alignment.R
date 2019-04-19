rm(list=ls())
setwd("C:/Users/Anja/Desktop/R/R for Bioinf")
library(seqinr)    #paket für Einlesen von FASTA
#biocLite("Biostrings")
library(Biostrings)   #Paket für alignemnts

s1 <- "HEAGAWGHEE"
s2 <- "PAWHEAE"

randomSeqAmount <- 1000

generateSeqsWithMultinomialModel <- function(inputsequence, X) #Generates X random Sequences with same length and same "amino-zusammensetzung" wie inputseq
  {
  require("seqinr") # This function requires the SeqinR package. 
  inputsequencevector <- s2c(inputsequence)  # Change the input sequence into a vector of letters 
  
  # Find the frequencies of the letters in the input sequence"inputsequencevector":
  mylength <- length(inputsequencevector)
  mytable <- table(inputsequencevector) # Find the names of the letters in the sequence 
  letters <- rownames(mytable)
  numletters <- length(letters) 
  probabilities <- numeric() # Make a vector to store the probabilities of letters 
  
  for (i in 1:numletters) 
    {
    letter <- letters[i] 
    count <- mytable[[i]] 
    probabilities[i] <- count/mylength
    } 
  
  # Make X random sequences using the multinomial model with probabilities "probabilities" 
  seqs <- numeric(X)
  
  for (j in 1:X) 
    { 
    seq <- sample(letters, mylength, rep=TRUE, prob=probabilities) # Sample with replacement
    seq <- c2s(seq)
    seqs[j] <- seq
  }
  # Return the vector of random sequences return(seqs)
  return(seqs)
  }

randomseqs <- generateSeqsWithMultinomialModel('PAWHEAE',randomSeqAmount) 
randomseqs[1:10]

randomscores <- double(randomSeqAmount) # Create a numeric vector with 1000 elements 

for (i in 1:randomSeqAmount) 
  {
  score <- pairwiseAlignment(s2, randomseqs[i], substitutionMatrix = "BLOSUM50", 
                             gapOpening = -2, gapExtension = -8, scoreOnly = TRUE)
  
  randomscores[i] <- score
}

hist(randomscores) # Draw histogram

inputScore <- pairwiseAlignment(s2, s1, substitutionMatrix = "BLOSUM50", 
                  gapOpening = -2, gapExtension = -8, scoreOnly = TRUE)

absP <- sum(randomscores >= inputScore) 
pValue <- absP / randomSeqAmount
pValue
