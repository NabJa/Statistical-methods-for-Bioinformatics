rm(list=ls())

setwd("C:/Users/Anja/Desktop/R for Bioinf")

library(seqinr)

dengue <- read.fasta(file = "sequence.fasta") #creates R list Object
dengueseq <- dengue[[1]] #Vektor of Sequence f dengue

slidingwindowplot <- function(windowsize, inputseq)
{
  starts <- seq(1, length(inputseq)-windowsize, by = windowsize)
  n <- length(starts) # Find the length of the vector "starts"
  chunkGCs <- numeric(n) # Make a vector of the same length as vector "starts",
  
  for (i in 1:n) {
    chunk <- inputseq[starts[i]:(starts[i]+windowsize-1)]
    chunkGC <- GC(chunk)
    #print(chunkGC)
    chunkGCs[i] <- chunkGC
  }
  plot(starts,chunkGCs,type="b",xlab="Nucleotide start position",ylab="GC content")
}

slidingwindowplot(350, dengueseq) #creates sliding window plot with specific windowsize and input
