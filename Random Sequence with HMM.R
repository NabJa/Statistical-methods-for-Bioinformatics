rm(list=ls())
library(seqinr)

nucleotides <- c("A", "T", "G", "C")

probA <- c(0.4, 0.4, 0.1, 0.1)
probT <- c(0.3, 0.3, 0.2, 0.2)
probG <- c(0.1, 0.1, 0.6, 0.2)
probC <- c(0.1, 0.1, 0.5, 0.3)

myMatr <- matrix(c(probA,probT,probG,probC), 4, 4, byrow = TRUE)

rownames(myMatr) <- c("A", "T", "G", "C")
colnames(myMatr) <- c("A", "T", "G", "C")

generateHMS <- function(transitionsmatrix, initalprop, seqlen){
  nucleotides <- c("A", "T", "G", "C")
  mySeq <- character()
  mySeq[1] <- sample(nucleotides, 1, replace = TRUE, initalprop)
  
  for (i in 2:seqlen){
    prevNucleo <- mySeq[i-1]
    propability <- transitionsmatrix[prevNucleo, ]
    nucleotide <- sample(nucleotides, 1, replace = TRUE, propability)
    mySeq[i] <- nucleotide
  }
  return(mySeq)
}

startprop <- c(0.25,0.25,0.25,0.25)

seq1 <- generateHMS(myMatr, startprop, 5000)
seq2 <- generateHMS(myMatr, startprop, 5000)
seq3 <- generateHMS(myMatr, startprop, 5000)
seq4 <- generateHMS(myMatr, startprop, 5000)

generateSlidingWindow <- function(myname, sequence, windowsize){
  
  starts <- seq(1, length(sequence), windowsize)
  n <- length(starts)
  chunksGCs <- numeric(n)
  
  for (i in 1:n){
    chunk <- sequence[starts[i] : (starts[i] + windowsize - 1)]
    chunkGC <- GC(chunk)
    chunksGCs[i] <- chunkGC
  }
  plot(starts, chunksGCs, type = "b", xlab = "Startpoints", ylab = "GC content", main = myname)
}

par(mfrow=c(2,2)) #makes a matix of 2x2 plots

generateSlidingWindow("Sequence 1", seq1, 50)
generateSlidingWindow("Sequence 2", seq2, 50)
generateSlidingWindow("Sequence 3", seq3, 50)
generateSlidingWindow("Sequence 4", seq4, 50)
