rm(list=ls())

setwd("C:/Users/Anja/Desktop/R for Bioinf")

#library(seqinr)

dengue <- read.fasta(file = "sequence.fasta") #creates R list Object
dengueseq <- dengue[[1]] #Vektor of Sequence f dengue

#Is the word "GC" over or under Represented?

oneWord <-count(dengueseq, 1)
twoWords <- count(dengueseq, 2)

#frquencie of GC / frq of G * frq of C
fGC = twoWords[["gc"]] / sum(count(dengueseq, 2)) 
fG = oneWord[["g"]] / sum(count(dengueseq, 1))
fC = oneWord[["c"]] / sum(count(dengueseq, 1))

ratio <- fGC / (fG * fC)
ratio #A ratio of 1 would mean exactly like expected. > 1 means more then expected...

