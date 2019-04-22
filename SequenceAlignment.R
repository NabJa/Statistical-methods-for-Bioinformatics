rm(list=ls())
library(seqinr)    #importing FASTA format
#biocLite("Biostrings")
library(Biostrings)   #computes alignments


leprae <- read.fasta(file = "Q9CD83.fasta")
ulcerans <- read.fasta(file = "OIN17619.fasta")
lapraeSeq <- leprae[[1]]
ulceransSeq <- ulcerans[[1]]

dotPlot(ulceransSeq, lapraeSeq, nmatch = 4, wsize = 4)

#################### Pairwise global alignment of DNA sequences using the Needleman-Wunsch algorithm

sigma <- nucleotideSubstitutionMatrix(match = 2, mismatch = -1, baseOnly = TRUE)

s1 <- "GAATTC" 
s2 <- "GATTA"

globalAligns1s2 <- pairwiseAlignment(s1, s2, substitutionMatrix = sigma,
                                     gapOpening = -2, gapExtension = -8, scoreOnly = FALSE)

globalAligns1s2


#################### Pairwise global alignment of PROTEIN sequences using the Needleman-Wunsch algorithm

data("BLOSUM50")

s3 <- "PAWHEAE"
s4 <- "HEAGAWGHEE"

globalAligns3s4 <- pairwiseAlignment(s3, s4, substitutionMatrix = "BLOSUM50",
                                     gapOpening = -2, gapExtension = -8, scoreOnly = FALSE, 
                                     type = "global")

globalAligns3s4


#################### Aligning UniProt sequences and printing long alignments

lepraeseqstring <- c2s(lapraeSeq)
ulceransseqstring <- c2s(ulceransSeq) # c2s makes array to string (partof seqinR package)

lepraeseqstring <- toupper(lepraeseqstring) 
ulceransseqstring <- toupper(ulceransseqstring) #makes Strings upper case


globalAlignLepraeUlcerans <- pairwiseAlignment(lepraeseqstring, 
                                               ulceransseqstring, substitutionMatrix = BLOSUM50,
                                               gapOpening = -2, gapExtension = -8,
                                               scoreOnly = FALSE)


printPairwiseAlignment <- function(alignment, chunksize=60, returnlist=FALSE) #Function to print long alignment
  {
  require(Biostrings) # This function requires the Biostrings package 
  seq1aln <- pattern(alignment) # Get the alignment for the first sequence 
  seq2aln <- subject(alignment) # Get the alignment for the second sequence 
  alnlen <- nchar(seq1aln) # Find the number of columns in the alignment 
  starts <- seq(1, alnlen, by=chunksize) 
  n <- length(starts) 
  seq1alnresidues <- 0 
  seq2alnresidues <- 0
  
  for (i in 1:n) { 
  chunkseq1aln <- substring(seq1aln, starts[i], starts[i]+chunksize-1)
  chunkseq2aln <- substring(seq2aln, starts[i], starts[i]+chunksize-1) 
  
  # Find out how many gaps there are in chunkseq1aln: 
  gaps1 <- countPattern("-",chunkseq1aln) # countPattern() is from Biostrings package 
  
  # Find out how many gaps there are in chunkseq2aln: 
  gaps2 <- countPattern("-",chunkseq2aln) # countPattern() is from Biostrings package 
  
  # Calculate how many residues of the first sequence we have printed so far in the alignment:
  seq1alnresidues <- seq1alnresidues + chunksize - gaps1 
  
  # Calculate how many residues of the second sequence we have printed so far in the alignment: 
  seq2alnresidues <- seq2alnresidues + chunksize - gaps2

  if (returnlist == 'FALSE') 
    { 
    print(paste(chunkseq1aln,seq1alnresidues)) 
    print(paste(chunkseq2aln,seq2alnresidues)) 
    print(paste(' ')) 
    }
  }
  
  if (returnlist == 'TRUE') 
  {
    vector1 <- s2c(substring(seq1aln, 1, nchar(seq1aln))) 
    vector2 <- s2c(substring(seq2aln, 1, nchar(seq2aln))) 
    mylist <- list(vector1, vector2)
    return(mylist) 
    }
}

printPairwiseAlignment(globalAlignLepraeUlcerans, 60)
  
  
  
  
