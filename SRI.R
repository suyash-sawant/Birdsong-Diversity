#This program calculates the song diversity using Song Richness Index (SRI) (Sawant S., Arvind C., Joshi V., Robin V. V., 2021)
#Input files: 1)Data frame with song structures in terms of the note sequences

#The notes selection table should include this columns- "Song No." and "Note types"

#-------------------------------------------------------------------------------------------------------------------------------
#Import required packages
library(readr)
library(tidyr)

#-------------------------------------------------------------------------------------------------------------------------------
#set working directory
#setwd('PATH')

#-------------------------------------------------------------------------------------------------------------------------------
#Import data frame containing song structures
#with an annotation column- "Song_structure"
#for allocating each note under some song
Input_sequences <- as.data.frame(read_tsv("All note combinations.txt", col_names = T))

#Extract the note sequences
Note_Seq <- as.matrix(Input_sequences$Song_structure)

#-------------------------------------------------------------------------------------------------------------------------------
#This part calculates the Note Types (Nt), Note Count (Nc) and a matrix with occurrence of each note type in song

#Create a data frame for occurrence of each note type in the song (max 50 note types per song)
Note_occr <- data.frame(matrix(nrow = nrow(Note_Seq), ncol = 50))

#Column names for the data frame
Note_pos1 <-as.data.frame(expand.grid("M",formatC(c(1:50), width=1)))
Note_pos <- paste(Note_pos1$Var1, Note_pos1$Var2, sep="_")
colnames(Note_occr) <- c(Note_pos)

#Create a data frame for Note count and Note Types 
Count_Type <- data.frame(matrix(nrow = nrow(Note_Seq), ncol = 2))
colnames(Count_Type) <- c("Nc", "Nt")


for (i in 1:nrow(Note_Seq)){
  Song_char <- strsplit(Note_Seq[i,1], " ")
  Nc <- lengths(Song_char)
  Nt <- length(table(Song_char))
  
  Note_type_count <- as.data.frame(table(Song_char))
  Note_type_freq <- t(Note_type_count$Freq)
  
  empty_cells <- data.frame(matrix(nrow = 1, ncol = 50-Nt))
  Note_occr[i,] <- cbind(Note_type_freq,empty_cells)
  
  Count_Type[i,] <- cbind(Nc,Nt)
}

#-------------------------------------------------------------------------------------------------------------------------------

#This part calculates the Song Richness Index (SRI)
#and extract the values of Nt, Nc, Note Diversity Index (NDI), and SRI

        ######   ######   ######
        ##       ##  ##     ##
        ######   #####      ##
            ##   ##  ##     ##
        ######   ##  ##   ######

#Create a data frame for SRI values output
SCI_output <- data.frame(matrix(nrow = nrow(Note_Seq), ncol = 4))

#Create the values of (Mi-1)^2 in the SRI formula
Note_rep_sqr <- (Note_occr-1)*(Note_occr-1)
Note_rep_sqr[is.na(Note_rep_sqr)] <- 0

for (i in 1:nrow(Count_Type)){
  Nc <- Count_Type[i,1]
  Nt <- Count_Type[i,2]
  sum <- sum(Note_rep_sqr[i,])
  
  SCI <- (Nt * sqrt((Nc*Nc)-sum))/(Nc*Nc)
  
  SCI_output[i,1] <- Nc
  SCI_output[i,2] <- Nt
  SCI_output[i,3] <- Nt/Nc
  SCI_output[i,4] <- SCI
}

#Add diversity values along with the note structures
SCI_output <- cbind(Note_Seq,SCI_output)
colnames(SCI_output) <- c("Song_structure", "Nc", "Nt", "NDI", "SCI")

#-------------------------------------------------------------------------------------------------------------------------------
#remove extra objects
rm("Count_Type","empty_cells","Note_occr","Note_pos1", "Note_Seq",
   "Note_rep_sqr", "Note_type_count","Note_type_freq","Song_char","i", 
   "Nc","Note_pos","Nt","SCI","sum")


#-------------------------------------------------------------------------------------------------------------------------------
#Write table for songs with added NVI values in txt format
write.table(SCI_output, "SCI_output.txt")

#_______________________________________________________________________________________________________________________________