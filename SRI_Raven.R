#This program calculates the song diversity using Song Richness Index (SRI) (Sawant S., Arvind C., Joshi V., Robin V. V., 2021)
#Input files: 1)Raven selection table for notes

#The notes selection table should include this columns- "Song No." and "Note types"

#-------------------------------------------------------------------------------------------------------------------------------
#Import required packages
library(readr)
library(tidyr)

#-------------------------------------------------------------------------------------------------------------------------------
#set working directory
#setwd('PATH')

#-------------------------------------------------------------------------------------------------------------------------------
#Import raven selection table for notes in the txt form
#with two annotation columns- "Song No." and "Note types"
#for allocating each note under some song
Input_notes <- as.data.frame(read_tsv("WBS-sample.txt", col_names = T))

#Extract note types from the data frame
Classified_notes <- as.data.frame(Input_notes$`Note types`, stringsAsFactors=FALSE)
colnames(Classified_notes) <- c("Note Types")

#-------------------------------------------------------------------------------------------------------------------------------
#This part extracts the start and end of each song based on the "Song No." column

#Extracting note count per song
Note_Count <- as.data.frame(aggregate(Input_notes$Channel , by = list(Category = as.numeric(Input_notes$`Song No.`)), FUN = sum )$x)

#Create datasheet for the start and end of each song
start_end <- data.frame(matrix(nrow = nrow(Note_Count), ncol = 2))
start_end <- cbind(Note_Count,start_end)
colnames(start_end) <- c("Note_Count","Start_note", "end_note")
for (l in 2:nrow(start_end)){
  start_end[1,2] <- 1
  start_end[1,3] <- start_end[1,1]
  start_end[l,2] <- 1 + start_end[l-1,3]
  start_end[l,3] <- start_end[l,1] + start_end[l-1,3]
}

#-------------------------------------------------------------------------------------------------------------------------------
#This part creates the note sequence based on the start and end of songs

#Create a data frame for output note sequences
Note_Seq <- data.frame(matrix(nrow = nrow(Note_Count)))

for (m in 1:nrow(start_end)){
  x = start_end[m,2]
  y = start_end[m,3]
  z = start_end[m,1]
  
  Note_Seq[m,] <- t(as.data.frame(do.call(paste, quote = T, as.list(Classified_notes[c(x:y),1]))))
  colnames(Note_Seq) <- c("Note Sequences")
}
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
rm("Classified_notes","Count_Type","empty_cells","Note_Count","Note_occr","Note_pos1", "Note_Seq",
   "Note_rep_sqr", "Note_type_count","Note_type_freq","Song_char","start_end", "i", "l",
   "m","Nc","Note_pos","Nt","SCI","sum","x","y","z")


#-------------------------------------------------------------------------------------------------------------------------------
#Write table for songs with added NVI values in txt format
write.table(SCI_output, "SCI_output.txt")

#_______________________________________________________________________________________________________________________________