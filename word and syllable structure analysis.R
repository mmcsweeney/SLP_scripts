setwd("/Users/melissa/Documents/NYU/speech sound disorders in children/project_1")

#excel spreadsheet saved as a .csv file with column names as the top row. One column should contain word structures (with vowels labeled as V and consonants as C, e.g. CVC)
bb<-read.csv('Phase1_GFTA3_Results_BB.csv',header = TRUE,sep = ',')

#view list of frequencies of all word structures
table(bb$Spoken.Word.Structure)

#count how many words have multiple syllables 
bb$mult.syll<-0
bb[grep('[.]',bb$Spoken.Word.Structure),]$mult.syll<-1
sum(bb$mult.syll) 

#convert word structure column to a list
syll<-as.vector(bb$Spoken.Word.Structure)

#split words with multiple syllables separated by a period
syll<-unlist(strsplit(syll,'[.]'))

#get number of syllables 
length(syll) 

#make frequency table of unique syllable structures 
table(syll)





