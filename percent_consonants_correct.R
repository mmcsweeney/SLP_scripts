# R script for identifying the total number of consonants in a speech sample to calculate Percent Consonants Correct (PCC)

# Written by Melissa McSweeney for Speech@NYU, April 2023

#################



setwd('/Users/melissa/Documents/NYU/speech sound disorders in children/ssd_project_2')

library(stringr)
rm(list=ls())

# read CSV with target and child form transcriptions of connected speech sample 
# CSV should have two columns -- Target and Child.Form 
# if prompted by R about encoding, select UTF-8 encoding 
ipa<-read.csv('PCC_to_process.csv',header=TRUE, sep=',')

#concatenate Target and Child Form columns to identify unique productions of target words 
ipa$concatenated<-paste(ipa$Target,ipa$Child.Form)

#only include unique (non-duplicated) productions of target words 
ipa<-ipa[!duplicated(ipa$concatenated),]

#make column to count number of consonants in each Target word 
ipa$target_consonants<-0

#list of consonants; include alternate character forms when necessary due to encoding differences
#(note: the affricates should be in the form of a single character for correct consonant differentiation later in the script) 
consonants<-c("p","b", "t" ,"d", "k" ,"g", "f" ,"v", "s", "z", "θ", "ð", "ʃ", "ʒ","ʤ","ʧ",
              "h", "w", "n", "m", "r", "j", "ŋ", "l")


#count how many consonants are in each Target word
for (i in c(1:length(ipa$Target))){

  char_split<-strsplit(ipa[i,1],"")[[1]]  #splits word into separate characters for analysis 
  
  for (j in char_split){
    if (j %in% consonants==TRUE){
    ipa[i,'target_consonants']<-ipa[i,'target_consonants']+1 
    }
  }
}


#make column to keep track of number of consonants correct in Child Form 
ipa$consonants_correct<-NA

#if Child Form was produced the same as Target, number of target consonants = number of consonants correct for that word 
ipa[ipa$Target==ipa$Child.Form,]$consonants_correct<-ipa[ipa$Target==ipa$Child.Form,]$target_consonants

#total number of consonants in the target sample 
sum(ipa$target_consonants) 

#write .csv file of current results, and identify the incorrect productions manually 
write.csv(ipa,'PCC_processed.csv')
