## This code combines individual expert benefits table into a single table for plotting and analysis ##

# Requires that each expert table is saved as a .csv file in a subfolder within the working directory, 
# contain the same number of rows and columns, and no other .csv files are in the same folder.
# Written by Abbey Camaclang (version date: 28 May 2019)

library(stringi)
library(tidyverse)
library(naniar)

files <- list.files(path = "./expert_est/", # Name of the subfolder in working directory
           pattern = "*.csv", 
           full.names = T)

temp <- read_csv(files[1], skip = 8) # Skips first few lines containing worksheet instructions
temp <- temp[1:9,1:116] # Keeps only the relevant rows and columns (e.g. excludes the columns used to check data quality)
temp <- select(temp,-contains("Quality"))
temp <- add_column(temp,Expert=rep(1,9),.before="Ecological Group")
byexpert <- temp

for (i in 3:length(files)){
  temp <- read_csv(files[i], skip = 8)
  temp <- temp[1:9,1:116]
  temp <- select(temp,-contains("Quality"))
  temp <- add_column(temp,Expert=rep(i,9),.before="Ecological Group")
  byexpert<-rbind(byexpert,temp)
  }

# Recode "x" to NA
na_strings <- c("X", "X ", "x", "x ")
byexpert <- byexpert %>% replace_with_na_all(condition=~.x %in% na_strings)

# Recode "B" to baseline values

# get the baseline values
bestguess_base <- byexpert[,grep("Best guess$", colnames(byexpert))]
lower_base <- byexpert[,grep("Lower$", colnames(byexpert))]
upper_base <- byexpert[,grep("Upper$", colnames(byexpert))]
conf_base <- byexpert[,grep("Confidence$", colnames(byexpert))]

# find column indices
bestguess <- grep("Best guess_",colnames(byexpert))
lowest <- grep("Lower_", colnames(byexpert))
highest <- grep("Upper_", colnames(byexpert))
conf <- grep("Confidence_",colnames(byexpert))

# For each relevant column, replace "b" with baseline values from the same row
for (i in 1:length(bestguess)) {
  bg_temp <- which(byexpert[,bestguess[i]]=="B" | byexpert[,bestguess[i]]=="b")
  byexpert[bg_temp,bestguess[i]] <- bestguess_base[bg_temp,]
  
  l_temp <- which(byexpert[,lowest[i]]=="B" | byexpert[,lowest[i]]=="b")
  byexpert[l_temp,lowest[i]] <- lower_base[l_temp,]
  
  u_temp <- which(byexpert[,highest[i]]=="B" | byexpert[,highest[i]]=="b")
  byexpert[u_temp,highest[i]] <- upper_base[u_temp,]
  
  byexpert[l_temp,conf[i]] <- conf_base[l_temp,] # using the index for lower as some may have been left blank/NA
}

write_csv(byexpert, "Results.csv")



