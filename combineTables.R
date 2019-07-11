#' ---
#' title: "Compile Expert Estimates"
#' author: "Adapted for the SJR PTM by Abbey Camaclang"
#' date: "3 July 2019"
#' output: github_document
#' ---

#' This script reads individual expert estimates from multiple .csv files
#' and compiles them into a single **Results.csv** file.
#' It requires that each expert table is saved as a .csv file in a subfolder within the working directory, 
#' contain the same number of rows and columns, and no other .csv files are in the same folder.

#+ warning = FALSE, message = FALSE
library(stringi)
library(tidyverse)
library(naniar)

#' Read in the individual tables and combine
#+ warning = FALSE, message = FALSE
files <- list.files(path = "./expert_est/", # Name of the subfolder in working directory
           pattern = "*.csv", 
           full.names = T)
temp <- read_csv(files[1], skip = 8) # Skips first few lines containing worksheet instructions
temp <- temp[1:9,1:116] %>% # Keeps only the relevant rows and columns (e.g. excludes the columns used to check data quality)
  select(.,-contains("Quality")) %>%
  add_column(.,Expert=rep(1,9),.before="Ecological Group")
byexpert <- temp

for (i in 3:length(files)){ # 3 because I am skipping expert #2. Change this to 2 if want to read that file
  temp <- read_csv(files[i], skip = 8)
  temp <- temp[1:9,1:116] %>%
    select(.,-contains("Quality")) %>%
    add_column(.,Expert=rep(i,9),.before="Ecological Group")
  byexpert<-rbind(byexpert,temp)
  }

#' Recode "X" to NA
na_strings <- c("X", "X ", "x", "x ")
byexpert <- byexpert %>% 
  replace_with_na_all(condition=~.x %in% na_strings)

#' Recode "B" to baseline values
# Get the baseline values
bestguess_base <- byexpert[,grep("Best guess$", colnames(byexpert))]
lower_base <- byexpert[,grep("Lower$", colnames(byexpert))]
upper_base <- byexpert[,grep("Upper$", colnames(byexpert))]
conf_base <- byexpert[,grep("Confidence$", colnames(byexpert))]

# Find strategy column indices
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

#' Standardize group labels if needed
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "Mature Forest Species")==1)] <- "Mature Forest and Peatland"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "Mature Forest/ Peatland Species")==1)] <- "Mature Forest and Peatland"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "Mature Forest/Peatland Species")==1)] <- "Mature Forest and Peatland"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "Grassland/Open Habitat species")==1)] <- "Grassland or Open Habitats"

#' Output results
write_csv(byexpert, "Results.csv")
