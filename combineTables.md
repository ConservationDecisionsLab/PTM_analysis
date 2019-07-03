Compile Expert Estimates
================
Adapted for the SJR PTM by Abbey Camaclang
3 July 2019

This script reads individual expert estimates from multiple .csv files and compiles them into a single Results.csv file. It requires that each expert table is saved as a .csv file in a subfolder within the working directory, contain the same number of rows and columns, and no other .csv files are in the same folder.

``` r
library(stringi)
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.1     v purrr   0.3.2
    ## v tibble  2.1.1     v dplyr   0.8.1
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts -------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(naniar)
```

Read in the individual tables and combine

``` r
files <- list.files(path = "./expert_est/", # Name of the subfolder in working directory
           pattern = "*.csv", 
           full.names = T)
temp <- read_csv(files[1], skip = 8) # Skips first few lines containing worksheet instructions
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   X183 = col_logical(),
    ##   X184 = col_logical(),
    ##   `Check if All > #1` = col_double(),
    ##   `Check if All > #2` = col_double(),
    ##   `Check if All > #3` = col_double(),
    ##   `Check if All > #4` = col_double(),
    ##   `Check if All > #5` = col_double(),
    ##   `Check if All > #6` = col_double(),
    ##   `Check if All > #7` = col_double(),
    ##   `Check if All > #8` = col_double(),
    ##   `Check if All > #9` = col_double(),
    ##   `Check if All > #10` = col_double(),
    ##   `Check if All > #11` = col_double(),
    ##   `Check if All > #12` = col_double(),
    ##   `Check if All > #13` = col_double(),
    ##   `Check if All > #14` = col_double(),
    ##   `Check if All > #15` = col_double(),
    ##   `Check if All > #16` = col_double(),
    ##   `Check if 3. > 1` = col_double(),
    ##   `Check if 3. > 2` = col_double()
    ##   # ... with 5 more columns
    ## )

    ## See spec(...) for full column specifications.

``` r
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
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   X117 = col_logical(),
    ##   `Best guess_23` = col_double(),
    ##   Lower_23 = col_double(),
    ##   Upper_23 = col_double(),
    ##   `Best guess_24` = col_double(),
    ##   Lower_24 = col_double(),
    ##   Upper_24 = col_double(),
    ##   `Best guess_25` = col_double(),
    ##   Lower_25 = col_double(),
    ##   Upper_25 = col_double(),
    ##   `Best guess_26` = col_double(),
    ##   Lower_26 = col_double(),
    ##   Upper_26 = col_double(),
    ##   `Best guess_27` = col_double(),
    ##   Lower_27 = col_double(),
    ##   Upper_27 = col_double(),
    ##   `Best guess_28` = col_double(),
    ##   Lower_28 = col_double(),
    ##   Upper_28 = col_double(),
    ##   `Best guess_29` = col_double()
    ##   # ... with 48 more columns
    ## )
    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   X117 = col_logical(),
    ##   `Best guess_23` = col_double(),
    ##   Lower_23 = col_double(),
    ##   Upper_23 = col_double(),
    ##   `Best guess_24` = col_double(),
    ##   Lower_24 = col_double(),
    ##   Upper_24 = col_double(),
    ##   `Best guess_25` = col_double(),
    ##   Lower_25 = col_double(),
    ##   Upper_25 = col_double(),
    ##   `Best guess_26` = col_double(),
    ##   Lower_26 = col_double(),
    ##   Upper_26 = col_double(),
    ##   `Best guess_27` = col_double(),
    ##   Lower_27 = col_double(),
    ##   Upper_27 = col_double(),
    ##   `Best guess_28` = col_double(),
    ##   Lower_28 = col_double(),
    ##   Upper_28 = col_double(),
    ##   `Best guess_29` = col_double()
    ##   # ... with 99 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Ecological Group` = col_character(),
    ##   `QUALITY CHECK` = col_character(),
    ##   `Best guess_1` = col_character(),
    ##   Lower_1 = col_character(),
    ##   Upper_1 = col_character(),
    ##   `QUALITY CHECK_1` = col_character(),
    ##   `Best guess_2` = col_character(),
    ##   Lower_2 = col_character(),
    ##   Upper_2 = col_character(),
    ##   `QUALITY CHECK_2` = col_character(),
    ##   `Best guess_3` = col_character(),
    ##   Lower_3 = col_character(),
    ##   Upper_3 = col_character(),
    ##   `QUALITY CHECK_3` = col_character(),
    ##   `Best guess_4` = col_character(),
    ##   Lower_4 = col_character(),
    ##   Upper_4 = col_character(),
    ##   `QUALITY CHECK_4` = col_character(),
    ##   `Best guess_5` = col_character(),
    ##   Lower_5 = col_character()
    ##   # ... with 152 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Ecological Group` = col_character(),
    ##   `QUALITY CHECK` = col_character(),
    ##   `Best guess_1` = col_character(),
    ##   Lower_1 = col_character(),
    ##   Upper_1 = col_character(),
    ##   Confidence_1 = col_character(),
    ##   `QUALITY CHECK_1` = col_character(),
    ##   `Best guess_2` = col_character(),
    ##   Lower_2 = col_character(),
    ##   Upper_2 = col_character(),
    ##   Confidence_2 = col_character(),
    ##   `QUALITY CHECK_2` = col_character(),
    ##   `QUALITY CHECK_3` = col_character(),
    ##   `Best guess_4` = col_character(),
    ##   Lower_4 = col_character(),
    ##   Upper_4 = col_character(),
    ##   Confidence_4 = col_character(),
    ##   `QUALITY CHECK_4` = col_character(),
    ##   `Best guess_5` = col_character(),
    ##   Lower_5 = col_character()
    ##   # ... with 145 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   X117 = col_logical(),
    ##   `Best guess_23` = col_double(),
    ##   Lower_23 = col_double(),
    ##   Upper_23 = col_double(),
    ##   `Best guess_24` = col_double(),
    ##   Lower_24 = col_double(),
    ##   Upper_24 = col_double(),
    ##   `Best guess_25` = col_double(),
    ##   Lower_25 = col_double(),
    ##   Upper_25 = col_double(),
    ##   `Best guess_26` = col_double(),
    ##   Lower_26 = col_double(),
    ##   Upper_26 = col_double(),
    ##   `Best guess_27` = col_double(),
    ##   Lower_27 = col_double(),
    ##   Upper_27 = col_double(),
    ##   `Best guess_28` = col_double(),
    ##   Lower_28 = col_double(),
    ##   Upper_28 = col_double(),
    ##   `Best guess_29` = col_double()
    ##   # ... with 130 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   X117 = col_logical(),
    ##   `Best guess_23` = col_double(),
    ##   Lower_23 = col_double(),
    ##   Upper_23 = col_double(),
    ##   `Best guess_24` = col_double(),
    ##   Lower_24 = col_double(),
    ##   Upper_24 = col_double(),
    ##   `Best guess_25` = col_double(),
    ##   Lower_25 = col_double(),
    ##   Upper_25 = col_double(),
    ##   `Best guess_26` = col_double(),
    ##   Lower_26 = col_double(),
    ##   Upper_26 = col_double(),
    ##   `Best guess_27` = col_double(),
    ##   Lower_27 = col_double(),
    ##   Upper_27 = col_double(),
    ##   `Best guess_28` = col_double(),
    ##   Lower_28 = col_double(),
    ##   Upper_28 = col_double(),
    ##   `Best guess_29` = col_double()
    ##   # ... with 130 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   X183 = col_logical(),
    ##   X184 = col_logical(),
    ##   `Check if All > #1` = col_double(),
    ##   `Check if All > #2` = col_double(),
    ##   `Check if All > #3` = col_double(),
    ##   `Check if All > #4` = col_double(),
    ##   `Check if All > #5` = col_double(),
    ##   `Check if All > #6` = col_double(),
    ##   `Check if All > #7` = col_double(),
    ##   `Check if All > #8` = col_double(),
    ##   `Check if All > #9` = col_double(),
    ##   `Check if All > #10` = col_double(),
    ##   `Check if All > #11` = col_double(),
    ##   `Check if All > #12` = col_double(),
    ##   `Check if All > #13` = col_double(),
    ##   `Check if All > #14` = col_double(),
    ##   `Check if All > #15` = col_double(),
    ##   `Check if All > #16` = col_double(),
    ##   `Check if 3. > 1` = col_double(),
    ##   `Check if 3. > 2` = col_double()
    ##   # ... with 96 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   `Best guess_17` = col_logical(),
    ##   Lower_17 = col_logical(),
    ##   Upper_17 = col_logical(),
    ##   Confidence_17 = col_logical(),
    ##   X117 = col_logical(),
    ##   `Best guess_23` = col_double(),
    ##   Lower_23 = col_double(),
    ##   Upper_23 = col_double(),
    ##   `Best guess_24` = col_double(),
    ##   Lower_24 = col_double(),
    ##   Upper_24 = col_double(),
    ##   `Best guess_25` = col_double(),
    ##   Lower_25 = col_double(),
    ##   Upper_25 = col_double(),
    ##   `Best guess_26` = col_double(),
    ##   Lower_26 = col_double(),
    ##   Upper_26 = col_double(),
    ##   `Best guess_27` = col_double(),
    ##   Lower_27 = col_double(),
    ##   Upper_27 = col_double()
    ##   # ... with 131 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   X117 = col_logical(),
    ##   `Best guess_23` = col_double(),
    ##   Lower_23 = col_double(),
    ##   Upper_23 = col_double(),
    ##   `Best guess_24` = col_double(),
    ##   Lower_24 = col_double(),
    ##   Upper_24 = col_double(),
    ##   `Best guess_25` = col_double(),
    ##   Lower_25 = col_double(),
    ##   Upper_25 = col_double(),
    ##   `Best guess_26` = col_double(),
    ##   Lower_26 = col_double(),
    ##   Upper_26 = col_double(),
    ##   `Best guess_27` = col_double(),
    ##   Lower_27 = col_double(),
    ##   Upper_27 = col_double(),
    ##   `Best guess_28` = col_double(),
    ##   Lower_28 = col_double(),
    ##   Upper_28 = col_double(),
    ##   `Best guess_29` = col_double()
    ##   # ... with 130 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   `Best guess` = col_double(),
    ##   Lower = col_double(),
    ##   Upper = col_double(),
    ##   Confidence = col_double(),
    ##   Confidence_1 = col_double(),
    ##   Confidence_2 = col_double(),
    ##   Confidence_3 = col_double(),
    ##   Confidence_5 = col_double(),
    ##   Confidence_6 = col_double(),
    ##   Confidence_7 = col_double(),
    ##   Confidence_12 = col_double(),
    ##   Confidence_13 = col_double(),
    ##   Confidence_14 = col_double(),
    ##   Confidence_15 = col_double(),
    ##   Confidence_16 = col_double(),
    ##   `Best guess_17` = col_double(),
    ##   Lower_17 = col_double(),
    ##   Upper_17 = col_double(),
    ##   Confidence_17 = col_double(),
    ##   Confidence_19 = col_double()
    ##   # ... with 32 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   X117 = col_logical(),
    ##   `Best guess_23` = col_double(),
    ##   Lower_23 = col_double(),
    ##   Upper_23 = col_double(),
    ##   `Best guess_24` = col_double(),
    ##   Lower_24 = col_double(),
    ##   Upper_24 = col_double(),
    ##   `Best guess_25` = col_double(),
    ##   Lower_25 = col_double(),
    ##   Upper_25 = col_double(),
    ##   `Best guess_26` = col_double(),
    ##   Lower_26 = col_double(),
    ##   Upper_26 = col_double(),
    ##   `Best guess_27` = col_double(),
    ##   Lower_27 = col_double(),
    ##   Upper_27 = col_double(),
    ##   `Best guess_28` = col_double(),
    ##   Lower_28 = col_double(),
    ##   Upper_28 = col_double(),
    ##   `Best guess_29` = col_double()
    ##   # ... with 130 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Ecological Group` = col_character(),
    ##   `QUALITY CHECK` = col_character(),
    ##   `Best guess_1` = col_character(),
    ##   Lower_1 = col_character(),
    ##   Upper_1 = col_character(),
    ##   Confidence_1 = col_character(),
    ##   `QUALITY CHECK_1` = col_character(),
    ##   `Best guess_2` = col_character(),
    ##   Lower_2 = col_character(),
    ##   Upper_2 = col_character(),
    ##   Confidence_2 = col_character(),
    ##   `QUALITY CHECK_2` = col_character(),
    ##   `Best guess_3` = col_character(),
    ##   Lower_3 = col_character(),
    ##   Upper_3 = col_character(),
    ##   Confidence_3 = col_character(),
    ##   `QUALITY CHECK_3` = col_character(),
    ##   `Best guess_4` = col_character(),
    ##   Lower_4 = col_character(),
    ##   Upper_4 = col_character()
    ##   # ... with 90 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   X117 = col_logical(),
    ##   `Best guess_23` = col_double(),
    ##   Lower_23 = col_double(),
    ##   Upper_23 = col_double(),
    ##   `Best guess_24` = col_double(),
    ##   Lower_24 = col_double(),
    ##   Upper_24 = col_double(),
    ##   `Best guess_25` = col_double(),
    ##   Lower_25 = col_double(),
    ##   Upper_25 = col_double(),
    ##   `Best guess_26` = col_double(),
    ##   Lower_26 = col_double(),
    ##   Upper_26 = col_double(),
    ##   `Best guess_27` = col_double(),
    ##   Lower_27 = col_double(),
    ##   Upper_27 = col_double(),
    ##   `Best guess_28` = col_double(),
    ##   Lower_28 = col_double(),
    ##   Upper_28 = col_double(),
    ##   `Best guess_29` = col_double()
    ##   # ... with 130 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   `Best guess_22` = col_logical(),
    ##   Lower_22 = col_logical(),
    ##   Upper_22 = col_logical(),
    ##   Confidence_22 = col_logical(),
    ##   X117 = col_logical(),
    ##   `Best guess_23` = col_double(),
    ##   Lower_23 = col_double(),
    ##   Upper_23 = col_double(),
    ##   `Best guess_24` = col_double(),
    ##   Lower_24 = col_double(),
    ##   Upper_24 = col_double(),
    ##   `Best guess_25` = col_double(),
    ##   Lower_25 = col_double(),
    ##   Upper_25 = col_double(),
    ##   `Best guess_26` = col_double(),
    ##   Lower_26 = col_double(),
    ##   Upper_26 = col_double(),
    ##   `Best guess_27` = col_double(),
    ##   Lower_27 = col_double(),
    ##   Upper_27 = col_double()
    ##   # ... with 131 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   X183 = col_logical(),
    ##   X184 = col_logical(),
    ##   `Check if All > #1` = col_double(),
    ##   `Check if All > #2` = col_double(),
    ##   `Check if All > #3` = col_double(),
    ##   `Check if All > #4` = col_double(),
    ##   `Check if All > #5` = col_double(),
    ##   `Check if All > #6` = col_double(),
    ##   `Check if All > #7` = col_double(),
    ##   `Check if All > #8` = col_double(),
    ##   `Check if All > #9` = col_double(),
    ##   `Check if All > #10` = col_double(),
    ##   `Check if All > #11` = col_double(),
    ##   `Check if All > #12` = col_double(),
    ##   `Check if All > #13` = col_double(),
    ##   `Check if All > #14` = col_double(),
    ##   `Check if All > #15` = col_double(),
    ##   `Check if All > #16` = col_double(),
    ##   `Check if 3. > 1` = col_double(),
    ##   `Check if 3. > 2` = col_double()
    ##   # ... with 96 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   X117 = col_logical(),
    ##   `Best guess_23` = col_double(),
    ##   Lower_23 = col_double(),
    ##   Upper_23 = col_double(),
    ##   `Best guess_24` = col_double(),
    ##   Lower_24 = col_double(),
    ##   Upper_24 = col_double(),
    ##   `Best guess_25` = col_double(),
    ##   Lower_25 = col_double(),
    ##   Upper_25 = col_double(),
    ##   `Best guess_26` = col_double(),
    ##   Lower_26 = col_double(),
    ##   Upper_26 = col_double(),
    ##   `Best guess_27` = col_double(),
    ##   Lower_27 = col_double(),
    ##   Upper_27 = col_double(),
    ##   `Best guess_28` = col_double(),
    ##   Lower_28 = col_double(),
    ##   Upper_28 = col_double(),
    ##   `Best guess_29` = col_double()
    ##   # ... with 130 more columns
    ## )

    ## See spec(...) for full column specifications.

Recode "X" to NA

``` r
na_strings <- c("X", "X ", "x", "x ")
byexpert <- byexpert %>% 
  replace_with_na_all(condition=~.x %in% na_strings)
```

Recode "B" to baseline values

``` r
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
```

Standardize group labels if needed

``` r
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "Mature Forest Species")==1)] <- "Mature Forest and Peatland"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "Mature Forest/ Peatland Species")==1)] <- "Mature Forest and Peatland"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "Mature Forest/Peatland Species")==1)] <- "Mature Forest and Peatland"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "Grassland/Open Habitat species")==1)] <- "Grassland or Open Habitats"
```

Output results

``` r
write_csv(byexpert, "Results.csv")
```
