Standardize Benefit Estimates
================
Adapted for the SJR PTM by Abbey Camaclang
28 June 2019

This script standardizes the benefit estimates and saves results tables as .csv files: 1) Results\_tidy.csv - Tidy (long) version of the original Results.csv file 2) Estimates\_per\_group.csv - Number of expert estimates for each ecological group 3) Estimates\_by\_strategy.csv - Number of expert estimates there are for each strategy+group 4) Standardized\_Estimates\_Wide.csv - Standardized estimates in same table format as Results.csv 5) Standardized\_Estimates\_Long.csv - Tidy version of Standardized estimates - for use in plotting

It requires output from combineTables.R, which organizes the estimates into a single table and saves it as 'Results.csv' in the current working directory.

Load packages

``` r
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
library(stringr)
```

Read in and tidy data
---------------------

``` r
results <- read.csv("Results.csv")
head(results)
```

    ##   Expert           Ecological.Group Best.guess Lower Upper Confidence
    ## 1      1             Migratory Fish         65    45    75         70
    ## 2      1           Riparian Species         60    40    60         60
    ## 3      1            Aquatic Species         65    40    70         70
    ## 4      1            Wetland Species         65    60    70         65
    ## 5      1 Grassland or Open Habitats         NA    NA    NA         NA
    ## 6      1 Mature Forest and Peatland         NA    NA    NA         NA
    ##   Best.guess_1 Lower_1 Upper_1 Confidence_1 Best.guess_2 Lower_2 Upper_2
    ## 1           80      70      90           75           70      60      75
    ## 2           75      60      80           80           75      65      85
    ## 3           NA      NA      NA           NA           65      40      70
    ## 4           80      70      90           85           75      70      80
    ## 5           NA      NA      NA           NA           NA      NA      NA
    ## 6           NA      NA      NA           NA           NA      NA      NA
    ##   Confidence_2 Best.guess_3 Lower_3 Upper_3 Confidence_3 Best.guess_4
    ## 1           90           75      60      80           80           80
    ## 2           90           80      70      90           85           80
    ## 3           70           NA      NA      NA           NA           NA
    ## 4           85           75      70      80           85           70
    ## 5           NA           NA      NA      NA           NA           NA
    ## 6           NA           NA      NA      NA           NA           NA
    ##   Lower_4 Upper_4 Confidence_4 Best.guess_5 Lower_5 Upper_5 Confidence_5
    ## 1      70      90           85           75      50      80           90
    ## 2      60      85           75           80      65      95           85
    ## 3      NA      NA           NA           65      40      70           70
    ## 4      50      80           80           65      60      70           65
    ## 5      NA      NA           NA           NA      NA      NA           NA
    ## 6      NA      NA           NA           NA      NA      NA           NA
    ##   Best.guess_6 Lower_6 Upper_6 Confidence_6 Best.guess_7 Lower_7 Upper_7
    ## 1           75      70      90           85           65      45      75
    ## 2           95      80      95           90           80      75      95
    ## 3           65      40      70           70           65      40      70
    ## 4           65      60      70           65           65      60      70
    ## 5           NA      NA      NA           NA           NA      NA      NA
    ## 6           NA      NA      NA           NA           NA      NA      NA
    ##   Confidence_7 Best.guess_8 Lower_8 Upper_8 Confidence_8 Best.guess_9
    ## 1           70           85      80      90           90           75
    ## 2           90           85      80      90           90           80
    ## 3           70           65      40      70           70           65
    ## 4           65           80      70      90           95           65
    ## 5           NA           NA      NA      NA           NA           NA
    ## 6           NA           NA      NA      NA           NA           NA
    ##   Lower_9 Upper_9 Confidence_9 Best.guess_10 Lower_10 Upper_10
    ## 1      60      80           90            75       45       85
    ## 2      70      85           85            75       60       80
    ## 3      40      70           70            NA       NA       NA
    ## 4      60      70           65            80       70       90
    ## 5      NA      NA           NA            NA       NA       NA
    ## 6      NA      NA           NA            NA       NA       NA
    ##   Confidence_10 Best.guess_11 Lower_11 Upper_11 Confidence_11
    ## 1            75            70       60       80            85
    ## 2            85            NA       NA       NA            NA
    ## 3            NA            NA       NA       NA            NA
    ## 4            75            NA       NA       NA            NA
    ## 5            NA            NA       NA       NA            NA
    ## 6            NA            NA       NA       NA            NA
    ##   Best.guess_12 Lower_12 Upper_12 Confidence_12 Best.guess_13 Lower_13
    ## 1            NA       NA       NA            NA            70       50
    ## 2            NA       NA       NA            NA            80       70
    ## 3            NA       NA       NA            NA            80       60
    ## 4            NA       NA       NA            NA            NA       NA
    ## 5            NA       NA       NA            NA            NA       NA
    ## 6            NA       NA       NA            NA            NA       NA
    ##   Upper_13 Confidence_13 Best.guess_14 Lower_14 Upper_14 Confidence_14
    ## 1       80            75            80       50       80            75
    ## 2       90            90            80       70       85            80
    ## 3       85            80            NA       NA       NA            NA
    ## 4       NA            NA            75       65       85            80
    ## 5       NA            NA            NA       NA       NA            NA
    ## 6       NA            NA            NA       NA       NA            NA
    ##   Best.guess_15 Lower_15 Upper_15 Confidence_15 Best.guess_16 Lower_16
    ## 1            75       65       85            85            70       50
    ## 2            80       70       90            80            70       50
    ## 3            75       70       80            85            75       70
    ## 4            NA       NA       NA            NA            NA       NA
    ## 5            NA       NA       NA            NA            50       40
    ## 6            NA       NA       NA            NA            NA       NA
    ##   Upper_16 Confidence_16 Best.guess_17 Lower_17 Upper_17 Confidence_17
    ## 1       75            80            70       50       80            98
    ## 2       80            80            75       60       80            75
    ## 3       90            90            75       70       80            85
    ## 4       NA            NA            75       65       85            80
    ## 5       70            75            50       40       70            75
    ## 6       NA            NA            NA       NA       NA            NA
    ##   Best.guess_18 Lower_18 Upper_18 Confidence_18 Best.guess_19 Lower_19
    ## 1            75       65       80            80            70       50
    ## 2            75       65       90            85            80       70
    ## 3            NA       NA       NA            NA            NA       NA
    ## 4            75       70       80            85            75       60
    ## 5            NA       NA       NA            NA            NA       NA
    ## 6            NA       NA       NA            NA            NA       NA
    ##   Upper_19 Confidence_19 Best.guess_20 Lower_20 Upper_20 Confidence_20
    ## 1       80            70            65       60       80            75
    ## 2       80            85            80       60       80            80
    ## 3       NA            NA            75       70       85            90
    ## 4       80            85            NA       NA       NA            NA
    ## 5       NA            NA            NA       NA       NA            NA
    ## 6       NA            NA            NA       NA       NA            NA
    ##   Best.guess_21 Lower_21 Upper_21 Confidence_21 Best.guess_22 Lower_22
    ## 1            70       45       70            90            60       55
    ## 2            NA       NA       NA            NA            80       70
    ## 3            NA       NA       NA            NA            NA       NA
    ## 4            NA       NA       NA            NA            75       70
    ## 5            NA       NA       NA            NA            NA       NA
    ## 6            NA       NA       NA            NA            NA       NA
    ##   Upper_22 Confidence_22
    ## 1       85            80
    ## 2       85            80
    ## 3       NA            NA
    ## 4       85            85
    ## 5       NA            NA
    ## 6       NA            NA

Use tidyr package to transform data to tidy version, with single columns for Estimate (e.g., best guess, lower, upper) and Value (value of estimate, 0-100)

``` r
rlong <-
  gather(results,
         key = Estimate,
         value = Value,
         Best.guess:Confidence_22) #' <!-- AC: updated with the SJR data column names -->
head(rlong)
```

    ##   Expert           Ecological.Group   Estimate Value
    ## 1      1             Migratory Fish Best.guess    65
    ## 2      1           Riparian Species Best.guess    60
    ## 3      1            Aquatic Species Best.guess    65
    ## 4      1            Wetland Species Best.guess    65
    ## 5      1 Grassland or Open Habitats Best.guess    NA
    ## 6      1 Mature Forest and Peatland Best.guess    NA

``` r
rlong <- na.omit(rlong)
write_csv(rlong, "Results_tidy.csv")

rlong$Value <- as.numeric((rlong$Value))
# str(rlong) # Check data type
```

Summarize the number of expert estimates
----------------------------------------

Tabulate how many expert estimates there are for each ecological group

``` r
table.data <-spread(rlong, Estimate, Value) 
table.subset <- table.data[, c(1, 2)] # Subset table.data to only include the columns "Expert" and "Ecological.Group"
exp.table <- table(table.subset$Ecological.Group)
write.csv(exp.table, "Estimates_per_group.csv", row.names=FALSE)
exp.table
```

    ## 
    ##                  Aquatic Species                             Bats 
    ##                                9                               10 
    ## Forest Openings and Young Forest                     Forest Trees 
    ##                               11                                9 
    ##       Grassland or Open Habitats       Mature Forest and Peatland 
    ##                               11                                8 
    ##                   Migratory Fish                 Riparian Species 
    ##                               12                               12 
    ##                  Wetland Species 
    ##                               11

Create new columns to specify Estimate Type and Strategy separately

``` r
# Find the "_" character and uses the digits that follow it as the Strategy name. 
rlong$Strategy <- "BLANK"
rlong$Strategy[which(str_detect(rlong$Estimate, "(?<=_)[:digit:]+")==1)] <- 
  paste0("S",str_extract(rlong$Estimate[which(str_detect(rlong$Estimate, "(?<=_)[:digit:]+")==1)], "(?<=_)[:digit:]+"))
rlong$Strategy[which(str_detect(rlong$Estimate, "(?<=_)[:digit:]+")==0)] <- 
  paste0("Baseline") # Rows without the "_" are Baseline estimates

# Create a new column for type of estimate
rlong$Est.Type <- "BLANK" # creates a new column in the table #' <!-- AC: not sure if this is really needed -->
rlong$Est.Type[grep("Best.guess", rlong$Estimate)] <- "Best.Guess"
rlong$Est.Type[grep("Lower", rlong$Estimate)] <- "Lower"
rlong$Est.Type[grep("Upper", rlong$Estimate)] <- "Upper"
rlong$Est.Type[grep("Confidence", rlong$Estimate)] <- "Confidence"
```

Tabulate how many estimates there are for each strategy

``` r
table.subset2 <- subset(rlong, Est.Type=="Best.Guess") # Subset to count how many experts provided estimates for each group and strategy
strat.levels <- unique(table.subset2$Strategy)
table.subset2$Strategy <- factor(table.subset2$Strategy,levels = strat.levels) #' <!-- AC: Note that using as.factor() changes the order to alphabetical -->
st.table <- table(table.subset2$Ecological.Group, table.subset2$Strategy)
write.csv(st.table, "Estimates_by_strategy.csv")
st.table
```

    ##                                   
    ##                                    Baseline S1 S2 S3 S4 S5 S6 S7 S8 S9 S10
    ##   Aquatic Species                         9  8  9  8  8  9  8  9  9  9   8
    ##   Bats                                   10 10 10  9  9  9  9  9  9  9   8
    ##   Forest Openings and Young Forest       10 10 10 10 10 10 10 10 10 10  10
    ##   Forest Trees                            9  8  8  8  8  8  8  8  8  8   7
    ##   Grassland or Open Habitats             10 10 10 10 10 10 10 10 10 10   9
    ##   Mature Forest and Peatland              8  8  8  8  8  8  8  8  8  8   7
    ##   Migratory Fish                         12 12 12 12 12 12 12 12 12 12  12
    ##   Riparian Species                       12 12 11 12 10 12 11 12 11 12  11
    ##   Wetland Species                        11 11 11 11 11 11 11 11 11 11  10
    ##                                   
    ##                                    S11 S12 S13 S14 S15 S16 S17 S18 S19 S20
    ##   Aquatic Species                    7   7   9   8   9   9   8   7   7   8
    ##   Bats                              10   8   9   9   9   9   9   9   8   8
    ##   Forest Openings and Young Forest   9   9  10  10   9  10   9   9   9   9
    ##   Forest Trees                       7   8   8   8   8   8   6   7   7   7
    ##   Grassland or Open Habitats         9   9  10  10  10  11  10   9   9   9
    ##   Mature Forest and Peatland         7   7   8   8   8   8   7   7   7   7
    ##   Migratory Fish                    10   9  12  11  11  12  12  11  11  11
    ##   Riparian Species                  10  10  12  11  11  12  11  11  11  11
    ##   Wetland Species                    9   9  10  11  10  10  10  10  10   9
    ##                                   
    ##                                    S21 S22
    ##   Aquatic Species                    7   7
    ##   Bats                               7   9
    ##   Forest Openings and Young Forest   8   9
    ##   Forest Trees                       6   5
    ##   Grassland or Open Habitats         8   9
    ##   Mature Forest and Peatland         6   7
    ##   Migratory Fish                    11   9
    ##   Riparian Species                  10  11
    ##   Wetland Species                    9  10

Standardize to a specified confidence level
-------------------------------------------

Standardize upper and lower estimates to 80% confidence.
1) For each estimate, divide confidence by 100
2) Take the upper and lower estimates, and standardize using
\* Lower standardized interval: B−((B−L)×(S∕C))
\* Upper standardized interval: B+((U−B)×(S∕C))
3) Leave Best Guess estimate as is

``` r
S <- 0.8 # confidence level to standardize estimates to
```

Subset dataframe by estimate type

``` r
bg <- subset(rlong, Est.Type == "Best.Guess")
low <- subset(rlong, Est.Type == "Lower")
up <- subset(rlong, Est.Type == "Upper")
conf <- subset(rlong, Est.Type == "Confidence")
```

Check that order of rows are the same

``` r
# Results should equal the number of rows in tables, if all entries are matching
# test1 <- sum(str_detect(bg$Ecological.Group,low$Ecological.Group)) 
# test2 <- sum(str_detect(bg$Ecological.Group,up$Ecological.Group))
# test3 <- sum(str_detect(bg$Ecological.Group,conf$Ecological.Group))
# test4 <- sum(bg$Expert == low$Expert) # checks that Experts also align
```

Create copies of the datasets and add new columns with the standardized estimates

``` r
low.new <- low
up.new <- up
bg.new <- bg
conf.new <- conf

conf.new$St.Value <- (conf$Value) / 100
low.new$St.Value <-
  bg$Value - (((bg$Value) - (low$Value)) * (S / (conf.new$St.Value)))
up.new$St.Value <-
  bg$Value + (((up$Value) - (bg$Value)) * (S / (conf.new$St.Value)))
bg.new$St.Value <- bg$Value
```

Combine into new tables and saves standardized estimates into .csv files

``` r
rlong.std <- rbind(low.new, up.new, bg.new, conf.new) 

# Constrain standardized values to 0 - 100
rlong.std$St.Value[rlong.std$St.Value < 0] <- 0 
rlong.std$St.Value[rlong.std$St.Value > 100] <- 100

# Create new table in wide format
rlong.sub <- rlong.std[,c(1,2,3,7)] 
rlong.wide <- spread(rlong.sub, Estimate, St.Value)

# Make sure Strategies are in correct order
est.levels <- unique(rlong$Estimate)
rlong.wide <- rlong.wide[, c("Expert", "Ecological.Group", est.levels)] 

# Make sure Ecological groups are in the same order as in original tables
grp.levels <- unique(rlong.std$Ecological.Group)
rlong.wide$Ecological.Group<-factor(rlong.wide$Ecological.Group, levels=grp.levels) 
rlong.wide <- with(rlong.wide, rlong.wide[order(Expert, Ecological.Group),]) 

# Output results
write_csv(rlong.wide, "Standardized_Estimates_Wide.csv") 
write_csv(rlong.std, "Standardized_Estimates_Long.csv")
```
