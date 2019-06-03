Benefit Estimates plots
================
Adapted for the SJR PTM by Abbey Camaclang
31 May 2019

This script standardizes the benefit estimates and creates two plots for each Ecological Group:
1) boxplots of the best guess, lower, and upper estimates for each Strategy from all Experts;
2) pointrange plots showing the best guess, lower and upper estimates of each Expert for each Strategy.

It requires output from combineTables.R, which organizes the estimates into a single table and saves it as 'Results.csv' in the current working directory.

``` r
# Load packages
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.1.1     v purrr   0.3.2
    ## v tibble  2.1.1     v dplyr   0.8.1
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts --------------------------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(cowplot)
```

    ## 
    ## Attaching package: 'cowplot'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     ggsave

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(sjPlot)
```

    ## Warning in checkMatrixPackageVersion(): Package version inconsistency detected.
    ## TMB was built with Matrix version 1.2.17
    ## Current Matrix version is 1.2.15
    ## Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package

    ## Learn more about sjPlot with 'browseVignettes("sjPlot")'.

    ## 
    ## Attaching package: 'sjPlot'

    ## The following objects are masked from 'package:cowplot':
    ## 
    ##     plot_grid, save_plot

``` r
library(stringr)
library(RColorBrewer)
```

Read in and tidy data ---------------------------------------------------
-------------------------------------------------------------------------

``` r
results <- read.csv("Results.csv")
head(results)
```

    ##   Expert               Ecological.Group Best.guess Lower Upper Confidence
    ## 1      1                 Migratory Fish         65    45    75         70
    ## 2      1               Riparian Species         60    40    60         60
    ## 3      1                Aquatic Species         65    40    70         70
    ## 4      1                Wetland Species         65    60    70         65
    ## 5      1 Grassland/Open Habitat species         NA    NA    NA         NA
    ## 6      1          Mature Forest Species         NA    NA    NA         NA
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

``` r
# AC: need to standardize labels - should probably do it in combineTables.R instead
results$Ecological.Group <- as.character(results$Ecological.Group)
results$Ecological.Group[which(results$Ecological.Group=="Mature Forest Species")] <- "Mature Forest and Peatland"
results$Ecological.Group[which(results$Ecological.Group=="Mature Forest/ Peatland Species")] <- "Mature Forest and Peatland"
results$Ecological.Group[which(results$Ecological.Group=="Mature Forest/Peatland Species")] <- "Mature Forest and Peatland"
```

Use tidyr package to transform data to tidy version, with single columns for Estimate (e.g., best guess, lower, upper) and Value (value of estimate, 0-100)

``` r
rlong <-
  gather(results,
         key = Estimate,
         value = Value,
         Best.guess:Confidence_22) #' <!-- AC: updated with the SJR data column names -->
head(rlong)
```

    ##   Expert               Ecological.Group   Estimate Value
    ## 1      1                 Migratory Fish Best.guess    65
    ## 2      1               Riparian Species Best.guess    60
    ## 3      1                Aquatic Species Best.guess    65
    ## 4      1                Wetland Species Best.guess    65
    ## 5      1 Grassland/Open Habitat species Best.guess    NA
    ## 6      1     Mature Forest and Peatland Best.guess    NA

``` r
rlong <- na.omit(rlong)
write_csv(rlong, "Results_tidy.csv")

rlong$Value <- as.numeric((rlong$Value))
# str(rlong) # Check data type
```

Summarize the number of expert estimates --------------------------------
-------------------------------------------------------------------------

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
    ##   Grassland/Open Habitat species       Mature Forest and Peatland 
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
    ##   Grassland/Open Habitat species         10 10 10 10 10 10 10 10 10 10   9
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
    ##   Grassland/Open Habitat species     9   9  10  10  10  11  10   9   9   9
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
    ##   Grassland/Open Habitat species     8   9
    ##   Mature Forest and Peatland         6   7
    ##   Migratory Fish                    11   9
    ##   Riparian Species                  10  11
    ##   Wetland Species                    9  10

Standardize to a specified confidence level -----------------------------
-------------------------------------------------------------------------

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

Combine into a new table and saves standardized estimates as a .csv file

``` r
rlong.std <- rbind(low.new, up.new, bg.new, conf.new) 

rlong.std$St.Value[rlong.std$St.Value < 0] <- 0 # Change any negative standardized values to zero
rlong.std$St.Value[rlong.std$St.Value > 100] <- 100

rlong.sub <- rlong.std[,c(1,2,3,7)] # subsets only relevant columns
```

Save standardized estimates into a new table, in the same format as Results.csv

``` r
est.levels <- unique(rlong$Estimate)
rlong.wide <- spread(rlong.sub, Estimate, St.Value)
rlong.wide <- rlong.wide[, c("Expert", "Ecological.Group", est.levels)] 

grp.levels <- unique(rlong.std$Ecological.Group)
rlong.wide$Ecological.Group<-factor(rlong.wide$Ecological.Group, levels=grp.levels) 

rlong.wide <- with(rlong.wide, rlong.wide[order(Expert, Ecological.Group),])
write_csv(rlong.wide, "Standardized_Estimates_Wide.csv") 
```

Create plots ------------------------------------------------------------
-------------------------------------------------------------------------

Order the strategies to plot in the desired order

``` r
rlong.std$Strategy <- factor(rlong.std$Strategy,levels = strat.levels)
rlong.std <- subset(rlong.std, Est.Type %in% c("Best.Guess", "Lower", "Upper")) # Subset to remove confidence estimates

# Rename Grassland/Open Habitat Species as it doesn't work as a filename as is (Should do this in combineTables.R instead)
grp.names <- grp.levels
grp.names[which(grp.levels=="Grassland/Open Habitat species")] <- paste0("Grassland Species")
```

Plot group estimates as boxplots and save as .pdf

``` r
for (i in seq_along(grp.levels)) {
  temp.grpdata <- subset(rlong.std, Ecological.Group == grp.levels[i])
  plot1 <-
    ggplot(temp.grpdata, aes(x = Est.Type, y = St.Value, fill = Est.Type)) + # using the data Ecological group, graph Estimate Type on x-axis and St.Value on y-axis, and colour the boxplots by estimate type
    geom_boxplot() +                                 # tell R to display data in boxplot form
    # geom_point(
    #   data = subset(temp.grpdata, Expert == "1"),
    #   # tell R to graph Expert 1's estimates for the ecol grp as red datapoints on top of the boxplots (keeping Estimate Type on x-axis and St.Value on y-axis)
    #   aes(x = Est.Type, y = St.Value),
    #   color = 'red'
    # ) +
    theme_cowplot() +  # use the theme "cowplot" for the plots, which is a nice minimalist theme
    theme(
      plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
      # adjust margins around the outside of the plot (bottom=0,left=1,top=0,right=0.5)
      panel.spacing = unit(1, "lines"),
      # adjust margins and between panels of the plot (spacing of 1)
      axis.title.y = element_text(margin = margin(
        t = 0,
        r = 10,
        b = 0,
        l = 0
      ))
    ) + # adjust space between y-axis numbers and y-axis label
    facet_wrap( ~ Strategy, nrow = 3) +  # tell R to create a separate panel of estimates for each management strategy
    scale_x_discrete(
      name = "",
      breaks = c("Best.Guess", "Lower", "Upper"),
      labels = c("B", "L", "U")
    ) + # Give the x-axis variables shortened labels
    scale_fill_manual(values = c("gray80", "white", "white"),
                      guide = FALSE) + # Assign colours to each type of estimate and don't show a legend
    labs(x = "", y = "Probability of persistence (%)", title = paste(grp.levels[i])) +  # put a horizontal label for the species group on the y axis
    ylim(0, 100) # set the y-axis limits from 0-100
  
  # plot1 # print the plot onscreen
  
  # Save plots as .pdf, one file per ecological group
  ggsave(plot1, file=paste0(grp.names[i], ".pdf", sep=''), width = 10, height = 8, units = "in")
}
```

Plot each expert estimate separately (x-axis = Expert, y-axis point = Best guess, range = lower-&gt;upper)

``` r
rlong.sub2 <- rlong.std[,c(1,2,5,6,7)]
rlong.std.wide <- spread(rlong.sub2,key=Est.Type,value=St.Value)
rlong.std.wide$Expert<-as.factor(rlong.std.wide$Expert)

for (i in seq_along(grp.levels)) {
  temp.expdata <- subset(rlong.std.wide, Ecological.Group == grp.levels[i])
  plot2 <-
    ggplot(temp.expdata, aes(x = Expert, y = Best.Guess, color = Expert)) + # using the data Ecological group, graph Estimate Type on x-axis and St.Value on y-axis, and colour the boxplots by estimate type
    geom_pointrange(aes(ymin = Lower, ymax = Upper))+
    theme_cowplot() +  # use the theme "cowplot" for the plots, which is a nice minimalist theme
    theme(
      plot.margin = unit(c(0, 1, 0, 0.75), "cm"), # adjust margins around the outside of the plot (bottom=0,left=1,top=0,right=0.5)
      panel.spacing = unit(1, "lines"), # adjust margins and between panels of the plot (spacing of 1)
      # panel.background = element_rect(fill='gray'),
      axis.title.y = element_text(margin = margin(
        t = 0,
        r = 10,
        b = 0,
        l = 0
      )),
      axis.text.x = element_blank(),
      legend.justification=c(1,0), legend.position=c(0.98,-0.05)
    ) + # adjust space between y-axis numbers and y-axis label, removes x-axis tick labels, repositions legend box
    # scale_color_brewer(palette='Paired') + # changes color palette
    facet_wrap( ~ Strategy, nrow = 3) +  # tell R to create a separate panel of estimates for each management strategy
    labs(x = "Experts", y = "Probability of persistence (%)", title = paste(grp.levels[i])) +  # put a horizontal label for the species group on the y axis
    ylim(0, 100) # set the y-axis limits from 0-100
  
  # plot1 # print the plot onscreen
  
  # Save plots as .pdf, one file per ecological group
  ggsave(plot2, file=paste0(grp.names[i],"_byExp.pdf", sep=''), width = 10, height = 8, units = "in")
}
```
