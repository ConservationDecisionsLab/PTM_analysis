Generate Benefits Matrix for Complementarity Analysis
================
Adapted for the SJR PTM by Abbey Camaclang
3 July 2019

This code weights benefits by feasibility, recalculates expected performance based on weighted benefit estimates, and generates the benefit matrix for use in the complementarity analysis. Based on sequence from Step 2 section of 1\_Cost-Effectiveness.R code from FRE PTM project, but using a different (shorter) way to implement

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

Use result from aggregateEstimates.R

``` r
ben.mat.agg <- read_csv("Aggregated_Benefits.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   Ecological.Group = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
base.mat.agg <- read_csv("Aggregated_Baseline.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Ecological.Group = col_character(),
    ##   Best.guess = col_double(),
    ##   Lower = col_double(),
    ##   Upper = col_double()
    ## )

Create table of Cost and Feasibility FOR TESTING ONLY

``` r
# feas <- c(1, runif(22, min = 0.5, max = 0.95))
# cost <- c(0, runif(22, min = 1000000, max = 400000000))
# Strategy <- c("Baseline", paste0("S", seq(1:22)))
# costfeas <- as.data.frame(cbind(cost, feas))
# names(costfeas)<- c("Cost", "Feasibility")
# costfeas <- cbind(Strategy, costfeas)
# write_csv(costfeas, "sample_CostFeas.csv")
```

Read in Cost & Feasibility table

``` r
costfeas <- read_csv("sample_CostFeas.csv") # sample file created by above code
```

    ## Parsed with column specification:
    ## cols(
    ##   Strategy = col_character(),
    ##   Cost = col_double(),
    ##   Feasibility = col_double()
    ## )

``` r
costfeas <- costfeas[-1,] # Remove baseline values
costfeas$Strategy <- as.character(costfeas$Strategy)
costfeas$Strategy <- as_factor(costfeas$Strategy)
```

Calculate the expected benefits (weighted by feasibility)

``` r
# Tidy data
rlong.ben <- gather(ben.mat.agg, key = Estimate, value = Value, 
                    colnames(ben.mat.agg)[2]:colnames(ben.mat.agg)[ncol(ben.mat.agg)]) %>%
  separate(., Estimate, c("Est.Type", "Strategy"), sep = "[_]", remove = FALSE)
rlong.ben$Strategy <- as_factor(paste0("S", rlong.ben$Strategy))

# Join with cost & feasibility table then weight benefits by feasibility
joined.data <- left_join(rlong.ben, costfeas, by = "Strategy") %>%
  mutate(., Wt.Value = Value * Feasibility)
names(joined.data)[1] <- "Ecological.Group"

# Spread table and output results
joined.wide <- joined.data %>%
  select(., c(Ecological.Group, Estimate, Wt.Value)) %>%
  spread(key = Estimate, value = Wt.Value)
est.levels <- unique(joined.data$Estimate)
joined.wide <- joined.wide[, c("Ecological.Group", est.levels)] # rearranges columns so strategies are in the correct order

write_csv(joined.wide, "Aggregated_Benefits_weighted.csv")
```

Calculate expected performance based on weighted benefit estimates

``` r
# Join with baseline estimates to make sure the observations (Ecol. groups) line up correctly
# then split again to add weighted benefits to (averaged) baseline and get the expected performance
joinedbase.wide <- left_join(base.mat.agg, joined.wide, by = "Ecological.Group") 

base.mat <- joinedbase.wide[,2:4]
perf.mat <- joinedbase.wide[,5:ncol(joinedbase.wide)] + as.matrix(base.mat)

perf.mat <- cbind(joinedbase.wide$Ecological.Group,base.mat,perf.mat)
names(perf.mat)[1] <- "Ecological.Group"

write_csv(perf.mat, "Aggregated_Performance_weighted.csv")
```

Generate (weighted) benefits matrix for optimization

``` r
wt.ben <- perf.mat %>%
  select(., c(Ecological.Group, contains("Best.guess"))) # select only Best Guess estimates from perf.mat
wt.ben.t <- data.frame(t(wt.ben[,-1]))
names(wt.ben.t) <- wt.ben$Ecological.Group # column names

# Create vector of strategy names to add to the table
strat.names <- vector()
strat.names[which(str_detect(rownames(wt.ben.t), "(?<=_)[:digit:]+")==1)] <- 
  paste0("S",str_extract(rownames(wt.ben.t)[which(str_detect(rownames(wt.ben.t), "(?<=_)[:digit:]+")==1)], "(?<=_)[:digit:]+"))
strat.names[which(str_detect(rownames(wt.ben.t), "(?<=_)[:digit:]+")==0)] <- 
  paste0("Baseline") # Rows without the "_" are Baseline estimates

wt.ben.t <- cbind(strat.names,wt.ben.t)
names(wt.ben.t)[1] <- "Strategy"

write_csv(wt.ben.t, "Benefits.csv") # use this table for the complementarity analysis
```
