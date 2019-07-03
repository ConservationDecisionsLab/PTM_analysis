Calculate Cost-effectiveness
================
Adapted for the SJR PTM by Abbey Camaclang
3 July 2019

This code calculates cost-effectiveness (CE) scores and ranks strategies by Benefit, Cost, and CE Based on algorithm from Step 2 section of 1\_Cost-Effectiveness.R code from FRE PTM project, but using a different way to implement

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

Calculate cost-effectiveness score: CE = (Ben\*Feas)/Cost

``` r
# Get Best Guess estimates and transpose so that Strategies are in rows and Groups are in columns
strat.ben <- data.frame(t(select(ben.mat.agg, contains("Best.guess"))))
names(strat.ben) <- ben.mat.agg$Ecological.Group

# Create vector of strategy names
strat.names <- vector()
strat.names[which(str_detect(rownames(strat.ben), "(?<=_)[:digit:]+")==1)] <- 
  paste0("S",str_extract(rownames(strat.ben)[which(str_detect(rownames(strat.ben), "(?<=_)[:digit:]+")==1)], "(?<=_)[:digit:]+"))
strat.names[which(str_detect(rownames(strat.ben), "(?<=_)[:digit:]+")==0)] <- 
  paste0("Baseline") # Rows without the "_" are Baseline estimates

# Add up (unweighted) aggregated benefits of each strategy across ecological groups
sum.ben <- data.frame(strat.names, rowSums(strat.ben))
names(sum.ben) <- c("Strategy", "Benefit")
sum.ben$Strategy <- as_factor(as.character(sum.ben$Strategy))

# Join with cost/feasibility table and calculate cost-effectiveness
strat.est <- full_join(sum.ben, costfeas, by="Strategy") %>%
  mutate(., Sc.Cost = Cost/1000000, # scale costs to get reasonable values
         Exp.Benefit = Benefit * Feasibility, # weight benefits
         CE = (Benefit * Feasibility)/Sc.Cost) # calculate cost-effectiveness scores
```

Rank strategies by (weighted)Benefit, Cost, and CE score

``` r
CE_Score <- select(strat.est, c("Strategy", "Benefit", "Cost", "Feasibility", "Exp.Benefit","CE")) %>%
  mutate(., CE_rank = rank(-CE), 
         ExpBenefit_rank = rank(-Exp.Benefit), 
         Cost_rank = rank(Cost))
```

Output results

``` r
write_csv(CE_Score, "Cost_Effectiveness.csv")
```
