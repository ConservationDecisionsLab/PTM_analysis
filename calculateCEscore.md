Calculate Cost-effectiveness
================
Adapted for the SJR PTM by Abbey Camaclang
3 July 2019

This code calculates cost-effectiveness (CE) scores and ranks strategies by Benefit, Cost, and CE Based on algorithm from Step 2 section of 1\_Cost-Effectiveness.R code from FRE PTM project, but using a different way to implement

Requires **Aggregated\_Benefits.csv** from *aggregateEstimates.R*, and a **CostFeas** table of strategy cost and feasibility

``` r
library(tidyverse)
```

Use result from aggregateEstimates.R

``` r
ben.mat.agg <- read_csv("Aggregated_Benefits.csv")
```

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

print(CE_Score)
```

    ##    Strategy   Benefit      Cost Feasibility Exp.Benefit         CE CE_rank
    ## 1        S1  70.80303  12333453   0.7697317    54.49934 4.41882242       2
    ## 2        S2  72.56591  29407613   0.6435381    46.69892 1.58798759       4
    ## 3        S3  80.45783 206712946   0.9442032    75.96854 0.36750740       9
    ## 4        S4  57.66061  87446833   0.9053618    52.20371 0.59697656       7
    ## 5        S5  42.66667 188523071   0.6428155    27.42679 0.14548243      17
    ## 6        S6  64.71212  32575158   0.6507847    42.11366 1.29281527       5
    ## 7        S7  15.47727  54764679   0.7033998    10.88671 0.19879074      13
    ## 8        S8  28.78662   3844216   0.8170373    23.51974 6.11821398       1
    ## 9        S9  28.37121   9047188   0.6210481    17.61989 1.94755392       3
    ## 10      S10  34.02652 281312042   0.5801821    19.74157 0.07017678      21
    ## 11      S11  25.87937 167454976   0.6621740    17.13664 0.10233583      19
    ## 12      S12  19.36508 146132376   0.5689654    11.01806 0.07539780      20
    ## 13      S13  28.63889 135914061   0.6140765    17.58647 0.12939404      18
    ## 14      S14  17.11364 231132457   0.6266001    10.72341 0.04639507      22
    ## 15      S15  33.36616 100833056   0.7149231    23.85424 0.23657163      12
    ## 16      S16  65.40278 161311705   0.6162669    40.30556 0.24986137      11
    ## 17      S17 184.41144 199961817   0.7798561   143.81438 0.71920918       6
    ## 18      S18 116.53824 349100148   0.8201025    95.57331 0.27377045      10
    ## 19      S19  83.30014 142146889   0.9248227    77.03786 0.54195953       8
    ## 20      S20  71.25000 251764025   0.6503179    46.33515 0.18404198      15
    ## 21      S21  49.14069 257429607   0.7844094    38.54642 0.14973577      16
    ## 22      S22  83.17172 311106574   0.7291607    60.64555 0.19493497      14
    ##    ExpBenefit_rank Cost_rank
    ## 1                6         3
    ## 2                8         4
    ## 3                4        16
    ## 4                7         7
    ## 5               13        14
    ## 6               10         5
    ## 7               21         6
    ## 8               15         1
    ## 9               17         2
    ## 10              16        20
    ## 11              19        13
    ## 12              20        11
    ## 13              18         9
    ## 14              22        17
    ## 15              14         8
    ## 16              11        12
    ## 17               1        15
    ## 18               2        22
    ## 19               3        10
    ## 20               9        18
    ## 21              12        19
    ## 22               5        21

Output results

``` r
write_csv(CE_Score, "Cost_Effectiveness.csv")
```
