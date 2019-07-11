Generate Benefits Matrix for Complementarity Analysis
================
Adapted for the SJR PTM by Abbey Camaclang
3 July 2019

This code weights benefits by feasibility, recalculates expected performance based on weighted benefit estimates, and generates the benefit matrix for use in the complementarity analysis. Based on 1\_Cost-Effectiveness.R code from FRE PTM project, but using a different (shorter) way to implement

Requires **Aggregated\_Benefits.csv** and **Aggregated\_Baseline.csv** from *aggregateEstimates.R*, and a **CostFeas** table of strategy costs and feasibilities

``` r
library(tidyverse)
```

Use results from *aggregateEstimates.R*

``` r
ben.mat.agg <- read_csv("Aggregated_Benefits.csv")
base.mat.agg <- read_csv("Aggregated_Baseline.csv")
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

print(costfeas)
```

    ## # A tibble: 23 x 3
    ##    Strategy       Cost Feasibility
    ##    <chr>         <dbl>       <dbl>
    ##  1 Baseline         0        1    
    ##  2 S1        12333453.       0.770
    ##  3 S2        29407613.       0.644
    ##  4 S3       206712946.       0.944
    ##  5 S4        87446833.       0.905
    ##  6 S5       188523071.       0.643
    ##  7 S6        32575158.       0.651
    ##  8 S7        54764679.       0.703
    ##  9 S8         3844216.       0.817
    ## 10 S9         9047188.       0.621
    ## # ... with 13 more rows

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

print(wt.ben.t, row.names = FALSE)
```

    ##  Strategy Migratory Fish Riparian Species Aquatic Species Wetland Species
    ##  Baseline       35.25000         48.75000        52.77778        54.09091
    ##        S1       37.81577         56.31903        57.58860        60.73859
    ##        S2       36.85885         57.11599        55.35193        56.72356
    ##        S3       40.36443         62.12621        59.26917        61.38702
    ##        S4       47.32149         62.87364        58.43629        65.61370
    ##        S5       45.26721         56.51735        59.20593        57.30499
    ##        S6       58.94941         59.39920        58.47214        56.16159
    ##        S7       39.35317         52.56008        52.77778        56.00927
    ##        S8       37.97346         54.39498        53.23169        63.37542
    ##        S9       40.16663         51.33770        57.95318        57.47844
    ##       S10       44.00108         49.80488        55.67869        54.96118
    ##       S11       38.89196         48.75000        52.77778        54.09091
    ##       S12       35.56609         48.75000        52.77778        54.09091
    ##       S13       39.44619         50.02933        57.21278        55.31906
    ##       S14       37.81336         51.99693        53.16940        56.65427
    ##       S15       38.82462         52.12964        56.74957        54.80583
    ##       S16       44.08316         54.39911        57.57096        55.63158
    ##       S17       55.72122         65.76504        65.93785        68.51825
    ##       S18       41.21438         60.30599        60.97880        63.93214
    ##       S19       51.64458         65.98533        69.95306        68.88807
    ##       S20       44.70917         55.54878        59.28096        62.03924
    ##       S21       56.07250         56.20189        61.18216        55.39826
    ##       S22       36.46527         61.67603        59.54856        62.11168
    ##      Bats Forest Trees Grassland or Open Habitats
    ##  27.00000     22.22222                   41.00000
    ##  30.84866     22.70330                   46.77299
    ##  28.02966     26.24434                   42.28708
    ##  28.15403     23.99260                   61.30037
    ##  27.00000     22.78807                   45.07413
    ##  27.00000     22.22222                   41.00000
    ##  27.00000     22.22222                   41.00000
    ##  27.00000     22.22222                   42.05510
    ##  27.00000     22.73287                   43.04259
    ##  27.00000     22.22222                   41.93157
    ##  27.36261     28.02404                   41.00000
    ##  37.13126     24.11415                   42.47150
    ##  27.00000     30.75670                   41.00000
    ##  27.00000     22.22222                   45.91261
    ##  27.00000     22.22222                   42.56650
    ##  27.79436     22.22222                   49.93654
    ##  27.00000     23.37772                   48.08707
    ##  38.52454     35.21982                   60.06315
    ##  28.36684     23.39380                   60.13573
    ##  27.00000     22.88281                   44.80205
    ##  27.81290     23.15125                   48.22575
    ##  27.56029     22.22222                   41.00000
    ##  29.02545     22.22222                   54.77304
    ##  Forest Openings and Young Forest Mature Forest and Peatland
    ##                          40.50000                   48.12500
    ##                          51.27624                   60.15206
    ##                          54.01430                   59.78913
    ##                          49.75319                   59.33741
    ##                          41.85804                   50.95426
    ##                          40.50000                   48.12500
    ##                          40.50000                   48.12500
    ##                          40.50000                   48.12500
    ##                          41.31704                   50.16759
    ##                          41.12105                   48.12500
    ##                          40.50000                   48.12500
    ##                          40.50000                   48.12500
    ##                          41.44828                   49.34421
    ##                          40.50000                   49.66019
    ##                          40.50000                   48.51663
    ##                          42.08872                   49.01865
    ##                          44.81387                   55.05800
    ##                          57.83013                   65.95028
    ##                          60.09134                   66.87020
    ##                          41.52758                   54.07029
    ##                          44.83545                   50.44756
    ##                          40.50000                   48.12500
    ##                          48.60179                   55.93744

``` r
write_csv(wt.ben.t, "Benefits.csv") # use this table for the complementarity analysis
```
