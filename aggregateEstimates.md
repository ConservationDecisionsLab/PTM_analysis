Aggregate Standardized Benefit Estimates
================
Adapted for the SJR PTM by Abbey Camaclang
24 July 2019

This code
a) calculates benefits of each strategy (strategy performance - baseline performance) for each ecological group,
b) aggregates (averages) across experts, and
c) calculates expected performance under each strategy based on the aggregated benefit estimate.
Based on first part of Step 2 section of 1\_Cost-Effectiveness.R code from FRE PTM project and uses **Standardized\_Estimates\_Wide.csv** from *standardizeConfidence.R*. If some of the expert estimates need to be weighted differently, must provide a table listing the species in each ecological group *EcolGroupsList.csv* and a table *SpecialCases.csv* indicating which expert estimates for which ecological groups and strategies require different weights, and the number of species scored for that estimate.

``` r
library(tidyverse)
```

Specify how estimates should be aggregated

``` r
#wt.by.numspp <- readline(prompt="Weight estimates by number of species scored? Enter 1 for Yes, 0 for No: ") # if experts scored only some of the species in a group, weight estimates by the number of species scored for each strategy
#wt.by.numspp <- as.numeric(wt.by.numspp)
wt.by.numspp <- 1
```

Read in and prepare data

``` r
rlong.wide <- read_csv("Standardized_Estimates_Wide.csv")
rlong.wide$Expert <- as_factor(rlong.wide$Expert)
rlong.wide$Ecological.Group <- as_factor(rlong.wide$Ecological.Group)

# Remove confidence estimates from the table
wide.colnames <- colnames(rlong.wide)
idx.colnames <- which(str_detect(wide.colnames, "Confidence") == 1)
DF <- rlong.wide[,-(idx.colnames)]
```

Calculate benefit: subtract baseline performance from strategy performance for each expert

``` r
base.mat <- DF[3:5]
strat.mat <- DF[6:ncol(DF)]  

ben.mat <- strat.mat - as.matrix(base.mat)
ben.mat[ben.mat < 0] <- 0

ben.mat <- cbind(DF[,1:2], ben.mat )
base.mat <- cbind(DF[,1:2], base.mat)
```

Aggregate benefit estimates: average benefit estimates for each species group + strategy across experts

``` r
if (wt.by.numspp == 1) {
  
  # Re-organize benefits table to make it easier to weight estimates
  ben.mat.long <- gather(ben.mat, key = Estimate, value = StValue, -c(1:2)) %>%
    separate(., Estimate, c("Est.Type", "Strategy"), sep = "[_]", remove = TRUE)
  strat.levels <- c("Baseline", unique(ben.mat.long$Strategy))
  ben.mat.long$Strategy <- factor(ben.mat.long$Strategy, levels = strat.levels)
  ben.mat.long$Est.Type <- as_factor(ben.mat.long$Est.Type)
  
  ben.mat.wide <- spread(ben.mat.long, key=Est.Type, value = StValue)
  
  # Get number of species in each group and number of species scored by each expert for each strategy
  grplist <- read_csv("EcolGroupsList.csv") # Table of species in each ecological group
  numspp <- apply(grplist, MARGIN = 2, FUN = function(x) length(x[!is.na(x)]) )
  grpwts <- data.frame(Ecological.Group=names(numspp), numspp) 
  grpwts$Ecological.Group <- factor(grpwts$Ecological.Group, levels = unique(names(numspp)))

  spcases <- read_csv("SpecialCases.csv") # Table of number of species in each group scored by individual experts (if different from total)
  spcases$Strategy <- factor(spcases$Strategy, levels = levels(ben.mat.wide$Strategy))
  spcases$Expert <- factor(spcases$Expert, levels = levels(ben.mat.wide$Expert))
  spcases$`Ecological Group`<- factor(spcases$`Ecological Group`, levels = levels(ben.mat.wide$Ecological.Group))
  names(spcases)[which(str_detect(names(spcases), "Ecological Group")==1)] <- "Ecological.Group"  

  # Combine tables to calculate weights for each expert - group - strategy 
  ben.mat.joined <- left_join(ben.mat.wide, spcases, by=c("Expert", "Ecological.Group", "Strategy")) %>%
    left_join(., grpwts, by = "Ecological.Group")
  fullwts.idx <- which(is.na(ben.mat.joined$NumSppScored))
  ben.mat.joined$NumSppScored[fullwts.idx] <- ben.mat.joined$numspp[fullwts.idx]

  fullwts <- aggregate(ben.mat.joined$NumSppScored, by = list(Ecological.Group = ben.mat.joined$Ecological.Group, Strategy = ben.mat.joined$Strategy), FUN = sum, na.rm = TRUE)
  ben.mat.joined <- ben.mat.joined %>%
    left_join(., fullwts, by = c("Ecological.Group", "Strategy")) %>%
    mutate(Wts = NumSppScored/x) %>%
    mutate(Wt.Best.guess = Best.guess*Wts, Wt.Lower = Lower*Wts, Wt.Upper = Upper*Wts)

  # Aggregate (sum) the weighted estimates and re-organize table for calculating performance
  ben.mat.agg <- aggregate(ben.mat.joined[,11:13], by = list(Ecological.Group = ben.mat.joined$Ecological.Group, Strategy = ben.mat.joined$Strategy), FUN = sum, na.rm = TRUE) %>%
    gather(., key = "Est.Type", value = "Wt.Avg", Wt.Best.guess, Wt.Lower, Wt.Upper)
  ben.mat.agg$Est.Type <- as_factor(ben.mat.agg$Est.Type)

  ben.mat.agg <- ben.mat.agg %>%
    arrange(Ecological.Group, Strategy, Est.Type) %>%
    unite(., col = "Estimate", c("Est.Type", "Strategy"), sep = "_", remove = TRUE)
  ben.mat.agg$Estimate <- as_factor(ben.mat.agg$Estimate)

  ben.mat.agg <- ben.mat.agg %>%
    spread(., Estimate, Wt.Avg)
  
  # Do the same for the baseline estimates table
  base.mat <- base.mat %>%
    add_column(Strategy = rep("Baseline", nrow(base.mat)), .before = "Best.guess")
  base.mat$Strategy <- factor(base.mat$Strategy, levels = strat.levels)
  
  base.mat.joined <- left_join(base.mat, spcases, by = c("Expert", "Ecological.Group", "Strategy")) %>%
    left_join(., grpwts, by = "Ecological.Group")
  base.fullwts.idx <- which(is.na(base.mat.joined$NumSppScored))
  base.mat.joined$NumSppScored[base.fullwts.idx] <- base.mat.joined$numspp[base.fullwts.idx]

  base.fullwts <- aggregate(base.mat.joined$NumSppScored, by = list(Ecological.Group = base.mat.joined$Ecological.Group, Strategy = base.mat.joined$Strategy), FUN = sum, na.rm = TRUE)
  base.mat.joined <- base.mat.joined %>%
    left_join(., base.fullwts, by = c("Ecological.Group", "Strategy")) %>%
    mutate(Wts = NumSppScored/x) %>%
    mutate(Wt.Best.guess = Best.guess*Wts,
         Wt.Lower = Lower*Wts,
         Wt.Upper = Upper*Wts)

  base.mat.agg <- aggregate(base.mat.joined[,11:13], by = list(Ecological.Group = base.mat.joined$Ecological.Group, Strategy = base.mat.joined$Strategy), FUN = sum, na.rm = TRUE) %>%
    select(., -Strategy)
  
  } else {
    
    if (wt.by.numspp == 0) {
      
      # Calculate the simple average
      ben.mat.agg <- aggregate(ben.mat[,3:ncol(ben.mat)], by=list(ben.mat$Ecological.Group), FUN = mean, na.rm = TRUE) 
      base.mat.agg <- aggregate(base.mat[,3:ncol(base.mat)], by=list(base.mat$Ecological.Group), FUN = mean, na.rm = TRUE)
      
      names(ben.mat.agg)[1] <- "Ecological.Group"
      names(base.mat.agg)[1] <- "Ecological.Group"
      
    }
  }
```

    ## Parsed with column specification:
    ## cols(
    ##   `Migratory Fish` = col_character(),
    ##   `Riparian Species` = col_character(),
    ##   `Aquatic Species` = col_character(),
    ##   `Wetland Species` = col_character(),
    ##   `Grassland or Open Habitat Species` = col_character(),
    ##   `Mature Forest and Peatland Species` = col_character(),
    ##   `Forest Openings and Young Forest Species` = col_character(),
    ##   Bats = col_character(),
    ##   `Forest Trees` = col_character()
    ## )

    ## Parsed with column specification:
    ## cols(
    ##   Expert = col_double(),
    ##   `Ecological Group` = col_character(),
    ##   Strategy = col_character(),
    ##   NumSppScored = col_double()
    ## )

``` r
print(ben.mat.agg)
```

    ##                           Ecological.Group Wt.Best.guess_1 Wt.Lower_1
    ## 1                           Migratory Fish        2.459016  0.3903201
    ## 2                         Riparian Species        9.704545  8.1750504
    ## 3                          Aquatic Species        6.250000  5.6112637
    ## 4                          Wetland Species        8.076923  5.2420635
    ## 5        Grassland or Open Habitat Species        6.607143  3.7593985
    ## 6       Mature Forest and Peatland Species       15.625000 13.0341880
    ## 7 Forest Openings and Young Forest Species       14.000000  8.9146199
    ## 8                                     Bats        6.111111  6.1111111
    ## 9                             Forest Trees        0.625000  2.6289683
    ##   Wt.Upper_1 Wt.Best.guess_2  Wt.Lower_2 Wt.Upper_2 Wt.Best.guess_3
    ## 1   3.358314       2.4590164  0.36065574  3.3208431        5.409836
    ## 2  12.171079      11.8636364  7.40304467 13.8352203       13.863636
    ## 3   4.425214       4.5000000  6.05555556  3.1666667        6.875000
    ## 4   8.888560       3.4423077  3.16080586  5.4110876        7.403846
    ## 5   6.826099       0.7142857  0.07142857  0.3834586       20.892857
    ## 6  11.329365      17.8750000 12.95146520 12.2783883       12.125000
    ## 7  11.737389      21.0000000 15.23684211 16.5947316        9.800000
    ## 8   4.012346       1.7777778  1.82051282  0.2345679        1.777778
    ## 9   3.672619       6.2500000  4.51984127  6.7023810        1.875000
    ##   Wt.Lower_3 Wt.Upper_3 Wt.Best.guess_4 Wt.Lower_4 Wt.Upper_4
    ## 1   3.656601  5.0062040       14.262295  9.7466037 13.6702992
    ## 2  10.241301 14.7342102       12.227273 10.3303004 13.4852500
    ## 3  11.036630  1.9946581        6.250000  8.4752747  5.6616300
    ## 4   4.805438  7.6632854       13.019231  7.2651028 11.7438292
    ## 5   9.233930 22.1607753        4.821429  4.0131579  5.5786788
    ## 6   9.798077  9.3432540        3.625000  3.8055556  5.3849206
    ## 7   7.214620  8.3890636        1.500000  0.1000000  0.5857143
    ## 8   1.142857  0.1728395        0.000000  0.5555556  0.0000000
    ## 9   3.069444  3.9861111        0.625000  0.1250000  0.1250000
    ##   Wt.Best.guess_5 Wt.Lower_5 Wt.Upper_5 Wt.Best.guess_6 Wt.Lower_6
    ## 1       14.377049  12.334196  17.118772       37.327869  31.609864
    ## 2       10.340909   8.692724   9.959069       11.818182  11.188566
    ## 3       11.250000  13.766331  12.382631        8.750000  14.298957
    ## 4        5.288462   3.282967   2.944139        3.365385   2.641941
    ## 5        0.000000   0.000000   0.000000        0.000000   0.000000
    ## 6        0.000000   0.000000   0.000000        0.000000   0.000000
    ## 7        0.000000   0.000000   0.000000        0.000000   0.000000
    ## 8        0.000000   0.000000   0.000000        0.000000   0.000000
    ## 9        0.000000   0.000000   0.000000        0.000000   0.000000
    ##   Wt.Upper_6 Wt.Best.guess_7 Wt.Lower_7 Wt.Upper_7 Wt.Best.guess_8
    ## 1  35.066807        6.885246  3.0008628   9.017133       1.9672131
    ## 2  10.380104        3.500000  2.0043290   3.177323       4.7272727
    ## 3  13.883323        0.000000  0.0000000   0.000000       0.6250000
    ## 4   2.303114        2.923077  2.1231615   3.102663      10.6153846
    ## 5   0.000000        1.250000  0.2763158   1.785714       2.1153846
    ## 6   0.000000        0.000000  0.0000000   0.000000       3.0000000
    ## 7   0.000000        0.000000  0.0000000   0.000000       0.5263158
    ## 8   0.000000        0.000000  0.0000000   0.000000       0.0000000
    ## 9   0.000000        0.000000  0.0000000   0.000000       0.6250000
    ##   Wt.Lower_8 Wt.Upper_8 Wt.Best.guess_9 Wt.Lower_9 Wt.Upper_9
    ## 1  1.0992235   3.607297        8.360656  3.9516825   9.443363
    ## 2  3.1106867   6.961150        2.204545  2.2932900   1.862554
    ## 3  3.6250000   0.625000        9.375000 14.9978632  10.927503
    ## 4  6.9680708   9.854212        5.807692  4.2299756   5.614286
    ## 5  1.2179487   2.234432        1.607143  1.4285714   3.362245
    ## 6  2.1388889   4.065476        0.000000  0.0000000   0.000000
    ## 7  0.7017544   1.027569        1.000000  1.3333333   1.952381
    ## 8  0.5555556   0.000000        0.000000  0.5555556   0.000000
    ## 9  0.1250000   0.125000        0.000000  0.0000000   0.000000
    ##   Wt.Best.guess_10 Wt.Lower_10 Wt.Upper_10 Wt.Best.guess_11 Wt.Lower_11
    ## 1       16.4098361 15.18119068 18.21869428        4.9180328    2.568306
    ## 2        0.4545455  0.45454545  0.45454545        0.0000000    0.000000
    ## 3        5.0000000  8.40064103  4.64224664        0.0000000    0.000000
    ## 4        0.0000000  0.00000000  0.00000000        0.0000000    0.000000
    ## 5        0.0000000  0.00000000  0.00000000        0.7142857    1.023810
    ## 6        0.0000000  0.00000000  0.00000000        0.0000000    0.000000
    ## 7        0.0000000  0.00000000  0.00000000        0.0000000    0.000000
    ## 8        0.5555556  0.03267974  0.03267974       17.5555556   15.121797
    ## 9        8.7500000  6.30952381  7.82936508        2.5000000    4.476190
    ##   Wt.Upper_11 Wt.Best.guess_12 Wt.Lower_12 Wt.Upper_12 Wt.Best.guess_13
    ## 1    5.508197        0.4918033   0.0000000  1.05386417        7.5737705
    ## 2    0.000000        0.0000000   0.0000000  0.00000000        0.5454545
    ## 3    0.000000        0.0000000   0.0000000  0.00000000        6.2500000
    ## 4    0.000000        0.0000000   0.0000000  0.00000000        1.9615385
    ## 5    0.764411        0.0000000   0.0000000  0.05012531        4.7368421
    ## 6    0.000000        1.8750000   1.2500000  0.00000000        2.5000000
    ## 7    0.000000        1.5000000   0.2368421  1.07894737        0.0000000
    ## 8   15.262890        0.0000000   0.0000000  0.00000000        0.0000000
    ## 9    3.357143       15.0000000   4.8055556  8.15674603        0.0000000
    ##   Wt.Lower_13 Wt.Upper_13 Wt.Best.guess_14 Wt.Lower_14 Wt.Upper_14
    ## 1   4.3357574   7.6728707         2.950820   0.7025761   4.0749415
    ## 2   0.0000000   0.1731602         4.272727   0.8778281   4.3603064
    ## 3   5.1469780   5.6036047         0.625000   0.6250000   0.0000000
    ## 4   0.9347985   3.4347985         3.846154   3.0412088   7.2786700
    ## 5   3.5782016   4.8322301         2.678571   3.6581633   1.8919620
    ## 6   3.5416667   1.4583333         0.625000   2.3273810   2.5059524
    ## 7   0.0000000   0.0000000         0.000000   0.4285714   0.5714286
    ## 8   0.0000000   0.0000000         0.000000   0.5555556   0.0000000
    ## 9   0.0000000   0.0000000         0.000000   0.0000000   0.0000000
    ##   Wt.Best.guess_15 Wt.Lower_15 Wt.Upper_15 Wt.Best.guess_16 Wt.Lower_16
    ## 1        4.4262295    1.303669    5.550351        16.426230    9.077366
    ## 2        2.7954545    1.381907    1.965368         8.863636    4.801143
    ## 3        5.0000000    4.950397    2.690476         7.500000    7.283730
    ## 4        0.9615385    1.793346    2.408425         2.403846    2.374847
    ## 5       11.9642857    6.573260   14.550565        10.892857    5.189801
    ## 6        1.2500000    2.083333    2.083333        10.500000    8.527778
    ## 7        2.0000000    0.700000    2.019048         7.000000    4.926901
    ## 8        1.1111111    2.000000    1.444444         0.000000    0.000000
    ## 9        0.0000000    0.000000    0.000000         1.875000    0.625000
    ##   Wt.Upper_16 Wt.Best.guess_17 Wt.Lower_17 Wt.Upper_17 Wt.Best.guess_18
    ## 1   16.942192         7.868852    4.529356    9.515181        19.295082
    ## 2    9.288323        17.386364    8.356338   14.762543        17.272727
    ## 3    8.452381         9.375000    9.931319    7.011142        16.250000
    ## 4    2.545788        12.788462    8.279797   10.991124        15.980769
    ## 5    9.757026        22.500000    8.972025   22.610992         6.428571
    ## 6    7.902778        20.750000   11.569902   15.945665         6.000000
    ## 7    6.900000        23.000000   13.649123   16.032338         2.000000
    ## 8    0.000000         6.111111    5.518926    4.173962         0.000000
    ## 9    2.541667         6.250000    3.894841    6.702381         0.625000
    ##   Wt.Lower_18 Wt.Upper_18 Wt.Best.guess_19 Wt.Lower_19 Wt.Upper_19
    ## 1  13.3551913  18.7425860        17.704918  9.74403221   17.171700
    ## 2  12.7813853  17.8667999        13.909091  5.95843046   13.860473
    ## 3  14.2727411  14.8722527        11.875000  5.27060440   10.405081
    ## 4  12.2554382  14.3736405        12.615385  9.20565887   11.124392
    ## 5   4.6679198   7.3848013        17.857143  7.43980143   19.325207
    ## 6   4.6250000   5.8154762        12.625000  8.55555556   10.071789
    ## 7   0.1555556   0.6412698        11.500000  8.96023392   11.231868
    ## 8   0.5555556   0.0000000         1.666667  2.03267974    1.477124
    ## 9   0.1250000   0.1250000         2.500000  0.06944444    3.142857
    ##   Wt.Best.guess_20 Wt.Lower_20 Wt.Upper_20 Wt.Best.guess_21 Wt.Lower_21
    ## 1       25.3606557 21.17260364 27.07037265         8.360656    4.468466
    ## 2       10.6818182  8.66999667  9.93634144        21.022727   10.492868
    ## 3       11.2500000 12.73611111 13.12438950         8.750000   10.447344
    ## 4        5.2884615  3.28296703  2.94413919        12.307692    8.681507
    ## 5        0.0000000  0.00000000  0.00000000        20.892857    9.480528
    ## 6        0.0000000  0.00000000  0.00000000        16.625000   11.686966
    ## 7        0.0000000  0.00000000  0.00000000        15.500000    8.614620
    ## 8        0.5555556  0.03267974  0.03267974         6.111111    5.518926
    ## 9        7.5000000  4.16666667  6.38888889         1.875000    3.069444
    ##   Wt.Upper_21 Wt.Best.guess_22 Wt.Lower_22 Wt.Upper_22
    ## 1    9.235712         40.77049    38.41516    35.25260
    ## 2   16.070790         28.40909    18.15184    21.84965
    ## 3    2.905372         20.00000    17.75717    15.35592
    ## 4   11.809500         22.30769    13.35747    18.40857
    ## 5   21.854653         26.78571    14.41760    26.28106
    ## 6   11.509921         22.00000    14.97955    16.49130
    ## 7   11.910116         23.50000    16.68246    18.06466
    ## 8    4.012346         19.77778    18.80651    16.92956
    ## 9    3.986111         16.87500    11.26786    16.56746

Calculate averaged performance: add averaged benefit estimates to the (averaged) baseline

``` r
exp.pop <- ben.mat.agg[,2:ncol(ben.mat.agg)] + as.matrix(base.mat.agg[,2:ncol(base.mat.agg)])
exp.pop <- cbind(base.mat.agg, exp.pop)

print(exp.pop)
```

    ##                           Ecological.Group Wt.Best.guess  Wt.Lower
    ## 1                           Migratory Fish      34.96721 17.624610
    ## 2                         Riparian Species      47.50000 31.052562
    ## 3                          Aquatic Species      51.25000 28.028846
    ## 4                          Wetland Species      52.30769 37.692308
    ## 5        Grassland or Open Habitat Species      41.78571 22.524484
    ## 6       Mature Forest and Peatland Species      48.12500 32.911325
    ## 7 Forest Openings and Young Forest Species      40.50000 19.698524
    ## 8                                     Bats      22.22222 11.012346
    ## 9                             Forest Trees      16.87500  2.460317
    ##   Wt.Upper Wt.Best.guess_1 Wt.Lower_1 Wt.Upper_1 Wt.Best.guess_2
    ## 1 55.51102        37.42623  18.014930   58.86934        37.42623
    ## 2 65.89409        57.20455  39.227612   78.06517        59.36364
    ## 3 69.35134        57.50000  33.640110   73.77656        55.75000
    ## 4 67.68010        60.38462  42.934371   76.56866        55.75000
    ## 5 57.22558        48.39286  26.283883   64.05167        42.50000
    ## 6 64.65354        63.75000  45.945513   75.98291        66.00000
    ## 7 58.00502        54.50000  28.613144   69.74241        61.50000
    ## 8 37.17327        28.33333  17.123457   41.18562        24.00000
    ## 9 35.53770        17.50000   5.089286   39.21032        23.12500
    ##   Wt.Lower_2 Wt.Upper_2 Wt.Best.guess_3 Wt.Lower_3 Wt.Upper_3
    ## 1  17.985266   58.83187        40.37705  21.281211   60.51723
    ## 2  38.455606   79.72931        61.36364  41.293863   80.62830
    ## 3  34.084402   72.51801        58.12500  39.065476   71.34600
    ## 4  40.853114   73.09119        59.71154  42.497746   75.34338
    ## 5  22.595913   57.60903        62.67857  31.758414   79.38635
    ## 6  45.862790   76.93193        60.25000  42.709402   73.99679
    ## 7  34.935366   74.59975        50.30000  26.913144   66.39409
    ## 8  12.832858   37.40784        24.00000  12.155203   37.34611
    ## 9   6.980159   42.24008        18.75000   5.529762   39.52381
    ##   Wt.Best.guess_4 Wt.Lower_4 Wt.Upper_4 Wt.Best.guess_5 Wt.Lower_5
    ## 1        49.22951  27.371214   69.18132        49.34426  29.958806
    ## 2        59.72727  41.382862   79.37934        57.84091  39.745286
    ## 3        57.50000  36.504121   75.01297        62.50000  41.795177
    ## 4        65.32692  44.957411   79.42393        57.59615  40.975275
    ## 5        46.60714  26.537642   62.80425        41.78571  22.524484
    ## 6        51.75000  36.716880   70.03846        48.12500  32.911325
    ## 7        42.00000  19.798524   58.59074        40.50000  19.698524
    ## 8        22.22222  11.567901   37.17327        22.22222  11.012346
    ## 9        17.50000   2.585317   35.66270        16.87500   2.460317
    ##   Wt.Upper_5 Wt.Best.guess_6 Wt.Lower_6 Wt.Upper_6 Wt.Best.guess_7
    ## 1   72.62980        72.29508  49.234474   90.57783        41.85246
    ## 2   75.85316        59.31818  42.241128   76.27419        51.00000
    ## 3   81.73397        60.00000  42.327803   83.23467        51.25000
    ## 4   70.62424        55.67308  40.334249   69.98321        55.23077
    ## 5   57.22558        41.78571  22.524484   57.22558        43.03571
    ## 6   64.65354        48.12500  32.911325   64.65354        48.12500
    ## 7   58.00502        40.50000  19.698524   58.00502        40.50000
    ## 8   37.17327        22.22222  11.012346   37.17327        22.22222
    ## 9   35.53770        16.87500   2.460317   35.53770        16.87500
    ##   Wt.Lower_7 Wt.Upper_7 Wt.Best.guess_8 Wt.Lower_8 Wt.Upper_8
    ## 1  20.625473   64.52816        36.93443  18.723834   59.11832
    ## 2  33.056891   69.07141        52.22727  34.163249   72.85524
    ## 3  28.028846   69.35134        51.87500  31.653846   69.97634
    ## 4  39.815469   70.78276        62.92308  44.660379   77.53431
    ## 5  22.800800   59.01129        43.90110  23.742433   59.46001
    ## 6  32.911325   64.65354        51.12500  35.050214   68.71902
    ## 7  19.698524   58.00502        41.02632  20.400278   59.03259
    ## 8  11.012346   37.17327        22.22222  11.567901   37.17327
    ## 9   2.460317   35.53770        17.50000   2.585317   35.66270
    ##   Wt.Best.guess_9 Wt.Lower_9 Wt.Upper_9 Wt.Best.guess_10 Wt.Lower_10
    ## 1        43.32787  21.576293   64.95439         51.37705   32.805801
    ## 2        49.70455  33.345852   67.75664         47.95455   31.507107
    ## 3        60.62500  43.026709   80.27885         56.25000   36.429487
    ## 4        58.11538  41.922283   73.29438         52.30769   37.692308
    ## 5        43.39286  23.953056   60.58782         41.78571   22.524484
    ## 6        48.12500  32.911325   64.65354         48.12500   32.911325
    ## 7        41.50000  21.031857   59.95740         40.50000   19.698524
    ## 8        22.22222  11.567901   37.17327         22.77778   11.045025
    ## 9        16.87500   2.460317   35.53770         25.62500    8.769841
    ##   Wt.Upper_10 Wt.Best.guess_11 Wt.Lower_11 Wt.Upper_11 Wt.Best.guess_12
    ## 1    73.72972         39.88525   20.192916    61.01922         35.45902
    ## 2    66.34864         47.50000   31.052562    65.89409         47.50000
    ## 3    73.99359         51.25000   28.028846    69.35134         51.25000
    ## 4    67.68010         52.30769   37.692308    67.68010         52.30769
    ## 5    57.22558         42.50000   23.548294    57.98999         41.78571
    ## 6    64.65354         48.12500   32.911325    64.65354         50.00000
    ## 7    58.00502         40.50000   19.698524    58.00502         42.00000
    ## 8    37.20595         39.77778   26.134143    52.43616         22.22222
    ## 9    43.36706         19.37500    6.936508    38.89484         31.87500
    ##   Wt.Lower_12 Wt.Upper_12 Wt.Best.guess_13 Wt.Lower_13 Wt.Upper_13
    ## 1   17.624610    56.56489         42.54098   21.960368    63.18389
    ## 2   31.052562    65.89409         48.04545   31.052562    66.06725
    ## 3   28.028846    69.35134         57.50000   33.175824    74.95495
    ## 4   37.692308    67.68010         54.26923   38.627106    71.11490
    ## 5   22.524484    57.27570         46.52256   26.102686    62.05781
    ## 6   34.161325    64.65354         50.62500   36.452991    66.11187
    ## 7   19.935366    59.08397         40.50000   19.698524    58.00502
    ## 8   11.012346    37.17327         22.22222   11.012346    37.17327
    ## 9    7.265873    43.69444         16.87500    2.460317    35.53770
    ##   Wt.Best.guess_14 Wt.Lower_14 Wt.Upper_14 Wt.Best.guess_15 Wt.Lower_15
    ## 1         37.91803   18.327186    59.58597         39.39344   18.928279
    ## 2         51.77273   31.930390    70.25440         50.29545   32.434469
    ## 3         51.87500   28.653846    69.35134         56.25000   32.979243
    ## 4         56.15385   40.733516    74.95877         53.26923   39.485653
    ## 5         44.46429   26.182648    59.11754         53.75000   29.097744
    ## 6         48.75000   35.238706    67.15949         49.37500   34.994658
    ## 7         40.50000   20.127095    58.57645         42.50000   20.398524
    ## 8         22.22222   11.567901    37.17327         23.33333   13.012346
    ## 9         16.87500    2.460317    35.53770         16.87500    2.460317
    ##   Wt.Upper_15 Wt.Best.guess_16 Wt.Lower_16 Wt.Upper_16 Wt.Best.guess_17
    ## 1    61.06138         51.39344   26.701976    72.45322         42.83607
    ## 2    67.85946         56.36364   35.853705    75.18241         64.88636
    ## 3    72.04182         58.75000   35.312576    77.80372         60.62500
    ## 4    70.08852         54.71154   40.067155    70.22589         65.09615
    ## 5    71.77614         52.67857   27.714286    66.98260         64.28571
    ## 6    66.73687         58.62500   41.439103    72.55632         68.87500
    ## 7    60.02407         47.50000   24.625424    64.90502         63.50000
    ## 8    38.61772         22.22222   11.012346    37.17327         28.33333
    ## 9    35.53770         18.75000    3.085317    38.07937         23.12500
    ##   Wt.Lower_17 Wt.Upper_17 Wt.Best.guess_18 Wt.Lower_18 Wt.Upper_18
    ## 1   22.153966    65.02621         54.26230   30.979801    74.25361
    ## 2   39.408900    80.65663         64.77273   43.833947    83.76089
    ## 3   37.960165    76.36248         67.50000   42.301587    84.22360
    ## 4   45.972105    78.67122         68.28846   49.947746    82.05374
    ## 5   31.496509    79.83657         48.21429   27.192404    64.61038
    ## 6   44.481227    80.59921         54.12500   37.536325    70.46902
    ## 7   33.347647    74.03736         42.50000   19.854079    58.64629
    ## 8   16.531271    41.34724         22.22222   11.567901    37.17327
    ## 9    6.355159    42.24008         17.50000    2.585317    35.66270
    ##   Wt.Best.guess_19 Wt.Lower_19 Wt.Upper_19 Wt.Best.guess_20 Wt.Lower_20
    ## 1         52.67213   27.368642    72.68272         60.32787   38.797214
    ## 2         61.40909   37.010992    79.75456         58.18182   39.722558
    ## 3         63.12500   33.299451    79.75642         62.50000   40.764957
    ## 4         64.92308   46.897967    78.80449         57.59615   40.975275
    ## 5         59.64286   29.964286    76.55078         41.78571   22.524484
    ## 6         60.75000   41.466880    74.72533         48.12500   32.911325
    ## 7         52.00000   28.658758    69.23689         40.50000   19.698524
    ## 8         23.88889   13.045025    38.65040         22.77778   11.045025
    ## 9         19.37500    2.529762    38.68056         24.37500    6.626984
    ##   Wt.Upper_20 Wt.Best.guess_21 Wt.Lower_21 Wt.Upper_21 Wt.Best.guess_22
    ## 1    82.58140         43.32787   22.093076    64.74674         75.73770
    ## 2    75.83043         68.52273   41.545430    81.96488         75.90909
    ## 3    82.47573         60.00000   38.476190    72.25672         71.25000
    ## 4    70.62424         64.61538   46.373814    79.48960         74.61538
    ## 5    57.22558         62.67857   32.005013    79.08023         68.57143
    ## 6    64.65354         64.75000   44.598291    76.16346         70.12500
    ## 7    58.00502         56.00000   28.313144    69.91514         64.00000
    ## 8    37.20595         28.33333   16.531271    41.18562         42.00000
    ## 9    41.92659         18.75000    5.529762    39.52381         33.75000
    ##   Wt.Lower_22 Wt.Upper_22
    ## 1    56.03977    90.76362
    ## 2    49.20440    87.74374
    ## 3    45.78602    84.70726
    ## 4    51.04978    86.08866
    ## 5    36.94209    83.50663
    ## 6    47.89087    81.14484
    ## 7    36.38098    76.06968
    ## 8    29.81885    54.10283
    ## 9    13.72817    52.10516

Output results

``` r
# write_csv(ben.mat.agg, "Aggregated_Benefits.csv")
# write_csv(base.mat.agg, "Aggregated_Baseline.csv")
# write_csv(exp.pop, "Aggregated_Performance.csv")
```
