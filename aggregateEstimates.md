Aggregate Standardized Benefit Estimates
================
Adapted for the SJR PTM by Abbey Camaclang
3 July 2019

This code a) calculates benefits of each strategy (strategy performance - baseline performance) for each ecological group, b) aggregates (averages) across experts, and c) calculates expected performance under each strategy based on the aggregated benefit estimate. Based on first part of Step 2 section of 1\_Cost-Effectiveness.R code from FRE PTM project and uses output from standardizeConfidence.R

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

Read in data

``` r
rlong.wide <- read_csv("Standardized_Estimates_Wide.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   Ecological.Group = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
rlong.wide$Expert <- as_factor(rlong.wide$Expert)
rlong.wide$Ecological.Group <- as_factor(rlong.wide$Ecological.Group)
```

Remove confidence estimates from the table

``` r
wide.colnames <- colnames(rlong.wide)
idx.colnames <- which(str_detect(wide.colnames, "Confidence")==1)
DF <- rlong.wide[,-(idx.colnames)]
```

Calculate benefit: subtract baseline performance from strategy performance for each expert

``` r
base.mat <- DF[3:5]
strat.mat <- DF[6:ncol(DF)]  

ben.mat <- strat.mat - as.matrix(base.mat)
ben.mat[ben.mat<0] <- 0
```

Aggregate benefit: average benefit estimates for each species group + strategy across experts

``` r
ben.mat <- cbind(DF[,1:2], ben.mat )
base.mat <- cbind(DF[,1:2], base.mat)

ben.mat.agg <- aggregate(ben.mat[,3:ncol(ben.mat)], by=list(ben.mat$Ecological.Group), FUN = mean, na.rm = TRUE) # should check this manually
base.mat.agg <- aggregate(base.mat[,3:ncol(base.mat)], by=list(base.mat$Ecological.Group), FUN = mean, na.rm = TRUE)

names(ben.mat.agg)[1] <- "Ecological.Group"
names(base.mat.agg)[1] <- "Ecological.Group"

write_csv(ben.mat.agg, "Aggregated_Benefits.csv")
write_csv(base.mat.agg, "Aggregated_Baseline.csv")
```

Calculate averaged performance: add averaged benefit estimates to (averaged) baseline

``` r
exp.pop <- ben.mat.agg[,2:ncol(ben.mat.agg)] + as.matrix(base.mat.agg[,2:ncol(base.mat.agg)])
exp.pop <- cbind(base.mat.agg, exp.pop)
```

Output results

``` r
write_csv(exp.pop, "Aggregated_Performance.csv")
```
