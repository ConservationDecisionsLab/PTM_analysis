Calculate Cost-effectiveness
================
Adapted for the SJR PTM by Abbey Camaclang
3 July 2019

This code calculates cost-effectiveness scores and ranks strategies by Benefit, Cost, and CE Based on algorithm from Step 2 section of *1\_Cost-Effectiveness.R* code from FRE PTM project, but using a different way to implement.

Also performs uncertainty analysis for costs.

Requires **Aggregated\_Benefits.csv** from *aggregateEstimates.R*, and a **CostFeas.csv** table of strategy cost and feasibility.
Load packages

``` r
library(tidyverse)
library(mc2d)
library(cowplot)
library(ggridges)
```

Set parameters

``` r
a <- 1000000 # scaling to get cost and benefits in same order of magnitude
uncrtn.anal <- 1 # 1 if running cost uncertainty analysis, 0 if not (to save time)
MC <-  10000 # number of iterations for uncertainty analysis
u <- 0.3 # prop. variation from the best estimate
```

Read in group-weighted benefit values from *aggregateEstimates.R* and cost/feasibility table

``` r
ben.mat.agg <- read_csv("Aggregated_Benefits_groupWtd.csv")

costfeas <- read_csv("CostFeas.csv")
costfeas <- costfeas[-1,] # Remove baseline values
costfeas$Strategy <- as_factor(costfeas$Strategy)
```

Calculate cost-effectiveness scores
-----------------------------------

CE = (Benefit \* Feasibility)/Cost

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

# Add up (group-weighted) aggregated benefits of each strategy across ecological groups
sum.ben <- data.frame(strat.names, rowSums(strat.ben))
names(sum.ben) <- c("Strategy", "Benefit")
sum.ben$Strategy <- as_factor(as.character(sum.ben$Strategy))

# Join with cost/feasibility table and calculate cost-effectiveness
strat.est <- full_join(sum.ben, costfeas, by="Strategy") %>%
  mutate(., Sc.Cost = Cost/a, # scale costs to get reasonable values
         Exp.Benefit = Benefit * Feasibility, # weight benefits by feasibility
         CE = (Benefit * Feasibility)/Sc.Cost) # divide by scaled costs
  
# Rank strategies by Expected Benefit, Cost, and CE score and save results
CE_Score <- select(strat.est, c("Strategy", "Benefit", "Cost", "Feasibility", "Exp.Benefit","CE")) %>%
  mutate(., CE_rank = rank(-CE), 
         ExpBenefit_rank = rank(-Exp.Benefit), 
         Cost_rank = rank(Cost))

print(CE_Score)
```

    ##    Strategy    Benefit         Cost Feasibility Exp.Benefit           CE
    ## 1        S1  357.68840    1535127.1      0.8525   304.92936 198.63460502
    ## 2        S2  321.00671    2945165.9      0.9217   295.87188 100.46017539
    ## 3        S3  468.75560   25998802.9      0.6167   289.08158  11.11903419
    ## 4        S4  323.95617   30166363.7      0.5250   170.07699   5.63796785
    ## 5        S5  196.72028  136569590.7      0.5375   105.73715   0.77423642
    ## 6        S6  314.27541 2236723608.0      0.4083   128.31865   0.05736902
    ## 7        S7   80.17655   15466504.7      0.4500    36.07945   2.33274732
    ## 8        S8  158.01417    7691338.9      0.5200    82.16737  10.68310303
    ## 9        S9  140.91340   12635565.0      0.5233    73.73998   5.83590698
    ## 10      S10  128.78975    3261560.9      0.7500    96.59231  29.61536330
    ## 11      S11   65.95734    1022670.1      0.8100    53.42545  52.24113650
    ## 12      S12   71.31157     447493.8      0.9660    68.88697 153.93950115
    ## 13      S13  152.07860   24067593.7      0.6416    97.57363   4.05414975
    ## 14      S14   95.87840    4615486.0      0.4969    47.64198  10.32220181
    ## 15      S15  186.22702    6595151.4      0.5625   104.75270  15.88328985
    ## 16      S16  413.23240   10947045.0      0.6407   264.75800  24.18533934
    ## 17      S17  641.06252   30479095.9      0.7969   510.86272  16.76108515
    ## 18      S18  453.74894  187062858.3      0.5265   238.89882   1.27710451
    ## 19      S19  556.66592   40700040.0      0.5433   302.43660   7.43086727
    ## 20      S20  272.63301  139831151.6      0.6438   175.52113   1.25523625
    ## 21      S21  565.57503   32149416.0      0.6553   370.62132  11.52808873
    ## 22      S22 1071.57634 2520689068.0      0.6389   684.63013   0.27160435
    ##    CE_rank ExpBenefit_rank Cost_rank
    ## 1        1               4         3
    ## 2        3               6         4
    ## 3       10               7        13
    ## 4       15              11        14
    ## 5       20              13        18
    ## 6       22              12        21
    ## 7       17              22        11
    ## 8       11              17         8
    ## 9       14              18        10
    ## 10       5              16         5
    ## 11       4              20         2
    ## 12       2              19         1
    ## 13      16              15        12
    ## 14      12              21         6
    ## 15       8              14         7
    ## 16       6               8         9
    ## 17       7               2        15
    ## 18      18               9        20
    ## 19      13               5        17
    ## 20      19              10        19
    ## 21       9               3        16
    ## 22      21               1        22

``` r
write_csv(CE_Score, "Cost_Effectiveness.csv")
```

Uncertainty analysis for cost uncertainty
-----------------------------------------

``` r
if (uncrtn.anal == 1) {
  
  samples <- matrix(nrow = nrow(costfeas),ncol = MC)
  MC.CE_Score <- list()

  # get min and max
  min.Cost <- costfeas$Cost * (1-u)
  max.Cost <- costfeas$Cost * (1+u)
  best.Cost <- costfeas$Cost
  
  for (it in 1:MC) {
    
    rnd <- runif(1,1,999999)
    set.seed(rnd)
    # sample cost values
    samples[,it] <- rpert(nrow(costfeas),
                          min = min.Cost,
                          mode =best.Cost,
                          max = max.Cost, shape=4)
    costfeas$Cost <- samples[,it]

    # Join with cost/feasibility table and calculate cost-effectiveness
    strat.est <- full_join(sum.ben, costfeas, by="Strategy") %>%
      mutate(., Sc.Cost = Cost/a, # scale costs to get reasonable values
             Exp.Benefit = Benefit * Feasibility, # weight benefits
             CE = (Benefit * Feasibility)/Sc.Cost) # calculate cost-effectiveness scores

    # Rank strategies by (weighted)Benefit, Cost, and CE score
    CE_Score <- select(strat.est, c("Strategy", "Benefit", "Cost", "Feasibility", "Exp.Benefit","CE")) %>%
      mutate(., CE_rank = rank(-CE),
             ExpBenefit_rank = rank(-Exp.Benefit),
             Cost_rank = rank(Cost))
    
    MC.CE_Score[[it]] <- CE_Score
    
    }
  
  # Get results and save as .csv files
  MC.CE_Table <- lapply(MC.CE_Score, "[", 1:length(strat.names), "CE")
  MC.Results <- matrix(unlist(MC.CE_Table), ncol = MC, byrow = FALSE)
  MC.Results <- data.frame(costfeas$Strategy, MC.Results)
  names(MC.Results)[1] <- "Strategy"
  
  MC.CE_Rank <- lapply(MC.CE_Score, "[", 1:length(strat.names), "CE_rank")
  MC.Ranks <- matrix(unlist(MC.CE_Rank), ncol = MC, byrow = FALSE)
  MC.Ranks <- data.frame(costfeas$Strategy, MC.Ranks)
  names(MC.Ranks)[1] <- "Strategy"

  write_csv(MC.Results, "MC_CEScores_cost.csv")
  MC_Samples <- data.frame(costfeas$Strategy, samples)
  write_csv(MC_Samples, "MC_PerfSamples_cost.csv")

  # Boxplot of CE scores
  MC.CE <- gather(MC.Results, key = MC.Iter, value = CE, 2:ncol(MC.Results))
  MC.CE$Strategy <- as_factor(MC.CE$Strategy)

  temp.plot <-
    ggplot(MC.CE, aes(x = Strategy # for each Ecological group, plot Estimate Type on x-axis
                      , y = CE # and St.Value on y-axis
                      )
           ) +
    geom_boxplot(
      lwd = 0.3 #lwd changes line width
      , fatten = 1 # thickness of median line; default is 2
      , outlier.size = 1 # size of outlier point
      ) + # tell R to display data in boxplot form
    theme_cowplot() +  # use the theme "cowplot" for the plots, which is a nice minimalist theme
    theme(
      axis.text = element_text(size = 10)
      , axis.line = element_line(size = 1)
      ) +
    scale_x_discrete(name = "Management strategies"
                     , breaks = MC.CE$Strategy
                     , labels = MC.CE$Strategy# Give the x-axis variables labels
                     ) +
    labs(x = "Management strategies"
         , y = "Cost-effectiveness score"
         ) +
    ylim(0, 200) # set the y-axis limits from 0-100
  
  ggsave(filename=paste0("Uncrtn_Cost_", MC, "R_Scores.pdf", sep = ""), temp.plot, width = 180, height = 115, units = "mm")
  ggsave(filename=paste0("Uncrtn_Cost_", MC, "R_Scores.tiff", sep = ""), temp.plot, width = 180, height = 115, units = "mm", dpi = 600)

  
  # Histogram of CE rank frequency
  # need to generalize number of bins/strategies
  MC.CE_r <- gather(MC.Ranks, key = MC.Iter, value = CE_rank, 2:ncol(MC.Ranks))
  MC.CE_r$Strategy <- as_factor(MC.CE_r$Strategy)
  
  count_ranks <- xyTable(MC.CE_r$Strategy, MC.CE_r$CE_rank)
  rank_table <- data.frame(count_ranks$x, count_ranks$y, count_ranks$number)
  rank_table_sort <- as_tibble(rank_table)
  names(rank_table_sort) <- c("Strategy", "CE_rank", "Count")
  # rank_table_sort <- group_by(rank_table_sort, Strategy) %>% # version used in SJR PTM; for each Strategy, finds the most frequent CE_rank
    # filter(Count == max(Count)) %>%
    # arrange(desc(CE_rank), Count)
  rank_table_sort <- group_by(rank_table_sort, CE_rank) %>% # for each CE_rank, finds the most frequent Strategy
    filter(Count == max(Count)) %>%
    arrange(desc(CE_rank), Count)
  
  strat.order <- rank_table_sort$Strategy
  new.names <- paste0("S", strat.order)
  
  temp.plot.r <-
    ggplot(MC.CE_r, aes(y = factor(Strategy, levels = new.names)
                        , x = CE_rank
                        , fill = factor(Strategy, levels = new.names)
                        )
           ) +
    geom_density_ridges(stat = "binline", bins = length(new.names), scale = 0.9, draw_baseline = FALSE) +
    theme_ridges(grid = TRUE, center_axis_labels = TRUE) +
    theme(
      legend.position = "none"
      , panel.spacing = unit(0.1, "lines")
      , strip.text = element_text(size = 8)
      , axis.ticks = element_blank()
      , axis.line = element_line(size = 0.3)
      , panel.grid = element_line(size = 0.3)
      ) +
    labs(x = "Cost-effectiveness rank"
         , y = "Management strategies"
         ) +
    scale_x_continuous(breaks = c(1:length(new.names))
                       , labels = c(1:length(new.names)) # Give the x-axis variables labels
                       # , limits = c(0, max(rank_table$count_ranks.x)+1)
                       )
  
  ggsave(filename=paste0("Uncrtn_Cost_", MC, "R_Ranks.pdf", sep = ""), temp.plot.r, width = 180, height = 180, units = "mm")
  ggsave(filename=paste0("Uncrtn_Cost_", MC, "R_Ranks.tiff", sep = ""), temp.plot.r, width = 180, height = 180, units = "mm", dpi = 600)
  
  }
```

    ## Warning: Removed 5272 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 5272 rows containing non-finite values (stat_boxplot).

``` r
print(temp.plot)
```

    ## Warning: Removed 5272 rows containing non-finite values (stat_boxplot).

![](calculateCEscore_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
print(temp.plot.r)
```

![](calculateCEscore_files/figure-markdown_github/unnamed-chunk-5-2.png)
