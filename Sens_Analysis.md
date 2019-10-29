Uncertainty Analysis (Benefit estimates)
================
Abbey Camaclang
27 Aug 2019

This code is used to run the uncertainty analysis using the confidence bounds from experts' benefit estimates.

Based on algorithm of R code written by Danial Stratford (<danial.stratford@csiro.au>), found in PTM Handbook Supporting Info.
For each MC iteration,
1. Sample performance value for each expert - ecological group - strategy using rpert from mc2d package, shape = 4 (default)
2. Calculate benefit Bi = pi - p0
3. Average across experts
4. Sum over all ecological groups to get total benefit value for each strategy
5. Calculate cost-effectiveness CEi = (Bi \* Fi)/Ci

Code not copied directly as data frames for this project are structured differently and wanted to avoid additional rearranging.
Load packages

``` r
library(mc2d)
library(tidyverse)
library(cowplot)

MC <-  100 # number of iterations
a <- 1000000 # scaling for CE scores
```

Prepare data for sampling
-------------------------

Code copied from *createBoxplots.R*, with minimal changes to accommodate the absence of some tables in the Environment

``` r
# Read in data from benefits aggregation. 
rlong.std <- read_csv("Standardized_Estimates_Longrev.csv") # use read_csv to make sure factors read in as character

# Specify factor levels
strat.levels <- unique(rlong.std$Strategy)
grp.levels <- unique(rlong.std$Ecological.Group)
est.levels <- c("Lower", "Best.Guess", "Upper")
expcode <- unique(rlong.std$Expert) 

# Order the strategies 
rlong.std$Strategy <- factor(rlong.std$Strategy, levels = strat.levels)

rlong.std <- subset(rlong.std, Est.Type %in% c("Best.Guess", "Lower", "Upper"))
rlong.std$Est.Type <- factor(rlong.std$Est.Type, levels = est.levels)

# Rearrange table so estimates for each group * strategy are on the same row
rlong.sub2 <- rlong.std[,c(1,2,5,6,7)]
rlong.std.wide <- spread(rlong.sub2,key=Est.Type,value=St.Value)
rlong.std.wide$Expert<-as.factor(rlong.std.wide$Expert)
rlong.std.wide$Ecological.Group <- factor(rlong.std.wide$Ecological.Group, levels = grp.levels)

# Get number of species in each group and number of species scored by each expert for each strategy
grplist <- read_csv("EcolGroupsList.csv") # Table of species in each ecological group
numspp <- apply(grplist, MARGIN = 2, FUN = function(x) length(x[!is.na(x)]) )
grpwts <- data.frame(Ecological.Group=names(numspp), numspp) 
grpwts$Ecological.Group <- factor(grpwts$Ecological.Group, levels = grp.levels)

spcases <- read_csv("SpecialCases.csv") # Table of number of species in each group scored by individual experts (if different from total)
spcases$Strategy <- str_c("S", spcases$Strategy)
spcases$Strategy[which(spcases$Strategy == "SBaseline")] <- "Baseline"
spcases$Strategy <- factor(spcases$Strategy, levels = strat.levels)
spcases$Expert <- factor(spcases$Expert, levels = levels(rlong.std.wide$Expert))
spcases$`Ecological Group`<- factor(spcases$`Ecological Group`, levels = grp.levels)
names(spcases)[which(str_detect(names(spcases), "Ecological Group")==1)] <- "Ecological.Group"  
```

Read in Cost & Feasibility table

``` r
costfeas <- read_csv("CostFeas2.csv") 
costfeas <- costfeas[-1,] # Remove baseline values
# costfeas$Strategy <- as.character(costfeas$Strategy)
# costfeas$Strategy <- as_factor(costfeas$Strategy)
costfeas$Strategy <- factor(costfeas$Strategy, levels = strat.levels)
```

Calculate cost and feasibility values for new combinations --------------

``` r
# This section is specfic to SJR and can be deleted for future projects

combos <- as.character(costfeas$Strategy[17:length(costfeas$Strategy)])

S22.exc <- c("Baseline", "S6", combos)
S22.feas <- mean(costfeas$Feasibility[!costfeas$Strategy %in% S22.exc])
S22.cost <- costfeas$Cost[which(costfeas$Strategy == "S22")] - costfeas$Cost[which(costfeas$Strategy == "S6")]

S23.exc <- c("Baseline", "S5", combos)
S23.feas <- mean(costfeas$Feasibility[!costfeas$Strategy %in% S23.exc])
S23.cost <- costfeas$Cost[which(costfeas$Strategy == "S22")] - costfeas$Cost[which(costfeas$Strategy == "S5")]

# Add new cost/feasibility values to costfeas:
costfeas <- costfeas %>%
  add_row(Strategy = "S23", Cost = S23.cost, Feasibility = S23.feas)
costfeas$Cost[which(costfeas$Strategy == "S22")] <- S22.cost
costfeas$Feasibility[which(costfeas$Strategy == "S22")] <- S22.feas

# get new strat.levels
newstrat.levels <- levels(costfeas$Strategy)

# comment out above section if not needed ---
```

Do uncertainty analysis
-----------------------

``` r
samples <- matrix(nrow = nrow(rlong.std.wide),ncol = MC)
MC.CE_Score <- list()
Benefits_uncrtn <- list()

for(it in 1:MC){
  rnd <- runif(1,1,999999)
  set.seed(rnd)
  
  # Sample benefit values
  samples[,it] <- rpert(nrow(rlong.std.wide),
                        min = rlong.std.wide$Lower,
                        mode = rlong.std.wide$Best.Guess,
                        max = rlong.std.wide$Upper, shape=4)
  
  temp <- cbind(rlong.std.wide[,1:3], samples[,it])
  names(temp)[4] <- "MC.Value"
  
  temp.wide <- spread(temp, key = Strategy, value = MC.Value)
  temp.strat <- select(temp.wide, as.character(strat.levels[2]):as.character(strat.levels[length(strat.levels)])) 
  
  temp.ben <- temp.strat - temp.wide$Baseline
  # temp.ben[temp.ben<0] <- 0 # replaces negative values with 0 (assume same as baseline)
  temp.ben <- cbind(temp.wide[,1:2], temp.ben) 
  
  temp.long <- gather(temp.ben, key = Strategy, value = MCValue, -c(1:2))
  
  # Combine tables to calculate weights for each expert - group - strategy 
  temp.joined <- left_join(temp.long, spcases, by=c("Expert", "Ecological.Group", "Strategy")) %>%
    left_join(., grpwts, by = "Ecological.Group")
  fullwts.idx <- which(is.na(temp.joined$NumSppScored))
  temp.joined$NumSppScored[fullwts.idx] <- temp.joined$numspp[fullwts.idx]
  
  fullwts <- aggregate(temp.joined$NumSppScored, 
                       by = list(Ecological.Group = temp.joined$Ecological.Group, Strategy = temp.joined$Strategy), 
                       FUN = sum, na.rm = TRUE)
  temp.joined <- temp.joined %>%
    left_join(., fullwts, by = c("Ecological.Group", "Strategy")) %>%
    mutate(Wts = NumSppScored/x) %>%
    mutate(Wt.MCValue = MCValue*Wts)
  temp.joined$Strategy <- factor(temp.joined$Strategy, levels = strat.levels)
  
  # Aggregate across experts
  temp.agg <- aggregate(temp.joined$Wt.MCValue, 
                        by = list(Ecological.Group = temp.joined$Ecological.Group, Strategy = temp.joined$Strategy), 
                        FUN = sum, na.rm = TRUE)

  # Weight benefits by number of species in group
  wt.temp.agg <- left_join(temp.agg, grpwts, by = "Ecological.Group") %>%
    mutate(Wt.Benefit = x*numspp)    
 
  
  # Calculate benefit values for new combinations -----------------------------------
  # This section is specific to SJR and can be deleted for future projects
  
  S6.fishben <- wt.temp.agg$Wt.Benefit[wt.temp.agg$Ecological.Group == "Migratory Fish" & wt.temp.agg$Strategy== "S6"]
  S5.fishben <- wt.temp.agg$Wt.Benefit[wt.temp.agg$Ecological.Group == "Migratory Fish" & wt.temp.agg$Strategy== "S5"]
  SAll.fishben <- wt.temp.agg$Wt.Benefit[wt.temp.agg$Ecological.Group == "Migratory Fish" & wt.temp.agg$Strategy== "S22"]
  
  S6.wt.fishben <- S6.fishben * costfeas$Feasibility[which(costfeas$Strategy == "S6")]
  S5.wt.fishben <- S5.fishben * costfeas$Feasibility[which(costfeas$Strategy == "S5")]
  SAll.wt.fishben <- SAll.fishben * costfeas$Feasibility[which(costfeas$Strategy == "S22")]
  
  S22.wtben <- SAll.wt.fishben - (S6.wt.fishben - S5.wt.fishben)
  S22.ben <- S22.wtben/costfeas$Feasibility[which(costfeas$Strategy == "S22")] # unweight
  
  # Copy old S22 values as S23:
  S23 <- wt.temp.agg[which(wt.temp.agg$Strategy == "S22"),]
  S23$Strategy <- "S23"
  rownames(S23) <- c(199:(199+8))
  
  # Replace old S22 migratory fish values with new ones, then add new S23 values
  wt.temp.agg$Wt.Benefit[which(wt.temp.agg$Strategy=="S22" & wt.temp.agg$Ecological.Group=="Migratory Fish")] <- S22.ben
  fish.numspp <- wt.temp.agg$numspp[which(wt.temp.agg$Strategy=="S22" & wt.temp.agg$Ecological.Group=="Migratory Fish")]
  wt.temp.agg$x[which(wt.temp.agg$Strategy=="S22" & wt.temp.agg$Ecological.Group=="Migratory Fish")] <- S22.ben/fish.numspp
  wt.temp.agg <- rbind(wt.temp.agg, S23) # new values (unweighted by feasibility) one for each ecol group and strategy. Use for getting new benefit matri
  
  # strat.levels <- newstrat.levels

  # comment out above section if not needed ---
  
  # Calculate total benefit of each strategy across ecological groups
  # First rearrange so strategies are in rows and ecol groups are in columns - basically need to end up with col vector of total benefit
  sum.ben <- aggregate(wt.temp.agg$Wt.Benefit, 
                       by = list(Strategy = wt.temp.agg$Strategy),
                       FUN = sum, na.rm = TRUE)
  names(sum.ben) <- c("Strategy", "Benefit")
  sum.ben$Strategy <- as_factor(sum.ben$Strategy)
  
  # Join with cost/feasibility table and calculate cost-effectiveness
  strat.est <- left_join(sum.ben, costfeas, by="Strategy") %>%
    mutate(., Sc.Cost = Cost/a, # scale costs to get reasonable values
           Exp.Benefit = Benefit * Feasibility, # weight benefits
           CE = (Benefit * Feasibility)/Sc.Cost) # calculate cost-effectiveness scores
  
  # Rank strategies by (weighted)Benefit, Cost, and CE score
  CE_Score <- select(strat.est, c("Strategy", "Benefit", "Cost", "Feasibility", "Exp.Benefit","CE")) %>%
    mutate(., CE_rank = rank(-CE), 
           ExpBenefit_rank = rank(-Exp.Benefit), 
           Cost_rank = rank(Cost))
  
  MC.CE_Score[[it]] <- CE_Score
  
  # Calculate expected performance for use as Benefit matrix in optimization
  # Get unweighted aggregated benefit estimates and weight by feasibility
  temp.joined.data <- left_join(wt.temp.agg, costfeas, by = "Strategy") %>%
    mutate(., Wt.Value = x * Feasibility) # weight by feasibility
  wide.temp.agg <- select(temp.joined.data, Ecological.Group, Strategy, Wt.Value) %>%
    spread(Strategy, Wt.Value)
  
  # Aggregate baseline
  temp.base <- temp.wide[,1:3]
  temp.base$Strategy <- "Baseline"
  names(temp.base)[3] <- "MCValue"
  temp.base.joined <- left_join(temp.base, spcases, by=c("Expert", "Ecological.Group", "Strategy")) %>%
    left_join(., grpwts, by = "Ecological.Group")
  fullwts.idx.base <- which(is.na(temp.base.joined$NumSppScored))
  temp.base.joined$NumSppScored[fullwts.idx.base] <- temp.base.joined$numspp[fullwts.idx.base]
  
  fullwts.base <- aggregate(temp.base.joined$NumSppScored, 
                            by = list(Ecological.Group = temp.base.joined$Ecological.Group, Strategy = temp.base.joined$Strategy), 
                            FUN = sum, na.rm = TRUE)
  temp.base.joined <- temp.base.joined %>%
    left_join(., fullwts.base, by = c("Ecological.Group", "Strategy")) %>%
    mutate(Wts = NumSppScored/x) %>%
    mutate(Wt.MCValue = MCValue*Wts)
  
  temp.base.agg <- aggregate(temp.base.joined$Wt.MCValue, 
                             by = list(Ecological.Group = temp.base.joined$Ecological.Group, Strategy = temp.base.joined$Strategy), 
                             FUN = sum, na.rm = TRUE)
  names(temp.base.agg)[3] <- "Wt.Best.guess"
  
  # Calculate performance and save in a list
  wt.perf <- wide.temp.agg[,2:ncol(wide.temp.agg)] + as.matrix(temp.base.agg[,3])
  Benefits_uncrtn[[it]] <- wt.perf %>%
    add_column(Ecological.Group = wide.temp.agg$Ecological.Group, .before = "S1")
  
}  

MC.CE_Table <- lapply(MC.CE_Score, "[", 1:length(strat.est$Strategy), "CE")
MC.Results <- matrix(unlist(MC.CE_Table), ncol = MC, byrow = FALSE)
MC.Results <- data.frame(strat.est$Strategy[1:length(strat.est$Strategy)], MC.Results)
names(MC.Results)[1] <- "Strategy"
```

Save results

``` r
write_csv(MC.Results, "MC_CEScores2.csv")
MC_Samples <- data.frame(rlong.std.wide[,1:3], samples)
write_csv(MC_Samples, "MC_PerfSamples2.csv")
saveRDS(Benefits_uncrtn, "Benefits_uncrtn.rds")
```

Box plots for visualization

``` r
MC.CE <- gather(MC.Results, key = MC.Iter, value = CE, 2:ncol(MC.Results))
# MC.CE$Strategy <- factor(MC.CE$Strategy, levels = unique(MC.CE$Strategy))

strat.levels <- newstrat.levels

temp.plot <-
  ggplot(MC.CE, aes(x = Strategy, # for each Ecological group, plot Estimate Type on x-axis 
                           y = CE # and St.Value on y-axis, 
                           ) # and colour the boxplots by estimate type
  ) + 
  geom_boxplot() + # tell R to display data in boxplot form
  theme_cowplot() +  # use the theme "cowplot" for the plots, which is a nice minimalist theme
  theme(plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"), # adjust margins around the outside of the plot (top, right, bottom, left)
        panel.spacing = unit(1, "lines"), # adjust margins and between panels of the plot (spacing of 1)
        axis.title.y = element_text(margin = margin(t = 0, 
                                                    r = 10,
                                                    b = 0,
                                                    l = 0) # adjust space between y-axis numbers and y-axis label
        ),
        plot.caption = element_text(size = 10, hjust = 0)
  ) + 
  # facet_wrap( ~ Strategy, nrow = 3) +  # create a separate panel of estimates for each management strategy
  scale_x_discrete(name = "Management strategies",
                   breaks = strat.levels[2:length(strat.levels)],
                   labels = strat.levels[2:length(strat.levels)]# Give the x-axis variables labels
  ) + 
  # scale_fill_manual(values = c("white", "gray80", "white"), # Assign colours to each type of estimate and don't show a legend
  #                   guide = FALSE 
  # ) + 
  labs(x = "Management Strategies", 
       y = "Cost-effectiveness Score"
       # title = paste(grp.levels[i]),
       # caption = paste0(
         # "Figure ", i, ". Boxplots summarizing the distribution of the lower (L), best guess (B), and upper (Upper) expert estimates of the probability of persistence 
         # of ", grp.levels[i], " under the Baseline scenario and each of the management strategies (S1 - S22). Lower and Upper estimates 
         # have been standardized to 80% confidence level. The thick horizontal lines indicate the median estimate, while the surrounding box shows the 
         # interquartile range. Any outliers are shown as points beyond the plot whiskers.Your individual estimates, standardized to 80% confidence level, 
         # are shown in blue.")
       ) +
  ylim(0, 200) # set the y-axis limits from 0-100

print(temp.plot)
```

    ## Warning: Removed 50 rows containing non-finite values (stat_boxplot).

![](Sens_Analysis_files/figure-markdown_github/unnamed-chunk-7-1.png)
