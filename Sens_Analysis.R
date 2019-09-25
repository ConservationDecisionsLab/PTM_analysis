#' ---
#' title: "Uncertainty Analysis"
#' author: "Abbey Camaclang"
#' date: "27 Aug 2019"
#' output: github_document
#' ---


#' This code is used to run the uncertainty analysis using the confidence bounds from experts.  
#' 
#' Based on algorithm of R code written by Danial Stratford (danial.stratford@csiro.au), found in PTM Handbook Supporting Info.   
#' For each MC iteration,  
#' 1. Sample performance value for each expert - ecological group - strategy using rpert from mc2d package, shape = 4 (default)  
#' 2. Calculate benefit Bi = pi - p0  
#' 3. Average across experts  
#' 4. Sum over all ecological groups to get total benefit value for each strategy  
#' 5. Calculate cost-effectiveness CEi = (Bi * Fi)/Ci  
#' 
#' Code note copied directly as data frames for this project are structured differently and wanted to avoid additional rearranging.  

#' Load packages
#+ message = FALSE, warning = FALSE
library(mc2d)
library(tidyverse)
library(cowplot)

MC <-  10000 # number of iterations

#' ## Code copied from createBoxplots.R, with minimal changes to accommodate the absence of some tables in the Environment

#' Read in data from benefits aggregation
#+ warning = FALSE, message = FALSE
rlong.std <- read_csv("Standardized_Estimates_Longrev.csv") # use read_csv to make sure factors read in as character

#' Specify factor levels
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

#' Read in Cost & Feasibility table
#+ warning = FALSE, message = FALSE
costfeas <- read_csv("CostFeas2.csv") 
costfeas <- costfeas[-1,] # Remove baseline values
# costfeas$Strategy <- as.character(costfeas$Strategy)
# costfeas$Strategy <- as_factor(costfeas$Strategy)
costfeas$Strategy <- factor(costfeas$Strategy, levels = strat.levels)

#' ## Do uncertainty analysis
samples <- matrix(nrow = nrow(rlong.std.wide),ncol = MC)
MC.CE_Score <- list()

# testsample <- subset(rlong.std.wide, Strategy == "Baseline" | Strategy == "S12")
# tsamples <- matrix(nrow = nrow(testsample), ncol = MC)
for(it in 1:MC){
  rnd <- runif(1,1,999999)
  set.seed(rnd)
  # Sample benefit values
  samples[,it] <- rpert(nrow(rlong.std.wide),
                        min = rlong.std.wide$Lower,
                        mode = rlong.std.wide$Best.Guess,
                        max = rlong.std.wide$Upper, shape=4)
  # tsamples[,it] <- rpert(nrow(testsample), 
  #                       min = testsample$Lower, 
  #                       mode = testsample$Best.Guess, 
  #                       max = testsample$Upper, shape=4)
  
  temp <- cbind(rlong.std.wide[,1:3], samples[,it])
  names(temp)[4] <- "MC.Value"
  
  temp.wide <- spread(temp, key = Strategy, value = MC.Value)
  temp.strat <- select(temp.wide, S1:S22)
  # temp.strat <- select(temp.wide, S12)
  temp.ben <- temp.strat - temp.wide$Baseline
  temp.ben <- cbind(temp.wide[,1:2], temp.ben) 
  
  temp.long <- gather(temp.ben, key = Strategy, value = MCValue, -c(1:2))
  
  #' Combine tables to calculate weights for each expert - group - strategy 
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
  
  #' Aggregate across experts
  temp.agg <- aggregate(temp.joined$Wt.MCValue, 
                        by = list(Ecological.Group = temp.joined$Ecological.Group, Strategy = temp.joined$Strategy), 
                        FUN = sum, na.rm = TRUE)
  
  #' Weight by number of species in group
  wt.temp.agg <- left_join(temp.agg, grpwts, by = "Ecological.Group") %>%
    mutate(Wt.Benefit = x*numspp)
  
  # Add up (unweighted) aggregated benefits of each strategy across ecological groups
  # first rearrange so strategies are in rows and ecol groups are in columns - basically need to end up with col vector of total benefit
  sum.ben <- aggregate(wt.temp.agg$Wt.Benefit, 
                       by = list(Strategy = wt.temp.agg$Strategy),
                       FUN = sum, na.rm = TRUE)
  names(sum.ben) <- c("Strategy", "Benefit")
  sum.ben$Strategy <- factor(sum.ben$Strategy, levels = strat.levels)
  
  # Join with cost/feasibility table and calculate cost-effectiveness
  strat.est <- left_join(sum.ben, costfeas, by="Strategy") %>%
    mutate(., Sc.Cost = Cost/1000000, # scale costs to get reasonable values
           Exp.Benefit = Benefit * Feasibility, # weight benefits
           CE = (Benefit * Feasibility)/Sc.Cost) # calculate cost-effectiveness scores
  
  #' Rank strategies by (weighted)Benefit, Cost, and CE score
  CE_Score <- select(strat.est, c("Strategy", "Benefit", "Cost", "Feasibility", "Exp.Benefit","CE")) %>%
    mutate(., CE_rank = rank(-CE), 
           ExpBenefit_rank = rank(-Exp.Benefit), 
           Cost_rank = rank(Cost))
  
  MC.CE_Score[[it]] <- CE_Score
}  

MC.CE_Table <- lapply(MC.CE_Score, "[", 1:22, "CE")
MC.Results <- matrix(unlist(MC.CE_Table), ncol = MC, byrow = FALSE)
MC.Results <- data.frame(strat.levels[2:length(strat.levels)], MC.Results)
names(MC.Results)[1] <- "Strategy"

#' Save results
write_csv(MC.Results, "MC_CEScores2.csv")
MC_Samples <- data.frame(rlong.std.wide[,1:3], samples)
write_csv(MC_Samples, "MC_PerfSamples2.csv")

#' Box plots for visualization
MC.CE <- gather(MC.Results, key = MC.Iter, value = CE, 2:ncol(MC.Results))
MC.CE$Strategy <- factor(MC.CE$Strategy, levels = strat.levels)

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
  scale_x_discrete(name = "Management Strategies",
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