#' ---
#' title: "Benefit Estimates plots"
#' author: "Adapted for the SJR PTM by Abbey Camaclang"
#' date: "31 May 2019"
#' output: github_document
#' ---

#' This script standardizes the benefit estimates and creates two plots for each Ecological Group:  
#' 1) boxplots of the best guess, lower, and upper estimates for each Strategy from all Experts;  
#' 2) pointrange plots showing the best guess, lower and upper estimates of each Expert for each Strategy.  
#'
#' It requires output from combineTables.R, which organizes the estimates into a
#' single table and saves it as 'Results.csv' in the current working directory.

# Load packages
library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(sjPlot)
library(stringr)
library(RColorBrewer)

#' ## Read in and tidy data ---------------------------------------------------
results <- read.csv("Results.csv")
head(results)

# AC: need to standardize labels - should probably do it in combineTables.R instead
results$Ecological.Group <- as.character(results$Ecological.Group)
results$Ecological.Group[which(results$Ecological.Group=="Mature Forest Species")] <- "Mature Forest and Peatland"
results$Ecological.Group[which(results$Ecological.Group=="Mature Forest/ Peatland Species")] <- "Mature Forest and Peatland"
results$Ecological.Group[which(results$Ecological.Group=="Mature Forest/Peatland Species")] <- "Mature Forest and Peatland"

#' Use tidyr package to transform data to tidy version, with single columns for Estimate (e.g., best guess, lower, upper) and Value (value of estimate, 0-100)
rlong <-
  gather(results,
         key = Estimate,
         value = Value,
         Best.guess:Confidence_22) #' <!-- AC: updated with the SJR data column names -->
head(rlong)

rlong <- na.omit(rlong)
write_csv(rlong, "Results_tidy.csv")

rlong$Value <- as.numeric((rlong$Value))
# str(rlong) # Check data type


#' ## Summarize the number of expert estimates --------------------------------
#' Tabulate how many expert estimates there are for each ecological group
table.data <-spread(rlong, Estimate, Value) 
table.subset <- table.data[, c(1, 2)] # Subset table.data to only include the columns "Expert" and "Ecological.Group"
exp.table <- table(table.subset$Ecological.Group)
write.csv(exp.table, "Estimates_per_group.csv", row.names=FALSE)
exp.table

#' Create new columns to specify Estimate Type and Strategy separately
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

#' Tabulate how many estimates there are for each strategy
table.subset2 <- subset(rlong, Est.Type=="Best.Guess") # Subset to count how many experts provided estimates for each group and strategy
strat.levels <- unique(table.subset2$Strategy)
table.subset2$Strategy <- factor(table.subset2$Strategy,levels = strat.levels) #' <!-- AC: Note that using as.factor() changes the order to alphabetical -->
st.table <- table(table.subset2$Ecological.Group, table.subset2$Strategy)
write.csv(st.table, "Estimates_by_strategy.csv")
st.table


#' ## Standardize to a specified confidence level -----------------------------
#' Standardize upper and lower estimates to 80% confidence.  
#' 1) For each estimate, divide confidence by 100  
#' 2) Take the upper and lower estimates, and standardize using  
#'     * Lower standardized interval: B−((B−L)×(S∕C))  
#'     * Upper standardized interval: B+((U−B)×(S∕C))  
#' 3) Leave Best Guess estimate as is  

S <- 0.8 # confidence level to standardize estimates to

#' Subset dataframe by estimate type 
bg <- subset(rlong, Est.Type == "Best.Guess")
low <- subset(rlong, Est.Type == "Lower")
up <- subset(rlong, Est.Type == "Upper")
conf <- subset(rlong, Est.Type == "Confidence")

#' Check that order of rows are the same
# Results should equal the number of rows in tables, if all entries are matching
# test1 <- sum(str_detect(bg$Ecological.Group,low$Ecological.Group)) 
# test2 <- sum(str_detect(bg$Ecological.Group,up$Ecological.Group))
# test3 <- sum(str_detect(bg$Ecological.Group,conf$Ecological.Group))
# test4 <- sum(bg$Expert == low$Expert) # checks that Experts also align

#' Create copies of the datasets and add new columns with the standardized estimates
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

#' Combine into a new table and saves standardized estimates as a .csv file
rlong.std <- rbind(low.new, up.new, bg.new, conf.new) 

rlong.std$St.Value[rlong.std$St.Value < 0] <- 0 # Change any negative standardized values to zero
rlong.std$St.Value[rlong.std$St.Value > 100] <- 100

rlong.sub <- rlong.std[,c(1,2,3,7)] # subsets only relevant columns

#' Save standardized estimates into a new table, in the same format as Results.csv
est.levels <- unique(rlong$Estimate)
rlong.wide <- spread(rlong.sub, Estimate, St.Value)
rlong.wide <- rlong.wide[, c("Expert", "Ecological.Group", est.levels)] 

grp.levels <- unique(rlong.std$Ecological.Group)
rlong.wide$Ecological.Group<-factor(rlong.wide$Ecological.Group, levels=grp.levels) 

rlong.wide <- with(rlong.wide, rlong.wide[order(Expert, Ecological.Group),])
write_csv(rlong.wide, "Standardized_Estimates_Wide.csv") 


#' ## Create plots ------------------------------------------------------------

#' Order the strategies to plot in the desired order
rlong.std$Strategy <- factor(rlong.std$Strategy,levels = strat.levels)
rlong.std <- subset(rlong.std, Est.Type %in% c("Best.Guess", "Lower", "Upper")) # Subset to remove confidence estimates

# Rename Grassland/Open Habitat Species as it doesn't work as a filename as is (Should do this in combineTables.R instead)
grp.names <- grp.levels
grp.names[which(grp.levels=="Grassland/Open Habitat species")] <- paste0("Grassland Species")

#' Plot group estimates as boxplots and save as .pdf
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

#' Plot each expert estimate separately (x-axis = Expert, y-axis point = Best guess, range = lower->upper)
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
