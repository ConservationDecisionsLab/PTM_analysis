#' ---
#' title: "Plot Standardized Benefit Estimates"
#' author: "Adapted for the SJR PTM by Abbey Camaclang"
#' date: "28 June 2019"
#' output: github_document
#' ---

#' Based on 'Boxplot_script.R' from FRE PTM project written by ?? [Found in Dropbox folder "\Fraser_River_Resilience\09_Plot_scripts\Final data scripts"]
#' This script creates two plots for each Ecological Group:  
#' 1) boxplots of the best guess, lower, and upper estimates for each Strategy from all Experts;  
#' 2) pointrange plots showing the best guess, lower and upper estimates of each Expert for each Strategy.  
#'
#' It requires output from Standardize.R, which standardizes the individual estimates to 80% confidence level
#' and saves results as 'Standardized_Estimates_Long.csv'
#' 
#' Load packages
#+ message = FALSE
#+ warning = FALSE
library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(sjPlot)
# library(RColorBrewer)

#' Read in data from benefits aggregation
rlong.std <- read_csv("Standardized_Estimates_Long.csv") # use read_csv to make sure factors read in as character

#' Order the strategies to plot in the desired order
strat.levels <- unique(rlong.std$Strategy)
grp.levels <- unique(rlong.std$Ecological.Group)
rlong.std$Strategy <- factor(rlong.std$Strategy,levels = strat.levels)
rlong.std <- subset(rlong.std, Est.Type %in% c("Best.Guess", "Lower", "Upper")) # Subset to remove confidence estimates

# Rename Grassland/Open Habitat Species as it doesn't work as a filename as is (now done in combineTables.R instead)
# grp.names <- grp.levels
# grp.names[which(grp.levels=="Grassland/Open Habitat species")] <- paste0("Grassland Species")

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

   plot1 # print the plot onscreen

  # Save plots as .pdf, one file per ecological group
  ggsave(plot1, file=paste0(grp.levels[i], ".pdf", sep=''), path = "./boxplots/", width = 10, height = 8, units = "in")
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

   plot2 # print the plot onscreen

  # Save plots as .pdf, one file per ecological group
  ggsave(plot2, file=paste0(grp.levels[i],"_byExp.pdf", sep=''), path = "./pointrange/", width = 10, height = 8, units = "in")
}
