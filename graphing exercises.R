#load the data
data <- read.table("biocorecrg.csv", header=TRUE, sep=",")
View(data)

#4- Using ggplot, create a simple scatter plot representing gene expression of "sampleB" on the x-axis and "sampleH" on the y-axis.
ggplot(data, aes(x=sampleB, y=sampleH)) + 
  geom_point()


#5- Add a column to the data frame "project1" (call this column "expr_limits"), that will be filled the following way:

#6- if the expression of a gene is > 13 in both sampleB and sampleH, set to the value in "expr_limits" to "high", if the expression of a gene is < 6 in both sampleB and sampleH, set it to "low", if different, set it to "normal".

project1 <- data %>% 
  mutate(expr_limits =
  case_when(sampleB >13 | sampleH > 13 ~ "high",
            sampleB <6 | sampleH < 6 ~"low",
            TRUE ~ "normal"
          ))

#7- Color the points of the scatter plot according to the newly created column "expr_limits". Save that plot in the object "p"
(p <- ggplot(data=project1, mapping=aes(x=sampleB, y=sampleH, color=expr_limits)) + 
  geom_point()
) #you can place parethesis around entire script to output the plot

#8- Add a layer to "p" in order to change the points colors to blue (for low), grey (for normal) and red (for high). Save this plot in the object "p2".

(p2 <- p + 
    scale_color_manual(values=c("red", "blue", "grey"))
)

#9- Save p2 in a jpeg file. a. Try with RStudio Plots window (Export), b. Try in the console:

ggsave("plot_p2.jpeg",
       device=jpeg,
       width=7,
       height=7)

#- Box plot
#1- Convert "project1" from a wide format to a long format: save in the object "project_long" 

project_long <- pivot_longer(project1, 
                cols=sampleA:sampleH, 
                names_to = c("sample"),                              values_to = c("values")
)
#2- Produce a boxplot of the expression of all samples (i.e. each sample is represented by a box)

ggplot(project_long, aes(x=sample, y=values)) + 
  geom_boxplot() 

#3- Modify the previous boxplot so as to obtain 3 "sub-boxplots" per sample, each representing the expression of either "low", "normal" or "high" genes.
ggplot(data=project_long, mapping=aes(x=sample, y=values, color=expr_limits)) + 
  geom_boxplot() 

#4- Rotate the x-axis labels (90 degrees angle). This is new ! Google it !!
ggplot(project_long, aes(x=sample, y=values, color= expr_limits)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) 

#5- Finally, add a title of your choice to the plot.
ggplot(project_long, aes(x=sample, y=values, color= expr_limits)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Boxplots")

#- Bar plot
#1- Produce a bar plot of how many low/normal/high genes are in the column "expr_limits" of "project1".
ggplot(project_long, aes(x=expr_limits)) +
  geom_bar()

#2- Add an horizontal line at counts 250 (y-axis). Save the plot in the object "bar"
(bar <- ggplot(project_long, aes(x=expr_limits)) +
  geom_bar() +
  geom_hline(aes(yintercept= 250))
)

#3- Swap the x and y axis. Save in object "bar2".
(bar2 <- bar +
      coord_flip()
)

#4- Save "bar" and "bar2" plots in a "png" file, using the png()** function, in the console: use grid.arrange (from the gridExtra package) to organize both plots in one page !**

#option 1, use gridExtra

library(gridExtra)
png("barplots.png")
grid.arrange(bar, bar2, nrow=1, ncol=2)
dev.off()

#option 2, use patchwork and ggsave
library(patchwork)
(barplots2 <- bar + bar2)
ggsave("barplots2.png")

#- Histogram
#1- Create a simple histogram using project_long (column "value").
ggplot(project_long, aes(values)) +
  geom_histogram(binwidth = 10)

ggplot(project_long, aes(values)) +
  geom_histogram(fill= "blue")
  
#2- Notice that you get the following warning message" stat_bin() using bins = 30. Pick better value with binwidth. Set "bins" parameter of geom_histogram() to 50.

ggplot(project_long, aes(values)) +
  geom_histogram(bins=50, fill= "blue")

#3- The histogram plots the expression values for All samples. Change the plot so as to obtain one histograms per sample.

ggplot(project_long, aes(values, fill=sample)) +
  geom_histogram(bins=50)

#4- By default, geom_histogram produces a stacked histogram.Change argument "position" to "dodge".

ggplot(project_long, aes(values, fill=sample)) +
  geom_histogram(position = "dodge") 
  
#5- A bit messy ?? Run the following:
(hist2 <- ggplot(data=project_long, mapping=aes(x= values, fill=sample)) + 
  geom_histogram(bins=50) + 
  facet_grid(~sample)
)
#facet_grid() is another easy way to split the views!

#6- Change the default colors with scale_fill_manual(). You can try the rainbow() function for coloring.
(hist2 <- ggplot(data=project_long, mapping=aes(x= values, fill=sample)) + 
    geom_histogram(bins=50) + 
    facet_grid(~sample) + 
    scale_fill_manual(values=rainbow(8))
)

#7- "Zoom in" the plots: set the x-axis limits from from 6 to 13. Add the xlim() layer.
(hist2 <- ggplot(data=project_long, mapping=aes(x= values, fill=sample)) + 
    geom_histogram(bins=50) + 
    facet_grid(~sample) + 
    scale_fill_manual(values=rainbow(8)) + 
    xlim(6,13)
)

#8- Change the default theme to theme_minimal()

(hist2 <- ggplot(data=project_long, mapping=aes(x= values, fill=sample)) + 
    geom_histogram(bins=50) + 
    facet_grid(~sample) + 
    scale_fill_manual(values=rainbow(8)) + 
   # xlim(6,13) + removing this layer will not output the warnings of removed rows (i.e. Removed 826 rows.)
    theme_minimal()
)

#9- Save that last plot to a file (format of your choice) with ggsave()
ggsave("histogram.pdf")
