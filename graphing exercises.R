#load the data
data <- read.table("biocorecrg.csv", 
        header=TRUE, sep=",")
View(data)

#4- Using ggplot, create a simple scatter plot representing gene expression of "sampleB" on the x-axis and "sampleH" on the y-axis.


#5- Add a column to the data frame "project1" (call this column "expr_limits"), that will be filled the following way:
  
#6- if the expression of a gene is > 13 in both sampleB and sampleH, set to the value in "expr_limits" to "high", if the expression of a gene is < 6 in both sampleB and sampleH, set it to "low", if different, set it to "normal". You can use a 'case_when()' statement. 

#7- Color the points of the scatter plot according to the newly created column "expr_limits". Save that plot in the object "p"


#8- Add a layer to "p" in order to change the points colors to blue (for low), grey (for normal) and red (for high). Save this plot in the object "p2".

#9- Save p2 in a jpeg file. a. Try with RStudio Plots window (Export), b. Try in the console:
  
#- Box plot
#1- Convert "project1" from a wide format to a long format: save in the object "project_long". Do not transform GeneName and expr_limit


#2- Produce a boxplot of the expression of all samples (i.e. each sample is represented by a box)


#3- Modify the previous boxplot so as to obtain 3 "sub-boxplots" per sample, each representing the expression of either "low", "normal" or "high" genes.


#4- Rotate the x-axis labels (90 degrees angle). This is new ! Google it !! See the theme() function.
  
  
#5- Finally, add a title of your choice to the plot.


#- Bar plot
#1- Produce a bar plot of how many low/normal/high genes are in the column "expr_limits" of "project1".


#2- Add an horizontal line at counts 250 (y-axis). Save the plot in the object "bar"


#3- Swap (i.e. flip) the x and y axis. Save in object "bar2".


#4- Save "bar" and "bar2" plots in a "png" file, using the png()** function, in the console: use grid.arrange (from the gridExtra package) to organize both plots in one page !** install (if needed) and load the library. See the help documentation for ggsave on using gridExtra. Scroll down to Examples..... Also you can try the (patchwork library)
  
  
#- Histogram
#1- Create a simple histogram using project_long (column "value").


#2- Notice that you get the following warning message" stat_bin() using bins = 30. Pick better value with binwidth. Set "bins" parameter of geom_histogram() to 50.

#3- The histogram plots the expression values for All samples. Change the plot so as to obtain one histograms per sample.

#4- By default, geom_histogram produces a stacked histogram.Change argument "position" to "dodge".

#5- A bit messy ?? Run the following:
   hist2 <- ggplot(data=project_long, mapping=aes(x=value, fill=variable)) + 
  geom_histogram(bins=50) + 
  facet_grid(~variable)
   
#facet_grid() is another easy way to split the views!
  
#6- Change the default colors with scale_fill_manual(). You can try the rainbow() function for coloring.


#7- "Zoom in" the plots: set the x-axis limits from from 6 to 13. Add the xlim() layer.


#8- Change the default theme to theme_minimal()


#9- Save that last plot to a file (format of your choice) with ggsave()

