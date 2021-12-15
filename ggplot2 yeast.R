library(dplyr)
library(ggplot2)

yeast <- read_csv("~/Data/yeast.csv")
yeast <- as.data.frame(yeast)

ggplot(yeast) + geom_point(mapping = aes(x=alm, y= gvh))

#if we want to color by Class
ggplot(yeast) + geom_point(mapping = aes(x=alm, y= gvh, color=ClassDistribution))

ggplot(yeast) + geom_histogram(mapping = aes(x=gvh, color=ClassDistribution), bins=5, fill='white', position='dodge')

#make dataframe tidy; from wide to long
y2 <- pivot_longer(yeast,cols=mcg:num, names_to= "bacteria", values_to="measurements")

y3 <-filter(y2, bacteria=="alm"| bacteria=="mit"| bacteria=="erl")

unique(y3$ClassDistribution)

groups <- filter(y3, ClassDistribution %in% c("ME1","ME2","ME3"))

#getting summary statistics for 3 types of bacteria
groups %>% dplyr::group_by(bacteria) %>%  summarize(min = min(measurements), 
q1 = quantile(measurements, 0.25), 
median = median(measurements), 
mean = mean(measurements), 
q3 = quantile(measurements, 0.75), 
max = max(measurements))

#creating boxplots for each bacteria type
ggplot(groups, aes(bacteria, measurements)) + 
  stat_boxplot(geom = "errorbar", # Error bars
               width = 0.25) +    # Bars width
  geom_boxplot(mapping = aes(x=bacteria, y=    measurements), fill='lightblue',outlier.colour = "red")+
  ggtitle("Boxplot by groups ggplot2")


#use faceting, in which each subgroup is shown in its own panel:
ggplot(groups, aes(x=bacteria, y=measurements, fill=ClassDistribution)) + 
  stat_boxplot(geom = "errorbar", # Error bars
               width = 0.25) +
  geom_boxplot(outlier.colour = "red") +
  facet_wrap(~ClassDistribution) +
  ggtitle("Boxplot by groups ggplot2")

#To remove the outliers, you can use the argument outlier.shape=NA, geom_boxplot(outlier.shape = NA)

#ggplot2 does not automatically adjust the y-axis. To adjust the axis, you can use coord_cartesian: +coord_cartesian(ylim=c(0, .6))

#Two side-by-side plots

#install.packages("patchwork")
library(patchwork)
#create box plot
plot1 <- ggplot(groups, aes(bacteria, measurements)) + 
  stat_boxplot(geom = "errorbar", # Error bars
               width = 0.25) +    # Bars width
  geom_boxplot(mapping = aes(x=bacteria, y= measurements), fill='lightblue',outlier.colour = "red")

#create density plot
plot2 <- ggplot(groups, aes(x = measurements, fill = bacteria)) +
  geom_density(alpha = 0.8)

#display plots side by side
plot1 + plot2 

#display plots stacked on top of each other
plot1 / plot2 

#display plots side by side with title, subtitle, and captions
patchwork <- plot1 + plot2 

patchwork + plot_annotation(
  title = 'This is a title',
  subtitle = 'This is a subtitle that describes more information about the plots',
  caption = 'This is a caption'
)

#We have covered some of the common visualization types. They can be applied to any dataset in tabular form.
#The important criteria when selecting the appropriate visualization type are the characteristic of data (e.g. discrete or continuous) and relationships between columns.
