options(scipen=999)  # turn-off scientific notation like 1e+48
library(ggplot2)
theme_set(theme_bw()) 
# pre-set the bw theme.
data("midwest", package = "ggplot2")
midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source

#dig into midwest using df functions
View(midwest)
# Scatterplot
gg <- ggplot(midwest, aes(x=area, y=poptotal))+ 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F)+ 
  #xlim(c(0, 0.1)) + 
  #ylim(c(0, 500000)) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")

plot(gg)

#using encircle (scatterplot + encircle)
#install.packages("ggalt")
library(ggalt)
midwest_select <- midwest[midwest$poptotal >                      350000 &                                 midwest$poptotal <= 500000 & 
                  midwest$area > 0.01 & 
                  midwest$area < 0.1, ]

# Plot
ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) +   # draw points
  geom_smooth(method="loess", se=F) + # draw       smoothing line
  #xlim(c(0, 0.1)) + 
  #ylim(c(0, 500000)) +   
  geom_encircle(aes(x=area, y=poptotal), 
                data=midwest_select, 
                color="red", 
                size=2, 
                expand=0.08) +   
# encircle, remember we created the 6 points in the midwest select dataframe
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot + Encircle", 
       caption="Source: midwest")

#scatterplot
data(mpg, package="ggplot2") # alternate source: "http://goo.gl/uEeRGu")
theme_set(theme_bw())  # pre-set the bw theme.

g <- ggplot(mpg, aes(cty, hwy)) #creating an object g

# Scatterplot, referencing object g
g + geom_point() + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Scatterplot with overlapping points", 
       caption="Source: midwest")

dim(mpg)
#The original data has 234 data points but the chart seems to display fewer points. What has happened? This is because there are many overlapping points appearing as a single dot. 

#The fact that both cty and hwy are integers in the source dataset made it all the more convenient to hide this detail. 
#Be extra careful the next time you make scatterplot with integers.

#jitter plot
#overlapping points are randomly jittered around its original position based on a threshold controlled by the width argument.
# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(mpg, aes(cty, hwy))
g + geom_jitter(width = .5, size=1) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Jittered Points")

#count charts
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(mpg, aes(cty, hwy))
g + geom_count(col="tomato3", show.legend=F) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Counts Plot")

#bubble plot
#While scatterplot lets you compare the relationship between 2 continuous variables, bubble chart serves well if you want to understand relationship within the underlying groups based on:
  
#A Categorical variable (by changing the color) and
#Another continuous variable (by changing the size of points).
#bubble charts are more suitable if you have 4-Dimensional data where two of them are numeric (X and Y) and one other categorical (color) and another numeric variable (size).

mpg_select <- mpg[mpg$manufacturer %in% c("audi", "ford", "honda", "hyundai"), ] #want all rows where manufacturer gives us brand

# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(mpg_select, aes(displ, cty)) + 
  labs(subtitle="mpg: Displacement vs City         Mileage",
       title="Bubble chart")
#plot(g) #stop here - an aes needs a geom else the plot area is blank
g + geom_jitter(aes(col=manufacturer, size=hwy))+     geom_smooth(aes(col=manufacturer), method="lm", se=F)

#correlogram
library(ggcorrplot)

# creates a correlation matrix
data(mtcars)
View(mtcars)
glimpse(mtcars)
corr <- round(cor(mtcars), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white",                     "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)

#heatmap
# http://margintale.blogspot.in/2012/04/ggplot2-time-series-heatmaps.html

library(plyr)
library(scales)
library(zoo)

df <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/yahoo.csv")
df$date <- as.Date(df$date)  # format date
df <- df[df$year >= 2012, ]  # filter reqd years
df
# Create Month Week
df$yearmonth <- as.yearmon(df$date)
df$yearmonthf <- factor(df$yearmonth)
df <- ddply(df,.(yearmonthf), transform, monthweek=1+week-min(week))  # compute week number of month
df <- df[, c("year", "yearmonthf", "monthf", "week", "monthweek", "weekdayf", "VIX.Close")]
head(df)

# Plot Heatmap
ggplot(df, aes(monthweek, weekdayf, fill = VIX.Close)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) + 
  scale_fill_gradient(low="red", high="green") +
  labs(x="Week of Month",
       y="",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Yahoo Closing Price", 
       fill="Close")

