#Visualizing survey data

#install.packages("ggalluvial")
library(ggalluvial)

data(vaccinations)
View(vaccinations)

# Provides the geom_alluvium and geom_stratum functions to create alluvial diagrams in ggplot2. 
# You will need to pass your data in long format, specify the axis variables inside aes and use the previous functions.

#Note that you can also add the text for each stratum, add the names of the axis variables (in the following examples you won't see them because we set theme_void) and change the ggplot2 theme, among other customizations.

ggplot(data = vaccinations,
  aes(axis1 = survey, axis2 = response, y = freq)) +
  geom_alluvium(aes(fill = response)) +
  geom_stratum() +
  geom_text(stat = "stratum",
  aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Survey", "Response"),
  expand = c(0.15, 0.05)) +
  theme_void()

#if data set contains more categorical variables you can pass them to aes (axis1, axis2, axis3 .)

library(ggalluvial)

ggplot(data = vaccinations,
       aes(axis1 = survey,   
           # First variable on the X-axis
           axis2 = response, 
           # Second variable on the X-axis
           axis3 = survey,   
           # Third variable on the X-axis
           y = freq)) +
  geom_alluvium(aes(fill = response)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Survey", "Response"),
            expand = c(0.15, 0.05)) +
  theme_void()