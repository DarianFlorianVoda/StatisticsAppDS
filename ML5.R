# Import libaries
library(ggplot2)
library(GGally)
library(dplyr)
library(plotly)


# Import the dataframe PenguinsWithoutMissingValues from the csv file.
data = read.csv("C:/Users/Dari-Laptop/Desktop/FH Karnten - Master - AppDs/StatisticsAppDSLaptop/PenguinsWithoutMissingValues.csv")



# 1) How many obserations are there?
count(data)

# 2) What are the names of the observed variables?
names(data)

# 3) How many penguins per Species are there?
data %>% count(Species)

# 4) How many penguins per Species and Gender are there?
data %>% 
  group_by(Species, Gender) %>%
  summarise(n = n())

# 5) BodyMass statistics per Species and Gender
# 
# Create a table with following information; 
# Per Species and Gender: 
#   
# - the amount of observations, 
# - minimum of BodyMass, 
# - median of BodyMass, 
# - average of BodyMass, 
# - maximum of BodyMass.

data %>%
  group_by(Species, Gender) %>%
  summarise(Observations = n(), 
            Minimum = min(BodyMass.g.),
            Median = median(BodyMass.g.),
            Average = mean(BodyMass.g.),
            Maximum = max(BodyMass.g.))


# 6. Quantitative statistics per Species and Gender?
#   Create a table with following information; Per Island, Species and Gender:
#   
# the amount of observations,
# average of CulmenLength.mm.,
# average of CulmenDepth.mm.,
# average of FlipperLength.mm.,
# average of BodyMass.g.

data %>%
  group_by(Island, Species, Gender) %>%
  summarise(Observations = n(), 
            CulmenLengthAvg = mean(CulmenLength.mm.),
            CulmenDepthAvg = mean(CulmenDepth.mm.),
            FlipperLengthAvg = mean(FlipperLength.mm.),
            BodyMassAvg = mean(BodyMass.g.))


# 7) Provide a summary statistics of the columns

summary(data)


## Pairs plot

ggpairs(data, columns=4:7, ggplot2::aes(colour=Species))


# 1) Use a histogram (from library ggplot) to visualize CulmenLength.mm.
ggplot(data, aes(x=CulmenLength.mm.)) + geom_histogram()

# 2) Use different colors for different Species
ggplot(data, aes(x=CulmenLength.mm., fill=Species)) + 
  geom_histogram()

# 3) Use the facet plot for different Species 
ggplot(data, aes(x=CulmenLength.mm., fill=Species)) + 
  geom_histogram() + facet_grid(.~Species)

# 4) Use the facet plot for different Species and Gender
ggplot(data, aes(x=CulmenLength.mm., fill=Species)) + 
  geom_histogram() + facet_grid(Species ~ Gender)

# 1) Use a density plot for CulmenLength.mm. for the different Species, facet_grid the Gender
ggplot(data, aes(x=CulmenLength.mm., fill=Species)) + 
  geom_density() + facet_grid(.~Gender)

# 2) Use a density plot for CulmenLength.mm. for the different Species, facet_grid the Island
ggplot(data, aes(x=CulmenLength.mm., fill=Species)) + 
  geom_density() + facet_grid(.~Island)

# Use a boxplot for CulmenLength.mm. for the different Species, facet_grid the Gender
ggplot(data, aes(x=CulmenLength.mm., fill=Species)) + 
  geom_boxplot() + facet_grid(.~Gender)

# Use a scatterplot for CulmenLength.mm. and BodyMass.g. 
# 
# Create 
# 
# - a scatterplot for CulmenLength.mm. and BodyMass.g. 
# - color different Species
# - facet_grid for Gender

ggplot(data, aes(x=CulmenLength.mm., y = BodyMass.g., color=Species)) + 
  geom_point() + facet_grid(.~Gender)


## Interactive Plots 

# Create 
# 
# - a scatterplot for CulmenLength.mm. and BodyMass.g. 
# - color the different Species
# - facet_grid for Gender
# - create a ggploty plot out of it
# - add the information of IndividualID in the hoover-text
# 
# Can you spot any outliers? What's the IndividualID of the potential outlier? 

p = ggplot(data, aes(x=CulmenLength.mm., y = BodyMass.g., color=Species, text = paste("IndividualID :", IndividualID))) + 
  geom_point() + facet_grid(.~Gender)

ggplotly(p)
