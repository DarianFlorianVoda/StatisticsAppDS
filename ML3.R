data = read.csv("C:/Users/Dari-Laptop/Desktop/FH Karnten - Master - AppDs/StatisticsAppDSLaptop/penguins_lter.csv")
library(dplyr)
library(ggplot2)

head(data)

names(data)

str(data)

drop <- c('studyName', 'Region', 'Stage', 'Clutch.Completion', 'Date.Egg', 'Delta.15.N..o.oo.',
          'Delta.13.C..o.oo.', 'Comments', 'Sample.Number')

df = data[,!(names(data) %in% drop)]

head(df)

## Rearrange

reorder = c("Individual.ID", names(df[,names(df)!="Individual.ID"]))
reorder

df <- df[, reorder]
head(df)


## Rename
names(df)[1] = "IndividualID"
names(df)[2] = "Species"
names(df)[3] = "Island"
names(df)[4] = "CulmenLength(mm)"
names(df)[5] = "CulmenDepth(mm)"
names(df)[6] = "FlipperLength(mm)"
names(df)[7] = "BodyMass(g)"
names(df)[8] = "Gender"

head(df)


# unique

unique(df$Species)

# rename species


df$Species[df$Species=="Adelie Penguin (Pygoscelis adeliae)"] = "Adele"
df$Species[df$Species=="Chinstrap penguin (Pygoscelis antarctica)"] = "Chinstrap"
df$Species[df$Species=="Gentoo penguin (Pygoscelis papua)"] = "Gentoo"

unique(df$Species)

head(df)


# missing values
summary(df)


# rows that have empty entries in the qualitative columns

# first method
qualitative = dplyr::select_if(df, is.numeric)
head(qualitative)
summary(qualitative)


# second method
cnt = rowSums(is.na(df) | df == "" | df == ".") > 0
cnt
sum(cnt)

# What about data imputation? Check the rows with NA values. Can you apply data imputation on it?

new_df <- df[-c(4, 340),]

head(new_df)

## Create Dataframe DataWithoutGender  


# Select the rows where the gender is unknown and create a new data set for it. Name it "PenguinsWithoutGender"
PenguinsWithoutGender <- df[is.na(df$Gender) | df$Gender == "" | df$Gender == ".", ]

PenguinsWithoutGender

# Delete the rows with NA values in the quantitative columns by using the function na.omit() 
na.omit(PenguinsWithoutGender)


# replace "." by "" in the column Gender
PenguinsWithoutGender$Gender[PenguinsWithoutGender$Gender=="."] = ""

PenguinsWithoutGender


write.csv(PenguinsWithoutGender, file='C:/Users/Dari-Laptop/Desktop/FH Karnten - Master - AppDs/StatisticsAppDSLaptop/DataWithoutGender.csv', row.names = FALSE)


## Create Dataframe PenguinsWithoutMissingValues  

# Delete the rows with NA values in the quantitative columns by using the function na.omit() 

PenguinsWithoutMissingValues = na.omit(df)

PenguinsWithoutMissingValues = PenguinsWithoutMissingValues[-which(PenguinsWithoutMissingValues$Gender== "" | PenguinsWithoutMissingValues$Gender =="."),]
head(PenguinsWithoutMissingValues)



write.csv(PenguinsWithoutMissingValues, file='C:/Users/Dari-Laptop/Desktop/FH Karnten - Master - AppDs/StatisticsAppDSLaptop/PenguinsWithoutMissingValues.csv', row.names = FALSE)

names(PenguinsWithoutMissingValues)
names(PenguinsWithoutMissingValues)[7] = "BodyMass"


ggplot(PenguinsWithoutMissingValues, aes(x=BodyMass, color=Species))+
  geom_boxplot(outlier.colour="orange", outlier.shape=8, outlier.size=4)


# Change histogram plot fill colors by groups
ggplot(PenguinsWithoutMissingValues, aes(x=BodyMass, fill=Species, color=Species)) +
  geom_histogram(position="identity")

# Use semi-transparent fill
p<-ggplot(df, aes(x=BodyMass, fill=Species, color=Species)) +
  geom_histogram(position="identity", alpha=0.5)
p
