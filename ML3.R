data = read.csv("C:/Users/Dari-Laptop/Desktop/FH Karnten - Master - AppDs/StatisticsAppDSLaptop/penguins_lter.csv")
library(dplyr)


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
cnt = rowSums(is.na(df) | df == "") > 0
sum(cnt)

# What about data imputation? Check the rows with NA values. Can you apply data imputation on it?

new_df <- df[rowSums(is.na(df)) > 0,]

head(new_df)


