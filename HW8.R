library(psych)
library(rela)

foreignsurvey<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/PCA-foreigner.txt",
                       stringsAsFactors=F)


head(foreignsurvey)


### Find number of principal components

# Remove NA vals
foreignsurvey = na.omit(foreignsurvey)

# Remove nr, ecosituation, commitment, position, yearbirth, sex

foreignsurvey$nr = NULL
foreignsurvey$ecosituation = NULL
foreignsurvey$commitment = NULL
foreignsurvey$position = NULL
foreignsurvey$yearbirth = NULL
foreignsurvey$sex = NULL

# KMO + Bartlett statistics 

paf.obj <- paf(as.matrix(foreignsurvey))
cat("KMO statistics:", paf.obj$KMO, " Bartlett statistics:",  paf.obj$Bartlett, "\n")

paste("KMO: 0.800 - very good")

bart <- cortest.bartlett(cor(foreignsurvey), n = nrow(foreignsurvey))
unlist(bart)

paste("Bartlett: < 0.05, thus we can use PCA test since not all correlation are zero")

# VSS Scree on data

VSS.scree(foreignsurvey)

paste("Our 15 variables measure 3 underlying components")

# Extract four components and do not perform rotation

pca.foreign <- principal(foreignsurvey, 3, rotate = "none")
pca.foreign$criteria <- NULL
pca.foreign

# Rotate with varimax pca

pca.foreign.r <- principal(foreignsurvey, 3)
pca.foreign.r$criteria <- NULL
print(pca.foreign.r, cut = 0.5, sort = TRUE, digits = 2)


# fa diagram

fa.diagram(pca.foreign.r, cut = 0.5, cex = 0.8, rsize = 0.5, main = "FA Diagram")


# a14 Foreigners out.
# a12 Multicultural means multicriminal.
# a15 Foreigner integration is genocide.
# a13 The boat is full.
# a4 Germany is not the social welfare office of the world.
# 
# C1: Discriminatory behavior
# 
# 
# a5 Good coexistence must be striven for. 
# a8 The right of asylum must be protected throughout Europe.
# a2 Refugee money must be reduced.
# a1 The integration of foreigners must be improved.
# a9 Xenophobia harms the German economy.
# 
# C2: Economical and Integrity based behaviour
# 
# a6 The right of asylum should be restricted.
# a3 German money should be spent on German issues.
# a10 Housing should be created for Germans first.
# a7 Germans are becoming a minority.
# 
# C3: Patriotic behavior

# C1: Discriminatory behavior
alpha(subset(foreignsurvey, select = c(a14, a12, a15, a13, a4)), check.keys =TRUE)

paste("Alpha: 0.82, which is good")

# C2: Economical and Integrity based behaviour
alpha(subset(foreignsurvey, select = c(a5, a8, a2, a1, a9)), check.keys =TRUE)

paste("Alpha: 0.74, which is acceptable, 0.77 without a9, so we can optionally drop a9")

alpha(subset(foreignsurvey, select = c(a5, a8, a2, a1)), check.keys =TRUE)

# C3: Patriotic behavior
alpha(subset(foreignsurvey, select = c(a6, a3, a10, a7)), check.keys =TRUE)
paste("Alpha: 0.74, which is acceptable")


foreignsurvey$discriminatory <- (foreignsurvey$a14 + foreignsurvey$a12 + foreignsurvey$a15 +
                                  foreignsurvey$a13 + foreignsurvey$a4)/5
foreignsurvey$ecoint <- (foreignsurvey$a5 + foreignsurvey$a8 + foreignsurvey$a2 + 
                           foreignsurvey$a1 + foreignsurvey$a9)/5
foreignsurvey$patriotic <- (foreignsurvey$a6 + foreignsurvey$a3 + foreignsurvey$a10 + 
                              foreignsurvey$a7)/4

write.table(foreignsurvey, "PCA-foreigner-survey-extended.txt", sep="\t", row.names=FALSE, na="")

summary(foreignsurvey$discriminatory)
summary(foreignsurvey$ecoint)
summary(foreignsurvey$patriotic)


# association between the extracted components and other variables

foreignsurvey2 = read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/PCA-foreigner.txt",
                                           stringsAsFactors=F)

# hotfix-encoding for categorical data

foreignsurvey2$ecosituation = as.numeric(factor(foreignsurvey2$ecosituation))
foreignsurvey2$commitment = as.numeric(factor(foreignsurvey2$commitment))
foreignsurvey2$position = as.numeric(factor(foreignsurvey2$position))
foreignsurvey2$yearbirth = as.numeric(factor(foreignsurvey2$yearbirth))
foreignsurvey2$sex = as.numeric(factor(foreignsurvey2$sex))
foreignsurvey2$nr = NULL


# Rotate with varimax pca
pca.foreign.r <- principal(foreignsurvey2, 3)
pca.foreign.r$criteria <- NULL
print(pca.foreign.r, cut = 0.5, sort = TRUE, digits = 2)


# fa diagram
fa.diagram(pca.foreign.r, cut = 0.5, cex = 0.8, rsize = 0.5, main = "FA Diagram")
