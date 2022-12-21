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
