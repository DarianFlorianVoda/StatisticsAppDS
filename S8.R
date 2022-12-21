library(psych)
library(rela)

dolesurvey<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/dole-survey.txt",
                       stringsAsFactors=F)

dole<-na.omit(dolesurvey)
dole$ssn<-NULL
dole$sex<-NULL
dole$overall<-NULL

head(dole)


paf.obj <- paf(as.matrix(dole))
cat("KMO statistics:", paf.obj$KMO, " Bartlett statistics:", paf.obj$Bartlett, "\n")
bart <- cortest.bartlett(cor(dole), n = nrow(dole))
unlist(bart)

VSS.scree(dole)


pca.dole <- principal(dole, 4, rotate = "none")
pca.dole$criteria <- NULL
pca.dole



pca.dole.r <- principal(dole, 4)
pca.dole.r$criteria <- NULL
print(pca.dole.r, cut = 0.5, sort = TRUE, digits = 2)

fa.diagram(pca.dole.r, cut = 0.5, cex = 0.8, rsize =
             0.5, main = "")

# Clarity of information
alpha(subset(dole, select = c(v16, v13, v17, v2, v9)), check.keys =TRUE)


# Decency and appropiateness
alpha(subset(dole, select = c(v3, v1, v5, v20)), check.keys =TRUE)

# Helpfulness of the contact person
alpha(subset(dole, select = c(v11, v7, v6, v8)), check.keys =TRUE)

# Reliability of agreements
alpha(subset(dole, select = c(v4, v14, v12)), check.keys=TRUE)

dole$reliability <- (dole$v4 + dole$v14 + dole$v12)/3
dole$clarity <- (dole$v16 + dole$v13 + dole$v17 + dole$v2 + dole$v9)/5
dole$decency <- (dole$v3 + dole$v1 + dole$v5 + dole$v20)/4
dole$helpfulness <- (dole$v11 + dole$v7 + dole$v6 +dole$v8)/4

write.table(dole, "dole-survey-extended.txt", sep="\t", row.names=FALSE, na="")


summary(dole$reliability)
summary(dole$clarity)
summary(dole$decency)
summary(dole$helpfulness)

