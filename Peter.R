#Set directory
setwd("~/Desktop/CLU/R")
#Reay the csv file
cc = read.csv("~/Desktop/CLU/R/CC GENERAL.csv")
View(cc)


#Structure the data
str(cc)
t(t(names(cc)))

#Check the number of variables in the dataset
length(cc)

#Check missing value in each column
missval = sapply(cc, function(x) sum(is.na(x)))
missval

#Check Numerical Variable and Character Variable
library(dplyr)
character = cc %>% select_if(is.character) 
numerical = cc %>% select_if(is.numeric) 
summary(character)
summary(numerical)

# Remove missing data
cc[cc == ""] = NA
cc = cc[complete.cases(cc),]

# Remove non-numerical variables - Cust_ID
cc = cc[,-c(1)]

#Check outliers
install.packages("outliers")
library(outliers)

#Boxplot Denormalized Dataset
boxplot(cc, main = "Denormalized Dataset")


#Check the highest outlier
cch = sapply(cc,function(x) grubbs.test(x))
cch

#Check the lowest outlier
ccl = sapply(cc,function(x) grubbs.test(x,opposite = TRUE))
ccl


#normalized Data
cc.norm = round(scale(cc),digits = 3)
cc.norm
#Boxplot Normalized Dataset
boxplot(cc.norm, main = "Normalized Dataset")

#Group Boxplot illustration
boxplot(cc.norm[, c(1,3,4,5,6,13,14,15)], main = "Normalized Larger Number Group")
boxplot(cc.norm[, c(2,7,8,9,10,11,12,16,17)], main = "Normalized Small Number Group")

#Compute normalized distance
cc.norm.dist = dist(cc.norm,method = "euclidean")
#cc.norm.dist

# Hierarchical Clustering Method testing
hcl = hclust(cc.norm.dist, method = "complete")
plot(hcl,hang = -1, ann = FALSE)
hcl = hclust(cc.norm.dist, method = "single")
plot(hcl,hang = -1, ann = FALSE)
plot(hcl,hang = -1, ann = FALSE)
hcl = hclust(cc.norm.dist, method = "average")
plot(hcl,hang = -1, ann = FALSE)
hcl = hclust(cc.norm.dist, method = "centroid")
plot(hcl,hang = -1, ann = FALSE)
hcl = hclust(cc.norm.dist, method = "ward.D")
plot(hcl,hang = -1, ann = FALSE)

#Hierarchical Clustering and plotting ** finalized ward.D method is the best to illustrate 
hcl = hclust(cc.norm.dist, method = "ward.D")

# Six clustering
plot(hcl,hang = -1, ann = FALSE, cex = 0.6,main = "Dendrogram")
rect.hclust(hcl,k = 6, border = 2:5)

# Eight clustering
plot(hcl,hang = -1, ann = FALSE, cex = 0.6,main = "Dendrogram")
rect.hclust(hcl,k = 8, border = 2:5)


#computing the number of Cluster
memb = cutree(hcl,k = 8)
table(memb)

#Characterize different cluster in un-normalized data
centers.unnorm = aggregate(. ~ memb, data = cc, FUN = mean)
centers.unnorm


#Characterize different cluster in normalized data
cc.norm = as.data.frame(cc.norm)
centers.norm = aggregate(. ~ memb, data = cc.norm, FUN = mean)
centers.norm

#Heat map Plot normlized data
install.packages("circlize")
install.packages("grid")
library(dendextend)
library(circlize)
library(grid)

row.names(cc.norm) = paste(memb, ": ",row.names(cc))
row.names(cc.norm)

row_dend = hclust(dist(cc.norm)) # row clustering
col_dend = hclust(dist(t(cc.norm))) # column clustering
heatmap(cc.norm, name = "cc.norm", 
        row_names_gp = gpar(fontsize = 7),
        cluster_rows = color_branches(row_dend, k = 4),
        cluster_columns = color_branches(col_dend, k = 2))


#heatmap(as.matrix(cc.norm),Colv = NA,hclustfun = hclust,scale = "row")

#Export the cluster center to csv file
write.csv(centers.norm,file = "characterize.csv", row.names = FALSE)





