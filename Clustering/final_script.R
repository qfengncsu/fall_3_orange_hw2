rm(list=ls()); gc()

setwd("C:/Users/Carlos7/Desktop/Courses/Clustering/Final_Project")
setwd("C:/Users/Bill/Documents/NCSU/Course Work/Fall/Clustering/Data")

library(dplyr)
library(splines)
library(mclust)
library(factoextra)
library(tidyverse)


##reading the R file:
load("final_data.RData")   #this will give me the final dataset


times <- seq(1,295)/100 # Observations in 1/100th of a second
X <- bs(times,intercept=TRUE,df=60) #create a spline to 
#model the data
betas <- matrix(0,ncol=60,nrow = 6792)
###########################################################
# run a linear regression on each data set
# here I am manipulating my data you I can cluster
###########################################################
for (ii in 1:6792){
  temp <- lm(as.numeric(final_data[ii,6:300])~X-1) #-1 removes the natural intercept
  betas[ii,]  <- coefficients(temp)
}
cdata <- cbind(final_data[,1:5],betas)

#CONVERT EVERTYING TO 'numbers'
cdata$AGE <- as.numeric(cdata$AGE)
cdata$EVER_SMOKE <- as.numeric(cdata$EVER_SMOKE)
cdata$ASTHMA <- as.numeric(cdata$ASTHMA)
cdata$POVERTY_RATIO <- as.numeric(cdata$POVERTY_RATIO)



##Part One###
#a) Perform a principal components analysis on columns 2 through 65. List the standard
#deviations for the first 5 components. 
set.seed(12345)

pca_result <- princomp(cdata[,2:65])
pca_result$sdev[1:5]





##b) Using all pca scores compute the optimal number of clusters using kmeans using both
##"wss" and the "silhouette" method. What is the optimal number of components using each 
##method. Why may this number be different?

set.seed(12345)
fviz_nbclust(pca_result$scores, kmeans, method = "wss",k.max=20) ##between 2 and 4 approx.
fviz_nbclust(pca_result$scores, kmeans, method = "silhouette",k.max=20)  ##2 would be the best


##c) Run the command "set.seed(12345)" and run a k-means clustering algorithm using the pca scores.


set.seed(12345)
kmean_4 <- kmeans(pca_result$scores,4,nstart=25)
cdata$clust <- kmean_4$cluster


#c)-a) Compute the graph of mean spirometry for the 4 clusters (all 4 on one graph). 


aux_1 <- aggregate(cdata[,6:65], by = list(cdata$clust), mean, na.rm=TRUE)

aux_1 <- as.matrix(aux_1)

class(aux_1)


bl1 <- matrix(aux_1[1,-1],60,1)
plot(times,as.matrix(X)%*%bl1,ylab="ML",xlab = "Time",type = 'l',lwd=2,col=1)#,ylim=c(-5,5))
bl2 <- matrix(aux_1[2,-1],60,1)  #red
lines(times,X%*%bl2,lwd=2,col=2)
bl3 <- matrix(aux_1[3,-1],60,1) #green
lines(times,X%*%bl3,lwd=2,col=3)
bl4 <- matrix(aux_1[4,-1],60,1) #blue
lines(times,X%*%bl4,lwd=2,col=4)
legend(1.8, 70, legend=c("Cluster 1", "Cluster 2", "Cluster 3","Cluster 4"),
       col=c("black","red","green" ,"blue"),lty=1,  cex=1 , y.intersp = 0.6)



#c)-b) Look at cluster 3. Plot the graph of this cluster and give the mean values (on
#the original scale) for columns 2-65. What makes this cluster different from 
#the other clusters?  Describe this cluster so a physician can better understand
#important characteristics of these clusters.  


bl3 <- matrix(aux_1[3,-1],60,1) #green
plot(times,X%*%bl3,ylab="ML",xlab = "Time",type = 'l',lwd=2,col="green")#,ylim=c(-5,5))
legend(1.8, 20, legend=c("Cluster 3"),
       col=c("green"),lty=1,  cex=1 , y.intersp = 0.6)

#aux_2 <- aggregate(cdata[,2:65], by = list(cdata$clust), mean, na.rm=TRUE)
#aux_2 <- as.matrix(aux_2)
#t(aux_2)



###
# Understanding the clusters
###
#
clusters <- list()
for( ii in 1:4){
  clusters[[ii]] <-  cdata %>% filter(clust == ii)
}
#
# Finding the means of each cluster to "Name them"
x_means <- cbind(colMeans(cdata))
y <- x_means
for (ii in 1:4) {
  x_means <- cbind(x_means,colMeans(clusters[[ii]]))
}

colnames(x_means) <- c("Overall","Cluster1","Cluster2","Cluster3","Cluster4")
print(x_means[1:5,])






#c)-c) Looking at clusters 1,2, and 4 which clusters has the largest lung capacity?
#which one has the least lung capacity? Describe these three groups in terms of 
#the curves as well as the additional variables that are available in the data 
#frame cdata. Provide figures with your descriptions. 


##Area under the curve - longest lung capacity

sum(bl1 * 0.01) #black  -largest lung capacity  - Cluster 1
sum(bl2 * 0.01) #red    -least lung capacity  - Cluster 2
sum(bl3 * 0.01) #green
sum(bl4 * 0.01) #blue




##Jacob's method:
sfun <- splinefun(times,X%*%bl1) 
integrate(sfun,min(times),max(times)) # Longest lung capacity  - Cluster 1

sfun <- splinefun(times,X%*%bl2) 
integrate(sfun,min(times),max(times))  #least lung capacity  - Cluster 2

sfun <- splinefun(times,X%*%bl3)
integrate(sfun,min(times),max(times))

sfun <- splinefun(times,X%*%bl4) 
integrate(sfun,min(times),max(times))


print(x_means[1:5,-4])


plot(times,as.matrix(X)%*%bl1,ylab="ML",xlab = "Time",type = 'l',lwd=2,col="black", ylim = c(0,95))#red
lines(times,X%*%bl2,lwd=2,col="red") #green
lines(times,X%*%bl4,lwd=2,col="blue") #blue
legend(1.8, 70, legend=c("Cluster 1", "Cluster 2","Cluster 4"),
       col=c("black","red" ,"blue"),lty=1,  cex=1 , y.intersp = 0.6)



##Part Two###
#NOW look at the data using MCLUST type 'set.seed(12345)': 


#a) Using mclustbic() and columns 10-20 of cdata (NOT the principal component values).
#estimate the optimal number of  cluster components using the BIC and only with 
#modelNames='VVV' and G = 1:20. Show a graph of the estimate. Is this number different than 
#the ones given above, why? (This will take a while). 

#less amount of columns, betas that have to be with specific portions of the distribution

library(mclust)

set.seed(12345)
clustBIC <-mclustBIC(cdata[,10:20], G = 1:20, modelNames = "VVV") 
plot(clustBIC)
summary(clustBIC)


#b) Now using G = 6 and modelNames='VVV' and the same columns, provide a graph of each cluster's mean curve (USING ALL OF THE DATA COLUMNS). 
#           Put all plots on one graph. 


set.seed(12345)
clustBIC_final <-Mclust(cdata[,10:20], G = 6, modelNames = "VVV") 

cdata$class <- as.factor(clustBIC_final$classification)


aux_2 <- aggregate(cdata[,6:65], by = list(cdata$class), mean, na.rm=TRUE)

aux_2 <- as.matrix(aux_2)
class(aux_2)


bl2_1 <- matrix(as.numeric(aux_2[1,-1]),60,1) #black
bl2_2 <- matrix(as.numeric(aux_2[2,-1]),60,1) #red
bl2_3 <- matrix(as.numeric(aux_2[3,-1]),60,1) #green
bl2_4 <- matrix(as.numeric(aux_2[4,-1]),60,1) #blue
bl2_5 <- matrix(as.numeric(aux_2[5,-1]),60,1) #orange
bl2_6 <- matrix(as.numeric(aux_2[6,-1]),60,1) #purple


plot(times,as.matrix(X)%*%bl2_1,ylab="ML",xlab = "Time",type = 'l',lwd=2,col=1, ylim = c(0,90))#,ylim=c(-5,5))
lines(times,X%*%bl2_2,lwd=2,col=2)
lines(times,X%*%bl2_3,lwd=2,col=3)
lines(times,X%*%bl2_4,lwd=2,col=4)
lines(times,X%*%bl2_5,lwd=2,col="orange")
lines(times,X%*%bl2_6,lwd=2,col="purple")
legend(1.8, 70, legend=c("Cluster 1", "Cluster 2", "Cluster 3","Cluster 4", "Cluster 5","Cluster 6"),
       col=c("black","red","green" ,"blue","orange","purple"),lty=1,  cex=1 , y.intersp = 0.6)



#	c) Using all of the data compare cluster 4 with cluster 3 from the kmeans() cluster what can you 	
#say about the similarities between these two clusters, what are the differences? Which estimate 
#makes more sense? What do you trust more? What are the benefits of using mixture modeling over
#kmeans, what are the issues?


sum(bl2_1 * 0.01)
sum(bl2_2 * 0.01)
sum(bl2_3 * 0.01)
sum(bl2_4 * 0.01) # Least lung capacity  - Cluster 4
sum(bl2_5 * 0.01)
sum(bl2_6 * 0.01) # Longest lung capacity  - Cluster 6

plot(times,as.matrix(X)%*%bl2_4,ylab="ML",xlab = "Time",type = 'l',lwd=2,col=1, ylim = c(0,30))
lines(times,X%*%bl3,lwd=2,col=2)

cdata$class <- as.numeric(cdata$class)

###
# Understanding the clusters
###
#
clusters2 <- list()
for( ii in 1:6){
  clusters2[[ii]] <-  cdata %>% filter(class == ii)
}
#


# Finding the means of each cluster to "Name them"
x_means2 <- cbind(colMeans(cdata))
y <- x_means2
for (ii in 1:6) {
  x_means2 <- cbind(x_means2,colMeans(clusters2[[ii]]))
}

colnames(x_means2) <- c("Overall","Cluster1","Cluster2","Cluster3","Cluster4","Cluster 5","Cluster 6")
print(x_means2[1:5,])

