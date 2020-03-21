#load packages
require("cluster")
require("fpc")
require("factoextra")
require("gridExtra")
library(cluster)
library(fpc)
library(factoextra)
library(gridExtra)
library(dlstats)
library(conjoint)




##drop nan
conjointData1 = conjointData[complete.cases(conjointData), ]
conjointDataNA <- conjointData[is.na(conjointData$ratings),]

iterations = 200
variables = 6
output <- data.frame(matrix(ncol=variables, nrow=iterations))

for(i in 1:200) {
  a = lm(ratings~price+size+motion+style, data = subset(conjointData1, ID==i))
  output[i,1] = i
  output[i,2:6] = a$coefficient[1:5]
  output
}

colnames(output) <- c('id',"intercept", "price",'size','motion','style')

output1 = output[rep(seq_len(nrow(output)), each = 4), ]
for (i in 1:800){
  for (j in 1: 800){
  conjointDataNA[j,3]= output1[i,1]+conjointDataNA[j,4]*output1[i,2]+conjointDataNA[j,5]*output1[i,3]+conjointDataNA[j,6]*output1[i,4]+conjointDataNA[j,7]*output1[i,5]
}
}

for (i in 1:200){
  for (j in c(3,6,10,16)){
    conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 3] = 
      output[i,2] + 
      conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 4] * output[i,3] + 
      conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 5] * output[i,4] + 
      conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 6] * output[i,5] + 
      conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 7] * output[i,6]
  }
}

conjointData_full <-rbind(conjointDataNA, conjointData1)
conjointData_full <-conjointData_full[order(conjointData_full$ID, conjointData_full$profile),]

#load data (conjoint Data)
clustTest = function(toClust,print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(scale){ toClust = scale(toClust);}
  set.seed(seed);   # set random number seed before doing cluster analysis
  wss <- (nrow(toClust)-1)*sum(apply(toClust,2,var))
  for (i in 2:maxClusts) wss[i] <- sum(kmeans(toClust,centers=i,nstart=nstart,iter.max=iter.max)$withinss)
  ##gpw essentially does the following plot using wss above. 
  #plot(1:maxClusts, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
  gpw = fviz_nbclust(toClust,kmeans,method="wss",iter.max=iter.max,nstart=nstart,k.max=maxClusts) #alternative way to get wss elbow chart.
  pm1 = pamk(toClust,scaling=TRUE)
  ## pm1$nc indicates the optimal number of clusters based on 
  ## lowest average silhoutte score (a measure of quality of clustering)
  #alternative way that presents it visually as well.
  gps = fviz_nbclust(toClust,kmeans,method="silhouette",iter.max=iter.max,nstart=nstart,k.max=maxClusts) 
  if(print){
    grid.arrange(gpw,gps, nrow = 1)
  }
  list(wss=wss,pm1=pm1$nc,gpw=gpw,gps=gps)
}
##Runs a set of clusters as kmeans
##Arguments:
##  toClust, data.frame with data to cluster
##  nClusts, vector of number of clusters, each run as separate kmeans 
##  ... some additional arguments to be passed to clusters
##Return:
##  list of 
##    kms, kmeans cluster output with length of nClusts
##    ps, list of plots of the clusters against first 2 principle components
runClusts = function(toClust,nClusts,print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(length(nClusts)>4){
    warning("Using only first 4 elements of nClusts.")
  }
  kms=list(); ps=list();
  for(i in 1:length(nClusts)){
    kms[[i]] = kmeans(toClust,nClusts[i],iter.max = iter.max, nstart=nstart)
    ps[[i]] = fviz_cluster(kms[[i]], geom = "point", data = toClust) + ggtitle(paste("k =",nClusts[i]))
    
  }
  library(gridExtra)
  if(print){
    tmp = marrangeGrob(ps, nrow = 2,ncol=2)
    print(tmp)
  }
  list(kms=kms,ps=ps)
}

##Plots a kmeans cluster as three plot report
##  pie chart with membership percentages
##  ellipse plot that indicates cluster definitions against principle components
##  barplot of the cluster means
plotClust = function(km,toClust,discPlot=FALSE){
  nc = length(km$size)
  if(discPlot){par(mfrow=c(2,2))}
  else {par(mfrow=c(3,1))}
  percsize = paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
  pie(km$size,labels=percsize,col=1:nc)
  
  clusplot(toClust, km$cluster, color=TRUE, shade=TRUE,
           labels=2, lines=0,col.clus=1:nc); #plot clusters against principal components
  
  if(discPlot){
    plotcluster(toClust, km$cluster,col=km$cluster); #plot against discriminant functions ()
  }
  rng = range(km$centers)
  dist = rng[2]-rng[1]
  locs = km$centers+.05*dist*ifelse(km$centers>0,1,-1)
  bm = barplot(km$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
  text(bm,locs,formatC(km$centers,format="f",digits=1))
}

checks = clustTest(output[2:6],print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100)
clusts = runClusts(output[2:6],c(3,4,9,14),print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100)

for(i in 1:3) plotClust(clusts[[1]][[i]],output)

plotClust(clusts[[1]][[1]],output[2:6])
##3 is the best number to make cluster

######################Question 3########################################
#merge two demograpihc and conjoint Data without na
conjointDemo <- merge(conjointData1, respondentData, by = 'ID')

#without segmentation
lm(ratings~price+size+motion+style, data = conjointData)
#when children's age is 2 years
lm(ratings~price+size+motion+style, data = subset(conjointDemo, age == 0))
#when children's age is 3-4 years
lm(ratings~price+size+motion+style, data = subset(conjointDemo, age == 1))
#when gender is 0
lm(ratings~price+size+motion+style, data = subset(conjointDemo, gender == 0))
#when gender is 1
lm(ratings~price+size+motion+style, data = subset(conjointDemo, gender == 1))

#######################question 4 #######################

conjointData_full

#current profile 13 and 5
scen0 = c(13, 5)#current situation
scen1 =c(13, 5, 4, 14, 16)#current situation with all segment preference and highest market share
scen2 = c(13, 4, 14, 16)#delete 5 which has no market share 
scen3 = c(4, 14, 16)# delet 13 which has lowest market share among others
scen4 = c(16, 14)#delere 4
scen5 = c(16,4)#delete 14
scen6 = c(4, 14)#delete 16

conjointChoice = aggregate(x = list(ratings = conjointData_full$ratings), by = list(id = conjointData_full$ID), max)

choice = data.frame(ID = rep(NA,200), profile = rep(NA,200), ratings = rep(NA,200),  price = rep(NA,200), size = rep(NA,200), motion = rep(NA,200),
               style = rep(NA,200))

for(i in 1:200) {
  Y = subset(conjointData_full, ID==i)[,3]
  choice[i,] = subset(conjointData_full, ID==i)[which.max(Y),]
}

unique(choice$profile) #choices include profile 16,6,12,14,4,2,8,15,13
choiceProfile = c(16,6,12,14,4,2,8,15,13)

mktShare = data.frame()
for (z in 1:length(choiceProfile)) {
  marketshare = nrow(subset(choice, profile == choiceProfile[z]))/nrow(choice)
  mktShare[z,1] = choiceProfile[z]
  mktShare[z,2] = marketshare
}
colnames(mktShare) = c('Profile', "market_share")
mktShare = mktShare[order(mktShare$market_share, decreasing = TRUE),]

simProfit = function(inputmat, scen, myProds, prices, vcost, fcosts, mktsize = 1){
  mktshr = mktShare(scen);
  vprofit = mktshr*(price-vcosts)*mktsize;
  sum(vproft[myProds])-fcosts
}


scen0 = c(13, 5)#current situation
scen1 =c(13, 5, 4, 14, 16)#current situation with all segment preference and highest market share
scen2 = c(13, 4, 14, 16)#delete 5 which has no market share 
scen3 = c(4, 14, 16)# delete 13 which has lowest market share among others
scen4 = c(16, 14)#delete 4
scen5 = c(16,4)#delete 14
scen6 = c(4, 14)#delete 16

Profit0 = 0.005*(111.99-33)*4000-40000
Profit1 = 0.005*(111.99-33)*4000+0.3*(95.99-41)*4000+0.13*(95.99-33)*4000+0.265*(95.99-29)*4000-100000
Profit2 =0.005*(111.99-33)*4000+0.3*(95.99-41)*4000+0.13*(95.99-33)*4000+0.265*(95.99-29)*4000-80000
profit3 = 0.3*(95.99-41)*4000+0.13*(95.99-33)*4000+0.265*(95.99-29)*4000-60000
profit4 = 0.3*(95.99-41)*4000+0.13*(95.99-33)*4000-40000
profit5 = 0.3*(95.99-41)*4000+0.265*(95.99-29)*4000-40000
profit6 = 0.13*(95.99-33)*4000+0.265*(95.99-29)*4000-40000


simProf0 = simProfit(choice, scen0, c(1,2), c(111.99,111.99),c(33, 33),40000,4000)







