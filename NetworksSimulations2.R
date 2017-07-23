#load libraries 
library(lattice)
library(car) 
library(psych)
library(mvtnorm)
library(arm)
library(qgraph)
library(bootnet)
library(ggplot2)
library(reshape2)
library(base)
library(base64)
library(foreach)
library(data.table)
library(plyr)

#setwd("I:/new documents/kasia/R")
#########################################################
###########################################################

rm(list = ls())

setwd("D:/Psychology/Master/NewSimulations/IndepB41-50")
set.seed(13)
# set the parameters values
N<- 500
a11 <- 0.7
a12 <- -0.2
a13<- 0.1
a21 <- -0.1
a22 <- 0.75
a23<- -0.3
a31<- 0.1
a32<- -0.1
a33<- 0.6
A <- matrix(c(a11, a12, a13, a21, a22, a23, a31, a32, a33), ncol = 3, byrow = T)
A
Re(eigen(A, only.values = T)$values) 

NMaxExtraNodes=3
n <- 500

varianceError=0.8^2
Sigma=diag(rep(varianceError,NMaxExtraNodes)) #each std will be 0.8
mu=rep(0,NMaxExtraNodes)

#create all errors in matrix form, one column are the errors of one observed A variable
epsAll=mvrnorm(n = n, mu, Sigma)
apply(epsAll, 2, sd) # see that each column has indeed std around 0.8

#Martix of y's
y=matrix(rep(t(0),NMaxExtraNodes),ncol=NMaxExtraNodes,nrow=500)
head(y) 

for (j in 2:n) {
  y[j,] = A%*%y[j-1,] + epsAll[j,]
}

# Plot to check how the processes look like - for information purposes only, also use further y1
x <- seq(1:500)
df <- data.frame(x, y1 = y[(1:500),1], y2 = y[(1:500),2], y3 = y[(1:500),3])
df.melted <- melt(df, id.vars = "x")
g <- ggplot(data = df.melted, aes(x = x, y = value, color = variable)) + geom_line()
g 

### put the data together into matrix
allData1 <-cbind(df$y1,df$y2,df$y3)
# linear regression
n <- 500
t <- 2:n

t<- 2:n
y.fit1<-list()
for (j in 1:3){
  y.fit1[[j]]<-lm(allData1[t,j]~allData1[t-1,]) #column j at t is predicted by all columns at time t-1
}

coefs<-ldply(y.fit1, coef)
LatE <-cbind(from=rep(1:3, each=3), to=rep(1:3,3),weightL=unlist(coefs[,2:4]))
LatE1 <- t(coefs)
LatE1<-LatE1[2:4,]
LatE2<-LatE1
LatE2<-LatE2[2:3,2:3]
##############Plot graph ##################
LatQ<- qgraph(LatE, edge.labels = TRUE, height = 60, width = 60,labels=c("A","B","C"))
LatQ<-qgraph(LatE, filetype = "pdf", filename="NetworkABC", edge.labels = TRUE, height = 54,width = 60,labels=c("A","B","C"))

LatQ1<- qgraph(LatE1, edge.labels = TRUE, height = 60, width = 60,labels=c("A","B","C"))
LatQ2<- qgraph(LatE2, edge.labels = TRUE, height = 60, width = 60,labels=c("A","B"))


# using qgraph package claculate the centrality measures of weighted graph 
centrality(LatQ)

# write the centrality plots into the pdf files
pdf("centNetworkABC.pdf")
centralityPlot(LatQ)
dev.off()

# save the centrality measure parameters into the .txt file
sink("centMeasNetworkABC.txt")
centrality_auto(LatQ)
sink()

write.csv(centrality_auto(LatQ),file="centMeasNetworkABC.csv")
write.xlsx
centrality_auto(LatQ)
sink()

##############################################################################33333
####################################################################################
############ Dependent variables ###############################################3
###############################################################################
####################################################################################

######################################################################################
###########   Network observable variables     ##################
######################################################################################
N=500
y1 <- y[(1:500),1]
y2 <- y[(1:500),2]
y3 <- y[(1:500),3]
epsB1 <- rnorm(N,0,0.8)
epsC1 <- rnorm(N,0,0.8)
B1 <- y2 + epsB1
C1 <- y3 + epsC1

NMaxExtraNodes=10
n <- 500

varianceError=0.8^2
Sigma=diag(rep(varianceError,NMaxExtraNodes)) #each std will be 0.8
mu=rep(0,NMaxExtraNodes)

#create all errors in matrix form, one column are the errors of one observed A variable
epsAAll=mvrnorm(n = n, mu, Sigma)
apply(epsAAll, 2, sd) # see that each column has indeed std around 0.8

#create matrix where eache column is y1.data
AAllLatent=matrix(rep(t(y1),NMaxExtraNodes),ncol=NMaxExtraNodes)
head(AAllLatent) #see that each column is the same

#create observed variables: we add the error to each column
AAll=AAllLatent+ epsAAll
head(AAll) #see that for each column we added error

#create a matrix with all data we will use
nObserved=10  #this is an example for nObserved=3. But you can now also loop over nObserved. Just use for(nObserved in 1:10){

for(nObserved in 1:10){
  allData=cbind(B1,C1,AAll[,1:nObserved])
  labels=c("B1","C1",paste("A", 1:nObserved, sep = ""))
  colnames(allData)=labels
  
  #same as you do but don't use formulas and just use matrices
  t<- 2:n
  y.fit<-list()
  for (j in 1:(nObserved+2)){
    y.fit[[j]]<-lm(allData[t,j]~allData[t-1,]) #column j at t is predicted by all columns at time t-1
  }
  

  #approximately same as you do from now on. I just used 'paste' a few times so the filename changes when you loop over nObserved
  coefsA<-ldply(y.fit, coef)
  EA <-cbind(from=rep(1:(nObserved+2), each=(nObserved+2)), to=rep(1:(nObserved+2),(nObserved+2)),weightA=unlist(coefsA[,2:(nObserved+3)]))

# create a network with a qgraph package, and save it as pdf
  #QA<- qgraph(EA, edge.labels = TRUE, height = 60, width = 60, labels=labels)
  QA<-qgraph(EA, filetype = "pdf", filename=paste("Network_A7",toString(nObserved),sep=""), edge.labels = TRUE, height = 54,width = 60, labels=labels)

# using qgraph package claculate the centrality measures of weighted graph 
centrality(QA)

# write the centrality plots into the pdf files
pdf(paste("CentNetwork_A7",toString(nObserved),".pdf",sep=""))
centralityPlot(QA)
dev.off()

# save the centrality measure parameters into the .txt file
sink(paste("centMeas_A7",toString(nObserved),".txt",sep=""))
print(centrality_auto(QA))
sink()
} 

#################################################
#################################################
#################################################
for (i in 1:10){
  corela <-cor(AAll[,i],y1)
  print(corela)
}
for (i in 1:9){
  corelaa <-cor(AAll[,i],AAll[,i+1])
  print(corelaa)
}

EA1<-t(coefsA)
EA1<-EA1[2:13,]
EA1<-EA1[3:12,3:12]

QAcheck<- qgraph(EA1, edge.labels = TRUE, height = 60, width = 60,labels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10"))
QA1<-qgraph(EA1, filetype = "pdf",filename="NetworkAa7", edge.labels = TRUE, height = 54,width = 60, labels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10"))
centrality(QCI1)

pdf("CentralityAa7.pdf")
centralityPlot(QA1)
dev.off()

sink("centMsAa7.txt")
print(centrality_auto(QA1))
sink()

EA2<-t(coefsA)
EA2<-EA2[2:13,]
EA2<-EA2[1:10,1:10]

QAcheck2<- qgraph(EA2, edge.labels = TRUE, height = 60, width = 60,labels=c("B1","C1","A1","A2","A3","A4","A5","A6","A7","A8"))
QA2<-qgraph(EA2, filetype = "pdf",filename="NetworkAb7", edge.labels = TRUE, height = 54,width = 60, labels=c("B1","C1","A1","A2","A3","A4","A5","A6","A7","A8"))
centrality(QA2)

pdf("CentralityAb7.pdf")
centralityPlot(QA2)
dev.off()

sink("centMsAb7.txt")
print(centrality_auto(QA2))
sink()
##################################################################
#######################################################################
####################################################################
###BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB BBBBBBBBBBBB BBBBB
#########################################################
##############################################################
#######################################################################

y1 = y[(1:500),1]
y2 = y[(1:500),2]
y3 = y[(1:500),3]
epsA1 <- rnorm(N,0,0.8)
epsC1 <- rnorm(N,0,0.8)
A1 <- y1 + epsA1
C1 <- y3 + epsC1
# AB <- y1.data + y2.data + epsAB

NMaxExtraNodes=10
n <- 500

varianceError=0.8^2
Sigma=diag(rep(varianceError,NMaxExtraNodes)) #each std will be 0.8
mu=rep(0,NMaxExtraNodes)

#create all errors in matrix form, one column are the errors of one observed A variable
epsBAll=mvrnorm(n = n, mu, Sigma)
apply(epsBAll, 2, sd) # see that each column has indeed std around 0.8

#create matrix where eache column is y1.data
BAllLatent=matrix(rep(t(y2),NMaxExtraNodes),ncol=NMaxExtraNodes)
head(BAllLatent) #see that each column is the same

#create observed variables: we add the error to each column
BAll=BAllLatent+epsBAll
head(BAll) #see that for each column we added error

#create a matrix with all data we will use
nObserved=10  #this is an example for nObserved=3. But you can now also loop over nObserved. Just use for(nObserved in 1:10){

for(nObserved in 1:10){
  allDataB=cbind(A1,C1,BAll[,1:nObserved])
  labels=c("A1","C1",paste("B", 1:nObserved, sep = ""))
  colnames(allDataB)=labels
  
  #same as you do but don't use formulas and just use matrices
  t2<- 2:n
  y.fitB=list()
  for (j in 1:(nObserved+2)){
    y.fitB[[j]]<-lm(allDataB[t,j]~allDataB[t-1,]) #column j at t is predicted by all columns at time t-1
  }
  
  #approximately same as you do from now on. I just used 'paste' a few times so the filename changes when you loop over nObserved
  coefsB<-ldply(y.fitB, coef)
  EB <-cbind(from=rep(1:(nObserved+2), each=(nObserved+2)), to=rep(1:(nObserved+2),(nObserved+2)),weightB=unlist(coefsB[,2:(nObserved+3)]))

  # create a network with a qgraph package, and save it as pdf
   #QB<- qgraph(EB, edge.labels = TRUE, height = 60, width = 60, labels=labels)
  QB<-qgraph(EB, filetype = "pdf", filename=paste("Network_B40",toString(nObserved),sep=""), edge.labels = TRUE, height = 54,width = 60, labels=labels)

# using qgraph package claculate the centrality measures of weighted graph 
centrality(QB)

# write the centrality plots into the pdf files
pdf(paste("CentNetwork_B40",toString(nObserved),".pdf",sep=""))
centralityPlot(QB)
dev.off()

# save the centrality measure parameters into the .txt file
sink(paste("centMs_B40",toString(nObserved),".txt",sep=""))
print(centrality_auto(QB))
sink()
}

for (i in 1:10){
  corelb[i] <-cor(BAll[,i],y2)
  print(corelb)
}
##################################################################
#######################################################################
####################################################################
###CCCCCCCCCCCC CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCCC CCCCCCCCCCCCCC
#########################################################
##############################################################
#######################################################################

y1 = y[(1:500),1]
y2 = y[(1:500),2]
y3 = y[(1:500),3]
epsA1 <- rnorm(N,0,0.8)
epsB1 <- rnorm(N,0,0.8)
A1 <- y1 + epsA1
B1 <- y2 + epsB1

NMaxExtraNodes=10
n <- 500

varianceError=0.8^2
Sigma=diag(rep(varianceError,NMaxExtraNodes)) #each std will be 0.8
mu=rep(0,NMaxExtraNodes)

#create all errors in matrix form, one column are the errors of one observed A variable
epsCAll=mvrnorm(n = n, mu, Sigma)
apply(epsCAll, 2, sd) # see that each column has indeed std around 0.8

#create matrix where eache column is y1.data
CAllLatent=matrix(rep(t(y3),NMaxExtraNodes),ncol=NMaxExtraNodes)
head(CAllLatent) #see that each column is the same

#create observed variables: we add the error to each column
CAll=CAllLatent+epsCAll
head(CAll) #see that for each column we added error

#create a matrix with all data we will use
nObserved=10  #this is an example for nObserved=3. But you can now also loop over nObserved. Just use for(nObserved in 1:10){

for(nObserved in 1:10){
  allDataC=cbind(A1,B1,CAll[,1:nObserved])
  labels=c("A1","B1",paste("C", 1:nObserved, sep = ""))
  colnames(allDataC)=labels
  
  #same as you do but don't use formulas and just use matrices
  t<- 2:n
  y.fitC=list()
  for (j in 1:(nObserved+2)){
    y.fitC[[j]]<-lm(allDataC[t,j]~allDataC[t-1,]) #column j at t is predicted by all columns at time t-1
  }
  
  #approximately same as you do from now on. I just used 'paste' a few times so the filename changes when you loop over nObserved
  coefsC<-ldply(y.fitC, coef)
  EC <-cbind(from=rep(1:(nObserved+2), each=(nObserved+2)), to=rep(1:(nObserved+2),(nObserved+2)),weightC=unlist(coefsC[,2:(nObserved+3)]))

# create a network with a qgraph package, and save it as pdf
   #QC <- qgraph(EC, edge.labels = TRUE, height = 60, width = 60, labels=labels)
  QC<-qgraph(EC, filetype = "pdf", filename=paste("Network_C88",toString(nObserved),sep=""), edge.labels = TRUE, height = 54,width = 60, labels=labels)

# using qgraph package claculate the centrality measures of weighted graph 
centrality(QC)

# write the centrality plots into the pdf files
pdf(paste("CentNetwork_C88",toString(nObserved),".pdf",sep=""))
centralityPlot(QC)
dev.off()

# save the centrality measure parameters into the .txt file
sink(paste("centMeas_C88",toString(nObserved),".txt",sep=""))
print(centrality_auto(QC))
sink()
}

for (i in 1:10){
  corelc[i]<-cor(CAll[,i],y3)
  print(corelc)
 # corelc[[i]] <- corelc[i]
}

#correlations
corel <- data.frame(corela,corelb,corelc)
colnames(corel) <- c("A", "B","C")
boxplot(corel)
l <- reshape(corel, 
             varying  = c("A", "B","C"), 
             v.names = "Correlation",
             timevar = "Nodes", 
             times = c("A", "B","C"), 
             # new.row.names = 1:318,
             direction = "long")
# geom_boxplot proposes several arguments to custom appearance
correlation <- ggplot(l, aes(x=as.factor(Nodes), y=Correlation)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Nodes")

pdf("Nodes_correlation.pdf")
print(correlation)
dev.off()


# ###############################################
# #################################################
# #### D D D D D ####################################
# ###############################################
# 
# y1 = y[(1:500),1]
# y2 = y[(1:500),2]
# y3 = y[(1:500),3]
# N=500
# epsB1 <- rnorm(N,0,0.8)
# epsC1 <- rnorm(N,0,0.8)
# B1 <- y2 + epsB1
# C1 <- y3 + epsC1
# 
# NMaxExtraNodes=10
# varianceError=0.8^2
# Sigma=diag(rep(varianceError,NMaxExtraNodes)) #each std will be 0.8
# mu=rep(0,NMaxExtraNodes)
# 
# #create all errors in matrix form, one column are the errors of one observed A variable
# epsDAll=mvrnorm(n = n, mu, Sigma)
# apply(epsDAll, 2, sd) # see that each column has indeed std around 0.8
# 
# DLatent=matrix(rep(t(y2+y3),NMaxExtraNodes),ncol=NMaxExtraNodes)
# head(DLatent)
# 
# # create observable variables
# Dall=DLatent + epsDAll
# head(Dall)
# 
# # Check the correlations between D and B and C
# sink("correlation_B&D.txt")
# for (i in 1:10){
#   corel<-cor(Dall[,i],B1)
#   print(corel)
# }
# sink()
# sink("correlation_C$D.txt")
# for (i in 1:10){
#   corel1<-cor(Dall[,i],C1)
#   print(corel1)
# }
# sink()
# 
# #create a matrix with all data we will use
# nObserved=2  #this is an example for nObserved=3. But you can now also loop over nObserved. Just use for(nObserved in 1:10){
# 
# for(nObserved in 1:2){
#   allDataD=cbind(B1,C1,Dall[,1:nObserved])
#   labels=c("B1","C1",paste("D", 1:nObserved, sep = ""))
#   colnames(allDataD)=labels
#   
#   #same as you do but don't use formulas and just use matrices
#   t<- 2:n
#   y.fitD=list()
#   for (j in 1:(nObserved+2)){
#     y.fitD[[j]]<-lm(allDataD[t,j]~allDataD[t-1,]) #column j at t is predicted by all columns at time t-1
#   }
#   
#   #approximately same as you do from now on. I just used 'paste' a few times so the filename changes when you loop over nObserved
#   coefsD<-ldply(y.fitD, coef)
#   ED <-cbind(from=rep(1:(nObserved+2), each=(nObserved+2)), to=rep(1:(nObserved+2),(nObserved+2)),weightD=unlist(coefsD[,2:(nObserved+3)]))
#   # create a network with a qgraph package, and save it as pdf
#   #QD<- qgraph(ED, edge.labels = TRUE, height = 60, width = 60, labels=labels)
#   QD<-qgraph(ED, filetype = "pdf", filename=paste("Network_1D",toString(nObserved),sep=""), edge.labels = TRUE, height = 54,width = 60, labels=labels)
#   
#   # using qgraph package claculate the centrality measures of weighted graph 
#   centrality(QD)
#   
#   # write the centrality plots into the pdf files
#   pdf(paste("CentNetworkD",toString(nObserved),".pdf",sep=""))
#   centralityPlot(QD)
#   dev.off()
#   
#   # save the centrality measure parameters into the .txt file
#   sink(paste("centMeasD",toString(nObserved),".txt",sep=""))
#   print(centrality_auto(QD))
#   sink()
# } 



#########################################################################
#########################################################################
#########################################################################
#########################################################################
N=500
y1 <- y[(1:500),1]
y2 <- y[(1:500),2]
y3 <- y[(1:500),3]
epsB1 <- rnorm(N,0,0.8)
epsC1 <- rnorm(N,0,0.8)
B1 <- y2 + epsB1
C1 <- y3 + epsC1

NMaxExtraNodes=10
n <- 500

#create matrix where eache column is y1.data
AIndep=matrix((mvrnorm(N*NMaxExtraNodes,0,0.8)),ncol=NMaxExtraNodes)
head(AIndep) #see that each column is the same


#create a matrix with all data we will use
nObserved=10  #this is an example for nObserved=3. But you can now also loop over nObserved. Just use for(nObserved in 1:10){

for(nObserved in 1:10){
  allDataI=cbind(B1,C1,AIndep[,1:nObserved])
  labels=c("BI1","CI1",paste("AI", 1:nObserved, sep = ""))
  colnames(allDataI)=labels
  
  #same as you do but don't use formulas and just use matrices
  t<- 2:n
  y.fitI<-list()
  for (j in 1:(nObserved+2)){
    y.fitI[[j]]<-lm(allDataI[t,j]~allDataI[t-1,]) #column j at t is predicted by all columns at time t-1
  }

  #approximately same as you do from now on. I just used 'paste' a few times so the filename changes when you loop over nObserved
  coefsAI<-ldply(y.fitI, coef)
  EAI <-cbind(from=rep(1:(nObserved+2), each=(nObserved+2)), to=rep(1:(nObserved+2),(nObserved+2)),weightAI=unlist(coefsAI[,2:(nObserved+3)]))
  
  # create a network with a qgraph package, and save it as pdf
 # QAI<- qgraph(EAI, edge.labels = TRUE, height = 60, width = 60, labels=labels)
  QAI<-qgraph(EAI, filetype = "pdf", filename=paste("Network_AI65",toString(nObserved),sep=""), edge.labels = TRUE, height = 54,width = 60, labels=labels)
  
  # using qgraph package claculate the centrality measures of weighted graph 
  centrality(QAI)
  
  # write the centrality plots into the pdf files
  pdf(paste("CentNetwork_AI65",toString(nObserved),".pdf",sep=""))
  centralityPlot(QAI)
  dev.off()
  
  # save the centrality measure parameters into the .txt file
  sink(paste("centMeas_AI65",toString(nObserved),".txt",sep=""))
  print(centrality_auto(QAI))
  sink()
} 



##################################################################
#######################################################################
####################################################################
###Indep BBBBB   INDEP BBBBBBBBBBBBBBBB #################################
#########################################################
##############################################################
#######################################################################

y1 = y[(1:500),1]
y2 = y[(1:500),2]
y3 = y[(1:500),3]
epsA1 <- rnorm(N,0,0.8)
epsC1 <- rnorm(N,0,0.8)
A1 <- y1 + epsA1
C1 <- y3 + epsC1
# AB <- y1.data + y2.data + epsAB

NMaxExtraNodes=10
n <- 500

#create matrix where eache column is y1.data
BIndep=matrix((mvrnorm(N*NMaxExtraNodes,0,0.8)),ncol=NMaxExtraNodes)
head(BIndep) #see that each column is the same
#create a matrix with all data we will use
nObserved=10  #this is an example for nObserved=3. But you can now also loop over nObserved. Just use for(nObserved in 1:10){

for(nObserved in 1:10){
  allDataBI=cbind(A1,C1,BIndep[,1:nObserved])
  labels=c("AI1","CI1",paste("BI", 1:nObserved, sep = ""))
  colnames(allDataBI)=labels
  
  #same as you do but don't use formulas and just use matrices
  t<- 2:n
  y.fitBI=list()
  for (j in 1:(nObserved+2)){
    y.fitBI[[j]]<-lm(allDataBI[t,j]~allDataBI[t-1,]) #column j at t is predicted by all columns at time t-1
  }
  
  #approximately same as you do from now on. I just used 'paste' a few times so the filename changes when you loop over nObserved
  coefsBI<-ldply(y.fitBI, coef)
  EBI <-cbind(from=rep(1:(nObserved+2), each=(nObserved+2)), to=rep(1:(nObserved+2),(nObserved+2)),weightBI=unlist(coefsBI[,2:(nObserved+3)]))
  
  # create a network with a qgraph package, and save it as pdf
  #QBI<- qgraph(EBI, edge.labels = TRUE, height = 60, width = 60, labels=labels)
  QBI<-qgraph(EBI, filetype = "pdf", filename=paste("Network_BI50",toString(nObserved),sep=""), edge.labels = TRUE, height = 54,width = 60, labels=labels)
  
  # using qgraph package claculate the centrality measures of weighted graph 
  centrality(QBI)
  
  # write the centrality plots into the pdf files
  pdf(paste("CentNetwork_BI50",toString(nObserved),".pdf",sep=""))
  centralityPlot(QBI)
  dev.off()
  
  # save the centrality measure parameters into the .txt file
  sink(paste("centMs_BI50",toString(nObserved),".txt",sep=""))
  print(centrality_auto(QBI))
  sink()
}


##################################################################
#######################################################################
####################################################################
###Indep BBBBB   INDEP CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC #################################
#########################################################
##############################################################
#######################################################################

y1 = y[(1:500),1]
y2 = y[(1:500),2]
y3 = y[(1:500),3]
epsA1 <- rnorm(N,0,0.8)
epsB1 <- rnorm(N,0,0.8)
A1 <- y1 + epsA1
B1 <- y2 + epsB1
# AB <- y1.data + y2.data + epsAB

NMaxExtraNodes=10
n <- 500

#create matrix where eache column is y1.data
CIndep=matrix((mvrnorm(N*NMaxExtraNodes,0,0.8)),ncol=NMaxExtraNodes)
head(CIndep) #see that each column is the same
#create a matrix with all data we will use
nObserved=10  #this is an example for nObserved=3. But you can now also loop over nObserved. Just use for(nObserved in 1:10){

for(nObserved in 1:10){
  allDataCI=cbind(A1,B1,CIndep[,1:nObserved])
  labels=c("AI1","BI1",paste("CI", 1:nObserved, sep = ""))
  colnames(allDataCI)=labels
  
  #same as you do but don't use formulas and just use matrices
  t<- 2:n
  y.fitCI=list()
  for (j in 1:(nObserved+2)){
    y.fitCI[[j]]<-lm(allDataCI[t,j]~allDataCI[t-1,]) #column j at t is predicted by all columns at time t-1
  }
  
  #approximately same as you do from now on. I just used 'paste' a few times so the filename changes when you loop over nObserved
  coefsCI<-ldply(y.fitCI, coef)
  ECI <-cbind(from=rep(1:(nObserved+2), each=(nObserved+2)), to=rep(1:(nObserved+2),(nObserved+2)),weightCI=unlist(coefsCI[,2:(nObserved+3)]))
  
  # create a network with a qgraph package, and save it as pdf
  #QBI<- qgraph(EBI, edge.labels = TRUE, height = 60, width = 60, labels=labels)
  QCI<-qgraph(ECI, filetype = "pdf", filename=paste("Network_CI90",toString(nObserved),sep=""), edge.labels = TRUE, height = 54,width = 60, labels=labels)
  
  # using qgraph package claculate the centrality measures of weighted graph 
  centrality(QCI)
  
  # write the centrality plots into the pdf files
  pdf(paste("CentNetwork_CI90",toString(nObserved),".pdf",sep=""))
  centralityPlot(QCI)
  dev.off()
  
  # save the centrality measure parameters into the .txt file
  sink(paste("centMs_CI90",toString(nObserved),".txt",sep=""))
  print(centrality_auto(QCI))
  sink()
}

# create observable variables
Dall=DLatent + epsDAll
head(Dall)

# Check the correlations between D and B and C
sink("correlation_c.txt")
for (i in 1:10){
  corel<-cor(Dall[,i],B1)
  print(corel)
}
sink()
sink("correlation_C$D.txt")
for (i in 1:10){
  corel1<-cor(CIndep[,i],C1)
  print(corel1)
}
sink()




##########All added nodes are based on a normally distributed random variable

y1 = y[(1:500),1]
y2 = y[(1:500),2]
y3 = y[(1:500),3]
epsA1 <- rnorm(N,0,0.8)
epsB1 <- rnorm(N,0,0.8)
A1 <- epsA1
B1 <- epsB1


NMaxExtraNodes=5
n <- 500

#create matrix where eache column is y1.data
CIndep=matrix((mvrnorm(N*NMaxExtraNodes,0,0.8)),ncol=NMaxExtraNodes)
head(CIndep) #see that each column is the same
#create a matrix with all data we will use
nObserved=5  #this is an example for nObserved=3. But you can now also loop over nObserved. Just use for(nObserved in 1:10){

for(nObserved in 1:5){
  allDataCI=cbind(A1,B1,CIndep[,1:nObserved])
  labels=c("AI1","BI1",paste("CI", 1:nObserved, sep = ""))
  colnames(allDataCI)=labels
  
  #same as you do but don't use formulas and just use matrices
  t<- 2:n
  y.fitCI=list()
  for (j in 1:(nObserved+2)){
    y.fitCI[[j]]<-lm(allDataCI[t,j]~allDataCI[t-1,]) #column j at t is predicted by all columns at time t-1
  }
  
  #approximately same as you do from now on. I just used 'paste' a few times so the filename changes when you loop over nObserved
  coefsCI<-ldply(y.fitCI, coef)
  ECI <-cbind(from=rep(1:(nObserved+2), each=(nObserved+2)), to=rep(1:(nObserved+2),(nObserved+2)),weightCI=unlist(coefsCI[,2:(nObserved+3)]))
  
  # create a network with a qgraph package, and save it as pdf
  
  QCI<-qgraph(ECI, filetype = "pdf", filename=paste("Network_C1",toString(nObserved),sep=""), edge.labels = TRUE, height = 54,width = 60, labels=labels)
  
  # using qgraph package claculate the centrality measures of weighted graph 
  centrality(QCI)
  
  # write the centrality plots into the pdf files
  pdf(paste("CentNetwork_C1",toString(nObserved),".pdf",sep=""))
  centralityPlot(QCI)
  dev.off()
  
  # save the centrality measure parameters into the .txt file
  sink(paste("centMs_C1",toString(nObserved),".txt",sep=""))
  print(centrality_auto(QCI))
  sink()
}


CD<-t(coefsCI)
CD1<-CD[2:8,]
numbers<-runif(50,-0.3,0.3)
row<-c(numbers[7:12],0.5)
CD2<-rbind(CD1,row)
column<-c(numbers[1:7],0.5)
CD2<-cbind(CD2,column)
CD2[8,7]<-runif(1,-0.3,0.3)


#QCI<- qgraph(ECI, edge.labels = TRUE, height = 60, width = 60, labels=labels)
QCDcheck<- qgraph(CD2, edge.labels = TRUE, height = 60, width = 60, labels=c("A1","B1","C1","C2","C3","C4","C5","D"))
QD2<-qgraph(CD2, filetype = "pdf",filename="NetworkDff20", edge.labels = TRUE, height = 54,width = 60, labels=c("A1","B1","C1","C2","C3","C4","C5","D"))

pdf("CentralityDff20.pdf")
centralityPlot(QD2)
dev.off()

sink("centMsDff20.txt")
print(centrality_auto(QD2))
sink()

###########################################
CD2
numbers<-runif(50,-0.3,0.3)
row1<-c(numbers[13:19],0.4)
CD3<-rbind(CD2,row1)
column1<-c(numbers[21:28],0.4)
CD3<-cbind(CD3,column1)
CD3[9,8]<-runif(1,-0.3,0.3)
CD3[9,2]<--0.6


#QCI<- qgraph(ECI, edge.labels = TRUE, height = 60, width = 60, labels=labels)
QCDcheck3<- qgraph(CD3, edge.labels = TRUE, height = 60, width = 60, labels=c("A1","B1","C1","C2","C3","C4","C5","D1","D2"))
QD3<-qgraph(CD3, filetype = "pdf",filename="NetworkDepdstd20", edge.labels = TRUE, height = 54,width = 60, labels=c("A1","B1","C1","C2","C3","C4","C5","D1","D2"))

pdf("CentralityDepdstd20.pdf")
centralityPlot(QD3)
dev.off()

sink("centMsDepdstd20.txt")
print(centrality_auto(QD3))
sink()





 ############## Try out ##################3
  
  y1 = y[(1:500),1]
  y2 = y[(1:500),2]
  y3 = y[(1:500),3]
  epsA1 <- rnorm(N,0,0.8)
  epsB1 <- rnorm(N,0,0.8)
  A1 <- y1 + epsA1
  B1 <- y2 + epsB1
  # AB <- y1.data + y2.data + epsAB
  
  NMaxExtraNodes=10
  n <- 500
  
  #create matrix where eache column is y1.data
  CIndep=matrix((mvrnorm(N*NMaxExtraNodes,0,0.8)),ncol=NMaxExtraNodes)
  head(CIndep) #see that each column is the same
  #create a matrix with all data we will use
  nObserved=10  #this is an example for nObserved=3. But you can now also loop over nObserved. Just use for(nObserved in 1:10){
  
  for(nObserved in 1:10){
    allDataCI=cbind(A1,B1,CIndep[,1:nObserved])
    labels=c("AI1","BI1",paste("CI", 1:nObserved, sep = ""))
    colnames(allDataCI)=labels
    
    #same as you do but don't use formulas and just use matrices
    t<- 2:n
    y.fitCI=list()
    for (j in 1:(nObserved+2)){
      y.fitCI[[j]]<-lm(allDataCI[t,j]~allDataCI[t-1,]) #column j at t is predicted by all columns at time t-1
    }
  
    #approximately same as you do from now on. I just used 'paste' a few times so the filename changes when you loop over nObserved
    coefsCI<-ldply(y.fitCI, coef)
    
    ECI <-cbind(from=rep(1:(nObserved+2), each=(nObserved+2)), to=rep(1:(nObserved+2),(nObserved+2)),weightCI=unlist(coefsCI[,2:(nObserved+3)]))
       # create a network with a qgraph package, and save it as pdf
    QCI<-qgraph(ECI, filetype = "pdf", filename=paste("Network_CI4",toString(nObserved),sep=""), edge.labels = TRUE, height = 54,width = 60, labels=labels)
    # using qgraph package claculate the centrality measures of weighted graph 
    centrality(QCI)
   
 # write the centrality plots into the pdf files
    pdf(paste("CentNetwork_CI4",toString(nObserved),".pdf",sep=""))
    centralityPlot(QCI)
    dev.off()
  # save the centrality measure parameters into the .txt file
    sink(paste("centMs_CI4",toString(nObserved),".txt",sep=""))
    print(centrality_auto(QCI))
    sink()

}
  
  ECI1<-t(coefsCI)
  ECI1<-ECI1[2:13,]
  ECI1<-ECI1[3:12,3:12]
  
  QCIcheck<- qgraph(ECI1, edge.labels = TRUE, height = 60, width = 60,labels=c("C1","C2","C3","C4","C5","C6","C7","C8"))
  QCI1<-qgraph(ECI1, filetype = "pdf",filename="NetworkCn1", edge.labels = TRUE, height = 54,width = 60, labels=c("C1","C2","C3","C4","C5","C6","C7","C8"))
  centrality(QCI1)
  
  pdf("CentralityCn1.pdf")
  centralityPlot(QCI1)
  dev.off()
  
  ECI1<-t(coefsCI)
  ECI1<-ECI1[2:13,]
  ECI1<-ECI1[3:10,3:10]
  
  QCIcheck<- qgraph(ECI1, edge.labels = TRUE, height = 60, width = 60,labels=c("C1","C2","C3","C4","C5","C6","C7","C8"))
  QCI1<-qgraph(ECI1, filetype = "pdf",filename="NetworkCn1", edge.labels = TRUE, height = 54,width = 60, labels=c("C1","C2","C3","C4","C5","C6","C7","C8"))
  centrality(QCI1)
  
  pdf("CentralityCn1.pdf")
  centralityPlot(QCI1)
  dev.off()
  
  sink("centMsCn1.txt")
  print(centrality_auto(QCI1))
  sink()
  
