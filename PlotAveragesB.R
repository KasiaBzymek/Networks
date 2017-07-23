library(xlsx)
library(ggplot2)
library(RColorBrewer)
rm(list = ls())
setwd("D:/Psychology/Master/NewSimulations/RankNodesB")
load("D:/Psychology/Master/NewSimulations/RankNodesB/DepBrankcloseness.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesB/DepBrankinstrength.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesB/DepBrankoutstrength.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesB/DependBrankbetween.RData")

setwd("D:/Psychology/Master/NewSimulations/RankNodesIndepB")
load("D:/Psychology/Master/NewSimulations/RankNodesIndepB/IndepBrankbetween.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesIndepB/IndepBrankcloseness.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesIndepB/IndepBrankinstrength.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesIndepB/IndepBrankoutstrength.RData")

dfdepB<-data.frame(BetweennesBdepB,BetweennesCdepB,BetweennesAdepB,BetweennesBIndB,BetweennesCIndB,BetweennesAIndB)
dfAB<-data.frame(BetweennesAdepB,BetweennesAIndB)
dfBB<-data.frame(BetweennesBdepB,BetweennesBIndB)
dfCB<-data.frame(BetweennesCdepB,BetweennesCIndB)

pdf("AllNodesDepBIndepBrank.pdf")
matplot(dfdepB, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Highest betweenness -total occurance",lty=1:6,ylim=c(0,100))
legend("topright", cex=1.2,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedBrank.pdf")
matplot(dfAB,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Highest betweenness -total occurance",lty=c(3,6),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedBrank.pdf")
matplot(dfBB,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Highest betweenness -total occurance",lty=c(1,4),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedBrank.pdf")
matplot(dfCB,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Highest betweenness -total occurance",lty=c(2,5),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()


########################################################
####### Closeness ######################################
########################################################
dfAllcloseB<-data.frame(CloseBdepB,CloseCdepB,CloseAdepB,CloseBIndB,CloseCIndB,CloseAIndB)
dfABcl<-data.frame(CloseAdepB,CloseAIndB)
dfBBcl<-data.frame(CloseBdepB,CloseBIndB)
dfCBcl<-data.frame(CloseCdepB,CloseCIndB)

pdf("AllNodesDepBIndepBClosenessrank.pdf")
matplot(dfAllcloseB, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Highest closeness -total occurance",lty=1:6,ylim=c(0,100))
legend("right",cex=1.2,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedBClosenessrank.pdf")
matplot(dfABcl,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Highest closeness -total occurance",lty=c(3,6),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedBcloseness.pdf")
matplot(dfBBcl,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Highest closeness -total occurance",lty=c(1,4),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedBCloseness.pdf")
matplot(dfCBcl,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Highest closeness -total occurance",lty=c(2,5),ylim=c(0,100))
legend("right", cex=1.2,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()

#################################################################
#################################################################
##################### In strength ###############################
#################################################################
#################################################################
dfAllinB<-data.frame(InstBdepB,InstCdepB,InstAdepB,InstBIndB,InstCIndB,InstAIndB)
dfABin<-data.frame(InstAdepB,InstAIndB)
dfBBin<-data.frame(InstBdepB,InstBIndB)
dfCBin<-data.frame(InstCdepB,InstCIndB)

pdf("AllNodesDepBIndepBInstrengthrank.pdf")
matplot(dfAllinB, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Highest instrength -total occurance",lty=1:6,ylim=c(0,100))
legend("right",cex=1.2,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedBInstrengthrank.pdf")
matplot(dfABin,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Highest instrength -total occurance",lty=c(3,6),ylim=c(0,100))
legend("right", cex=1.2,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedBInstrengthrank.pdf")
matplot(dfBBin,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Highest instrength -total occurance",lty=c(1,4),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedBInstrengthrank.pdf")
matplot(dfCBin,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Highest instrength -total occurance",lty=c(2,5),ylim=c(0,100))
legend("right", cex=1.2,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()

#################################################################
#################################################################
##################### Out strength ###############################
#################################################################
#################################################################
dfAlloutB<-data.frame(OutBdepB,OutCdepB,OutAdepB,OutBIndB,OutCIndB,OutAIndB)
dfABout<-data.frame(OutAdepB,OutAIndB)
dfBBout<-data.frame(OutBdepB,OutBIndB)
dfCBout<-data.frame(OutCdepB,OutCIndB)

pdf("AllNodesDepBIndepBOutstrengthrank.pdf")
matplot(dfAlloutB, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Highest outstrength -total occurance",lty=1:6,ylim=c(0,100))
legend("right",cex=1.2,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedBOutstrengthrank.pdf")
matplot(dfABout,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Highest outstrength -total occurance",lty=c(3,6),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedBOutstrengthrank.pdf")
matplot(dfBBout,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Highest outstrength -total occurance",lty=c(1,4),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedBOutstrengthrank.pdf")
matplot(dfCBout,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Highest outstrength -total occurance",lty=c(2,5),ylim=c(0,100))
legend("right", cex=1.2,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()