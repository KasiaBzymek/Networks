library(xlsx)
library(ggplot2)
library(RColorBrewer)
rm(list = ls())

setwd("D:/Psychology/Master/NewSimulations/RankNodesA")
load("D:/Psychology/Master/NewSimulations/RankNodesA/DepArankclose.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesA/DepArankInStrength.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesA/DepArankOutStrength.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesA/DepAranksbetween.RData")

setwd("D:/Psychology/Master/NewSimulations/RankNodesIndepA")
load("D:/Psychology/Master/NewSimulations/RankNodesIndepA/IndepArankcloseness.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesIndepA/IndepArankinstrength.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesIndepA/IndepArankoutstrength.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesIndepA/IndepenArankbetween.RData")

##
df<-data.frame(BetweennesBdepA,BetweennesCdepA,BetweennesAdepA,BetweennesBIndA,BetweennesCIndA,BetweennesAIndA)
dfAA<-data.frame(BetweennesAdepA,BetweennesAIndA)
dfBA<-data.frame(BetweennesBdepA,BetweennesBIndA)
dfCA<-data.frame(BetweennesCdepA,BetweennesCIndA)

pdf("AllNodesDepAIndepARank.pdf")
matplot(df, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Highest betweenness -total occurance",lty=1:6,ylim=c(0,100))
legend("top", cex=1,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedARank.pdf")
matplot(dfAA,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Highest betweenness -total occurance",lty=c(3,6),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedARank.pdf")
matplot(dfBA,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Highest betweenness -total occurance",lty=c(1,4),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedARank.pdf")
matplot(dfCA,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Highest betweenness -total occurance",lty=c(2,5),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()

########################################################
####### Closeness ######################################
########################################################
dfAllclose<-data.frame(CloseBdepA,CloseCdepA,CloseAdepA,CloseBIndA,CloseCIndA,CloseAIndA)
dfAAcl<-data.frame(CloseAdepA,CloseAIndA)
dfBAcl<-data.frame(CloseBdepA,CloseBIndA)
dfCAcl<-data.frame(CloseCdepA,CloseCIndA)

pdf("AllNodesDepAIndepAClosenessRank.pdf")
matplot(dfAllclose, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Highest closeness -total occurance",lty=1:6,ylim=c(0,100))
legend("right",cex=1.2,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedAClosenessRank.pdf")
matplot(dfAAcl,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Highest closeness -total occurance",lty=c(3,6),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedAclosenessRank.pdf")
matplot(dfBAcl,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Highest closeness -total occurance",lty=c(1,4),ylim=c(0,100))
legend("right", cex=1.2,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedAClosenessRank.pdf")
matplot(dfCAcl,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Highest closeness -total occurance",lty=c(2,5),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()

#################################################################
#################################################################
##################### In strength ###############################
#################################################################
#################################################################
dfAllin<-data.frame(InstBdepA,InstCdepA,InstAdepA,InstBIndA,InstCIndA,InstAIndA)
dfAAin<-data.frame(InstAdepA,InstAIndA)
dfBAin<-data.frame(InstBdepA,InstBIndA)
dfCAin<-data.frame(InstCdepA,InstCIndA)

pdf("AllAinstrengthrank.pdf")
matplot(dfAllin, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Highest instrength -total occurance",lty=1:6,ylim=c(0,100))
legend("right",cex=1.2,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedAInstrengthRank.pdf")
matplot(dfAAin,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Highest instrength -total occurance",lty=c(3,6),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedAInstrengthRank.pdf")
matplot(dfBAin,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Highest instrength -total occurance",lty=c(1,4),ylim=c(0,100))
legend("right", cex=1.2,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedAInstrengthRank.pdf")
matplot(dfCAin,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Highest instrength -total occurance",lty=c(2,5),ylim=c(0,100))
legend("right", cex=1.2,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()

#################################################################
#################################################################
##################### Out strength ###############################
#################################################################
#################################################################
dfAllout<-data.frame(OutBdepA,OutCdepA,OutAdepA,OutBIndA,OutCIndA,OutAIndA)
dfAAout<-data.frame(OutAdepA,OutAIndA)
dfBAout<-data.frame(OutBdepA,OutBIndA)
dfCAout<-data.frame(OutCdepA,OutCIndA)

pdf("AllNodesDepAIndepAOutstrengthRank.pdf")
matplot(dfAllout, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Highest outstrength -total occurance",lty=1:6,ylim=c(0,100))
legend("topright",cex=0.8,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedAOutstrengthRank.pdf")
matplot(dfAAout,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Highest outstrength -total occurance",lty=c(3,6),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedAOutstrengthRank.pdf")
matplot(dfBAout,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Highest outstrength -total occurance",lty=c(1,4),ylim=c(0,100))
legend("right", cex=1.2,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedAOutstrengthRank.pdf")
matplot(dfCAout,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Highest outstrength -total occurance",lty=c(2,5),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()