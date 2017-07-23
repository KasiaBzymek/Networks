library(xlsx)
library(ggplot2)
library(RColorBrewer)
rm(list = ls())
setwd("D:/Psychology/Master/NewSimulations/RankNodesIndepC")
load("D:/Psychology/Master/NewSimulations/RankNodesIndepC/IndepCrankbetweenMax.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesIndepC/IndepCrankcloseMax.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesIndepC/IndepCrankinstrenMax.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesIndepC/IndepCrankoutstrengthMax.RData")

setwd("D:/Psychology/Master/NewSimulations/RankNodesC")
load("D:/Psychology/Master/NewSimulations/RankNodesC/DependCrankoutstrength.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesC/DependCrankinstrength.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesC/DependCrankcloseness.RData")
load("D:/Psychology/Master/NewSimulations/RankNodesC/DependCrankbetween.RData")

#################################
dfdepC<-data.frame(BetweennesBdepC,BetweennesCdepC,BetweennesAdepC,BetweennesBIndC,BetweennesCIndC,BetweennesAIndC)
dfAC<-data.frame(BetweennesAdepC,BetweennesAIndC)
dfBC<-data.frame(BetweennesBdepC,BetweennesBIndC)
dfCC<-data.frame(BetweennesCdepC,BetweennesCIndC)

pdf("AllNodesDepCIndepCrank.pdf")
matplot(dfdepC, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Highest betweenness -total occurance",lty=1:6,ylim=c(0,100))
legend("topright", cex=1.2,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedCrank.pdf")
matplot(dfAC,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Highest betweenness -total occurance",lty=c(3,6),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedCrank.pdf")
matplot(dfBC,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Highest betweenness -total occurance",lty=c(1,4),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedCrank.pdf")
matplot(dfCC,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Highest betweenness -total occurance",lty=c(2,5),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()


########################################################
####### Closeness ######################################
########################################################
dfAllcloseC<-data.frame(CloseBdepC,CloseCdepC,CloseAdepC,CloseBIndC,CloseCIndC,CloseAIndC)
dfACcl<-data.frame(CloseAdepC,CloseAIndC)
dfBCcl<-data.frame(CloseBdepC,CloseBIndC)
dfCCcl<-data.frame(CloseCdepC,CloseCIndC)


pdf("AllNodesDepCIndepCClosenessrank.pdf")
matplot(dfAllcloseC, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Highest closeness -total occurance",lty=1:6,ylim=c(0,100))
legend("topright",cex=1.2,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedCClosenessrank.pdf")
matplot(dfACcl,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Highest closeness -total occurance",lty=c(3,6),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedCclosenessrank.pdf")
matplot(dfBCcl,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Highest closeness -total occurance",lty=c(1,4),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedCClosenessrank.pdf")
matplot(dfCCcl,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Highest closeness -total occurance",lty=c(2,5),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()

#################################################################
#################################################################
##################### In strength ###############################
#################################################################
#################################################################
dfAllinC<-data.frame(InstBdepC,InstCdepC,InstAdepC,InstBIndC,InstCIndC,InstAIndC)
dfACin<-data.frame(InstAdepC,InstAIndC)
dfBCin<-data.frame(InstBdepC,InstBIndC)
dfCCin<-data.frame(InstCdepC,InstCIndC)

pdf("AllNodesDepCIndepCInstrengthrank.pdf")
matplot(dfAllinC, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Highest instrength -total occurance",lty=1:6,ylim=c(0,100))
legend("topright",cex=1,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedCInstrengthrank.pdf")
matplot(dfACin,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Highest instrength -total occurance",lty=c(3,6),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedCInstrengthrank.pdf")
matplot(dfBCin,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Highest instrength -total occurance",lty=c(1,4),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedCInstrengthrank.pdf")
matplot(dfCCin,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Highest instrength -total occurance",lty=c(2,5),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()

#################################################################
#################################################################
##################### Out strength ###############################
#################################################################
#################################################################
dfAlloutC<-data.frame(OutBdepC,OutCdepC,OutAdepC,OutBIndC,OutCIndC,OutAIndC)
dfACout<-data.frame(OutAdepC,OutAIndC)
dfBCout<-data.frame(OutBdepC,OutBIndC)
dfCCout<-data.frame(OutCdepC,OutCIndC)

pdf("AllNodesDepCIndepCOutstrengthrank.pdf")
matplot(dfAlloutC, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Highest outstrength -total occurance",lty=1:6,ylim=c(0,100))
legend("topright",cex=1.2,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedCOutstrengthrank.pdf")
matplot(dfACout,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Highest outstrength -total occurance",lty=c(3,6),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedCOutstrengthrank.pdf")
matplot(dfBCout,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Highest outstrength -total occurance",lty=c(1,4),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedCOutstrengthrank.pdf")
matplot(dfCCout,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Highest outstrength -total occurance",lty=c(2,5),ylim=c(0,100))
legend("topright", cex=1.2,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()