library(xlsx)
library(ggplot2)
library(RColorBrewer)
rm(list = ls())
setwd("D:/Psychology/Master/NewSimulations/PlotswithaddednodeB")


normal<-c(2,6,12,20,30,42,56,72,90,110)
NBdepB<-BetweennesBdepB/normal
NCdepB<-BetweennesCdepB/normal
NAdepB<-BetweennesAdepB/normal
NBindepB<-BetweennesBIndB/normal
NCindepB<-BetweennesCIndB/normal
NAindepB<-BetweennesAIndB/normal

dfdepB<-data.frame(NBdepB,NCdepB,NAdepB,NBindepB,NCindepB,NAindepB)
dfAB<-data.frame(NAdepB,NAindepB)
dfBB<-data.frame(NBdepB,NBindepB)
dfCB<-data.frame(NCdepB,NCindepB)

pdf("AllNodesDepBIndepB.pdf")
matplot(dfdepB, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Betweenness",lty=1:6)
legend("topright", cex=0.6,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedB.pdf")
matplot(dfAB,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Betweennes",lty=c(3,6))
legend(8,0.02, cex=0.6,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedB.pdf")
matplot(dfBB,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Betweennes",lty=c(1,4))
legend("topright", cex=0.9,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedB.pdf")
matplot(dfCB,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Betweennes",lty=c(2,5))
legend("topright", cex=0.6,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()


########################################################
####### Closeness ######################################
########################################################
normalclos<-c(2:11)
NBdepBcl<-CloseBdepB*normalclos
NCdepBcl<-CloseCdepB*normalclos
NAdepBcl<-CloseAdepB*normalclos
NBIndBcl<-CloseBIndB*normalclos
NCIndBcl<-CloseCIndB*normalclos
NAIndBcl<-CloseAIndB*normalclos

dfAllcloseB<-data.frame(NBdepBcl,NCdepBcl,NAdepBcl,NBIndBcl,NCIndBcl,NAIndBcl)
dfABcl<-data.frame(NAdepBcl,NAIndBcl)
dfBBcl<-data.frame(NBdepBcl,NBIndBcl)
dfCBcl<-data.frame(NCdepBcl,NCIndBcl)

pdf("AllNodesDepBIndepBCloseness.pdf")
matplot(dfAllcloseB, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Closeness",lty=1:6,ylim=c(0,0.3))
legend("topright",cex=0.7,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedBCloseness.pdf")
matplot(dfABcl,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Closeness",lty=c(3,6))
legend("topright", cex=0.7,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedBcloseness.pdf")
matplot(dfBBcl,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Closeness",lty=c(1,4))
legend("topright", cex=0.8,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedBCloseness.pdf")
matplot(dfCBcl,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Closeness",lty=c(2,5))
legend(8,0.12, cex=0.7,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()

#################################################################
#################################################################
##################### In strength ###############################
#################################################################
#################################################################
normalin<-c(2:11)
NBdepBin<-InstBdepB/normalin
NCdepBin<-InstCdepB/normalin
NAdepBin<-InstAdepB/normalin
NBIndBin<-InstBIndB/normalin
NCIndBin<-InstCIndB/normalin
NAIndBin<-InstAIndB/normalin

dfAllinB<-data.frame(NBdepBin,NCdepBin,NAdepBin,NBIndBin,NCIndBin,NAIndBin)
dfABin<-data.frame(NAdepBin,NAIndBin)
dfBBin<-data.frame(NBdepBin,NBIndBin)
dfCBin<-data.frame(NCdepBin,NCIndBin)

pdf("AllNodesDepBIndepBInstrength.pdf")
matplot(dfAllinB, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="In-strength",lty=1:6,ylim=c(0,0.33))
legend("topright",cex=0.7,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedBInstrength.pdf")
matplot(dfABin,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="In-strength",lty=c(3,6))
legend("topright", cex=0.8,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedBInstrength.pdf")
matplot(dfBBin,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="In-strength",lty=c(1,4))
legend("topright", cex=0.8,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedBInstrength.pdf")
matplot(dfCBin,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="In-strength",lty=c(2,5))
legend("topright", cex=0.8,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()

#################################################################
#################################################################
##################### Out strength ###############################
#################################################################
#################################################################
normalout<-c(2:11)
NBdepBout<-OutBdepB/normalout
NCdepBout<-OutCdepB/normalout
NAdepBout<-OutAdepB/normalout
NBindBout<-OutBIndB/normalout
NCindBout<-OutCIndB/normalout
NAindBout<-OutAIndB/normalout

dfAlloutB<-data.frame(NBdepBout,NCdepBout,NAdepBout,NBindBout,NCindBout,NAindBout)
dfABout<-data.frame(NAdepBout,NAindBout)
dfBBout<-data.frame(NBdepBout,NBindBout)
dfCBout<-data.frame(NCdepBout,NCindBout)

pdf("AllNodesDepBIndepBOutstrength.pdf")
matplot(dfAlloutB, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Out-strength",lty=1:6,ylim=c(0,0.4))
legend("topright",cex=0.7,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedBOutstrength.pdf")
matplot(dfABout,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Out-strength",lty=c(3,6))
legend("topright", cex=0.7,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedBOutstrength.pdf")
matplot(dfBBout,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Out-strength",lty=c(1,4))
legend("topright", cex=0.7,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedBOutstrength.pdf")
matplot(dfCBout,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Out-strength",lty=c(2,5))
legend(8,0.14, cex=0.7,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()