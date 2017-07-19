library(xlsx)
library(ggplot2)
library(RColorBrewer)
rm(list = ls())
setwd("D:/Psychology/Master/NewSimulations/PlotswithaddednodeC")

#Normalized the data. The number of nodes in network differs, therefore we want to normlized 
#our results. In wighted graphs Betweennes is normalized by dividing the original value by (N-1)(N-2)
#where N is number of nodes in a network
normal<-c(2,6,12,20,30,42,56,72,90,110) #(N-1)(N-2) for all the consecutive networks
normal<-c(2,6,12,20,30,42,56,72,90,110)
NBdepC<-BetweennesBdepC/normal
NCdepC<-BetweennesCdepC/normal
NAdepC<-BetweennesAdepC/normal
NBindepC<-BetweennesBIndC/normal
NCindepC<-BetweennesCIndC/normal
NAindepC<-BetweennesAIndC/normal

dfdepC<-data.frame(NBdepC,NCdepC,NAdepC,NBindepC,NCindepC,NAindepC)
dfAC<-data.frame(NAdepC,NAindepC)
dfBC<-data.frame(NBdepC,NBindepC)
dfCC<-data.frame(NCdepC,NCindepC)

pdf("AllNodesDepCIndepC.pdf")
matplot(dfdepC, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Betweenness",lty=1:6)
legend("topright", cex=0.8,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedC.pdf")
matplot(dfAC,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Betweennes",lty=c(3,6))
legend("topright", cex=0.8,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedC.pdf")
matplot(dfBC,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Betweennes",lty=c(1,4))
legend("topright", cex=0.9,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedC.pdf")
matplot(dfCC,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Betweennes",lty=c(2,5))
legend("topright", cex=0.8,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()


########################################################
####### Closeness ######################################
########################################################
normalclos<-c(2:11)
NBdepCcl<-CloseBdepC*normalclos
NCdepCcl<-CloseCdepC*normalclos
NAdepCcl<-CloseAdepC*normalclos
NBIndCcl<-CloseBIndC*normalclos
NCIndCcl<-CloseCIndC*normalclos
NAIndCcl<-CloseAIndC*normalclos

dfAllcloseC<-data.frame(NBdepCcl,NCdepCcl,NAdepCcl,NBIndCcl,NCIndCcl,NAIndCcl)
dfACcl<-data.frame(NAdepCcl,NAIndCcl)
dfBCcl<-data.frame(NBdepCcl,NBIndCcl)
dfCCcl<-data.frame(NCdepCcl,NCIndCcl)

pdf("AllNodesDepCIndepCCloseness.pdf")
matplot(dfAllcloseC, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Closeness",lty=1:6,ylim=c(0,0.3))
legend("topright",cex=0.7,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedCCloseness.pdf")
matplot(dfACcl,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Closeness",lty=c(3,6))
legend("topright", cex=0.8,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedCcloseness.pdf")
matplot(dfBCcl,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Closeness",lty=c(1,4))
legend("topright", cex=0.8,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedCCloseness.pdf")
matplot(dfCCcl,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Closeness",lty=c(2,5))
legend("topright", cex=0.9,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()

#################################################################
#################################################################
##################### In strength ###############################
#################################################################
#################################################################
normalin<-c(2:11)
NBdepCin<-InstBdepC/normalin
NCdepCin<-InstCdepC/normalin
NAdepCin<-InstAdepC/normalin
NBIndCin<-InstBIndC/normalin
NCIndCin<-InstCIndC/normalin
NAIndCin<-InstAIndC/normalin

dfAllinC<-data.frame(NBdepCin,NCdepCin,NAdepCin,NBIndCin,NCIndCin,NAIndCin)
dfACin<-data.frame(NAdepCin,NAIndCin)
dfBCin<-data.frame(NBdepCin,NBIndCin)
dfCCin<-data.frame(NCdepCin,NCIndCin)

pdf("AllNodesDepCIndepCInstrength.pdf")
matplot(dfAllinC, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="In-strength",lty=1:6,ylim=c(0,0.33))
legend("topright",cex=0.8,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedCInstrength.pdf")
matplot(dfACin,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="In-strength",lty=c(3,6))
legend("topright", cex=0.8,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedCInstrength.pdf")
matplot(dfBCin,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="In-strength",lty=c(1,4))
legend("topright", cex=0.8,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedCInstrength.pdf")
matplot(dfCCin,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="In-strength",lty=c(2,5))
legend("topright", cex=0.8,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()

#################################################################
#################################################################
##################### Out strength ###############################
#################################################################
#################################################################
normalout<-c(2:11)
NBdepCout<-OutBdepC/normalout
NCdepCout<-OutCdepC/normalout
NAdepCout<-OutAdepC/normalout
NBindCout<-OutBIndC/normalout
NCindCout<-OutCIndC/normalout
NAindCout<-OutAIndC/normalout

dfAlloutC<-data.frame(NBdepCout,NCdepCout,NAdepCout,NBindCout,NCindCout,NAindCout)
dfACout<-data.frame(NAdepCout,NAindCout)
dfBCout<-data.frame(NBdepCout,NBindCout)
dfCCout<-data.frame(NCdepCout,NCindCout)

pdf("AllNodesDepCIndepCOutstrength.pdf")
matplot(dfAlloutC, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Out-strength",lty=1:6,ylim=c(0,0.4))
legend("topright",cex=0.8,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedCOutstrength.pdf")
matplot(dfACout,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Out-strength",lty=c(3,6))
legend("topright", cex=0.9,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedCOutstrength.pdf")
matplot(dfBCout,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Out-strength",lty=c(1,4))
legend("topright", cex=0.9,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedCOutstrength.pdf")
matplot(dfCCout,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Out-strength",lty=c(2,5))
legend("topright", cex=0.9,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()