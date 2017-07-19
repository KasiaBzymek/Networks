library(xlsx)
library(ggplot2)
library(RColorBrewer)
rm(list = ls())
setwd("D:/Psychology/Master/NewSimulations/PlotwithaddednodeA")

normal<-c(2,6,12,20,30,42,56,72,90,110)
NBdepA<-BetweennesBdepA/normal
NCdepA<-BetweennesCdepA/normal
NAdepA<-BetweennesAdepA/normal
NBindepA<-BetweennesBIndA/normal
NCindepA<-BetweennesCIndA/normal
NAindepA<-BetweennesAIndA/normal

df<-data.frame(NBdepA,NCdepA,NAdepA,NBindepA,NCindepA,NAindepA)
dfAA<-data.frame(NAdepA,NAindepA)
dfBA<-data.frame(NBdepA,NBindepA)
dfCA<-data.frame(NCdepA,NCindepA)
pdf("AllNodesDepAIndepA.pdf")
matplot(df, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Betweenness",lty=1:6)
legend("topright", cex=0.7,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedA.pdf")
matplot(dfAA,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Betweennes",lty=c(3,6))
legend("topright", cex=0.9,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedA.pdf")
matplot(dfBA,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Betweennes",lty=c(1,4))
legend("topright", cex=0.9,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedA.pdf")
matplot(dfCA,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Betweennes",lty=c(2,5))
legend("topright", cex=0.9,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()


########################################################
####### Closeness ######################################
########################################################
normalclos<-c(2:11)
NBdepAcl<-CloseBdepA*normalclos
NCdepAcl<-CloseCdepA*normalclos
NAdepAcl<-CloseAdepA*normalclos
NBIndAcl<-CloseBIndA*normalclos
NCIndAcl<-CloseCIndA*normalclos
NAIndAcl<-CloseAIndA*normalclos

dfAllclose<-data.frame(NBdepAcl,NCdepAcl,NAdepAcl,NBIndAcl,NCIndAcl,NAIndAcl)
dfAAcl<-data.frame(NAdepAcl,NAIndAcl)
dfBAcl<-data.frame(NBdepAcl,NBIndAcl)
dfCAcl<-data.frame(NCdepAcl,NCIndAcl)
pdf("AllNodesDepAIndepACloseness.pdf")
matplot(dfAllclose, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Closeness",lty=1:6,ylim=c(0,0.33))
legend("topright",cex=0.5,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedACloseness.pdf")
matplot(dfAAcl,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Closeness",lty=c(3,6))
legend("topright", cex=0.7,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedAcloseness.pdf")
matplot(dfBAcl,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Closeness",lty=c(1,4))
legend(8,0.12, cex=0.6,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedACloseness.pdf")
matplot(dfCAcl,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Closeness",lty=c(2,5))
legend("topright", cex=0.7,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()

#################################################################
#################################################################
##################### In strength ###############################
#################################################################
#################################################################
normalin<-c(2:11)
NBdepAin<-InstBdepA/normalin
NCdepAin<-InstCdepA/normalin
NAdepAin<-InstAdepA/normalin
NBIndAin<-InstBIndA/normalin
NCIndAin<-InstCIndA/normalin
NAIndAin<-InstAIndA/normalin

dfAllin<-data.frame(NBdepAin,NCdepAin,NAdepAin,NBIndAin,NCIndAin,NAIndAin)
dfAAin<-data.frame(NAdepAin,NAIndAin)
dfBAin<-data.frame(NBdepAin,NBIndAin)
dfCAin<-data.frame(NCdepAin,NCIndAin)

pdf("AllNodesDepAIndepAInstrength.pdf")
matplot(dfAllin, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="In-strength",lty=1:6,ylim=c(0,0.33))
legend("topright",cex=0.6,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedAInstrength.pdf")
matplot(dfAAin,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="In-strength",lty=c(3,6))
legend("topright", cex=0.7,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedAInstrength.pdf")
matplot(dfBAin,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="In-strength",lty=c(1,4))
legend("topright", cex=0.7,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedAInstrength.pdf")
matplot(dfCAin,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="In-strength",lty=c(2,5))
legend("topright", cex=0.7,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()

#################################################################
#################################################################
##################### Out strength ###############################
#################################################################
#################################################################
normalout<-c(2:11)
NBdepAout<-OutBdepA/normalout
NCdepAout<-OutCdepA/normalout
NAdepAout<-OutAdepA/normalout
NBindAout<-OutBIndA/normalout
NCindAout<-OutCIndA/normalout
NAindAout<-OutAIndA/normalout

dfAllout<-data.frame(NBdepAout,NCdepAout,NAdepAout,NBindAout,NCindAout,NAindAout)
dfAAout<-data.frame(NAdepAout,NAindAout)
dfBAout<-data.frame(NBdepAout,NBindAout)
dfCAout<-data.frame(NCdepAout,NCindAout)

pdf("AllNodesDepAIndepAOutstrength.pdf")
matplot(dfAllout, type = c("b"),pch=16,col=c("black","red","blue","green","darkorchid4","chocolate4"),xlab="Added nodes", ylab="Out-strength",lty=1:6,ylim=c(0,0.4))
legend("topright",cex=0.5,legend=c("B1-dep","C1-dep","A1-dep","B1-ind","C1-ind","A1-ind"), col=c("black","red","blue","green","darkorchid4","chocolate4"),lty=1:6, pch=16)
dev.off()

pdf("NodesAaddedAOutstrength.pdf")
matplot(dfAAout,type=c("b"),pch=16,col=c("blue","chocolate4"),xlab="Added nodes", ylab="Out-strength",lty=c(3,6))
legend("topright", cex=0.7,legend=c("A1-dep","A1-ind"), col=c("blue","chocolate4"), pch=16,lty=c(3,6))
dev.off()

pdf("NodesBaddedAOutstrength.pdf")
matplot(dfBAout,type=c("b"),pch=16,col=c("black","green"),xlab="Added nodes", ylab="Out-strength",lty=c(1,4))
legend(8,0.12, cex=0.7,legend=c("B1-dep","B1-ind"), col=c("black","green"), pch=16,lty=c(1,4))
dev.off()

pdf("NodesCaddedAOutstrength.pdf")
matplot(dfCAout,type=c("b"),pch=16,col=c("red","darkorchid4"),xlab="Added nodes", ylab="Out-strength",lty=c(2,5))
legend("topright", cex=0.7,legend=c("C1-dep","C1-ind"), col=c("red","darkorchid4"), pch=16,lty=c(2,5))
dev.off()