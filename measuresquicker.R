library(xlsx)

rm(list = ls())
setwd("D:/Psychology/Master/NewSimulations/AveragedPlots")

setwd("D:/Psychology/Master/NewSimulations/IndepB1-10")

file_list <- list.files(path="D:/Psychology/Master/NewSimulations/IndepB1-10", pattern="*.txt", full.names=T, recursive=FALSE)

outFile1 <- file("output.txt","w") 
for (i in file_list){ 
  x <- readLines(i) 
  group <- cumsum( grepl("[:space:]", x))
  df <-read.table(text = x[group == 2], header = TRUE, sep= "", blank.lines.skip = TRUE, fill =TRUE, row.names = NULL)
  write.table(df, outFile1)
} 
close(outFile1) 

# read in the data with columns as numeric 
data <-read.xlsx("output.xlsx",1 , stringsAsFactors=F,colClasses = c("character","character",rep("numeric",4)))
colnames(data)<-c("names","nodes","Betweennes","Closenes","Instrength","Outstrength")
head(data)

N1<-data[c(1:3,86:88,171:173,256:258,341:343,426:428,511:513,596:598,681:683,766:768),]
N2<-data[c(18:21,103:106,188:191,273:276,358:361,443:446,528:531,613:616,698:701,783:786),]
N3<-data[c(23:27,108:112,193:197,278:282,363:367,448:452,533:537,618:622,703:707,788:792),]
N4<-data[c(29:34,114:119,199:204,284:289,369:374,454:459,539:544,624:629,709:714,794:799),]
N5<-data[c(36:42,121:127,206:212,291:297,376:382,461:467,546:552,631:637,716:722,801:807),]
N6<-data[c(44:51,129:136,214:221,299:306,384:391,469:476,554:561,639:646,724:731,809:816),]
N7<-data[c(53:61,138:146,223:231,308:316,393:401,478:486,563:571,648:656,733:741,818:826),]
N8<-data[c(63:72,148:157,233:242,318:327,403:412,488:497,573:582,658:667,743:752,828:837),]
N9<-data[c(74:84,159:169,244:254,329:339,414:424,499:509,584:594,669:679,754:764,839:849),]
N10<-data[c(5:16,90:101,175:186,260:271,345:356,430:441,515:526,600:611,685:696,770:781),]


# df.list <- list(N1,N2,N3,N4,N5,N6,N7,N8,N9,N10)
# 
# NodeB1<-lapply(df.list, function(w) { w$Bbet <- mean(w[w$nodes=="B1",]$Betweennes);w$Bclos<-mean(w[w$nodes=="B1",]$Closenes);
# w$Binstre<-mean(w[w$nodes=="B1",]$Instrength);w$Bout<-mean(w[w$nodes=="B1",]$Outstrength);w })
# NodeC1<-lapply(df.list, function(w) { w$Cbet <- mean(w[w$nodes=="C1",]$Betweennes);w$Cclos<-mean(w[w$nodes=="C1",]$Closenes);
# w$Cinstre<-mean(w[w$nodes=="C1",]$Instrength);w$Cout<-mean(w[w$nodes=="C1",]$Outstrength);w })
# NodeA1<-lapply(df.list, function(w) { w$Abet <- mean(w[w$nodes=="A1",]$Betweennes);w$Aclos<-mean(w[w$nodes=="A1",]$Closenes);
# w$Ainstre<-mean(w[w$nodes=="A1",]$Instrength);w$Aout<-mean(w[w$nodes=="A1",]$Outstrength);w })

df.list <- list(N1,N2,N3,N4,N5,N6,N7,N8,N9,N10)

NodeB1<-lapply(df.list, function(w) { w$Bbet <- mean(w[w$nodes=="BI1",]$Betweennes);w$Bclos<-mean(w[w$nodes=="BI1",]$Closenes);
w$Binstre<-mean(w[w$nodes=="BI1",]$Instrength);w$Bout<-mean(w[w$nodes=="BI1",]$Outstrength);w })
NodeC1<-lapply(df.list, function(w) { w$Cbet <- mean(w[w$nodes=="CI1",]$Betweennes);w$Cclos<-mean(w[w$nodes=="CI1",]$Closenes);
w$Cinstre<-mean(w[w$nodes=="CI1",]$Instrength);w$Cout<-mean(w[w$nodes=="CI1",]$Outstrength);w })
NodeA1<-lapply(df.list, function(w) { w$Abet <- mean(w[w$nodes=="AI1",]$Betweennes);w$Aclos<-mean(w[w$nodes=="AI1",]$Closenes);
w$Ainstre<-mean(w[w$nodes=="AI1",]$Instrength);w$Aout<-mean(w[w$nodes=="AI1",]$Outstrength);w })


NodesB<-do.call(rbind.data.frame, NodeB1)
NodesC<-do.call(rbind.data.frame, NodeC1)
NodesA<-do.call(rbind.data.frame, NodeA1)
nodes<-c(1:10)
write.xlsx(NodesB,"NodesB.xlsx")
write.xlsx(NodesC,"NodesC.xlsx")
write.xlsx(NodesA,"NodesA.xlsx")
NodeBMean<-c(NodesB$Bbet[1],NodesB$Bbet[31],NodesB$Bbet[71],NodesB$Bbet[121],NodesB$Bbet[181],
             NodesB$Bbet[251],NodesB$Bbet[331],NodesB$Bbet[421],NodesB$Bbet[521],NodesB$Bbet[641])

NodeBMeanclo<-c(NodesB$Bclos[1],NodesB$Bclos[31],NodesB$Bclos[71],NodesB$Bclos[121],NodesB$Bclos[181],
                NodesB$Bclos[251],NodesB$Bclos[331],NodesB$Bclos[421],NodesB$Bclos[521],NodesB$Bclos[641])

NodeBMeanin<-c(NodesB$Binstre[1],NodesB$Binstre[31],NodesB$Binstre[71],NodesB$Binstre[121],NodesB$Binstre[181],
               NodesB$Binstre[251],NodesB$Binstre[331],NodesB$Binstre[421],NodesB$Binstre[521],NodesB$Binstre[641])

NodeBMeanout<-c(NodesB$Bout[1],NodesB$Bout[31],NodesB$Bout[71],NodesB$Bout[121],NodesB$Bout[181],
                NodesB$Bout[251],NodesB$Bout[331],NodesB$Bout[421],NodesB$Bout[521],NodesB$Bout[641])

##Node C
NodeCMean<-c(NodesC$Cbet[1],NodesC$Cbet[31],NodesC$Cbet[71],NodesC$Cbet[121],NodesC$Cbet[181],
             NodesC$Cbet[251],NodesC$Cbet[331],NodesC$Cbet[421],NodesC$Cbet[521],NodesC$Cbet[641])
NodeCMeanclo<-c(NodesC$Cclos[1],NodesC$Cclos[31],NodesC$Cclos[71],NodesC$Cclos[121],NodesC$Cclos[181],
                NodesC$Cclos[251],NodesC$Cclos[331],NodesC$Cclos[421],NodesC$Cclos[521],NodesC$Cclos[641])

NodeCMeanin<-c(NodesC$Cinstre[1],NodesC$Cinstre[31],NodesC$Cinstre[71],NodesC$Cinstre[121],NodesC$Cinstre[181],
               NodesC$Cinstre[251],NodesC$Cinstre[331],NodesC$Cinstre[421],NodesC$Cinstre[521],NodesC$Cinstre[641])

NodeCMeanout<-c(NodesC$Cout[1],NodesC$Cout[31],NodesC$Cout[71],NodesC$Cout[121],NodesC$Cout[181],
                NodesC$Cout[251],NodesC$Cout[331],NodesC$Cout[421],NodesC$Cout[521],NodesC$Cout[641])

######## Node A
NodeAMean<-c(NodesA$Abet[1],NodesA$Abet[31],NodesA$Abet[71],NodesA$Abet[121],NodesA$Abet[181],
             NodesA$Abet[251],NodesA$Abet[331],NodesA$Abet[421],NodesA$Abet[521],NodesA$Abet[641])

NodeAMeanclo<-c(NodesA$Aclos[1],NodesA$Aclos[31],NodesA$Aclos[71],NodesA$Aclos[121],NodesA$Aclos[181],
                NodesA$Aclos[251],NodesA$Aclos[331],NodesA$Aclos[421],NodesA$Aclos[521],NodesA$Aclos[641])

NodeAMeanin<-c(NodesA$Ainstre[1],NodesA$Ainstre[31],NodesA$Ainstre[71],NodesA$Ainstre[121],NodesA$Ainstre[181],
               NodesA$Ainstre[251],NodesA$Ainstre[331],NodesA$Ainstre[421],NodesA$Ainstre[521],NodesA$Ainstre[641])

NodeAMeanout<-c(NodesA$Aout[1],NodesA$Aout[31],NodesA$Aout[71],NodesA$Aout[121],NodesA$Aout[181],
                NodesA$Aout[251],NodesA$Aout[331],NodesA$Aout[421],NodesA$Aout[521],NodesA$Aout[641])

###############write to a data frame 
#########################################
NodesB1to10Ind<-data.frame(NodeAMean,NodeBMean,NodeCMean,NodeBMeanclo,NodeCMeanclo,NodeAMeanclo,
                          NodeBMeanin,NodeCMeanin,NodeAMeanin,NodeBMeanout,NodeCMeanout,NodeAMeanout)
write.xlsx(NodesB1to10Ind,"NodesB1to10Ind.xlsx")

############################################################################################
############################################################################################
###################                  Betweennees                           #################
############################################################################################
############################################################################################

N<-10
allNodesA <- list(NodesA1to10,NodesA11to20,NodesA21to30,
                  NodesA31to40,NodesA41to50,NodesA51to60,NodesA61to70,
                  NodesA71to80,NodesA81to90,NodesA91to100)
save(allNodesA,file="allNodesAdep.RData")

BetweennesBdepA <- numeric()

for(i in 1:nrow(NodesA91to100)){
  BetweennesBdepA <- append(BetweennesBdepA, sum(sapply(allNodesA, function(x) x$NodeBMean[i])) / N)
}

BetweennesBdepA

BetweennesCdepA <- numeric()
for(i in 1:nrow(NodesA91to100)){
  BetweennesCdepA <- append(BetweennesCdepA, sum(sapply(allNodesA, function(x) x$NodeCMean[i])) / N)
}

BetweennesAdepA <- numeric()
for(i in 1:nrow(NodesA91to100)){
  BetweennesAdepA <- append(BetweennesAdepA, sum(sapply(allNodesA, function(x) x$NodeAMean[i])) / N)
}

normal<-c(2,6,12,20,30,42,56,72,90,110)
NodeBnormalizeddepA<-BetweennesBdepA/normal
NodeCnormalizeddepA<-BetweennesCdepA/normal
NodeAnormalizeddepA<-BetweennesAdepA/normal

save(BetweennesAdepA,BetweennesBdepA,BetweennesCdepA,file="betweennessDepA.RData")
load("betweennessDepA.RData")
setwd("D:/Psychology/Master/NewSimulations/AveragedPlots")
# first plot
pdf("BetweennesABCdependA.pdf")
plot(nodes, NodeBnormalizeddepA, ylim=range(c(NodeBnormalizeddepA,NodeCnormalizeddepA,NodeAnormalizeddepA)),pch=16,col="green",ylab="Betweenness", xlab="Added nodes",title("An average betweenness for three main nodes"))
lines(nodes[order(nodes)], NodeBnormalizeddepA[order(nodes)], ylim=range(NodeBnormalizeddepA), xlim=range(nodes), pch=16,lty=1)
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(nodes, NodeCnormalizeddepA, ylim=range(c(NodeBnormalizeddepA,NodeCnormalizeddepA,NodeAnormalizeddepA)), axes = FALSE, xlab = "", ylab = "",pch=17,col="blue")
lines(nodes[order(nodes)], NodeCnormalizeddepA[order(nodes)], ylim=range(NodeCnormalizeddepA), xlim=range(nodes), pch=16,lty=2)
par(new = TRUE)
plot(nodes, NodeAnormalizeddepA, ylim=range(c(NodeBnormalizeddepA,NodeCnormalizeddepA,NodeAnormalizeddepA)), axes = FALSE, xlab = "", ylab = "",pch=8, col="purple")
lines(nodes[order(nodes)], NodeAnormalizeddepA[order(nodes)], ylim=range(NodeAnormalizeddepA), xlim=range(nodes), pch=16,lty=4)
legend(8.5,0.3,lty=c(1,2,4),cex=0.8,pch=c(16,17,8),col=c("green","blue","purple"),legend=c("B1", "C1","A1"))
dev.off()


############################################################################################
############################################################################################
###################                   Closeness                           ################## 
############################################################################################
############################################################################################
N<-10
allNodesA <- list(NodesA1to10,NodesA11to20,NodesA21to30,
                  NodesA31to40,NodesA41to50,NodesA51to60,NodesA61to70,
                  NodesA71to80,NodesA81to90,NodesA91to100)

CloseBdepA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  CloseBdepA <- append(CloseBdepA, sum(sapply(allNodesA, function(x) x[i,"NodeBMeanclo"])) / N)
}


CloseCdepA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  CloseCdepA <- append(CloseCdepA, sum(sapply(allNodesA, function(x) x[i,"NodeCMeanclo"])) / N)
}

CloseAdepA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  CloseAdepA <- append(CloseAdepA, sum(sapply(allNodesA, function(x) x[i,"NodeAMeanclo"])) / N)
}

######### Normalization vector. In order to normalized the value of betweennes we divide each value by (N-1)(N-2) where N is the number of nodes in a network 
normalclos<-c(2:11)
NodeBnormalizedclosdepA<-CloseBdepA*normalclos
NodeCnormalizedclosdepA<-CloseCdepA*normalclos
NodeAnormalizedclosdepA<-CloseAdepA*normalclos

save(CloseBdepA,CloseCdepA,CloseCdepA,file="closenessDepA.RData")


setwd("D:/Psychology/Master/NewSimulations/AveragedPlots")
# first plot
pdf("ClosenesABCdependA.pdf")
plot(nodes, NodeBnormalizedclosdepA, ylim=range(c(NodeBnormalizedclosdepA,NodeCnormalizedclosdepA,NodeAnormalizedclosdepA)),pch=16,col="green",ylab="closdepAeness", xlab="Added nodes")
lines(nodes[order(nodes)], NodeBnormalizedclosdepA[order(nodes)], ylim=range(NodeBnormalizedclosdepA), xlim=range(nodes), pch=16,lty=1)
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(nodes, NodeCnormalizedclosdepA, ylim=range(c(NodeBnormalizedclosdepA,NodeCnormalizedclosdepA,NodeAnormalizedclosdepA)), axes = FALSE, xlab = "", ylab = "",pch=17,col="blue")
lines(nodes[order(nodes)], NodeCnormalizedclosdepA[order(nodes)], ylim=range(NodeCnormalizedclosdepA), xlim=range(nodes), pch=16,lty=2)
par(new = TRUE)
plot(nodes, NodeAnormalizedclosdepA, ylim=range(c(NodeBnormalizedclosdepA,NodeCnormalizedclosdepA,NodeAnormalizedclosdepA)), axes = FALSE, xlab = "", ylab = "",pch=8, col="purple")
lines(nodes[order(nodes)], NodeAnormalizedclosdepA[order(nodes)], ylim=range(NodeAnormalizedclosdepA), xlim=range(nodes), pch=16,lty=4)
legend(8.5,0.18,lty=c(1,2,4),cex=0.8,pch=c(16,17,8),col=c("green","blue","purple"),legend=c("B1", "C1","A1"))
dev.off()


############################################################################################
############################################################################################
###################                  In-strength                          ################## 
############################################################################################
############################################################################################
N<-10
allNodesA <- list(NodesA1to10,NodesA11to20,NodesA21to30,
                  NodesA31to40,NodesA41to50,NodesA51to60,NodesA61to70,
                  NodesA71to80,NodesA81to90,NodesA91to100)

InstBdepA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  InstBdepA <- append(InstBdepA, sum(sapply(allNodesA, function(x) x[i,"NodeBMeanin"])) / N)
}


InstCdepA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  InstCdepA <- append(InstCdepA, sum(sapply(allNodesA, function(x) x[i,"NodeCMeanin"])) / N)
}

InstAdepA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  InstAdepA <- append(InstAdepA, sum(sapply(allNodesA, function(x) x[i,"NodeAMeanin"])) / N)
}


######### Normalization vector. In order to normalized the value of betweennes we divide each value by (N-1) where N is the number of nodes in a network 
normalin<-c(2:11)
NodeBnormalizedindepA<-InstBdepA/normalin
NodeCnormalizedindepA<-InstCdepA/normalin
NodeAnormalizedindepA<-InstAdepA/normalin

save(InstBdepA,InstCdepA,InstAdepA,file="InstrengthDepA.RData")

setwd("D:/Psychology/Master/NewSimulations/AveragedPlots")
# first plot
pdf("InstrengthABCdependA.pdf")
plot(nodes, NodeBnormalizedindepA, ylim=range(c(NodeBnormalizedindepA,NodeCnormalizedindepA,NodeAnormalizedindepA)),pch=16,col="green",ylab="in-strength", xlab="Added nodes")
lines(nodes[order(nodes)], NodeBnormalizedindepA[order(nodes)], ylim=range(NodeBnormalizedindepA), xlim=range(nodes), pch=16,lty=1)
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(nodes, NodeCnormalizedindepA, ylim=range(c(NodeBnormalizedindepA,NodeCnormalizedindepA,NodeAnormalizedindepA)), axes = FALSE, xlab = "", ylab = "",pch=17,col="blue")
lines(nodes[order(nodes)], NodeCnormalizedindepA[order(nodes)], ylim=range(NodeCnormalizedindepA), xlim=range(nodes), pch=16,lty=2)
par(new = TRUE)
plot(nodes, NodeAnormalizedindepA, ylim=range(c(NodeBnormalizedindepA,NodeCnormalizedindepA,NodeAnormalizedindepA)), axes = FALSE, xlab = "", ylab = "",pch=8, col="purple")
lines(nodes[order(nodes)], NodeAnormalizedindepA[order(nodes)], ylim=range(NodeAnormalizedindepA), xlim=range(nodes), pch=16,lty=4)
legend(8.5,0.2,lty=c(1,2,4),cex=0.8,pch=c(16,17,8),col=c("green","blue","purple"),legend=c("B1", "C1","A1"))
dev.off()


############################################################################################
############################################################################################
###################                  Out-strength                         ################## 
############################################################################################
############################################################################################
N<-10
allNodesA <- list(NodesA1to10,NodesA11to20,NodesA21to30,
                  NodesA31to40,NodesA41to50,NodesA51to60,NodesA61to70,
                  NodesA71to80,NodesA81to90,NodesA91to100)

OutBdepA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  OutBdepA <- append(OutBdepA, sum(sapply(allNodesA, function(x) x[i,"NodeBMeanout"])) / N)
}


OutCdepA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  OutCdepA <- append(OutCdepA, sum(sapply(allNodesA, function(x) x[i,"NodeCMeanout"])) / N)
}

OutAdepA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  OutAdepA <- append(OutAdepA, sum(sapply(allNodesA, function(x) x[i,"NodeAMeanout"])) / N)
}

######### Normalization vector. out order to normalized the value of betweennes we divide each value by (N-1) where N is the number of nodes out a network 
normalout<-c(2:11)
NodeBnormalizedoutdepA<-OutBdepA/normalout
NodeCnormalizedoutdepA<-OutCdepA/normalout
NodeAnormalizedoutdepA<-OutAdepA/normalout

save(OutBdepA,OutCdepA,OutCdepA,file="OutstrengthDepA.RData")

setwd("D:/Psychology/Master/NewSimulations/AveragedPlots")
# first plot
pdf("outstrengthABCdependA.pdf")
plot(nodes, NodeBnormalizedoutdepA, ylim=range(c(NodeBnormalizedoutdepA,NodeCnormalizedoutdepA,NodeAnormalizedoutdepA)),pch=16,col="green",ylab="out-strength", xlab="Added nodes")
lines(nodes[order(nodes)], NodeBnormalizedoutdepA[order(nodes)], ylim=range(NodeBnormalizedoutdepA), xlim=range(nodes), pch=16,lty=1)
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(nodes, NodeCnormalizedoutdepA, ylim=range(c(NodeBnormalizedoutdepA,NodeCnormalizedoutdepA,NodeAnormalizedoutdepA)), axes = FALSE, xlab = "", ylab = "",pch=17,col="blue")
lines(nodes[order(nodes)], NodeCnormalizedoutdepA[order(nodes)], ylim=range(NodeCnormalizedoutdepA), xlim=range(nodes), pch=16,lty=2)
par(new = TRUE)
plot(nodes, NodeAnormalizedoutdepA, ylim=range(c(NodeBnormalizedoutdepA,NodeCnormalizedoutdepA,NodeAnormalizedoutdepA)), axes = FALSE, xlab = "", ylab = "",pch=8, col="purple")
lines(nodes[order(nodes)], NodeAnormalizedoutdepA[order(nodes)], ylim=range(NodeAnormalizedoutdepA), xlim=range(nodes), pch=16,lty=4)
legend(8.5,0.20,lty=c(1,2,4),cex=0.9,pch=c(16,17,8),col=c("green","blue","purple"),legend=c("B1", "C1","A1"))
dev.off()

save(NodeBnormalizedoutdepA,NodeCnormalizedoutdepA,NodeAnormalizedoutdepA,
     file="myfile.RData")




############################################################################################
############################################################################################
###################                  Betweennees IndepA                       #################
############################################################################################
############################################################################################
N<-10
allNodesAInd <- list(NodesA1to10Ind,NodesA11to20Ind,NodesA21to30Ind,
                  NodesA31to40Ind,NodesA41to50Ind,NodesA51to60Ind,NodesA61to70Ind,
                  NodesA71to80Ind,NodesA81to90Ind,NodesA91to100Ind)
save(allNodesAInd,file="allNodesAInd.RData")

BetweennesBIndA <- numeric()

for(i in 1:nrow(NodesA1to10Ind)){
  BetweennesBIndA <- append(BetweennesBIndA, sum(sapply(allNodesAInd, function(x) x$NodeBMean[i])) / N)
}

BetweennesBIndA

BetweennesCIndA <- numeric()
for(i in 1:nrow(NodesA91to100)){
  BetweennesCIndA <- append(BetweennesCIndA, sum(sapply(allNodesAInd, function(x) x$NodeCMean[i])) / N)
}

BetweennesAIndA <- numeric()
for(i in 1:nrow(NodesA91to100)){
  BetweennesAIndA <- append(BetweennesAIndA, sum(sapply(allNodesAInd, function(x) x$NodeAMean[i])) / N)
}

normal<-c(2,6,12,20,30,42,56,72,90,110)
NodeBnormalizedIndA<-BetweennesBIndA/normal
NodeCnormalizedIndA<-BetweennesCIndA/normal
NodeAnormalizedIndA<-BetweennesAIndA/normal

save(BetweennesAIndA,BetweennesBIndA,BetweennesCIndA,file="betweennessIndA.RData")

setwd("D:/Psychology/Master/NewSimulations/AveragedPlots")
# first plot
pdf("BetweennesABCIndA.pdf")
plot(nodes, NodeBnormalizedIndA, ylim=range(c(NodeBnormalizedIndA,NodeCnormalizedIndA,NodeAnormalizedIndA)),pch=16,col="green",ylab="Betweenness", xlab="Added nodes",title("An average betweenness for three main nodes"))
lines(nodes[order(nodes)], NodeBnormalizedIndA[order(nodes)], ylim=range(NodeBnormalizedIndA), xlim=range(nodes), pch=16,lty=1)
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(nodes, NodeCnormalizedIndA, ylim=range(c(NodeBnormalizedIndA,NodeCnormalizedIndA,NodeAnormalizedIndA)), axes = FALSE, xlab = "", ylab = "",pch=17,col="blue")
lines(nodes[order(nodes)], NodeCnormalizedIndA[order(nodes)], ylim=range(NodeCnormalizedIndA), xlim=range(nodes), pch=16,lty=2)
par(new = TRUE)
plot(nodes, NodeAnormalizedIndA, ylim=range(c(NodeBnormalizedIndA,NodeCnormalizedIndA,NodeAnormalizedIndA)), axes = FALSE, xlab = "", ylab = "",pch=8, col="purple")
lines(nodes[order(nodes)], NodeAnormalizedIndA[order(nodes)], ylim=range(NodeAnormalizedIndA), xlim=range(nodes), pch=16,lty=4)
legend(8.5,0.7,lty=c(1,2,4),cex=0.8,pch=c(16,17,8),col=c("green","blue","purple"),legend=c("B1", "C1","A1"))
dev.off()


############################################################################################
############################################################################################
###################                   Closeness                           ################## 
############################################################################################
############################################################################################
N<-10
allNodesAInd <- list(NodesA1to10Ind,NodesA11to20Ind,NodesA21to30Ind,
                     NodesA31to40Ind,NodesA41to50Ind,NodesA51to60Ind,NodesA61to70Ind,
                     NodesA71to80Ind,NodesA81to90Ind,NodesA91to100Ind)

CloseBIndA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  CloseBIndA <- append(CloseBIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeBMeanclo"])) / N)
}


CloseCIndA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  CloseCIndA <- append(CloseCIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeCMeanclo"])) / N)
}

CloseAIndA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  CloseAIndA <- append(CloseAIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeAMeanclo"])) / N)
}

######### Normalization vector. In order to normalized the value of betweennes we divide each value by (N-1)(N-2) where N is the number of nodes in a network 
normalclos<-c(2:11)
NodeBnormalizedclosIndA<-CloseBIndA*normalclos
NodeCnormalizedclosIndA<-CloseCIndA*normalclos
NodeAnormalizedclosIndA<-CloseAIndA*normalclos

save(CloseBIndA,CloseCIndA,CloseCIndA,file="closenessIndA.RData")


setwd("D:/Psychology/Master/NewSimulations/AveragedPlots")
# first plot
pdf("ClosenesABCIndepA.pdf")
plot(nodes, NodeBnormalizedclosIndA, ylim=range(c(NodeBnormalizedclosIndA,NodeCnormalizedclosIndA,NodeAnormalizedclosIndA)),pch=16,col="green",ylab="closIndAeness", xlab="Added nodes")
lines(nodes[order(nodes)], NodeBnormalizedclosIndA[order(nodes)], ylim=range(NodeBnormalizedclosIndA), xlim=range(nodes), pch=16,lty=1)
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(nodes, NodeCnormalizedclosIndA, ylim=range(c(NodeBnormalizedclosIndA,NodeCnormalizedclosIndA,NodeAnormalizedclosIndA)), axes = FALSE, xlab = "", ylab = "",pch=17,col="blue")
lines(nodes[order(nodes)], NodeCnormalizedclosIndA[order(nodes)], ylim=range(NodeCnormalizedclosIndA), xlim=range(nodes), pch=16,lty=2)
par(new = TRUE)
plot(nodes, NodeAnormalizedclosIndA, ylim=range(c(NodeBnormalizedclosIndA,NodeCnormalizedclosIndA,NodeAnormalizedclosIndA)), axes = FALSE, xlab = "", ylab = "",pch=8, col="purple")
lines(nodes[order(nodes)], NodeAnormalizedclosIndA[order(nodes)], ylim=range(NodeAnormalizedclosIndA), xlim=range(nodes), pch=16,lty=4)
legend(8.5,0.07,lty=c(1,2,4),cex=0.8,pch=c(16,17,8),col=c("green","blue","purple"),legend=c("B1", "C1","A1"))
dev.off()


############################################################################################
############################################################################################
###################                  In-strength                          ################## 
############################################################################################
############################################################################################
N<-10
allNodesAInd <- list(NodesA1to10Ind,NodesA11to20Ind,NodesA21to30Ind,
                     NodesA31to40Ind,NodesA41to50Ind,NodesA51to60Ind,NodesA61to70Ind,
                     NodesA71to80Ind,NodesA81to90Ind,NodesA91to100Ind)

InstBIndA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  InstBIndA <- append(InstBIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeBMeanin"])) / N)
}


InstCIndA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  InstCIndA <- append(InstCIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeCMeanin"])) / N)
}

InstAIndA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  InstAIndA <- append(InstAIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeAMeanin"])) / N)
}


######### Normalization vector. In order to normalized the value of betweennes we divide each value by (N-1) where N is the number of nodes in a network 
normalin<-c(2:11)
NodeBnormalizedinIndA<-InstBIndA/normalin
NodeCnormalizedinIndA<-InstCIndA/normalin
NodeAnormalizedinIndA<-InstAIndA/normalin

save(InstBIndA,InstCIndA,InstAIndA,file="InstrengthIndA.RData")

setwd("D:/Psychology/Master/NewSimulations/AveragedPlots")
# first plot
pdf("InstrengthABCIndA.pdf")
plot(nodes, NodeBnormalizedinIndA, ylim=range(c(NodeBnormalizedinIndA,NodeCnormalizedinIndA,NodeAnormalizedinIndA)),pch=16,col="green",ylab="in-strength", xlab="Added nodes")
lines(nodes[order(nodes)], NodeBnormalizedinIndA[order(nodes)], ylim=range(NodeBnormalizedinIndA), xlim=range(nodes), pch=16,lty=1)
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(nodes, NodeCnormalizedinIndA, ylim=range(c(NodeBnormalizedinIndA,NodeCnormalizedinIndA,NodeAnormalizedinIndA)), axes = FALSE, xlab = "", ylab = "",pch=17,col="blue")
lines(nodes[order(nodes)], NodeCnormalizedinIndA[order(nodes)], ylim=range(NodeCnormalizedinIndA), xlim=range(nodes), pch=16,lty=2)
par(new = TRUE)
plot(nodes, NodeAnormalizedinIndA, ylim=range(c(NodeBnormalizedinIndA,NodeCnormalizedinIndA,NodeAnormalizedinIndA)), axes = FALSE, xlab = "", ylab = "",pch=8, col="purple")
lines(nodes[order(nodes)], NodeAnormalizedinIndA[order(nodes)], ylim=range(NodeAnormalizedinIndA), xlim=range(nodes), pch=16,lty=4)
legend(8.5,0.18,lty=c(1,2,4),cex=0.8,pch=c(16,17,8),col=c("green","blue","purple"),legend=c("B1", "C1","A1"))
dev.off()


############################################################################################
############################################################################################
###################                  Out-strength                         ################## 
############################################################################################
############################################################################################
N<-10
allNodesAInd <- list(NodesA1to10Ind,NodesA11to20Ind,NodesA21to30Ind,
                     NodesA31to40Ind,NodesA41to50Ind,NodesA51to60Ind,NodesA61to70Ind,
                     NodesA71to80Ind,NodesA81to90Ind,NodesA91to100Ind)

OutBIndA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  OutBIndA <- append(OutBIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeBMeanout"])) / N)
}


OutCIndA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  OutCIndA <- append(OutCIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeCMeanout"])) / N)
}

OutAIndA <- numeric()

for(i in 1:nrow(NodesA1to10)){
  OutAIndA <- append(OutAIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeAMeanout"])) / N)
}

######### Normalization vector. out order to normalized the value of betweennes we divide each value by (N-1) where N is the number of nodes out a network 
normalout<-c(2:11)
NodeBnormalizedoutIndA<-OutBIndA/normalout
NodeCnormalizedoutIndA<-OutCIndA/normalout
NodeAnormalizedoutIndA<-OutAIndA/normalout

save(OutBIndA,OutCIndA,OutCIndA,file="OutstrengthIndA.RData")

setwd("D:/Psychology/Master/NewSimulations/AveragedPlots")
# first plot
pdf("outstrengthABCIndendA.pdf")
plot(nodes, NodeBnormalizedoutIndA, ylim=range(c(NodeBnormalizedoutIndA,NodeCnormalizedoutIndA,NodeAnormalizedoutIndA)),pch=16,col="green",ylab="out-strength", xlab="Added nodes")
lines(nodes[order(nodes)], NodeBnormalizedoutIndA[order(nodes)], ylim=range(NodeBnormalizedoutIndA), xlim=range(nodes), pch=16,lty=1)
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(nodes, NodeCnormalizedoutIndA, ylim=range(c(NodeBnormalizedoutIndA,NodeCnormalizedoutIndA,NodeAnormalizedoutIndA)), axes = FALSE, xlab = "", ylab = "",pch=17,col="blue")
lines(nodes[order(nodes)], NodeCnormalizedoutIndA[order(nodes)], ylim=range(NodeCnormalizedoutIndA), xlim=range(nodes), pch=16,lty=2)
par(new = TRUE)
plot(nodes, NodeAnormalizedoutIndA, ylim=range(c(NodeBnormalizedoutIndA,NodeCnormalizedoutIndA,NodeAnormalizedoutIndA)), axes = FALSE, xlab = "", ylab = "",pch=8, col="purple")
lines(nodes[order(nodes)], NodeAnormalizedoutIndA[order(nodes)], ylim=range(NodeAnormalizedoutIndA), xlim=range(nodes), pch=16,lty=4)
legend(8,0.16,lty=c(1,2,4),cex=0.9,pch=c(16,17,8),col=c("green","blue","purple"),legend=c("B1", "C1","A1"))
dev.off()

save(NodeBnormalizedoutIndA,NodeCnormalizedoutIndA,NodeAnormalizedoutIndA,
     file="myfile.RData")





############################################################################################
############################################################################################
###################                  Betweennees                           #################
############################################################################################
############################################################################################
N<-10
allNodesB <- list(NodesB1to10,NodesB11to20,NodesB21to30,
                  NodesB31to40,NodesB41to50,NodesB51to60,NodesB61to70,
                  NodesB71to80,NodesB81to90,NodesB91to100)
save(allNodesB,file="allNodesBdep.RData")

BetweennesBdepB <- numeric()

for(i in 1:nrow(NodesB91to100)){
  BetweennesBdepB <- append(BetweennesBdepB, sum(sapply(allNodesB, function(x) x$NodeBMean[i])) / N)
}

BetweennesBdepB

BetweennesCdepB <- numeric()
for(i in 1:nrow(NodesB91to100)){
  BetweennesCdepB <- append(BetweennesCdepB, sum(sapply(allNodesB, function(x) x$NodeCMean[i])) / N)
}

BetweennesAdepB <- numeric()
for(i in 1:nrow(NodesB91to100)){
  BetweennesAdepB <- append(BetweennesAdepB, sum(sapply(allNodesB, function(x) x$NodeAMean[i])) / N)
}

normal<-c(2,6,12,20,30,42,56,72,90,110)
NodeBnormalizeddepB<-BetweennesBdepB/normal
NodeCnormalizeddepB<-BetweennesCdepB/normal
NodeAnormalizeddepB<-BetweennesAdepB/normal

save(BetweennesAdepB,BetweennesBdepB,BetweennesCdepB,file="betweennessDepB.RData")

setwd("D:/Psychology/Master/NewSimulations/AveragedPlots")
# first plot
pdf("BetweennesABCdependB.pdf")
plot(nodes, NodeBnormalizeddepB, ylim=range(c(NodeBnormalizeddepB,NodeCnormalizeddepB,NodeAnormalizeddepB)),pch=16,col="green",ylab="Betweenness", xlab="Added nodes",title("An average betweenness for three main nodes"))
lines(nodes[order(nodes)], NodeBnormalizeddepB[order(nodes)], ylim=range(NodeBnormalizeddepB), xlim=range(nodes), pch=16,lty=1)
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(nodes, NodeCnormalizeddepB, ylim=range(c(NodeBnormalizeddepB,NodeCnormalizeddepB,NodeAnormalizeddepB)), axes = FALSE, xlab = "", ylab = "",pch=17,col="blue")
lines(nodes[order(nodes)], NodeCnormalizeddepB[order(nodes)], ylim=range(NodeCnormalizeddepB), xlim=range(nodes), pch=16,lty=2)
par(new = TRUE)
plot(nodes, NodeAnormalizeddepB, ylim=range(c(NodeBnormalizeddepB,NodeCnormalizeddepB,NodeAnormalizeddepB)), axes = FALSE, xlab = "", ylab = "",pch=8, col="purple")
lines(nodes[order(nodes)], NodeAnormalizeddepB[order(nodes)], ylim=range(NodeAnormalizeddepB), xlim=range(nodes), pch=16,lty=4)
legend(8.5,0.4,lty=c(1,2,4),cex=0.8,pch=c(16,17,8),col=c("green","blue","purple"),legend=c("B1", "C1","A1"))
dev.off()


############################################################################################
############################################################################################
###################                   Closeness                           ################## 
############################################################################################
############################################################################################
N<-10
allNodesB <- list(NodesB1to10,NodesB11to20,NodesB21to30,
                  NodesB31to40,NodesB41to50,NodesB51to60,NodesB61to70,
                  NodesB71to80,NodesB81to90,NodesB91to100)

CloseBdepB <- numeric()

for(i in 1:nrow(NodesB1to10)){
  CloseBdepB <- append(CloseBdepB, sum(sapply(allNodesB, function(x) x[i,"NodeBMeanclo"])) / N)
}


CloseCdepB <- numeric()

for(i in 1:nrow(NodesB1to10)){
  CloseCdepB <- append(CloseCdepB, sum(sapply(allNodesB, function(x) x[i,"NodeCMeanclo"])) / N)
}

CloseAdepB <- numeric()

for(i in 1:nrow(NodesB1to10)){
  CloseAdepB <- append(CloseAdepB, sum(sapply(allNodesB, function(x) x[i,"NodeAMeanclo"])) / N)
}

######### Normalization vector. In order to normalized the value of betweennes we divide each value by (N-1)(N-2) where N is the number of nodes in a network 
normalclos<-c(2:11)
NodeBnormalizedclosdepB<-CloseBdepB*normalclos
NodeCnormalizedclosdepB<-CloseCdepB*normalclos
NodeAnormalizedclosdepB<-CloseAdepB*normalclos

save(CloseBdepB,CloseAdepB,CloseCdepB,file="closenessDepB.RData")


setwd("D:/Psychology/Master/NewSimulations/AveragedPlots")
# first plot
pdf("ClosenesABCdependB.pdf")
plot(nodes, NodeBnormalizedclosdepB, ylim=range(c(NodeBnormalizedclosdepB,NodeCnormalizedclosdepB,NodeAnormalizedclosdepB)),pch=16,col="green",ylab="Closeness", xlab="Added nodes")
lines(nodes[order(nodes)], NodeBnormalizedclosdepB[order(nodes)], ylim=range(NodeBnormalizedclosdepB), xlim=range(nodes), pch=16,lty=1)
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(nodes, NodeCnormalizedclosdepB, ylim=range(c(NodeBnormalizedclosdepB,NodeCnormalizedclosdepB,NodeAnormalizedclosdepB)), axes = FALSE, xlab = "", ylab = "",pch=17,col="blue")
lines(nodes[order(nodes)], NodeCnormalizedclosdepB[order(nodes)], ylim=range(NodeCnormalizedclosdepB), xlim=range(nodes), pch=16,lty=2)
par(new = TRUE)
plot(nodes, NodeAnormalizedclosdepB, ylim=range(c(NodeBnormalizedclosdepB,NodeCnormalizedclosdepB,NodeAnormalizedclosdepB)), axes = FALSE, xlab = "", ylab = "",pch=8, col="purple")
lines(nodes[order(nodes)], NodeAnormalizedclosdepB[order(nodes)], ylim=range(NodeAnormalizedclosdepB), xlim=range(nodes), pch=16,lty=4)
legend(8.5,0.16,lty=c(1,2,4),cex=0.8,pch=c(16,17,8),col=c("green","blue","purple"),legend=c("B1", "C1","A1"))
dev.off()


############################################################################################
############################################################################################
###################                  In-strength                          ################## 
############################################################################################
############################################################################################
N<-10
allNodesB <- list(NodesB1to10,NodesB11to20,NodesB21to30,
                  NodesB31to40,NodesB41to50,NodesB51to60,NodesB61to70,
                  NodesB71to80,NodesB81to90,NodesB91to100)

InstBdepB <- numeric()

for(i in 1:nrow(NodesB1to10)){
  InstBdepB <- append(InstBdepB, sum(sapply(allNodesB, function(x) x[i,"NodeBMeanin"])) / N)
}


InstCdepB <- numeric()

for(i in 1:nrow(NodesB1to10)){
  InstCdepB <- append(InstCdepB, sum(sapply(allNodesB, function(x) x[i,"NodeCMeanin"])) / N)
}

InstAdepB <- numeric()

for(i in 1:nrow(NodesB1to10)){
  InstAdepB <- append(InstAdepB, sum(sapply(allNodesB, function(x) x[i,"NodeAMeanin"])) / N)
}


######### Normalization vector. In order to normalized the value of betweennes we divide each value by (N-1) where N is the number of nodes in a network 
normalin<-c(2:11)
NodeBnormalizedindepB<-InstBdepB/normalin
NodeCnormalizedindepB<-InstCdepB/normalin
NodeAnormalizedindepB<-InstAdepB/normalin

save(InstBdepB,InstCdepB,InstAdepB,file="InstrengthDepB.RData")

setwd("D:/Psychology/Master/NewSimulations/AveragedPlots")
# first plot
pdf("InstrengthABCdependB.pdf")
plot(nodes, NodeBnormalizedindepB, ylim=range(c(NodeBnormalizedindepB,NodeCnormalizedindepB,NodeAnormalizedindepB)),pch=16,col="green",ylab="in-strength", xlab="Added nodes")
lines(nodes[order(nodes)], NodeBnormalizedindepB[order(nodes)], ylim=range(NodeBnormalizedindepB), xlim=range(nodes), pch=16,lty=1)
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(nodes, NodeCnormalizedindepB, ylim=range(c(NodeBnormalizedindepB,NodeCnormalizedindepB,NodeAnormalizedindepB)), axes = FALSE, xlab = "", ylab = "",pch=17,col="blue")
lines(nodes[order(nodes)], NodeCnormalizedindepB[order(nodes)], ylim=range(NodeCnormalizedindepB), xlim=range(nodes), pch=16,lty=2)
par(new = TRUE)
plot(nodes, NodeAnormalizedindepB, ylim=range(c(NodeBnormalizedindepB,NodeCnormalizedindepB,NodeAnormalizedindepB)), axes = FALSE, xlab = "", ylab = "",pch=8, col="purple")
lines(nodes[order(nodes)], NodeAnormalizedindepB[order(nodes)], ylim=range(NodeAnormalizedindepB), xlim=range(nodes), pch=16,lty=4)
legend(8.5,0.23,lty=c(1,2,4),cex=0.8,pch=c(16,17,8),col=c("green","blue","purple"),legend=c("B1", "C1","A1"))
dev.off()


############################################################################################
############################################################################################
###################                  Out-strength                         ################## 
############################################################################################
############################################################################################
N<-10
allNodesB <- list(NodesB1to10,NodesB11to20,NodesB21to30,
                  NodesB31to40,NodesB41to50,NodesB51to60,NodesB61to70,
                  NodesB71to80,NodesB81to90,NodesB91to100)
OutBdepB <- numeric()

for(i in 1:nrow(NodesB1to10)){
  OutBdepB <- append(OutBdepB, sum(sapply(allNodesB, function(x) x[i,"NodeBMeanout"])) / N)
}


OutCdepB <- numeric()

for(i in 1:nrow(NodesB1to10)){
  OutCdepB <- append(OutCdepB, sum(sapply(allNodesB, function(x) x[i,"NodeCMeanout"])) / N)
}

OutAdepB <- numeric()

for(i in 1:nrow(NodesB1to10)){
  OutAdepB <- append(OutAdepB, sum(sapply(allNodesB, function(x) x[i,"NodeAMeanout"])) / N)
}

######### Normalization vector. out order to normalized the value of betweennes we divide each value by (N-1) where N is the number of nodes out a network 
normalout<-c(2:11)
NodeBnormalizedoutdepB<-OutBdepB/normalout
NodeCnormalizedoutdepB<-OutCdepB/normalout
NodeAnormalizedoutdepB<-OutAdepB/normalout

save(OutBdepB,OutCdepB,OutCdepB,file="OutstrengthDepB.RData")

setwd("D:/Psychology/Master/NewSimulations/AveragedPlots")
# first plot
pdf("outstrengthABCdependB.pdf")
plot(nodes, NodeBnormalizedoutdepB, ylim=range(c(NodeBnormalizedoutdepB,NodeCnormalizedoutdepB,NodeAnormalizedoutdepB)),pch=16,col="green",ylab="out-strength", xlab="Added nodes")
lines(nodes[order(nodes)], NodeBnormalizedoutdepB[order(nodes)], ylim=range(NodeBnormalizedoutdepB), xlim=range(nodes), pch=16,lty=1)
# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(nodes, NodeCnormalizedoutdepB, ylim=range(c(NodeBnormalizedoutdepB,NodeCnormalizedoutdepB,NodeAnormalizedoutdepB)), axes = FALSE, xlab = "", ylab = "",pch=17,col="blue")
lines(nodes[order(nodes)], NodeCnormalizedoutdepB[order(nodes)], ylim=range(NodeCnormalizedoutdepB), xlim=range(nodes), pch=16,lty=2)
par(new = TRUE)
plot(nodes, NodeAnormalizedoutdepB, ylim=range(c(NodeBnormalizedoutdepB,NodeCnormalizedoutdepB,NodeAnormalizedoutdepB)), axes = FALSE, xlab = "", ylab = "",pch=8, col="purple")
lines(nodes[order(nodes)], NodeAnormalizedoutdepB[order(nodes)], ylim=range(NodeAnormalizedoutdepB), xlim=range(nodes), pch=16,lty=4)
legend(8,0.18,lty=c(1,2,4),cex=0.9,pch=c(16,17,8),col=c("green","blue","purple"),legend=c("B1", "C1","A1"))
dev.off()



