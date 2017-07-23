library(xlsx)
library(dplyr)
#clear the working directory
rm(list = ls())
#set your working directory
setwd("D:/Psychology/Master/NewSimulations/IndepC91-100")


NodeRankMax<- function(z){
  df.g<-group_by(z,adnode)
  bet<-filter(df.g, Betweennes== max(Betweennes))
  clos<-filter(df.g, Closenes== max(Closenes))
  ins<-filter(df.g, Instrength== max(Instrength))
  out<-filter(df.g, Outstrength== max(Outstrength))
  bsums<-c(sum(bet$nodes=="A1"),sum(bet$nodes=="B1"),sum(bet$nodes=="C1"))
  csums<-c(sum(clos$nodes=="A1"),sum(clos$nodes=="B1"),sum(clos$nodes=="C1"))
  inst<-c(sum(ins$nodes=="A1"),sum(ins$nodes=="B1"),sum(ins$nodes=="C1"))
  outs<-c(sum(out$nodes=="A1"),sum(out$nodes=="B1"),sum(out$nodes=="C1"))
  resultsMax<-list(bsums,csums,inst,outs)
  return(resultsMax)
}

NodeRankMin<- function(k){
  df.g<-group_by(k,adnode)
  bet<-filter(df.g, Betweennes== min(Betweennes))
  clos<-filter(df.g, Closenes== min(Closenes))
  ins<-filter(df.g, Instrength== min(Instrength))
  out<-filter(df.g, Outstrength== min(Outstrength))
  bsums<-c(sum(bet$nodes=="A1"),sum(bet$nodes=="B1"),sum(bet$nodes=="C1"))
  csums<-c(sum(clos$nodes=="A1"),sum(clos$nodes=="B1"),sum(clos$nodes=="C1"))
  inst<-c(sum(ins$nodes=="A1"),sum(ins$nodes=="B1"),sum(ins$nodes=="C1"))
  outs<-c(sum(out$nodes=="A1"),sum(out$nodes=="B1"),sum(out$nodes=="C1"))
  resultsMin<-list(bsums,csums,inst,outs)
  return(resultsMin)
}

#########################Independent case
NodeRankMax<- function(z){
  df.g<-group_by(z,adnode)
  bet<-filter(df.g, Betweennes== max(Betweennes))
  clos<-filter(df.g, Closenes== max(Closenes))
  ins<-filter(df.g, Instrength== max(Instrength))
  out<-filter(df.g, Outstrength== max(Outstrength))
  bsums<-c(sum(bet$nodes=="AI1"),sum(bet$nodes=="BI1"),sum(bet$nodes=="CI1"))
  csums<-c(sum(clos$nodes=="AI1"),sum(clos$nodes=="BI1"),sum(clos$nodes=="CI1"))
  inst<-c(sum(ins$nodes=="AI1"),sum(ins$nodes=="BI1"),sum(ins$nodes=="CI1"))
  outs<-c(sum(out$nodes=="AI1"),sum(out$nodes=="BI1"),sum(out$nodes=="CI1"))
  resultsMax<-list(bsums,csums,inst,outs)
  return(resultsMax)
}

NodeRankMin<- function(k){
  df.g<-group_by(k,adnode)
  bet<-filter(df.g, Betweennes== min(Betweennes))
  clos<-filter(df.g, Closenes== min(Closenes))
  ins<-filter(df.g, Instrength== min(Instrength))
  out<-filter(df.g, Outstrength== min(Outstrength))
  bsums<-c(sum(bet$nodes=="AI1"),sum(bet$nodes=="BI1"),sum(bet$nodes=="CI1"))
  csums<-c(sum(clos$nodes=="AI1"),sum(clos$nodes=="BI1"),sum(clos$nodes=="CI1"))
  inst<-c(sum(ins$nodes=="AI1"),sum(ins$nodes=="BI1"),sum(ins$nodes=="CI1"))
  outs<-c(sum(out$nodes=="AI1"),sum(out$nodes=="BI1"),sum(out$nodes=="CI1"))
  resultsMin<-list(bsums,csums,inst,outs)
  return(resultsMin)
}

#set the path to a file 
file_list <- list.files(path="D:/Psychology/Master/NewSimulations/IndepC91-100", pattern="*.txt", full.names=T, recursive=FALSE)

#read all the tables from text files in the folder specified in "file_list" and save it to .txt file 
outFile1 <- file("output.txt","w") 
for (i in file_list){ 
  x <- readLines(i) 
  group <- cumsum( grepl("[:space:]", x))
  df <-read.table(text = x[group == 2], header = TRUE, sep= "", blank.lines.skip = TRUE, fill =TRUE, row.names = NULL)
  write.table(df, outFile1)
} 
close(outFile1) 

#Note, the .txt files are converted to .xlsx files manually 

# read in the data with columns as numeric 
data <-read.xlsx("output.xlsx",1 , stringsAsFactors=F,colClasses = c("character","character",rep("numeric",4)))
colnames(data)<-c("names","nodes","Betweennes","Closenes","Instrength","Outstrength") #set new column names
head(data) #check your data 

#Subset the data into 10 new data frames
N1<-data[c(1:3,86:88,171:173,256:258,341:343,426:428,511:513,596:598,681:683,766:768),]
addednodes1<-c(rep(1:10,each=3))
N1$adnode<-addednodes1
N2<-data[c(18:21,103:106,188:191,273:276,358:361,443:446,528:531,613:616,698:701,783:786),]
addednodes2<-c(rep(1:10,each=4))
N2$adnode<-addednodes2
N3<-data[c(23:27,108:112,193:197,278:282,363:367,448:452,533:537,618:622,703:707,788:792),]
addednodes3<-c(rep(1:10,each=5))
N3$adnode<-addednodes3
N4<-data[c(29:34,114:119,199:204,284:289,369:374,454:459,539:544,624:629,709:714,794:799),]
addednodes4<-c(rep(1:10,each=6))
N4$adnode<-addednodes4
N5<-data[c(36:42,121:127,206:212,291:297,376:382,461:467,546:552,631:637,716:722,801:807),]
addednodes5<-c(rep(1:10,each=7))
N5$adnode<-addednodes5
N6<-data[c(44:51,129:136,214:221,299:306,384:391,469:476,554:561,639:646,724:731,809:816),]
addednodes6<-c(rep(1:10,each=8))
N6$adnode<-addednodes6
N7<-data[c(53:61,138:146,223:231,308:316,393:401,478:486,563:571,648:656,733:741,818:826),]
addednodes7<-c(rep(1:10,each=9))
N7$adnode<-addednodes7
N8<-data[c(63:72,148:157,233:242,318:327,403:412,488:497,573:582,658:667,743:752,828:837),]
addednodes8<-c(rep(1:10,each=10))
N8$adnode<-addednodes8
N9<-data[c(74:84,159:169,244:254,329:339,414:424,499:509,584:594,669:679,754:764,839:849),]
addednodes9<-c(rep(1:10,each=11))
N9$adnode<-addednodes9
N10<-data[c(5:16,90:101,175:186,260:271,345:356,430:441,515:526,600:611,685:696,770:781),]
addednodes10<-c(rep(1:10,each=12))
N10$adnode<-addednodes10


df.list <- list(N1,N2,N3,N4,N5,N6,N7,N8,N9,N10)
NodesMax<-lapply(df.list,function(x2) {NodeRankMax(x2)} )
NodesallMax<-unlist(NodesMax)
AIbet<-NodesallMax[seq(1,length(NodesallMax),12)]
AIclo<-NodesallMax[seq(4,length(NodesallMax),12)]
AIin<-NodesallMax[seq(7,length(NodesallMax),12)]
AIout<-NodesallMax[seq(10,length(NodesallMax),12)]

BIbet<-NodesallMax[seq(2,length(NodesallMax),12)]
BIclo<-NodesallMax[seq(5,length(NodesallMax),12)]
BIin<-NodesallMax[seq(8,length(NodesallMax),12)]
BIout<-NodesallMax[seq(11,length(NodesallMax),12)]

CIbet<-NodesallMax[seq(3,length(NodesallMax),12)]
CIclo<-NodesallMax[seq(6,length(NodesallMax),12)]
CIin<-NodesallMax[seq(9,length(NodesallMax),12)]
CIout<-NodesallMax[seq(12,length(NodesallMax),12)]

#finding minimum
NodesMin<-lapply(df.list,function(x3) {NodeRankMin(x3)} )
NodesallMin<-unlist(NodesMin)
AIbetm<-NodesallMin[seq(1,length(NodesallMin),12)]
AIclom<-NodesallMin[seq(4,length(NodesallMin),12)]
AIinm<-NodesallMin[seq(7,length(NodesallMin),12)]
AIoutm<-NodesallMin[seq(10,length(NodesallMin),12)]

BIbetm<-NodesallMin[seq(2,length(NodesallMin),12)]
BIclom<-NodesallMin[seq(5,length(NodesallMin),12)]
BIinm<-NodesallMin[seq(8,length(NodesallMin),12)]
BIoutm<-NodesallMin[seq(11,length(NodesallMin),12)]

CIbetm<-NodesallMin[seq(3,length(NodesallMin),12)]
CIclom<-NodesallMin[seq(6,length(NodesallMin),12)]
CIinm<-NodesallMin[seq(9,length(NodesallMin),12)]
CIoutm<-NodesallMin[seq(12,length(NodesallMin),12)]

NodesC91to100IndMax<-data.frame(AIbet,AIclo,AIin,AIout,BIbet,BIclo,BIin,BIout,CIbet,CIclo,CIin,CIout)
write.xlsx(NodesC91to100IndMax,"NodesC91to100IndMax.xlsx")

NodesC91to100IndMin<-data.frame(AIbetm,AIclom,AIinm,AIoutm,BIbetm,BIclom,BIinm,BIoutm,CIbetm,CIclom,CIinm,CIoutm)
write.xlsx(NodesC91to100IndMin,"NodesC91to100IndMin.xlsx")


############################################################################################
############################################################################################
###################                  Betweennees                           #################
############################################################################################
############################################################################################
#put all the 10 subfiles into a list 
allNodesAMax <- list(NodesA1to10Max,NodesA11to20Max,NodesA21to30Max,
                  NodesA31to40Max,NodesA41to50Max,NodesA51to60Max,NodesA61to70Max,
                  NodesA71to80Max,NodesA81to90Max,NodesA91to100Max)

save(allNodesAMax,file="DepArankallNodesMax.RData")
BetweennesBdepA
BetweennesAdepA <- numeric()
for(i in 1:nrow(NodesA1to10Max)){
  BetweennesAdepA <- append(BetweennesAdepA, sum(sapply(allNodesAMax, function(x) x$AIbet[i])))
}
#create empty numeric vector
BetweennesBdepA <- numeric()
#calculate average betweennes for nodes of all 100 simulations
for(i in 1:nrow(NodesA1to10Max)){
  BetweennesBdepA <- append(BetweennesBdepA, sum(sapply(allNodesAMax, function(x) x$BIbet[i])))
}

BetweennesCdepA <- numeric()
for(i in 1:nrow(NodesA1to10Max)){
  BetweennesCdepA <- append(BetweennesCdepA, sum(sapply(allNodesAMax, function(x) x$CIbet[i])))
}

#save the averages of betweenness of different nodes to a workspace
setwd("D:/Psychology/Master/NewSimulations/RankNodesA")
save(BetweennesAdepA,BetweennesBdepA,BetweennesCdepA,file="DepAranksbetween.RData")

############################################################################################
############################################################################################
###################                   Closeness                           ################## 
############################################################################################
############################################################################################
NodesA1to10Max<-read.xlsx("NodesA1to10Max.xlsx",1)
allNodesAMax <- list(NodesA1to10Max,NodesA11to20Max,NodesA21to30Max,
                     NodesA31to40Max,NodesA41to50Max,NodesA51to60Max,NodesA61to70Max,
                     NodesA71to80Max,NodesA81to90Max,NodesA91to100Max)

CloseAdepA <- numeric()
for(i in 1:nrow(NodesA1to10Max)){
  CloseAdepA <- append(CloseAdepA, sum(sapply(allNodesAMax, function(x) x[i,"AIclo"])))
}

CloseBdepA <- numeric()
for(i in 1:nrow(NodesA1to10Max)){
  CloseBdepA <- append(CloseBdepA, sum(sapply(allNodesAMax, function(x) x[i,"BIclo"])))
}

CloseCdepA <- numeric()
for(i in 1:nrow(NodesA1to10Max)){
  CloseCdepA <- append(CloseCdepA, sum(sapply(allNodesAMax, function(x) x[i,"CIclo"])))
}

save(CloseAdepA,CloseBdepA,CloseCdepA,file="DepArankclose.RData")

############################################################################################
############################################################################################
###################                  In-strength                          ################## 
############################################################################################
############################################################################################
allNodesAMax <- list(NodesA1to10Max,NodesA11to20Max,NodesA21to30Max,
                     NodesA31to40Max,NodesA41to50Max,NodesA51to60Max,NodesA61to70Max,
                     NodesA71to80Max,NodesA81to90Max,NodesA91to100Max)

InstAdepA <- numeric()
for(i in 1:nrow(NodesA1to10Max)){
  InstAdepA <- append(InstAdepA, sum(sapply(allNodesAMax, function(x) x[i,"AIin"])))
}

InstBdepA <- numeric()
for(i in 1:nrow(NodesA1to10Max)){
  InstBdepA <- append(InstBdepA, sum(sapply(allNodesAMax, function(x) x[i,"BIin"])))
}

InstCdepA <- numeric()
for(i in 1:nrow(NodesA1to10Max)){
  InstCdepA <- append(InstCdepA, sum(sapply(allNodesAMax, function(x) x[i,"CIin"])))
}


save(InstAdepA,InstBdepA,InstCdepA,file="DepArankInStrength.RData")

############################################################################################
############################################################################################
###################                  Out-strength                         ################## 
############################################################################################
############################################################################################
allNodesAMax <- list(NodesA1to10Max,NodesA11to20Max,NodesA21to30Max,
                     NodesA31to40Max,NodesA41to50Max,NodesA51to60Max,NodesA61to70Max,
                     NodesA71to80Max,NodesA81to90Max,NodesA91to100Max)

OutAdepA <- numeric()
for(i in 1:nrow(NodesA1to10Max)){
  OutAdepA <- append(OutAdepA, sum(sapply(allNodesAMax, function(x) x[i,"AIout"])))
}

OutBdepA <- numeric()
for(i in 1:nrow(NodesA1to10Max)){
  OutBdepA <- append(OutBdepA, sum(sapply(allNodesAMax, function(x) x[i,"BIout"])))
}

OutCdepA <- numeric()
for(i in 1:nrow(NodesA1to10Max)){
  OutCdepA <- append(OutCdepA, sum(sapply(allNodesAMax, function(x) x[i,"CIout"])))
}

save(OutAdepA,OutBdepA,OutCdepA,file="DepArankOutStrength.RData")


############################################################################################
############################################################################################
###################                  Betweennees IndepA                       #################
############################################################################################
############################################################################################
setwd("D:/Psychology/Master/NewSimulations/RankNodesIndepA")
allNodesAIndMax <- list(NodesA1to10IndMax,NodesA11to20IndMax,NodesA21to30IndMax,
                     NodesA31to40IndMax,NodesA41to50IndMax,NodesA51to60IndMax,NodesA61to70IndMax,
                     NodesA71to80IndMax,NodesA81to90IndMax,NodesA91to100IndMax)
save(allNodesAIndMax,file="IndepArankAllNodesMax.RData")
NodesA1to10IndMax<-read.xlsx("NodesA1to10IndMax.xlsx",1)
BetweennesBIndA <- numeric()
for(i in 1:nrow(NodesA1to10IndMax)){
  BetweennesBIndA <- append(BetweennesBIndA, sum(sapply(allNodesAIndMax, function(x) x$BIbet[i])))
}
BetweennesCIndA <- numeric()
for(i in 1:nrow(NodesA1to10IndMax)){
  BetweennesCIndA <- append(BetweennesCIndA, sum(sapply(allNodesAIndMax, function(x) x$CIbet[i])))
}
BetweennesAIndA <- numeric()
for(i in 1:nrow(NodesA1to10IndMax)){
  BetweennesAIndA <- append(BetweennesAIndA, sum(sapply(allNodesAIndMax, function(x) x$AIbet[i])))
}

save(BetweennesAIndA,BetweennesBIndA,BetweennesCIndA,file="IndepenArankbetween.RData")


############################################################################################
############################################################################################
###################                   Closeness                           ################## 
############################################################################################
############################################################################################
allNodesAIndMax <- list(NodesA1to10IndMax,NodesA11to20IndMax,NodesA21to30IndMax,
                        NodesA31to40IndMax,NodesA41to50IndMax,NodesA51to60IndMax,NodesA61to70IndMax,
                        NodesA71to80IndMax,NodesA81to90IndMax,NodesA91to100IndMax)

CloseBIndA <- numeric()
for(i in 1:nrow(NodesA1to10IndMax)){
  CloseBIndA <- append(CloseBIndA, sum(sapply(allNodesAIndMax, function(x) x[i,"BIclo"])))
}

CloseCIndA <- numeric()
for(i in 1:nrow(NodesA1to10IndMax)){
  CloseCIndA <- append(CloseCIndA, sum(sapply(allNodesAIndMax, function(x) x[i,"CIclo"])))
}

CloseAIndA <- numeric()
for(i in 1:nrow(NodesA1to10IndMax)){
  CloseAIndA <- append(CloseAIndA, sum(sapply(allNodesAIndMax, function(x) x[i,"AIclo"])))
}

save(CloseBIndA,CloseCIndA,CloseAIndA, file="IndepArankcloseness.RData")

############################################################################################
############################################################################################
###################                  In-strength                          ################## 
############################################################################################
############################################################################################
allNodesAIndMax <- list(NodesA1to10IndMax,NodesA11to20IndMax,NodesA21to30IndMax,
                        NodesA31to40IndMax,NodesA41to50IndMax,NodesA51to60IndMax,NodesA61to70IndMax,
                        NodesA71to80IndMax,NodesA81to90IndMax,NodesA91to100IndMax)

InstBIndA <- numeric()
for(i in 1:nrow(NodesA1to10IndMax)){
  InstBIndA <- append(InstBIndA, sum(sapply(allNodesAIndMax, function(x) x[i,"BIin"])))
}

InstCIndA <- numeric()
for(i in 1:nrow(NodesA1to10IndMax)){
  InstCIndA <- append(InstCIndA, sum(sapply(allNodesAIndMax, function(x) x[i,"CIin"])))
}

InstAIndA <- numeric()
for(i in 1:nrow(NodesA1to10IndMax)){
  InstAIndA <- append(InstAIndA, sum(sapply(allNodesAIndMax, function(x) x[i,"AIin"])))
}


save(InstBIndA,InstCIndA,InstAIndA,file="IndepArankinstrength.RData")


############################################################################################
############################################################################################
###################                  Out-strength                         ################## 
############################################################################################
############################################################################################
allNodesAIndMax <- list(NodesA1to10IndMax,NodesA11to20IndMax,NodesA21to30IndMax,
                        NodesA31to40IndMax,NodesA41to50IndMax,NodesA51to60IndMax,NodesA61to70IndMax,
                        NodesA71to80IndMax,NodesA81to90IndMax,NodesA91to100IndMax)

OutBIndA <- numeric()
for(i in 1:nrow(NodesA1to10IndMax)){
  OutBIndA <- append(OutBIndA, sum(sapply(allNodesAIndMax, function(x) x[i,"BIout"])))
}

OutCIndA <- numeric()
for(i in 1:nrow(NodesA1to10IndMax)){
  OutCIndA <- append(OutCIndA, sum(sapply(allNodesAIndMax, function(x) x[i,"CIout"])))
}

OutAIndA <- numeric()
for(i in 1:nrow(NodesA1to10IndMax)){
  OutAIndA <- append(OutAIndA, sum(sapply(allNodesAIndMax, function(x) x[i,"AIout"])))
}

save(OutBIndA,OutCIndA,OutAIndA,file="IndepArankoutstrength.RData")


############################################################################################
############################################################################################
###################                  Betweennees B                          ################
############################################################################################
############################################################################################
allNodesBMin <- list(NodesB1to10Min,NodesB11to20Min,NodesB21to30Min,
                     NodesB31to40Min,NodesB41to50Min,NodesB51to60Min,NodesB61to70Min,
                     NodesB71to80Min,NodesB81to90Min,NodesB91to100Min)
save(allNodesBMin,file="DepBrankallnodesMin.RData")
setwd("D:/Psychology/Master/NewSimulations/RankNodesB")

allNodesBMax <- list(NodesB1to10Max,NodesB11to20Max,NodesB21to30Max,
                  NodesB31to40Max,NodesB41to50Max,NodesB51to60Max,NodesB61to70Max,
                  NodesB71to80Max,NodesB81to90Max,NodesB91to100Max)

save(allNodesBMax,file="DepBrankallnodesMax.RData")

BetweennesBdepB <- numeric()
for(i in 1:nrow(NodesB1to10Max)){
  BetweennesBdepB <- append(BetweennesBdepB, sum(sapply(allNodesBMax, function(x) x$BIbet[i])))
}

BetweennesCdepB <- numeric()
for(i in 1:nrow(NodesB1to10Max)){
  BetweennesCdepB <- append(BetweennesCdepB, sum(sapply(allNodesBMax, function(x) x$CIbet[i])))
}

BetweennesAdepB <- numeric()
for(i in 1:nrow(NodesB1to10Max)){
  BetweennesAdepB <- append(BetweennesAdepB, sum(sapply(allNodesBMax, function(x) x$AIbet[i])))
}

save(BetweennesAdepB,BetweennesBdepB,BetweennesCdepB,file="DependBrankbetween.RData")

############################################################################################
############################################################################################
###################                   Closeness                           ################## 
############################################################################################
############################################################################################
allNodesBMax <- list(NodesB1to10Max,NodesB11to20Max,NodesB21to30Max,
                     NodesB31to40Max,NodesB41to50Max,NodesB51to60Max,NodesB61to70Max,
                     NodesB71to80Max,NodesB81to90Max,NodesB91to100Max)

CloseBdepB <- numeric()
for(i in 1:nrow(NodesB1to10Max)){
  CloseBdepB <- append(CloseBdepB, sum(sapply(allNodesBMax, function(x) x[i,"BIclo"])))
}

CloseCdepB <- numeric()
for(i in 1:nrow(NodesB1to10Max)){
  CloseCdepB <- append(CloseCdepB, sum(sapply(allNodesBMax, function(x) x[i,"CIclo"])))
}

CloseAdepB <- numeric()
for(i in 1:nrow(NodesB1to10Max)){
  CloseAdepB <- append(CloseAdepB, sum(sapply(allNodesBMax, function(x) x[i,"AIclo"])))
}


save(CloseBdepB,CloseAdepB,CloseCdepB,file="DepBrankcloseness.RData")


############################################################################################
############################################################################################
###################                  In-strength                          ################## 
############################################################################################
############################################################################################
allNodesBMax <- list(NodesB1to10Max,NodesB11to20Max,NodesB21to30Max,
                     NodesB31to40Max,NodesB41to50Max,NodesB51to60Max,NodesB61to70Max,
                     NodesB71to80Max,NodesB81to90Max,NodesB91to100Max)

NodesB1to10Max<-read.xlsx("NodesB1to10Max.xlsx",1)
InstBdepB <- numeric()
for(i in 1:nrow(NodesB1to10Max)){
  InstBdepB <- append(InstBdepB, sum(sapply(allNodesBMax, function(x) x[i,"BIin"])))
}

InstCdepB <- numeric()
for(i in 1:nrow(NodesB1to10Max)){
  InstCdepB <- append(InstCdepB, sum(sapply(allNodesBMax, function(x) x[i,"CIin"])))
}

InstAdepB <- numeric()
for(i in 1:nrow(NodesB1to10Max)){
  InstAdepB <- append(InstAdepB, sum(sapply(allNodesBMax, function(x) x[i,"AIin"])))
}

save(InstBdepB,InstCdepB,InstAdepB,file="DepBrankinstrength.RData")


############################################################################################
############################################################################################
###################                  Out-strength                         ################## 
############################################################################################
############################################################################################
allNodesBMax <- list(NodesB1to10Max,NodesB11to20Max,NodesB21to30Max,
                     NodesB31to40Max,NodesB41to50Max,NodesB51to60Max,NodesB61to70Max,
                     NodesB71to80Max,NodesB81to90Max,NodesB91to100Max)

OutBdepB <- numeric()
for(i in 1:nrow(NodesB1to10Max)){
  OutBdepB <- append(OutBdepB, sum(sapply(allNodesBMax, function(x) x[i,"BIout"])))
}

OutCdepB <- numeric()
for(i in 1:nrow(NodesB1to10Max)){
  OutCdepB <- append(OutCdepB, sum(sapply(allNodesBMax, function(x) x[i,"CIout"])))
}

OutAdepB <- numeric()
for(i in 1:nrow(NodesB1to10Max)){
  OutAdepB <- append(OutAdepB, sum(sapply(allNodesBMax, function(x) x[i,"AIout"])))
}

save(OutBdepB,OutCdepB,OutAdepB,file="DepBrankoutstrength.RData")

############################################################################################
############################################################################################
###################                  Betweennees IndepB                      #################
############################################################################################
############################################################################################
setwd("D:/Psychology/Master/NewSimulations/RankNodesIndepB")

allNodesBIndMin <- list(NodesB1to10IndMin,NodesB11to20IndMin,NodesB21to30IndMin,
                     NodesB31to40IndMin,NodesB41to50IndMin,NodesB51to60IndMin,NodesB61to70IndMin,
                     NodesB71to80IndMin,NodesB81to90IndMin,NodesB91to100IndMin)
save(allNodesBIndMin,file="IndepBrankallnodesMin.RData")

allNodesBIndMax <- list(NodesB1to10IndMax,NodesB11to20IndMax,NodesB21to30IndMax,
                        NodesB31to40IndMax,NodesB41to50IndMax,NodesB51to60IndMax,NodesB61to70IndMax,
                        NodesB71to80IndMax,NodesB81to90IndMax,NodesB91to100IndMax)
save(allNodesBIndMax,file="IndepBrankallnodesMax.RData")

BetweennesBIndB <- numeric()
for(i in 1:nrow(NodesB1to10IndMax)){
  BetweennesBIndB <- append(BetweennesBIndB, sum(sapply(allNodesBIndMax, function(x) x$BIbet[i])))
}

BetweennesCIndB <- numeric()
for(i in 1:nrow(NodesB1to10IndMax)){
  BetweennesCIndB <- append(BetweennesCIndB, sum(sapply(allNodesBIndMax, function(x) x$CIbet[i])))
}

BetweennesAIndB <- numeric()
for(i in 1:nrow(NodesB1to10IndMax)){
  BetweennesAIndB <- append(BetweennesAIndB, sum(sapply(allNodesBIndMax, function(x) x$AIbet[i])))
}


save(BetweennesBIndB,BetweennesAIndB,BetweennesCIndB,file="IndepBrankbetween.RData")

############################################################################################
############################################################################################
###################                   Closeness                           ################## 
############################################################################################
############################################################################################
allNodesBIndMax <- list(NodesB1to10IndMax,NodesB11to20IndMax,NodesB21to30IndMax,
                        NodesB31to40IndMax,NodesB41to50IndMax,NodesB51to60IndMax,NodesB61to70IndMax,
                        NodesB71to80IndMax,NodesB81to90IndMax,NodesB91to100IndMax)

CloseBIndB <- numeric()

for(i in 1:nrow(NodesB1to10IndMax )){
  CloseBIndB <- append(CloseBIndB, sum(sapply(allNodesBIndMax , function(x) x[i,"BIclo"])))
}

CloseCIndB <- numeric()
for(i in 1:nrow(NodesB1to10IndMax )){
  CloseCIndB <- append(CloseCIndB, sum(sapply(allNodesBIndMax , function(x) x[i,"CIclo"])))
}

CloseAIndB <- numeric()
for(i in 1:nrow(NodesB1to10IndMax )){
  CloseAIndB <- append(CloseAIndB, sum(sapply(allNodesBIndMax , function(x) x[i,"AIclo"])))
}


save(CloseBIndB,CloseCIndB,CloseAIndB,file="IndepBrankcloseness.RData")


############################################################################################
############################################################################################
###################                  In-strength                          ################## 
############################################################################################
############################################################################################
allNodesBIndMax <- list(NodesB1to10IndMax,NodesB11to20IndMax,NodesB21to30IndMax,
                        NodesB31to40IndMax,NodesB41to50IndMax,NodesB51to60IndMax,NodesB61to70IndMax,
                        NodesB71to80IndMax,NodesB81to90IndMax,NodesB91to100IndMax)

InstBIndB <- numeric()
for(i in 1:nrow(NodesB1to10IndMax )){
  InstBIndB <- append(InstBIndB, sum(sapply(allNodesBIndMax , function(x) x[i,"BIin"])))
}
InstCIndB <- numeric()
for(i in 1:nrow(NodesB1to10IndMax )){
  InstCIndB <- append(InstCIndB, sum(sapply(allNodesBIndMax , function(x) x[i,"CIin"])))
}

InstAIndB <- numeric()
for(i in 1:nrow(NodesB1to10IndMax )){
  InstAIndB <- append(InstAIndB, sum(sapply(allNodesBIndMax , function(x) x[i,"AIin"])))
}


save(InstBIndB,InstCIndB,InstAIndB,file="IndepBrankinstrength.RData")


############################################################################################
############################################################################################
###################                  Out-strength                         ################## 
############################################################################################
############################################################################################
allNodesBIndMax <- list(NodesB1to10IndMax,NodesB11to20IndMax,NodesB21to30IndMax,
                        NodesB31to40IndMax,NodesB41to50IndMax,NodesB51to60IndMax,NodesB61to70IndMax,
                        NodesB71to80IndMax,NodesB81to90IndMax,NodesB91to100IndMax)
OutBIndB <- numeric()
for(i in 1:nrow(NodesB1to10IndMax)){
  OutBIndB <- append(OutBIndB, sum(sapply(allNodesBIndMax, function(x) x[i,"BIout"])))
}

OutCIndB <- numeric()
for(i in 1:nrow(NodesB1to10IndMax)){
  OutCIndB <- append(OutCIndB, sum(sapply(allNodesBIndMax, function(x) x[i,"CIout"])))
}

OutAIndB <- numeric()
for(i in 1:nrow(NodesB1to10IndMax)){
  OutAIndB <- append(OutAIndB, sum(sapply(allNodesBIndMax, function(x) x[i,"AIout"])))
}

save(OutBIndB,OutCIndB,OutAIndB,file="IndepBrankoutstrength.RData")

############################################################################################
############################################################################################
###################                  Betweennees             Node C              #################
############################################################################################
############################################################################################
setwd("D:/Psychology/Master/NewSimulations/RankNodesC")
allNodesCMin <- list(NodesC1to10Min ,NodesC11to20Min ,NodesC21to30Min ,
                     NodesC31to40Min ,NodesC41to50Min ,NodesC51to60Min ,NodesC61to70Min ,
                     NodesC71to80Min ,NodesC81to90Min ,NodesC91to100Min )
save(allNodesCMin,file="DependCallnodesrankMin.RData")


allNodesCMax <- list(NodesC1to10Max ,NodesC11to20Max ,NodesC21to30Max ,
                  NodesC31to40Max ,NodesC41to50Max ,NodesC51to60Max ,NodesC61to70Max ,
                  NodesC71to80Max ,NodesC81to90Max ,NodesC91to100Max )

save(allNodesCMax,file="DependCrankallnodesMax.RData")

BetweennesBdepC <- numeric()
for(i in 1:nrow(NodesC1to10Max)){
  BetweennesBdepC <- append(BetweennesBdepC, sum(sapply(allNodesCMax, function(x) x$BIbet[i])))
}
#BetweennesBdepC
BetweennesCdepC <- numeric()
for(i in 1:nrow(NodesC1to10Max)){
  BetweennesCdepC <- append(BetweennesCdepC, sum(sapply(allNodesCMax, function(x) x$CIbet[i])))
}
BetweennesAdepC <- numeric()
for(i in 1:nrow(NodesC1to10Max)){
  BetweennesAdepC <- append(BetweennesAdepC, sum(sapply(allNodesCMax, function(x) x$AIbet[i])))
}

save(BetweennesBdepC,BetweennesAdepC,BetweennesCdepC, file="DependCrankbetween.RData")

############################################################################################
############################################################################################
###################                   Closeness                           ################## 
############################################################################################
############################################################################################
allNodesCMax <- list(NodesC1to10Max ,NodesC11to20Max ,NodesC21to30Max ,
                     NodesC31to40Max ,NodesC41to50Max ,NodesC51to60Max ,NodesC61to70Max ,
                     NodesC71to80Max ,NodesC81to90Max ,NodesC91to100Max )

CloseBdepC <- numeric()
for(i in 1:nrow(NodesC1to10Max)){
  CloseBdepC <- append(CloseBdepC, sum(sapply(allNodesCMax, function(x) x[i,"BIclo"])))
}

CloseCdepC <- numeric()
for(i in 1:nrow(NodesC1to10Max)){
  CloseCdepC <- append(CloseCdepC, sum(sapply(allNodesCMax, function(x) x[i,"CIclo"])))
}

CloseAdepC <- numeric()
for(i in 1:nrow(NodesC1to10Max)){
  CloseAdepC <- append(CloseAdepC, sum(sapply(allNodesCMax, function(x) x[i,"AIclo"])))
}


save(CloseBdepC,CloseCdepC,CloseAdepC,file="DependCrankcloseness.RData")

############################################################################################
############################################################################################
###################                  In-strength                          ################## 
############################################################################################
############################################################################################
allNodesCMax <- list(NodesC1to10Max ,NodesC11to20Max ,NodesC21to30Max ,
                     NodesC31to40Max ,NodesC41to50Max ,NodesC51to60Max ,NodesC61to70Max ,
                     NodesC71to80Max ,NodesC81to90Max ,NodesC91to100Max )

InstBdepC <- numeric()
for(i in 1:nrow(NodesC1to10Max)){
  InstBdepC <- append(InstBdepC, sum(sapply(allNodesCMax , function(x) x[i,"BIin"])))
}

InstCdepC <- numeric()
for(i in 1:nrow(NodesC1to10Max )){
  InstCdepC <- append(InstCdepC, sum(sapply(allNodesCMax , function(x) x[i,"CIin"])))
}

InstAdepC <- numeric()
for(i in 1:nrow(NodesC1to10Max )){
  InstAdepC <- append(InstAdepC, sum(sapply(allNodesCMax , function(x) x[i,"AIin"])))
}

save(InstBdepC,InstCdepC,InstAdepC,file="DependCrankinstrength.RData")

############################################################################################
############################################################################################
###################                  Out-strength                         ################## 
############################################################################################
############################################################################################
allNodesCMax <- list(NodesC1to10Max ,NodesC11to20Max ,NodesC21to30Max ,
                     NodesC31to40Max ,NodesC41to50Max ,NodesC51to60Max ,NodesC61to70Max ,
                     NodesC71to80Max ,NodesC81to90Max ,NodesC91to100Max )

OutBdepC <- numeric()
for(i in 1:nrow(NodesC1to10Max)){
  OutBdepC <- append(OutBdepC, sum(sapply(allNodesCMax, function(x) x[i,"BIout"])))
}

OutCdepC <- numeric()
for(i in 1:nrow(NodesC1to10Max)){
  OutCdepC <- append(OutCdepC, sum(sapply(allNodesCMax, function(x) x[i,"CIout"])))
}

OutAdepC <- numeric()
for(i in 1:nrow(NodesC1to10Max)){
  OutAdepC <- append(OutAdepC, sum(sapply(allNodesCMax, function(x) x[i,"AIout"])))
}

save(OutBdepC,OutCdepC,OutAdepC,file="DependCrankoutstrength.RData")


############################################################################################
############################################################################################
###################                  Betweennees   Independent C                        #################
############################################################################################
############################################################################################
setwd("D:/Psychology/Master/NewSimulations/RankNodesIndepC")
allNodesCindMin <- list(NodesC1to10IndMin,NodesC11to20IndMin,NodesC21to30IndMin,
                     NodesC31to40IndMin,NodesC41to50IndMin,NodesC51to60IndMin,NodesC61to70IndMin,
                     NodesC71to80IndMin,NodesC81to90IndMin,NodesC91to100IndMin)

save(allNodesCindMin,file="IndependCrankallnodesMin.RData")

allNodesCindMax <- list(NodesC1to10IndMax,NodesC11to20IndMax,NodesC21to30IndMax,
                        NodesC31to40IndMax,NodesC41to50IndMax,NodesC51to60IndMax,NodesC61to70IndMax,
                        NodesC71to80IndMax,NodesC81to90IndMax,NodesC91to100IndMax)

save(allNodesCindMax,file="IndependCrankallnodesMax.RData")


BetweennesBIndC <- numeric()
for(i in 1:nrow(NodesC1to10IndMax)){
  BetweennesBIndC <- append(BetweennesBIndC, sum(sapply(allNodesCindMax, function(x) x$BIbet[i])))
}
#BetweennesBIndC
BetweennesCIndC <- numeric()
for(i in 1:nrow(NodesC1to10IndMax)){
  BetweennesCIndC <- append(BetweennesCIndC, sum(sapply(allNodesCindMax, function(x) x$CIbet[i])))
}
BetweennesAIndC <- numeric()
for(i in 1:nrow(NodesC1to10IndMax)){
  BetweennesAIndC <- append(BetweennesAIndC, sum(sapply(allNodesCindMax, function(x) x$AIbet[i])))
}

save(BetweennesAIndC,BetweennesBIndC,BetweennesCIndC, file="IndepCrankbetweenMax.RData")

############################################################################################
############################################################################################
###################                   Closeness                           ################## 
############################################################################################
############################################################################################
allNodesCindMax <- list(NodesC1to10IndMax,NodesC11to20IndMax,NodesC21to30IndMax,
                        NodesC31to40IndMax,NodesC41to50IndMax,NodesC51to60IndMax,NodesC61to70IndMax,
                        NodesC71to80IndMax,NodesC81to90IndMax,NodesC91to100IndMax)

save(allNodesCindMax,file="IndependCrankallnodesMax.RData")

CloseBIndC <- numeric()
for(i in 1:nrow(NodesC1to10IndMax)){
  CloseBIndC <- append(CloseBIndC, sum(sapply(allNodesCindMax, function(x) x[i,"BIclo"])))
}

CloseCIndC <- numeric()
for(i in 1:nrow(NodesC1to10IndMax)){
  CloseCIndC <- append(CloseCIndC, sum(sapply(allNodesCindMax, function(x) x[i,"CIclo"])))
}

CloseAIndC <- numeric()
for(i in 1:nrow(NodesC1to10IndMax)){
  CloseAIndC <- append(CloseAIndC, sum(sapply(allNodesCindMax, function(x) x[i,"AIclo"])))
}


save(CloseBIndC,CloseCIndC,CloseAIndC,file="IndepCrankcloseMax.RData")

############################################################################################
############################################################################################
###################                  In-strength                          ################## 
############################################################################################
############################################################################################
allNodesCindMax <- list(NodesC1to10IndMax,NodesC11to20IndMax,NodesC21to30IndMax,
                        NodesC31to40IndMax,NodesC41to50IndMax,NodesC51to60IndMax,NodesC61to70IndMax,
                        NodesC71to80IndMax,NodesC81to90IndMax,NodesC91to100IndMax)

InstBIndC <- numeric()
for(i in 1:nrow(NodesC1to10IndMax )){
  InstBIndC <- append(InstBIndC, sum(sapply(allNodesCindMax , function(x) x[i,"BIin"])))
}

InstCIndC <- numeric()
for(i in 1:nrow(NodesC1to10IndMax )){
  InstCIndC <- append(InstCIndC, sum(sapply(allNodesCindMax , function(x) x[i,"CIin"])))
}

InstAIndC <- numeric()
for(i in 1:nrow(NodesC1to10IndMax )){
  InstAIndC <- append(InstAIndC, sum(sapply(allNodesCindMax , function(x) x[i,"AIin"])))
}

save(InstBIndC,InstCIndC,InstAIndC,file="IndepCrankinstrenMax.RData")

############################################################################################
############################################################################################
###################                  Out-strength                         ################## 
############################################################################################
############################################################################################
allNodesCindMax <- list(NodesC1to10IndMax,NodesC11to20IndMax,NodesC21to30IndMax,
                        NodesC31to40IndMax,NodesC41to50IndMax,NodesC51to60IndMax,NodesC61to70IndMax,
                        NodesC71to80IndMax,NodesC81to90IndMax,NodesC91to100IndMax)

OutBIndC <- numeric()
for(i in 1:nrow(NodesC1to10IndMax)){
  OutBIndC <- append(OutBIndC, sum(sapply(allNodesCindMax, function(x) x[i,"BIout"])))
}

OutCIndC <- numeric()
for(i in 1:nrow(NodesC1to10IndMax)){
  OutCIndC <- append(OutCIndC, sum(sapply(allNodesCindMax, function(x) x[i,"CIout"])))
}

OutAIndC <- numeric()
for(i in 1:nrow(NodesC1to10IndMax)){
  OutAIndC <- append(OutAIndC, sum(sapply(allNodesCindMax, function(x) x[i,"AIout"])))
}

save(OutBIndC,OutCIndC,OutAIndC,file="IndepCrankoutstrengthMax.RData")
