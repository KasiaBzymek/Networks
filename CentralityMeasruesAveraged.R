library(xlsx)
#clear the working directory
rm(list = ls())
#set your working directory
setwd("D:/Psychology/Master/NewSimulations/AveragedPlots")

setwd("D:/Psychology/Master/NewSimulations/IndepC91-100")

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
N2<-data[c(18:21,103:106,188:191,273:276,358:361,443:446,528:531,613:616,698:701,783:786),]
N3<-data[c(23:27,108:112,193:197,278:282,363:367,448:452,533:537,618:622,703:707,788:792),]
N4<-data[c(29:34,114:119,199:204,284:289,369:374,454:459,539:544,624:629,709:714,794:799),]
N5<-data[c(36:42,121:127,206:212,291:297,376:382,461:467,546:552,631:637,716:722,801:807),]
N6<-data[c(44:51,129:136,214:221,299:306,384:391,469:476,554:561,639:646,724:731,809:816),]
N7<-data[c(53:61,138:146,223:231,308:316,393:401,478:486,563:571,648:656,733:741,818:826),]
N8<-data[c(63:72,148:157,233:242,318:327,403:412,488:497,573:582,658:667,743:752,828:837),]
N9<-data[c(74:84,159:169,244:254,329:339,414:424,499:509,584:594,669:679,754:764,839:849),]
N10<-data[c(5:16,90:101,175:186,260:271,345:356,430:441,515:526,600:611,685:696,770:781),]

#Below there are two options to choose from, depending on the names used previously in saving files
# save all the 10 data frames into a list
# df.list <- list(N1,N2,N3,N4,N5,N6,N7,N8,N9,N10)
# 
#calculate means for all the centrality measaures for the three main nodes: B1, C1 and A1
# NodeB1<-lapply(df.list, function(w) { w$Bbet <- mean(w[w$nodes=="B1",]$Betweennes);w$Bclos<-mean(w[w$nodes=="B1",]$Closenes);
# w$Binstre<-mean(w[w$nodes=="B1",]$Instrength);w$Bout<-mean(w[w$nodes=="B1",]$Outstrength);w })
# NodeC1<-lapply(df.list, function(w) { w$Cbet <- mean(w[w$nodes=="C1",]$Betweennes);w$Cclos<-mean(w[w$nodes=="C1",]$Closenes);
# w$Cinstre<-mean(w[w$nodes=="C1",]$Instrength);w$Cout<-mean(w[w$nodes=="C1",]$Outstrength);w })
# NodeA1<-lapply(df.list, function(w) { w$Abet <- mean(w[w$nodes=="A1",]$Betweennes);w$Aclos<-mean(w[w$nodes=="A1",]$Closenes);
# w$Ainstre<-mean(w[w$nodes=="A1",]$Instrength);w$Aout<-mean(w[w$nodes=="A1",]$Outstrength);w })

# save all the 10 data frames into a list
df.list <- list(N1,N2,N3,N4,N5,N6,N7,N8,N9,N10)
#calculate means for all the centrality measaures for the three main nodes: B1, C1 and A1
NodeB1<-lapply(df.list, function(w) { w$Bbet <- mean(w[w$nodes=="BI1",]$Betweennes);w$Bclos<-mean(w[w$nodes=="BI1",]$Closenes);
w$Binstre<-mean(w[w$nodes=="BI1",]$Instrength);w$Bout<-mean(w[w$nodes=="BI1",]$Outstrength);w })
NodeC1<-lapply(df.list, function(w) { w$Cbet <- mean(w[w$nodes=="CI1",]$Betweennes);w$Cclos<-mean(w[w$nodes=="CI1",]$Closenes);
w$Cinstre<-mean(w[w$nodes=="CI1",]$Instrength);w$Cout<-mean(w[w$nodes=="CI1",]$Outstrength);w })
NodeA1<-lapply(df.list, function(w) { w$Abet <- mean(w[w$nodes=="AI1",]$Betweennes);w$Aclos<-mean(w[w$nodes=="AI1",]$Closenes);
w$Ainstre<-mean(w[w$nodes=="AI1",]$Instrength);w$Aout<-mean(w[w$nodes=="AI1",]$Outstrength);w })

#save lists as data frames with the means calcualted for all centrality measuares
NodesB<-do.call(rbind.data.frame, NodeB1)
NodesC<-do.call(rbind.data.frame, NodeC1)
NodesA<-do.call(rbind.data.frame, NodeA1)
nodes<-c(1:10)
#save the results into xlsx files
write.xlsx(NodesB,"NodesB.xlsx")
write.xlsx(NodesC,"NodesC.xlsx")
write.xlsx(NodesA,"NodesA.xlsx")

#Extract only the 10 needed values, as there were previously saved in repetition. This is done in the same 
#fashion for all the nodes and centrality measures below
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

#########################################
#save the results with all means into a single data frame and to xlsx file
NodesC91to100Ind<-data.frame(NodeAMean,NodeBMean,NodeCMean,NodeBMeanclo,NodeCMeanclo,NodeAMeanclo,
                             NodeBMeanin,NodeCMeanin,NodeAMeanin,NodeBMeanout,NodeCMeanout,NodeAMeanout)
write.xlsx(NodesC91to100Ind,"NodesC91to100Ind.xlsx")


############################################################################################
############################################################################################
###################                  Betweennees                           #################
############################################################################################
############################################################################################
setwd("D:/Psychology/Master/NewSimulations/DependentA")
NodesA1to10<-read.xlsx("NodesA1toA10.xlsx",1)
NodesA11to20<-read.xlsx("NodesA11toA20.xlsx",1)
NodesA21to30<-read.xlsx("NodesA21toA30.xlsx",1)
NodesA31to40<-read.xlsx("NodesA31toA40.xlsx",1)
NodesA41to50<-read.xlsx("NodesA41toA50.xlsx",1)
NodesA51to60<-read.xlsx("NodesA51toA60.xlsx",1)
NodesA61to70<-read.xlsx("NodesA61toA70.xlsx",1)
NodesA71to80<-read.xlsx("NodesA71toA80.xlsx",1)
NodesA81to90<-read.xlsx("NodesA81toA90.xlsx",1)
NodesA91to100<-read.xlsx("NodesA91toA100.xlsx",1)

#set number of nodes
N<-10
#put all the 10 subfiles into a list 
allNodesA <- list(NodesA1to10,NodesA11to20,NodesA21to30,
                  NodesA31to40,NodesA41to50,NodesA51to60,NodesA61to70,
                  NodesA71to80,NodesA81to90,NodesA91to100)
#save it to R workspace for further use
save(allNodesA,file="DepAallNodes.RData")

#create empty numeric vector
BetweennesBdepA <- numeric()
#calculate average betweennes for nodes of all 100 simulations
for(i in 1:nrow(NodesA1to10)){
  BetweennesBdepA <- append(BetweennesBdepA, sum(sapply(allNodesA, function(x) x$NodeBMean[i])) / N)
}
#BetweennesBdepA
BetweennesCdepA <- numeric()
for(i in 1:nrow(NodesA1to10)){
  BetweennesCdepA <- append(BetweennesCdepA, sum(sapply(allNodesA, function(x) x$NodeCMean[i])) / N)
}
BetweennesAdepA <- numeric()
for(i in 1:nrow(NodesA1to10)){
  BetweennesAdepA <- append(BetweennesAdepA, sum(sapply(allNodesA, function(x) x$NodeAMean[i])) / N)
}

#save the averages of betweenness of different nodes to a workspace
save(BetweennesAdepA,BetweennesBdepA,BetweennesCdepA,file="DepAbetween.RData")

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

save(CloseBdepA,CloseCdepA,CloseAdepA,file="DepAclose.RData")

############################################################################################
############################################################################################
###################                  In-strength                          ################## 
############################################################################################
############################################################################################
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


save(InstBdepA,InstCdepA,InstAdepA,file="DepAInStrength.RData")

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

save(OutBdepA,OutCdepA,OutAdepA,file="DepAOutStrength.RData")



############################################################################################
############################################################################################
###################                  Betweennees IndepA                       #################
############################################################################################
############################################################################################
setwd("D:/Psychology/Master/NewSimulations/IndependentA")
NodesA1to10Ind<-read.xlsx("NodesA1toA10Ind.xlsx",1)
NodesA11to20Ind<-read.xlsx("NodesA11toA20Ind.xlsx",1)
NodesA21to30Ind<-read.xlsx("NodesA21toA30Ind.xlsx",1)
NodesA31to40Ind<-read.xlsx("NodesA31toA40Ind.xlsx",1)
NodesA41to50Ind<-read.xlsx("NodesA41toA50Ind.xlsx",1)
NodesA51to60Ind<-read.xlsx("NodesA51toA60Ind.xlsx",1)
NodesA61to70Ind<-read.xlsx("NodesA61toA70Ind.xlsx",1)
NodesA71to80Ind<-read.xlsx("NodesA71toA80Ind.xlsx",1)
NodesA81to90Ind<-read.xlsx("NodesA81toA90Ind.xlsx",1)
NodesA91to100Ind<-read.xlsx("NodesA91toA100Ind.xlsx",1)
N<-10
allNodesAInd <- list(NodesA1to10Ind,NodesA11to20Ind,NodesA21to30Ind,
                     NodesA31to40Ind,NodesA41to50Ind,NodesA51to60Ind,NodesA61to70Ind,
                     NodesA71to80Ind,NodesA81to90Ind,NodesA91to100Ind)
save(allNodesAInd,file="IndepAAllNodes.RData")

BetweennesBIndA <- numeric()

for(i in 1:nrow(NodesA1to10Ind)){
  BetweennesBIndA <- append(BetweennesBIndA, sum(sapply(allNodesAInd, function(x) x$NodeBMean[i])) / N)
}

BetweennesBIndA

BetweennesCIndA <- numeric()
for(i in 1:nrow(NodesA1to10Ind)){
  BetweennesCIndA <- append(BetweennesCIndA, sum(sapply(allNodesAInd, function(x) x$NodeCMean[i])) / N)
}

BetweennesAIndA <- numeric()
for(i in 1:nrow(NodesA1to10Ind)){
  BetweennesAIndA <- append(BetweennesAIndA, sum(sapply(allNodesAInd, function(x) x$NodeAMean[i])) / N)
}

save(BetweennesAIndA,BetweennesBIndA,BetweennesCIndA,file="IndepenAbetween.RData")


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
for(i in 1:nrow(NodesA1to10Ind)){
  CloseBIndA <- append(CloseBIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeBMeanclo"])) / N)
}


CloseCIndA <- numeric()
for(i in 1:nrow(NodesA1to10Ind)){
  CloseCIndA <- append(CloseCIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeCMeanclo"])) / N)
}

CloseAIndA <- numeric()
for(i in 1:nrow(NodesA1to10Ind)){
  CloseAIndA <- append(CloseAIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeAMeanclo"])) / N)
}

save(CloseBIndA,CloseCIndA,CloseAIndA, file="IndepAcloseness.RData")

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
for(i in 1:nrow(NodesA1to10Ind)){
  InstBIndA <- append(InstBIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeBMeanin"])) / N)
}

InstCIndA <- numeric()
for(i in 1:nrow(NodesA1to10Ind)){
  InstCIndA <- append(InstCIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeCMeanin"])) / N)
}

InstAIndA <- numeric()
for(i in 1:nrow(NodesA1to10Ind)){
  InstAIndA <- append(InstAIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeAMeanin"])) / N)
}


save(InstBIndA,InstCIndA,InstAIndA,file="IndepAinstrength.RData")


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
for(i in 1:nrow(NodesA1to10Ind)){
  OutBIndA <- append(OutBIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeBMeanout"])) / N)
}

OutCIndA <- numeric()
for(i in 1:nrow(NodesA1to10Ind)){
  OutCIndA <- append(OutCIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeCMeanout"])) / N)
}

OutAIndA <- numeric()
for(i in 1:nrow(NodesA1to10Ind)){
  OutAIndA <- append(OutAIndA, sum(sapply(allNodesAInd, function(x) x[i,"NodeAMeanout"])) / N)
}

save(OutBIndA,OutCIndA,OutAIndA,file="IndepAoutstrength.RData")


############################################################################################
############################################################################################
###################                  Betweennees B                          ################
############################################################################################
############################################################################################
setwd("D:/Psychology/Master/NewSimulations/DependentB")
NodesB1to10<-read.xlsx("NodesB1to10.xlsx",1)
NodesB11to20<-read.xlsx("NodesB11to20.xlsx",1)
NodesB21to30<-read.xlsx("NodesB21to30.xlsx",1)
NodesB31to40<-read.xlsx("NodesB31to40.xlsx",1)
NodesB41to50<-read.xlsx("NodesB41to50.xlsx",1)
NodesB51to60<-read.xlsx("NodesB51to60.xlsx",1)
NodesB61to70<-read.xlsx("NodesB61to70.xlsx",1)
NodesB71to80<-read.xlsx("NodesB71to80.xlsx",1)
NodesB81to90<-read.xlsx("NodesB81to90.xlsx",1)
NodesB91to100<-read.xlsx("NodesB91to100.xlsx",1)
N<-10
allNodesB <- list(NodesB1to10,NodesB11to20,NodesB21to30,
                  NodesB31to40,NodesB41to50,NodesB51to60,NodesB61to70,
                  NodesB71to80,NodesB81to90,NodesB91to100)
save(allNodesB,file="DepBallnodes.RData")

BetweennesBdepB <- numeric()
for(i in 1:nrow(NodesB91to100)){
  BetweennesBdepB <- append(BetweennesBdepB, sum(sapply(allNodesB, function(x) x$NodeBMean[i])) / N)
}


BetweennesCdepB <- numeric()
for(i in 1:nrow(NodesB91to100)){
  BetweennesCdepB <- append(BetweennesCdepB, sum(sapply(allNodesB, function(x) x$NodeCMean[i])) / N)
}

BetweennesAdepB <- numeric()
for(i in 1:nrow(NodesB91to100)){
  BetweennesAdepB <- append(BetweennesAdepB, sum(sapply(allNodesB, function(x) x$NodeAMean[i])) / N)
}

save(BetweennesAdepB,BetweennesBdepB,BetweennesCdepB,file="DependBbetween.RData")

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


save(CloseBdepB,CloseAdepB,CloseCdepB,file="DepBcloseness.RData")


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



save(InstBdepB,InstCdepB,InstAdepB,file="DepBinstrength.RData")


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


save(OutBdepB,OutCdepB,OutAdepB,file="DepBoutstrength.RData")


############################################################################################
############################################################################################
###################                  Betweennees IndepB                      #################
############################################################################################
############################################################################################
setwd("D:/Psychology/Master/NewSimulations/IndependentB")
NodesB1to10Ind<-read.xlsx("NodesB1to10Ind.xlsx",1)
NodesB11to20Ind<-read.xlsx("NodesB11to20Ind.xlsx",1)
NodesB21to30Ind<-read.xlsx("NodesB21to30Ind.xlsx",1)
NodesB31to40Ind<-read.xlsx("NodesB31to40Ind.xlsx",1)
NodesB41to50Ind<-read.xlsx("NodesB41to50Ind.xlsx",1)
NodesB51to60Ind<-read.xlsx("NodesB51to60Ind.xlsx",1)
NodesB61to70Ind<-read.xlsx("NodesB61to70Ind.xlsx",1)
NodesB71to80Ind<-read.xlsx("NodesB71to80Ind.xlsx",1)
NodesB81to90Ind<-read.xlsx("NodesB81to90Ind.xlsx",1)
NodesB91to100Ind<-read.xlsx("NodesB91to100Ind.xlsx",1)
N<-10
allNodesBInd <- list(NodesB1to10Ind,NodesB11to20Ind,NodesB21to30Ind,
                     NodesB31to40Ind,NodesB41to50Ind,NodesB51to60Ind,NodesB61to70Ind,
                     NodesB71to80Ind,NodesB81to90Ind,NodesB91to100Ind)
save(allNodesBInd,file="IndepBallnodes.RData")

BetweennesBIndB <- numeric()
for(i in 1:nrow(NodesB1to10Ind)){
  BetweennesBIndB <- append(BetweennesBIndB, sum(sapply(allNodesBInd, function(x) x$NodeBMean[i])) / N)
}

BetweennesCIndB <- numeric()
for(i in 1:nrow(NodesB1to10Ind)){
  BetweennesCIndB <- append(BetweennesCIndB, sum(sapply(allNodesBInd, function(x) x$NodeCMean[i])) / N)
}

BetweennesAIndB <- numeric()
for(i in 1:nrow(NodesB1to10Ind)){
  BetweennesAIndB <- append(BetweennesAIndB, sum(sapply(allNodesBInd, function(x) x$NodeAMean[i])) / N)
}


save(BetweennesBIndB,BetweennesAIndB,BetweennesCIndB,file="IndepBbetween.RData")

############################################################################################
############################################################################################
###################                   Closeness                           ################## 
############################################################################################
############################################################################################
N<-10
allNodesBInd <- list(NodesB1to10Ind,NodesB11to20Ind,NodesB21to30Ind,
                     NodesB31to40Ind,NodesB41to50Ind,NodesB51to60Ind,NodesB61to70Ind,
                     NodesB71to80Ind,NodesB81to90Ind,NodesB91to100Ind)

CloseBIndB <- numeric()

for(i in 1:nrow(NodesB1to10Ind)){
  CloseBIndB <- append(CloseBIndB, sum(sapply(allNodesBInd, function(x) x[i,"NodeBMeanclo"])) / N)
}

CloseCIndB <- numeric()
for(i in 1:nrow(NodesB1to10Ind)){
  CloseCIndB <- append(CloseCIndB, sum(sapply(allNodesBInd, function(x) x[i,"NodeCMeanclo"])) / N)
}

CloseAIndB <- numeric()
for(i in 1:nrow(NodesB1to10Ind)){
  CloseAIndB <- append(CloseAIndB, sum(sapply(allNodesBInd, function(x) x[i,"NodeAMeanclo"])) / N)
}


save(CloseBIndB,CloseCIndB,CloseAIndB,file="IndepBcloseness.RData")


############################################################################################
############################################################################################
###################                  In-strength                          ################## 
############################################################################################
############################################################################################
N<-10
allNodesBInd <- list(NodesB1to10Ind,NodesB11to20Ind,NodesB21to30Ind,
                     NodesB31to40Ind,NodesB41to50Ind,NodesB51to60Ind,NodesB61to70Ind,
                     NodesB71to80Ind,NodesB81to90Ind,NodeSB91to100Ind)

InstBIndB <- numeric()
for(i in 1:nrow(NodesB1to10Ind)){
  InstBIndB <- append(InstBIndB, sum(sapply(allNodesBInd, function(x) x[i,"NodeBMeanin"])) / N)
}
InstCIndB <- numeric()
for(i in 1:nrow(NodesB1to10Ind)){
  InstCIndB <- append(InstCIndB, sum(sapply(allNodesBInd, function(x) x[i,"NodeCMeanin"])) / N)
}

InstAIndB <- numeric()
for(i in 1:nrow(NodesB1to10Ind)){
  InstAIndB <- append(InstAIndB, sum(sapply(allNodesBInd, function(x) x[i,"NodeAMeanin"])) / N)
}


save(InstBIndB,InstCIndB,InstAIndB,file="IndepBinstrength.RData")


############################################################################################
############################################################################################
###################                  Out-strength                         ################## 
############################################################################################
############################################################################################
N<-10
allNodesBInd <- list(NodesB1to10Ind,NodesB11to20Ind,NodesB21to30Ind,
                     NodesB31to40Ind,NodesB41to50Ind,NodesB51to60Ind,NodesB61to70Ind,
                     NodesB71to80Ind,NodesB81to90Ind,NodesB91to100Ind)

OutBIndB <- numeric()
for(i in 1:nrow(NodesB1to10Ind)){
  OutBIndB <- append(OutBIndB, sum(sapply(allNodesBInd, function(x) x[i,"NodeBMeanout"])) / N)
}

OutCIndB <- numeric()
for(i in 1:nrow(NodesB1to10Ind)){
  OutCIndB <- append(OutCIndB, sum(sapply(allNodesBInd, function(x) x[i,"NodeCMeanout"])) / N)
}

OutAIndB <- numeric()
for(i in 1:nrow(NodesB1to10Ind)){
  OutAIndB <- append(OutAIndB, sum(sapply(allNodesBInd, function(x) x[i,"NodeAMeanout"])) / N)
}



save(OutBIndB,OutCIndB,OutAIndB,file="IndepBoutstrength.RData")


############################################################################################
############################################################################################
###################                  Betweennees             Node C              #################
############################################################################################
############################################################################################
setwd("D:/Psychology/Master/NewSimulations/DependentC")
NodesC1to10<-read.xlsx("NodesC1to10.xlsx",1)
NodesC11to20<-read.xlsx("NodesC11to20.xlsx",1)
NodesC21to30<-read.xlsx("NodesC21to30.xlsx",1)
NodesC31to40<-read.xlsx("NodesC31to40.xlsx",1)
NodesC41to50<-read.xlsx("NodesC41to50.xlsx",1)
NodesC51to60<-read.xlsx("NodesC51to60.xlsx",1)
NodesC61to70<-read.xlsx("NodesC61to70.xlsx",1)
NodesC71to80<-read.xlsx("NodesC71to80.xlsx",1)
NodesC81to90<-read.xlsx("NodesC81to90.xlsx",1)
NodesC91to100<-read.xlsx("NodesC91to100.xlsx",1)
N<-10
allNodesC <- list(NodesC1to10,NodesC11to20,NodesC21to30,
                  NodesC31to40,NodesC41to50,NodesC51to60,NodesC61to70,
                  NodesC71to80,NodesC81to90,NodesC91to100)

save(allNodesC,file="DependCallnodes.RData")

BetweennesBdepC <- numeric()
for(i in 1:nrow(NodesC1to10)){
  BetweennesBdepC <- append(BetweennesBdepC, sum(sapply(allNodesC, function(x) x$NodeBMean[i])) / N)
}
#BetweennesBdepC
BetweennesCdepC <- numeric()
for(i in 1:nrow(NodesC1to10)){
  BetweennesCdepC <- append(BetweennesCdepC, sum(sapply(allNodesC, function(x) x$NodeCMean[i])) / N)
}
BetweennesAdepC <- numeric()
for(i in 1:nrow(NodesC1to10)){
  BetweennesAdepC <- append(BetweennesAdepC, sum(sapply(allNodesC, function(x) x$NodeAMean[i])) / N)
}

save(BetweennesBdepC,BetweennesAdepC,BetweennesCdepC, file="DependCbetween.RData")

############################################################################################
############################################################################################
###################                   Closeness                           ################## 
############################################################################################
############################################################################################
allNodesC <- list(NodesC1to10,NodesC11to20,NodesC21to30,
                  NodesC31to40,NodesC41to50,NodesC51to60,NodesC61to70,
                  NodesC71to80,NodesC81to90,NodesC91to100)

CloseBdepC <- numeric()
for(i in 1:nrow(NodesC1to10)){
  CloseBdepC <- append(CloseBdepC, sum(sapply(allNodesC, function(x) x[i,"NodeBMeanclo"])) / N)
}

CloseCdepC <- numeric()
for(i in 1:nrow(NodesC1to10)){
  CloseCdepC <- append(CloseCdepC, sum(sapply(allNodesC, function(x) x[i,"NodeCMeanclo"])) / N)
}

CloseAdepC <- numeric()
for(i in 1:nrow(NodesC1to10)){
  CloseAdepC <- append(CloseAdepC, sum(sapply(allNodesC, function(x) x[i,"NodeAMeanclo"])) / N)
}


save(CloseBdepC,CloseCdepC,CloseAdepC,file="DependCcloseness.RData")

############################################################################################
############################################################################################
###################                  In-strength                          ################## 
############################################################################################
############################################################################################
allNodesC <- list(NodesC1to10,NodesC11to20,NodesC21to30,
                  NodesC31to40,NodesC41to50,NodesC51to60,NodesC61to70,
                  NodesC71to80,NodesC81to90,NodesC91to100)

InstBdepC <- numeric()
for(i in 1:nrow(NodesC1to10)){
  InstBdepC <- append(InstBdepC, sum(sapply(allNodesC, function(x) x[i,"NodeBMeanin"])) / N)
}

InstCdepC <- numeric()
for(i in 1:nrow(NodesC1to10)){
  InstCdepC <- append(InstCdepC, sum(sapply(allNodesC, function(x) x[i,"NodeCMeanin"])) / N)
}

InstAdepC <- numeric()
for(i in 1:nrow(NodesC1to10)){
  InstAdepC <- append(InstAdepC, sum(sapply(allNodesC, function(x) x[i,"NodeAMeanin"])) / N)
}

save(InstBdepC,InstCdepC,InstAdepC,file="DependCinstrength.RData")

############################################################################################
############################################################################################
###################                  Out-strength                         ################## 
############################################################################################
############################################################################################
N<-10
allNodesC <- list(NodesC1to10,NodesC11to20,NodesC21to30,
                  NodesC31to40,NodesC41to50,NodesC51to60,NodesC61to70,
                  NodesC71to80,NodesC81to90,NodesC91to100)

OutBdepC <- numeric()

for(i in 1:nrow(NodesC1to10)){
  OutBdepC <- append(OutBdepC, sum(sapply(allNodesC, function(x) x[i,"NodeBMeanout"])) / N)
}

OutCdepC <- numeric()

for(i in 1:nrow(NodesC1to10)){
  OutCdepC <- append(OutCdepC, sum(sapply(allNodesC, function(x) x[i,"NodeCMeanout"])) / N)
}

OutAdepC <- numeric()

for(i in 1:nrow(NodesC1to10)){
  OutAdepC <- append(OutAdepC, sum(sapply(allNodesC, function(x) x[i,"NodeAMeanout"])) / N)
}

save(OutBdepC,OutCdepC,OutAdepC,file="DependCoutstrength.RData")

############################################################################################
############################################################################################
###################                  Betweennees   Independent C                        #################
############################################################################################
############################################################################################
setwd("D:/Psychology/Master/NewSimulations/IndependentC")
NodesC1to10Ind<-read.xlsx("NodesC1to10Ind.xlsx",1)
NodesC11to20Ind<-read.xlsx("NodesC11to20Ind.xlsx",1)
NodesC21to30Ind<-read.xlsx("NodesC21to30Ind.xlsx",1)
NodesC31to40Ind<-read.xlsx("NodesC31to40Ind.xlsx",1)
NodesC41to50Ind<-read.xlsx("NodesC41to50Ind.xlsx",1)
NodesC51to60Ind<-read.xlsx("NodesC51to60Ind.xlsx",1)
NodesC61to70Ind<-read.xlsx("NodesC61to70Ind.xlsx",1)
NodesC71to80Ind<-read.xlsx("NodesC71to80Ind.xlsx",1)
NodesC81to90Ind<-read.xlsx("NodesC81to90Ind.xlsx",1)
NodesC91to100Ind<-read.xlsx("NodesC91to100Ind.xlsx",1)
N<-10
allNodesCind <- list(NodesC1to10Ind,NodesC11to20Ind,NodesC21to30Ind,
                     NodesC31to40Ind,NodesC41to50Ind,NodesC51to60Ind,NodesC61to70Ind,
                     NodesC71to80Ind,NodesC81to90Ind,NodesC91to100Ind)

save(allNodesCind,file="IndependCallnodes.RData")

BetweennesBIndC <- numeric()
for(i in 1:nrow(NodesC1to10Ind)){
  BetweennesBIndC <- append(BetweennesBIndC, sum(sapply(allNodesCind, function(x) x$NodeBMean[i])) / N)
}
#BetweennesBIndC
BetweennesCIndC <- numeric()
for(i in 1:nrow(NodesC1to10Ind)){
  BetweennesCIndC <- append(BetweennesCIndC, sum(sapply(allNodesCind, function(x) x$NodeCMean[i])) / N)
}
BetweennesAIndC <- numeric()
for(i in 1:nrow(NodesC1to10Ind)){
  BetweennesAIndC <- append(BetweennesAIndC, sum(sapply(allNodesCind, function(x) x$NodeAMean[i])) / N)
}

save(BetweennesAIndC,BetweennesBIndC,BetweennesCIndC, file="IndepCbetween.RData")

############################################################################################
############################################################################################
###################                   Closeness                           ################## 
############################################################################################
############################################################################################
allNodesCind <- list(NodesC1to10Ind,NodesC11to20Ind,NodesC21to30Ind,
                     NodesC31to40Ind,NodesC41to50Ind,NodesC51to60Ind,NodesC61to70Ind,
                     NodesC71to80Ind,NodesC81to90Ind,NodesC91to100Ind)

CloseBIndC <- numeric()
for(i in 1:nrow(NodesC1to10Ind)){
  CloseBIndC <- append(CloseBIndC, sum(sapply(allNodesCind, function(x) x[i,"NodeBMeanclo"])) / N)
}

CloseCIndC <- numeric()
for(i in 1:nrow(NodesC1to10Ind)){
  CloseCIndC <- append(CloseCIndC, sum(sapply(allNodesCind, function(x) x[i,"NodeCMeanclo"])) / N)
}

CloseAIndC <- numeric()
for(i in 1:nrow(NodesC1to10Ind)){
  CloseAIndC <- append(CloseAIndC, sum(sapply(allNodesCind, function(x) x[i,"NodeAMeanclo"])) / N)
}


save(CloseBIndC,CloseCIndC,CloseAIndC,file="IndepCclose.RData")

############################################################################################
############################################################################################
###################                  In-strength                          ################## 
############################################################################################
############################################################################################
allNodesCind <- list(NodesC1to10Ind,NodesC11to20Ind,NodesC21to30Ind,
                     NodesC31to40Ind,NodesC41to50Ind,NodesC51to60Ind,NodesC61to70Ind,
                     NodesC71to80Ind,NodesC81to90Ind,NodesC91to100Ind)

InstBIndC <- numeric()
for(i in 1:nrow(NodesC1to10Ind)){
  InstBIndC <- append(InstBIndC, sum(sapply(allNodesCind, function(x) x[i,"NodeBMeanin"])) / N)
}

InstCIndC <- numeric()
for(i in 1:nrow(NodesC1to10Ind)){
  InstCIndC <- append(InstCIndC, sum(sapply(allNodesCind, function(x) x[i,"NodeCMeanin"])) / N)
}

InstAIndC <- numeric()
for(i in 1:nrow(NodesC1to10Ind)){
  InstAIndC <- append(InstAIndC, sum(sapply(allNodesCind, function(x) x[i,"NodeAMeanin"])) / N)
}

save(InstBIndC,InstCIndC,InstAIndC,file="IndepCinstren.RData")



############################################################################################
############################################################################################
###################                  Out-strength                         ################## 
############################################################################################
############################################################################################
N<-10
allNodesCind <- list(NodesC1to10Ind,NodesC11to20Ind,NodesC21to30Ind,
                     NodesC31to40Ind,NodesC41to50Ind,NodesC51to60Ind,NodesC61to70Ind,
                     NodesC71to80Ind,NodesC81to90Ind,NodesC91to100Ind)

OutBIndC <- numeric()
for(i in 1:nrow(NodesC1to10Ind)){
  OutBIndC <- append(OutBIndC, sum(sapply(allNodesCind, function(x) x[i,"NodeBMeanout"])) / N)
}


OutCIndC <- numeric()

for(i in 1:nrow(NodesC1to10Ind)){
  OutCIndC <- append(OutCIndC, sum(sapply(allNodesCind, function(x) x[i,"NodeCMeanout"])) / N)
}

OutAIndC <- numeric()

for(i in 1:nrow(NodesC1to10Ind)){
  OutAIndC <- append(OutAIndC, sum(sapply(allNodesCind, function(x) x[i,"NodeAMeanout"])) / N)
}

save(OutBIndC,OutCIndC,OutAIndC,file="IndepCoutstrength.RData")

