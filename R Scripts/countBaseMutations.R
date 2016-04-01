#to count base pairs mutated nonsense/missense

suppressMessages(library(DBI))
suppressMessages(library(RMySQL))
library(dplyr)

dbcon.hgmd <- dbConnect(MySQL(),user = "root", password = "paparam", host = "localhost", db = "hgmd_pro") # connect to database hgmd_pro in local server

tables <- dbListTables(dbcon.hgmd) #list of all tables
mut.tbl = dbReadTable(dbcon.hgmd,name = 'allmut')


tbl = mut.tbl[,c("disease","gene")]
#group the record by disease.
disease.Genes = group_by(tbl,disease)
#list the genes for each disease
list.genes.associated.with.diseases =    with(disease.Genes, by(disease.Genes,disease,function(x) return(x$gene) ))

all.genes = unname(list.genes.associated.with.diseases)

Mutated.genes= lapply(all.genes, unique)
Mutated.genes = as.character(Mutated.genes)

disease.names = names(list.genes.associated.with.diseases)

disease.names = as.list(disease.names)
df.diseaseGene = data.frame(Disease = disease.names, genes = Mutated.genes)
write.table(df.diseaseGene,file = "diseaseGenes.txt", sep ='\t',row.names = F)
#summarise(count.disease.Genes, count = n())
all.diseases = names(list.genes.associated.with.diseases)

bases = mut.tbl[3]
ac=0;gc=0;cc=0;tc=0;nomut=0
for (term in bases[1:91118,]){
  
  data = strsplit(term,"-")[[1]]
  A = substr(data[1],1,1);G = substr(data[1],2,2);C = substr(data[1],3,3);D = substr(data[1],4,4)
  
  if (A != substr(data[2],1,1)){
    if (A == "G"){
      gc = gc + 1
    } else if(A == "C"){
      cc = cc + 1
    } else if (A == "T"){
      tc = tc + 1
    }else{
    }
  }
  if (C != substr(data[2],2,2)){
    if (C == "A"){
      ac = ac + 1
      print(paste(ac,substr(data[2],1,1),sep=" "))
    }
    else if (C == "G"){
      gc = gc + 1
    }
    }else if(C == "T"){
      tc = tc + 1
  }else{
    
  }
  if (G != substr(data[2],3,3)){
    if (G == "A"){
      ac = ac + 1
    }
    } else if (G == "C"){
      cc = cc + 1
    }else if(G == "T"){
      tc = tc + 1
  }else{
    
  }
  if (T != substr(data[2],4,4)){
    if (T == "A"){
      ac = ac + 1
    }
    else if (T == "G"){
      gc = gc + 1
    } else if(T == "C"){
      cc = cc + 1
  } else{
    
  }

}
}
#display the counts for each
ac;gc;cc;tc
#put the count in dataframe
basepair.mutated = data.frame(A = ac, G = gc, C = cc, T = tc)

#read the file containing disease and genes only, we didnt have to, but just for showing
#reading 
diseasGenes = read.csv("diseaseGenes.txt",sep="\t",header=T)
#search with keyword "breast"  for disease, and retrieve the index number of found record
all.breast.diseases = with(disease.Genes,grep(".*[Bb]reast.*",disease,perl=T,value=F))
#extract second column which is gene for the breast cancer
all.breast.genes = unique(with(disease.Genes, disease.Genes[all.breast.diseases,c(2)]))
write.table(all.breast.genes,"breastCancerGenes.txt",row.names=F)


all.colorectal.diseases = with(disease.Genes,grep(".*[cC]olon|[cC]olorectal.*",disease,ignore.case=T, perl = T, value=))
all.colorectal.genes = unique(with(disease.Genes,disease.Genes[all.colorectal.diseases,c(2)]))
write.table(all.colorectal.genes,"colonCancergenes.txt",row.names=F)

# for the whole data
feat.all = mut.tbl[,c("disease","gene","base","amino","codon","tag")]
feat.all = feat.all[rowSums(!is.na(feat.all)) > 0, ]
cancer_ids = with(feat.all,grep(".[cC]ance.*",disease,ignore.case=T,perl=T))
cancers = feat.all[cancer_ids,]
non.cancers=feat.all[-cancer_ids,]
len.non.cancer = length(non.cancers[,2])
non.cancers.required = non.cancers[sample(1:len.non.cancer,len.non.cancer),]
#create 50/50 cancer non-cancer data
train.numbers = with(sample.data, createDataPartition(y = Label,p=0.8,list=F))
test.numbers
mixed = rbind(cancers,non.cancers.required)

mixed.train
mixed.test
label=with(mixed, ifelse(grepl(".cancer.", disease,ignore.case = T,perl=T),"Cancer","Non-Cancer"))
mixed$Label=label
mixed$Label=as.factor(mixed$Label)
mixed.clean=mixed[,c("disease","gene","base","amino","codon","tag","Label")]


train = with(mixed.clean, createDataPartition(y = Label,p=0.8,list=F))

#giving gene names a numberic label
data = mixed.clean[,c("gene","base","amino","codon","tag","Label")]
l=length(unique(data$gene))
lev = unique(data$gene)
data$gene = factor(data$gene,levels=lev,label=1:l)
base = unique(data$base)
amino = unique(data$amino)
tag=unique(data$tag)
data$base=factor(data$base,levels=base,label=1:length(base))
data$amino=factor(data$amino,levels=amino,label=1:length(amino))
data$tag=factor(data$tag,levels=tag,label=1:length(tag))

data$gene = as.numeric(data$gene)
data$base = as.numeric(data$base)
data$amino = as.numeric(data$amino)
data$tag = as.numeric(data$tag)


#small random number of cancer samples only diseases
sample.data = data[sample(1:length(data[,2]),length(data[,2])),]
#trainX = sample.data[,names(sample.data)!="Label"]
#preProcess.values = preProcess(trainX,method=c("scale","center"))

train = with(sample.data, createDataPartition(y = Label,p=0.8,list=F))
training.data = sample.data[train,]
testing.data = sample.data[-train,]
ctrl = trainControl(method="repeatedcv",repeats=1)
#scaled.trainData=predict(preProcess.values,trainX)

my.model = train(Label~.,data=training.data,method="rf",trControl=ctrl,preProc=c("scale","center"),importance=T)
predict.cancer = predict(my.model,newdata=testing.data)

more.data = data[sample(1:length(data[,2]),100),]
for.ROC.predict=predict(my.model,more.data,type="prob")
more.predict = predict(my.model,more.data)



confusionMatrix(more.predict, more.data$Label)

#ROC curve
library(pROC)


#plot tree splits
library(rpart)
cancer.tree = rpart(Label~.,data=training.data)
for.plot = as.party(cancer.tree)
plot(for.plot)
#save multiple plots in one pdf
spdf("carpet-histos.pdf")
save = function(x){
  p = histogram(x)
  multiplot(p)
}



#library(gridExtra) library(cowplot)
#for multiple different plots in same pdf file






