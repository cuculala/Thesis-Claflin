#plot gene freq by Cancer and Non-cancer

suppressMessages(library(DBI))
suppressMessages(library(RMySQL))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))

dbcon.hgmd <- dbConnect(MySQL(),user = "root", password = "paparam", host = "localhost", db = "hgmd_pro")
tbl.mutation <- dbReadTable(conn = dbcon.hgmd, name = "mutation")
label=with(tbl.mutation, ifelse(grepl(".cancer.", disease,ignore.case = T,perl=T),"Cancer","Non-Cancer"))
tbl.mutation$Label=as.factor(tbl.mutation$Label)
cancer_ids = with(tbl.mutation,grep(".[cC]ance.*",disease,ignore.case=T,perl=T))


gene.group <- tbl.mutation%>%
  subset(!is.na(gene))%>%
  group_by(gene,Label)%>%
  summarise(count = n())

gene.group$gene <- 1:5722
p <- ggplot(aes(x = gene, y = count), data = subset(gene.group,count<100 )) +
  geom_point(aes(size=count))+scale_area()+
  geom_smooth()+ xlab("gene index")+
  ylab("frequency of the mutation gene")+facet_wrap(~Label)
p
