suppressMessages(library(DBI))
suppressMessages(library(RMySQL))
library(dplyr)
cat("Connecting to mysql server database")
dbcon.hgmd <- dbConnect(MySQL(),user = "root", password = "paparam", host = "localhost", db = "hgmd_pro") # connect to database hgmd_pro in local server

tables <- dbListTables(dbcon.hgmd) #list of all tables

length(tables)
# to write each table as tab separated file
cat("Writing all files into tab separated files")
for (files in tables){
  tbl <- dbReadTable(conn = dbcon.hgmd, name = files) #read each table from hgmd_pro
  write.table(tbl, file = paste(files, ".txt",sep = ""), sep = "\t") # write table into tab separated txt file
}

#group by Go term acc number 
goterms.group <- tbl%>%
  subset(!is.na(go_term_acc))%>%
  group_by(go_term_acc)%>%
  summarise(frequency = n())

write.table(gene.group, file = "go-terms-acc-Count.txt", sep = "\t")

#saving files 
dbcon.hgmd <- dbConnect(MySQL(),user = "root", password = "paparam", host = "localhost", db = "hgmd_pro") # connect to database hgmd_pro in local server

tables <- dbListTables(dbcon.hgmd) #list of all tables

length(tables)

# to write each table as tab separated file
for (files in tables){
  tbl <- dbReadTable(conn = dbcon.hgmd, name = files) #read each table from hgmd_pro
  write.table(tbl, file = paste(files, ".txt",sep = ""), sep = "\t", row.names=F) # write table into tab separated txt file
}

#practice
table1 = data.frame(id=c("a","b","c","d","e","g"),exData=c(1,3,4,6,4,5))
table2 = data.frame(id=c("b","a","e","f","c"),exData=c(1,5,6,4,5))
table1$id = as.character(table1$id)
table2$id = as.character(table2$id)
table1 = table1[table1$id %in% table2$id,]
table2  = table2[table2$id %in% table1$id,]
mt = match(table1$id,table2$id)
any(is.na(mt))
table1.mt = table1[mt,]