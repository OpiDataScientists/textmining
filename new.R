library(XML)
library(xml2)
xml = xmlTreeParse("FR_BCKST66_TO_2017-34_0001.xml", useInternalNodes=TRUE,encoding = "UTF-8") # faut supprimer la balise Transaction et la remplacer

app_number=xpathApply(xml, "//ApplicationNumber", xmlValue)
app_des=xpathApply(xml, "//GoodsServicesDetails", xmlValue)
app_date=xpathApply(xml, "//ApplicationDate", xmlValue)


app_des=gsub('[0-9]+', '', app_des)


app_des=gsub(paste("Nice", collapse="|"), "", app_des)

app_date=(substr(app_date,1,4))
save(app_number, file="app_number.RData")
save(app_des, file="app_des.RData")
save(app_date, file="app_date.RData")

rm(list=ls())
gc()
load("app_number.RData")
load("app_des.RData")
load("app_date.RData")

app_number=cbind(c(app_number))
app_number=as.data.frame(app_number)

app_des=cbind(c(app_des))
app_des=as.data.frame(app_des)

app_date=cbind(c(app_date))
app_date=as.data.frame(app_date)



data=cbind(app_number,app_des,app_date)
colnames(data)=c("application numb","descrip","app_date")





save(data, file="data.RData")
rm(list=ls())
gc()
load("data.RData")


colnames(data)=c("doc_id","text","year")
data$text=as.character(data$text)
data$doc_id=as.character(data$doc_id)
s=strsplit(as.character(data$text),',')
x=data.frame(text=unlist(s),doc_id=rep(data$doc_id, sapply(s, FUN=length)))
s=strsplit(as.character(x$text),';')
x=data.frame(text=unlist(s),doc_id=rep(x$doc_id, sapply(s, FUN=length)))
x=x[,c(2,1)]
x=merge(x,data[,c(1,3)],by="doc_id")

library(plyr)
freq1=count(x, c("doc_id","text","year")) #freq pr chaque marque
freq1<- freq1[-which(freq1$text == " "), ]
save(freq1,file="sep_vir.RData")
rm(list=ls())
gc()
load("sep_vir.RData")

##%######################################################%##
#                                                          #
####                    2eme partie                     ####
#                                                          #
##%######################################################%##


load("data.RData")
colnames(data)=c("doc_id","text","year") 
data$year=as.character(data$year)
library(stringr)
#data[,2]=gsub("'", " ", data[,2])
#data[,2]=gsub(".", "", data[,2]) #probleme au niveaau des pts
data[,2]=str_replace_all(data[,2], "[^[:alnum:]]", " ")
data[,2]=gsub("non ", "non_", data[,2])
data[,2]=gsub("ni ", "ni_", data[,2])
s=strsplit(as.character(data$text),' ')
vect=c("   ","  "," ","","et","ou","pour","POUR","ce","ces","des","les","à","l","en","de","dans","d","où","un","autre","autres","plus","du","le","la","avec","une","un","Tous","tous","tout","toutes","toute","DE","ET","A","par","PAR","SUR","sur","se","a","OU","leurs","D","notamment")
for (i in 1:length(s))  { for   ( j in  1:length(vect))  { s[[i]]=s[[i]][s[[i]]!= vect[j]]  }        } 




library(data.table)
lstData <- Map(as.data.frame, s)
id=vector()
for (i in 1:length(lstData))
{ id[i]=dim(lstData[[i]])[1]}
app_number=rep(data[,c(1)],id)
year=rep(data[,c(3)],id)

app_number=cbind(c(app_number))
year=cbind(c(year))
dfrData <- rbindlist(lstData)

d=cbind(app_number,dfrData,year)
colnames(d)=c("doc_id","word","year")
d$doc_id=unlist(d$doc_id)
d$year=unlist(d$year)

library(plyr)
freq2=count(d, c("doc_id","word","year")) #freq pr chaque marque

save(freq2,file="sep_esp.RData")
rm(list=ls())
gc()
load("sep_esp.RData")


########