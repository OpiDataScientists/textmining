library(XML)
xml = xmlTreeParse("FR_BCKST66_TO_2017-34_0015.xml", useInternalNodes=TRUE,encoding = "UTF-8") # faut supprimer la balise Transaction et la remplacer
app_number=xpathApply(xml, "//ApplicationNumber", xmlValue)
app_des=xpathApply(xml, "//GoodsServicesDetails", xmlValue)


save(app_number, file="app_number.RData")
save(app_des, file="app_des.RData")
rm(list=ls())
gc()
load("app_number.RData")
load("app_des.RData")
app_number=cbind(c(app_number))
app_number=as.data.frame(app_number)
app_des=cbind(c(app_des))
app_des=as.data.frame(app_des)
data=cbind(app_number,app_des)
colnames(data)=c("application numb","descrip")





save(data, file="data.RData")
rm(list=ls())
gc()



load("data.RData")
colnames(data)=c("doc_id","text")
data$text=as.character(data$text)
data$doc_id=as.character(data$doc_id)
s=strsplit(as.character(data$text),',')
x=data.frame(t1=unlist(s),t2=rep(data$doc_id, sapply(s, FUN=length)))
s=strsplit(as.character(x$t1),';')
x=data.frame(t1=unlist(s),t2=rep(x$t2, sapply(s, FUN=length)))

library(plyr)
freq=count(x, "t1") #en general 
freq1=count(x, c("t1","t2")) #freq pr chaque marque
d=merge(x =x , y =freq, by = "t1") #en general 
d=d[,c(2,1,3)]
freq1=freq1[,c(2,1,3)]
colnames(d)=c("doc_id","text","freq")
colnames(freq1)=c("doc_id","text","freq")

save(d,file="sep_vir_gen.RData")
save(freq1,file="sep_vir.RData")


load("sep_vir.RData")
load("sep_vir_gen.RData")
##%######################################################%##
#                                                          #
####                    2eme partie                     ####
#                                                          #
##%######################################################%##


load("data.RData")
colnames(data)=c("doc_id","text") 
library(stringr)
#data[,2]=gsub("'", " ", data[,2])
#data[,2]=gsub(".", "", data[,2]) #probleme au niveaau des pts
data[,2]=str_replace_all(data[,2], "[^[:alnum:]]", " ")
data[,2]=gsub("non ", "non_", data[,2])
data[,2]=gsub("ni ", "ni_", data[,2])
s=strsplit(as.character(data$text),' ')
vect=c("   ","  "," ","","et","ou","pour","des","les","à","l","en","de","dans","d","où","un","autre","autres","plus","du","le","la","avec","une","un","Tous","tous","tout","toutes","toute")
for (i in 1:length(s))  { for   ( j in  1:length(vect))  { s[[i]]=s[[i]][s[[i]]!= vect[j]]  }        } 




library(data.table)
lstData <- Map(as.data.frame, s)
id=vector()
for (i in 1:length(lstData))
{ id[i]=dim(lstData[[i]])[1]}
x=rep(data[,1],id)

app_number=cbind(c(x))

dfrData <- rbindlist(lstData)

d=cbind(app_number,dfrData)
colnames(d)=c("app_number","word")
d$app_number=unlist(d$app_number)


library(plyr)
freq=count(d, "word") #en general 
dd=merge(x =d , y =freq, by = "word") #en general 
dd=dd[,c(2,1,3)]
colnames(dd)=c("doc_id","text","freq")
save(dd,file="sep_esp_gen.RData")
freq2=count(d, c("app_number","word")) #freq pr chaque marque
save(freq2,file="sep_esp.RData")



d1<- get(load("sep_vir.RData"))
d2<- get(load("sep_vir_gen.RData"))
d3<- get(load("sep_esp.RData"))
d4<- get(load("sep_esp_gen.RData"))



##%######################################################%##
#                                                          #
####   des preposition à ajouter "ces","ce","POUR",""   ####
#                                                          #
##%######################################################%##

