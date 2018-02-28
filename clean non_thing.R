#CONSIDERER LES NON ET NI

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
