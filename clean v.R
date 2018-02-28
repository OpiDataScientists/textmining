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

###########################################
Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

colnames(data)=c("doc_id","text")
data$text=as.character(data$text)
data$text=Unaccent(data$text)

data$doc_id=as.character(data$doc_id)





library(tm)
ds <- DataframeSource(data)
dsc<-Corpus(ds)

dtm<- DocumentTermMatrix(dsc,control = list(removePunctuation = TRUE,removeNumbers = TRUE,stopwords = TRUE)) # on perd UTF 8
#print(Terms(dtm))


library(stringi)
#stri_enc_mark(Terms(dtm))
#all(stri_enc_isutf8(Terms(dtm)))
findFreqTerms(dtm, lowfreq=30)


library(tidytext)
#### we loose UTF 8 ICI
dat<- tidy(dtm,encoding = "UTF-8")
prep=c("pour","non","les","des","notamment")
dat =dat[ ! dat$term %in% prep, ]




##%######################################################%##
#                                                          #
####                    2eme partie        #manuellle             ####
#                                                          #
##%######################################################%##

load("data.RData")
colnames(data)=c("doc_id","text")
data$text=as.character(data$text)

data$doc_id=as.character(data$doc_id)

s=strsplit(as.character(data$text),',')


x=data.frame(t1=unlist(s),t2=rep(data$doc_id, sapply(s, FUN=length)))


ss=strsplit(as.character(x$t1),';')
xx=data.frame(t1=unlist(ss),t2=rep(x$t2, sapply(ss, FUN=length)))

library(plyr)
freq=count(xx, "t1")
xx=merge(x =xx , y =freq, by = "t1")
colnames(xx)[3]<- "freq"

rm(list=ls())
gc()

save(xx, file="xx")
load("xx")

dd= xx[!duplicated(xx), ]

##%######################################################%##
#                                                          #
####             1ére partie autre methode              ####
#                                                          #
##%######################################################%##

load("data.RData")
colnames(data)=c("doc_id","text")
library(stringr)
data[,2]=str_replace_all(data[,2], "[^[:alnum:]]", " ")
#data[,2]=gsub(",", " ", data[,2])
#data[,2]=gsub(";", " ", data[,2])
#data[,2]=gsub("'", " ", data[,2])
#data[,2]=gsub(".", "", data[,2]) #probleme au niveaau des pts

s=strsplit(as.character(data$text),' ')
vect=c("   ","  "," ","","et","ou","pour","des","les","à","l","en","de","dans","d","où","un","autre","autres","plus","du","le","la","avec","une","un","Tous","tous","tout","toutes","toute")
for (i in 1:length(s))  { for   ( j in  1:length(vect))  { s[[i]]=s[[i]][s[[i]]!= vect[j]]  }        } 




library(data.table)
lstData <- Map(as.data.frame, s)
lstData[2]
id=vector()
for (i in 1:length(lstData))
{ id[i]=dim(lstData[[i]])[1]}
x=rep(data[,1],id)

app_number=cbind(c(x))

dfrData <- rbindlist(lstData)

d=cbind(app_number,dfrData)
colnames(d)=c("app_number","word")
str(x)
library(plyr)
counts <- ddply(x, .(x$word), nrow)
counts_a <- ddply(x, .(as.factor(x$app_number),x$word), nrow)


#x=data.frame(t1=unlist(s[1]),t2=rep(data$doc_id, sapply(s[1], FUN=length)))






k=as.data.frame(s[[1]])
k=as.data.frame(s)

x=data.frame(t1=unlist(s),t2=rep(data$doc_id, sapply(s, FUN=length)))


library(tm)
ds <- DataframeSource(data)
dsc<-Corpus(ds)
 
dtm<- DocumentTermMatrix(dsc,control = list(removePunctuation = TRUE,removeNumbers = TRUE,stopwords = TRUE)) # on perd UTF 8
#print(Terms(dtm))


library(stringi)
#stri_enc_mark(Terms(dtm))
#all(stri_enc_isutf8(Terms(dtm)))
findFreqTerms(dtm, lowfreq=70)


library(tidytext)
#### we loose UTF 8 ICI
dat<- tidy(dtm,encoding = "UTF-8")
prep=c("pour","non","les","des","notamment")
dat =dat[ ! dat$term %in% prep, ]



s=strsplit(as.character(data$text),' ')
x=data.frame(t1=unlist(s),t2=rep(data$doc_id, sapply(s, FUN=length)))

