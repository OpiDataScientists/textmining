library(XML)


xml = xmlTreeParse("FR_BCKST66_TO_2017-34_0015.xml", useInternalNodes=TRUE,encoding = "UTF-8") # faut supprimer la balise Transaction et la remplacer

app_number=xpathApply(xml, "//ApplicationNumber", xmlValue)
app_d=xpathApply(xml, "//ApplicationDate", xmlValue)
app_filling_place=xpathApply(xml, "//FilingPlace", xmlValue)
app_des=xpathApply(xml, "//GoodsServicesDetails", xmlValue)
descri=xpathApply(xml, "//ClassDescriptionDetails", xmlValue)
desc=xpathApply(xml, "//GoodsServicesDescription", xmlValue)


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

###########################################
load("data.RData")
colnames(data)=c("doc_id","text")

library(tm)
ds <- DataframeSource(data)
dsc<-Corpus(ds)
dtm<- DocumentTermMatrix(dsc)
library(tidytext)
#### we loose UTF 8 ICI
dat<- tidy(dtm,encoding = "UTF-8")
prep=c("pour","non","les","des","notamment")
dat =dat[ ! dat$term %in% prep, ]

scanner <- function(x) strsplit(x,c(",",";"))
dtm2 <- DocumentTermMatrix(dsc,control=list(tokenize=scanner))
newd <- tidy(dtm2)






View(dtm)

library(tm)
ds <- DataframeSource(test)
dsc<-Corpus(ds)

dtm<- DocumentTermMatrix(dsc)
library(tidytext)

dat<- tidy(dtm)
scanner <- function(x) strsplit(x," ")
dtm2 <- DocumentTermMatrix(dsc,control=list(tokenize=scanner))
newd <- tidy(dtm2)









