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
print(Terms(dtm))


library(stringi)
stri_enc_mark(Terms(dtm))
all(stri_enc_isutf8(Terms(dtm)))


print(Terms(dtm))


scanner <- function(x) strsplit(x," ")
ap.tdm <- TermDocumentMatrix(dsc,control=list(tokenize=scanner))
findFreqTerms(ap.tdm, lowfreq=30)


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


##%######################################################%##
#                                                          #
####                    2emepartie        #manuellle             ####
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


library(tm)
ds <- DataframeSource(data)
dsc<-Corpus(ds)

dtm<- DocumentTermMatrix(dsc,control = list(removePunctuation = TRUE,removeNumbers = TRUE,stopwords = TRUE)) # on perd UTF 8
print(Terms(dtm))

library(stringi)
stri_enc_mark(Terms(dtm))
all(stri_enc_isutf8(Terms(dtm)))


print(Terms(dtm))

scanner <- function(x) strsplit(x," ")
ap.tdm <- TermDocumentMatrix(dsc,control=list(tokenize=scanner))
findFreqTerms(ap.tdm, lowfreq=30)

library(tidytext)
#### we loose UTF 8 ICI
dat<- tidy(dtm,encoding = "UTF-8")
prep=c("pour","non","les","des","notamment")
dat =dat[ ! dat$term %in% prep, ]


##%######################################################%##
#                                                          #
####                         s                          ####
#                                                          #
##%######################################################%##












text  <- "mécanique , chérie bébé elle est magnifique mécanique éléve syb orazhhha zkkk zzz kk kk bébé chérie , mécanique soléil soléil platoaaàzaiiea"
text <- iconv(text, from="ASCII//TRANSLIT//IGNORE", to="UTF-8")

Encoding(text)
# [1] "unknown"

Encoding(text)  <- "UTF-8"
# [1] "UTF-8"

ap.corpus <- Corpus(DataframeSource(data.frame(text)))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, content_transformer(tolower))

content(ap.corpus[[1]])

ap.tdm <- TermDocumentMatrix(ap.corpus)

ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)

print(table(ap.d$freq))
# 1  2  3 
# 62  5  1 

print(findFreqTerms(ap.tdm, lowfreq=2))

plot(tdm, terms = findFreqTerms(tdm, lowfreq = 6)[1:25], corThreshold = 0.5)

