##%######################################################%##
#                                                          #
####                   SIMPLE EXAMPLE                   ####
#                                                          #
##%######################################################%##


library(XML)
library(xml2)
doc = xmlTreeParse("a.xml", useInternal = TRUE,encoding="ISO-8859-1")

top = xmlRoot(doc)
xmlName(top)
names(top) #child nodes of this root
names( top[[ 1 ]] )
art = top[[ 1 ]] [[ "AuthorList" ]] #author list containt authors
names(art)
names(art[[1]])
#make a loop
xmlSApply(art[[1]], xmlValue)
xmlSApply(art[[2]], xmlValue)
xmlSApply(art[[3]], xmlValue)
######===============> 
liste=xmlSApply(art, function(x) xmlSApply(x, xmlValue))
mode(liste)
length(liste)


##%######################################################%##
#                                                          #
####              exemple sur sample inpi               ####
#                                                          #
##%######################################################%##




xml = xmlTreeParse("FR_BCKST66_TO_2017-34_0015.xml", useInternalNodes=TRUE) # faut supprimer la balise Transaction et la remplacer

app_number=xpathApply(xml, "//ApplicationNumber", xmlValue)
app_d=xpathApply(xml, "//ApplicationDate", xmlValue)
app_filling_place=xpathApply(xml, "//FilingPlace", xmlValue)
app_des=xpathApply(xml, "//GoodsServicesDetails", xmlValue)
descri=xpathApply(xml, "//ClassDescriptionDetails", xmlValue)
desc=xpathApply(xml, "//GoodsServicesDescription", xmlValue)

d=do.call(rbind, Map(data.frame, A=app_number, B=app_des))

save(app_number, file="app_number.RData")
save(app_des, file="app_des.RData")

rm(list=ls())
gc()
load("app_number.RData")
load("app_des.RData")

 
app_number=cbind(c(app_number))

app_des=cbind(c(app_des))

data=cbind(app_number,app_des)
colnames(data)=c("application numb","descrip")





save(data, file="data.RData")
rm(list=ls())
gc()
load("data.RData")







##%######################################################%##
#                                                          #
####                     zip partie                     ####
#                                                          #
##%######################################################%##
unzip("FR_BCKST66_TO_2017-34_0015.zip")




#devtools::install_github("r-lib/xml2")
library(xml2)


doc<-xmlParse("samplekit.xml")
devtools::install_github("r-lib/xml2")
theData <- extract_xml("samplekit.xml")
xml = xmlTreeParse("samplekit.xml", useInternalNodes=TRUE,encoding="ISO-8859-1")
xpathApply(xml, "//ApplicationNumber", xmlValue)


















#devtools::install_github("r-lib/xml2")
library(xml2)
x <- read_xml("samplekit.xml")
xml_name(x)
xml_children(x)

xpathApply(x, "//ApplicationNumber", xmlValue)












require("XML")
require("plyr")
require("ggplot2")
require("gridExtra")

xmlfile=xmlParse("samplekit.xml")
class(xmlfile)
xmltop = xmlRoot(xmlfile) #gives content of root
class(xmltop)#"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
xmlName(xmltop) #give name of node, PubmedArticleSet
xmlSize(xmltop) #how many children in node, 19
xmlName(xmltop[[1]]) #name of root's 1st children
# have a look at the content of the first child entry
xmltop[[1]]
# have a look at the content of the 2nd child entry

#Root Node's children
xmlSize(xmltop[[2]]) #number of nodes in each child
xmlSApply(xmltop[[2]], xmlName) #name(s)
xmlSApply(xmltop[[2]], xmlAttrs) #attribute(s)
xmlSApply(xmltop[[2]], xmlSize) #size

data=ldply(xmlToList("samplekit.xml"), data.frame)






require(XML)
xmlToDF = function(doc, xpath, isXML = TRUE, usewhich = TRUE, verbose = TRUE) {
  
  if (!isXML) 
    doc = xmlParse(doc)
  #### get the records for that form
  nodeset <- getNodeSet(doc, xpath)
  
  ## get the field names
  var.names <- lapply(nodeset, names)
  
  ## get the total fields that are in any record
  fields = unique(unlist(var.names))
  
  ## extract the values from all fields
  dl = lapply(fields, function(x) {
    if (verbose) 
      print(paste0("  ", x))
    xpathSApply(proc, paste0(xpath, "/", x), xmlValue)
  })
  
  ## make logical matrix whether each record had that field
  name.mat = t(sapply(var.names, function(x) fields %in% x))
  df = data.frame(matrix(NA, nrow = nrow(name.mat), ncol = ncol(name.mat)))
  names(df) = fields
  
  ## fill in that data.frame
  for (icol in 1:ncol(name.mat)) {
    rep.rows = name.mat[, icol]
    if (usewhich) 
      rep.rows = which(rep.rows)
    df[rep.rows, icol] = dl[[icol]]
  }
  
  return(df)
}
a=ldply(xmlToList(xmlfile), data.frame) #completes with errors: "row names were found from a short variable and have been discarded"















library(xml2)
pg <- read_xml("samplekit.xml")
recs <- xml_find_all(pg,"//Transaction/TransactionHeader")
