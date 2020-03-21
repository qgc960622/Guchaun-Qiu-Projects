
rm(list=ls())
install.packages('tm')
library(tm)
install.packages('NMF')
library(NMF)
install.packages('ggplot2')
library(ggplot2)

docs<-Corpus(DirSource("/Users/macbook/Desktop/cis 434/HW4/fb") )

# transformations and dtm building
dtm = DocumentTermMatrix(docs, control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, 
    stopwords=c(stopwords("english"), stopwords("spanish"),stopwords("portuguese"))))

dtm = removeSparseTerms(dtm,0.996)

#get frequency
freq_all = as.matrix(dtm)

cauliflower = data.frame(freq_all[,'cauliflower'])
cauliflower = cbind(rownames(cauliflower),cauliflower)

names(cauliflower) = c('date','freq_cau')


#generate a list containing frequency of 'cauliflower' in each every month
l = list()
for (i in 1: nrow(cauliflower)){
  x = unlist(strsplit(unlist(strsplit(row.names(cauliflower)[i], "-")),'.csv'))
  len_3 = length(unlist(strsplit(x[3],'')))
  if (len_3 == 1){
    x[3] = paste('0',x[3],sep = '')
  }
  l[i] = paste(x[2],x[3],sep = '.')
}

l
cauliflower['date'] = unlist(l)
cauliflower = cauliflower[order(cauliflower['date']),]

#plot the trend over time using ggplot 
ggplot(cauliflower, 
  aes(x=date, y=freq_cau, group=1)) + geom_line(linetype="dotted", 
  color='blue') + theme(axis.text.x=element_blank())+
  ggtitle("Frequency of Cauliflower")

