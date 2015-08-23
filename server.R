library(shiny)
options(shiny.trace = F) 
library(shinysky)

shinyServer(function(input, output) {
  library(tm)
  library(RWeka)
  library(data.table)
  library(SnowballC)
  library(stringr)
  
  makeCorpus<- function(x) {
    corpus<- Corpus(VectorSource(x))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, stemDocument)
    corpus<- tm_map(corpus,removePunctuation)
    return(corpus)
  }
  
  process<- function(x) {
    x=gsub(",?", "", x)
    x=gsub("\\.{3,}", "", x)
    x=gsub("\\:", "", x)
    x=gsub("\\'", "", x)
    x=gsub("\\|", "", x)
    x=gsub("\\{", "", x)
    x=gsub("\\}", "", x)
    x<-strsplit(unlist(x),"[\\.]{1}")
    x<-strsplit(unlist(x),"\\?+")
    x<-strsplit(unlist(x),"\\!+")
    x<-strsplit(unlist(x),"\\-+")
    x<-strsplit(unlist(x),"\\(+")
    x<-strsplit(unlist(x),"\\)+")
    x<-strsplit(unlist(x),"\\\"")
    x<-gsub("^\\s+", "", unlist(x))
    x<-gsub("\\s+$", "", unlist(x))
    x<-gsub("\\s*~\\s*", " ", unlist(x))
    x<-gsub("\\/", " ", unlist(x))
    x<-gsub("\\+", " ", unlist(x))
    x<-gsub("it s ", "its ", unlist(x))
    x<-gsub("i m not", "im not", unlist(x))
    x<-gsub("i didn t", "i didnt", unlist(x))
    x<-gsub("i don t", "i dont", unlist(x))
    x<-gsub(" i m ", " im ", unlist(x))
    x=x[which(nchar(x)!=1)]
    x=x[which(nchar(x)!=0)]
  }
  
  getPred=function(x){
    test=x
    test=iconv(test, to='ASCII', sub=' ')
    test=process(test)
    test=paste0(test, collapse=" ")
    corpus<-makeCorpus(test)
    corpus=as.character(corpus[[1]][1])
    words<-unlist(strsplit(corpus,"\\s+"))
    Tfreq=afreq
    history=words[(length(words)-1):length(words)]
    nMin1=words[length(words)]
    history=paste(as.character(history),collapse=' ')
    histstring=str_replace_all(history, "[[:punct:]]", "?")
    Tpred=data.table(Tfreq[grep(paste0("^",histstring," "),Tfreq$grams),][order(-counts)])
    pred=Tpred[1]$grams
    pred=unlist(strsplit(pred,"\\s+"))
    pred=pred[length(pred)]
    if(is.na(pred)){
      pred="the"
    }
    return(pred)
  }
  
  afreq=readRDS("allcount.RDS")
  
  library(compiler)
  getPred.=cmpfun(getPred)
  
  output$prediction <- renderText({  
    as.character(getPred.(input$text))
  })
})