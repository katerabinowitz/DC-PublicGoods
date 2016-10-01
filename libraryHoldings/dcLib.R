require(dplyr)
require(data.table)
require(reshape2)
require(ggplot2)
setwd ("/Users/katerabinowitz/Documents/DataLensDC Org/libraries")

###read in IMLS files###
###read in IMLS files###
###read in IMLS files###
#source: https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey

files = list.files(pattern="*.csv")

names<-data.frame(c(2014,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2000,1992,1993,1994,1995,1996,1997,1998,1999))
colnames(names)<-"year"

libList <- list()
for (i in seq_along(files)) {
  libList[[i]] <- read.csv(file = files[i], stringsAsFactors=FALSE, strip.white=TRUE)
}

dcLibList<-lapply(libList, subset,STABR=="DC")
dcLib<-cbind(rbindlist(dcLibList,fill=TRUE),names)

###subset and factor###
###subset and factor###
###subset and factor###
colnames(dcLib)
dcLib<-select(dcLib,LIBNAME,BKVOL,EBOOK,AUDIO_PH,AUDIO,VIDEO,AUDIO_DL,VIDEO_PH,VIDEO_DL,DATABASE,SUBSCRIP,SUBSCRIPT,
              REFERENC,REFERENCE,TOTCIR,KIDCIRCL,GPTERMS,TOTEXPCO, TOTINCM,TOTOPEXP,STAFFEXP,year)
dcLib<-arrange(dcLib,year)

dcLibSum<-mutate(dcLib,
              AUDIO=ifelse(is.na(AUDIO_DL),AUDIO,AUDIO_DL+AUDIO_PH),
              VIDEO=ifelse(is.na(VIDEO_DL),VIDEO,VIDEO_DL+VIDEO_PH))

dcLibSum<-select(dcLibSum,year,BKVOL,EBOOK,AUDIO,VIDEO,TOTCIR,KIDCIRCL)


pct <- function(x) {((x-(lag(x)))/lag(x))*100}
dcLibCh<- mutate_each(dcLibSum,funs(pct), c(BKVOL,EBOOK,AUDIO,VIDEO,TOTCIR,KIDCIRCL))
colnames(dcLibCh)<-c("year","bookCh","ebookCh","audioCh","videoCh","cirCh","kidCirCh")

dcLibFin<-merge(dcLibSum,dcLibCh,by="year")
write.csv(dcLibFin,"dcLibFin.csv")

#rough graph cuts
dcLibG<-melt(dcLibSum,id.var="year")
dcLibC<-filter(dcLibG,variable %in% c("TOTCIR","KIDCIRCL"))
dcLibG<-filter(dcLibG,variable %in% c("BKVOL","EBOOK","AUDIO","VIDEO","GPTERMS"))

ggplot(dcLibG, aes(x=year, y=value, fill=variable)) + 
  geom_area()

ggplot(dcLibC, aes(x=year, y=value, group=variable, colour=variable)) +
  geom_line() +
  geom_point()
