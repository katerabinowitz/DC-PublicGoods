require(dplyr)
require(data.table)
require(reshape2)
require(ggplot2)
require(scales)
setwd ("/Users/katerabinowitz/Documents/DataLensDC Org/libraries")

###read in IMLS files###
###read in IMLS files###
###read in IMLS files###
#source: https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey

files = list.files(pattern="*.csv")

names<-data.frame(c(2014,2006,2007,2008,2009,2010,2011,2012,2013))
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
dcLib<-select(dcLib,LIBNAME,BKVOL,EBOOK,AUDIO_PH,AUDIO,VIDEO,AUDIO_DL,VIDEO_PH,VIDEO_DL,DATABASE,SUBSCRIP,
              REFERENC,TOTCIR,KIDCIRCL,GPTERMS,TOTEXPCO, TOTINCM,TOTOPEXP,STAFFEXP,VISITS,TOTPRO,PRMATEXP,ELMATEXP,year)
dcLib<-arrange(dcLib,year)

dcLibSum<-mutate(dcLib,
              AUDIO=ifelse(is.na(AUDIO_DL),AUDIO,AUDIO_DL+AUDIO_PH),
              VIDEO=ifelse(is.na(VIDEO_DL),VIDEO,VIDEO_DL+VIDEO_PH))

dcLibSum<-select(dcLibSum,year,BKVOL,EBOOK,AUDIO,VIDEO,TOTCIR,KIDCIRCL,VISITS,TOTPRO)


pct <- function(x) {((x-(lag(x)))/lag(x))*100}
dcLibCh<- mutate_each(dcLibSum,funs(pct), c(BKVOL,EBOOK,AUDIO,VIDEO,TOTCIR,KIDCIRCL,VISITS,TOTPRO))
colnames(dcLibCh)<-c("year","bookCh","ebookCh","audioCh","videoCh","cirCh","kidCirCh","visitCh","proCh")
colnames(dcLibSum)<-c("year","Books","e-Books","Audio","Video","Circulation","kidCirc","Library Visits","Programs")

dcLibFin<-merge(dcLibSum,dcLibCh,by="year")

#rough graph cuts
dcLibG<-melt(dcLibSum,id.var="year")
dcLibC<-filter(dcLibG,variable %in% c("Circulation","Library Visits"))
dcLibP<-filter(dcLibG,variable %in% c("Programs"))
dcLibG<-filter(dcLibG,variable %in% c("Books","e-Books","Audio","Video"))

palette<-c("#7A9EAF","#655989","#DE88A5","#FFCEBB")

ggplot(dcLibG, aes(x=year, y=value, fill=variable)) + 
  geom_area() +
  scale_fill_manual(values=palette) +
  scale_y_continuous(labels = function(x)x/1000000,limits = c(0,2500000),
                     expand=c(0,0),breaks=c(500000, 1000000, 1500000, 2000000, 2500000)) +
  scale_x_continuous(expand=c(0,0.08)) +
  theme(legend.position="top") + 
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(color="#505050")) +
  theme(legend.background = element_rect(fill = '#EFF0F1')) +
  theme(legend.key = element_rect(color = '#EFF0F1')) +
  theme(legend.text=element_text(color="#505050")) +  
  theme(axis.text.x = element_text(hjust=0.6)) +
  theme(axis.text.y = element_text(hjust=1)) +
  theme(plot.background = element_rect(fill = '#EFF0F1',colour=NA))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=13)) +
  theme(axis.title.x = element_text(color="#505050")) +
  theme(axis.title.y = element_text(color="#505050")) +
  theme(plot.title=element_text(family="Helvetica", size=15, face="bold")) +
  theme(plot.subtitle=element_text(color="#505050")) +
  theme(plot.caption=element_text(color="#808080")) +
  labs(x="",y="Thousands", 
       title="Books Make Up Less of Today's DC Library Collection",
       subtitle="DCPL resources to borrow, by type",
       caption="\nSource: IMLS")
ggsave("/Users/katerabinowitz/Documents/DataLensDC/Website/DataLensDCsite/Images/DCPLholdingsArea.png",width=6, height=5,dpi=100)

ggplot(dcLibG, aes(x=year, y=value, fill=variable)) + 
  geom_area() +
  scale_y_continuous(labels = function(x)x/1000,expand=c(0,0)) +
  scale_x_continuous(expand=c(.02,.02)) +
  guides(fill=FALSE) +
  scale_fill_manual(values=palette) +
  theme(axis.text.x = element_text()) +
  theme(axis.text.y = element_text()) +
  theme(plot.background = element_rect(fill = '#EFF0F1',colour=NA))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=13)) +
  theme(axis.title.x = element_text(color="#505050")) +
  theme(axis.title.y = element_text(color="#505050")) +
  theme(plot.title=element_text(family="Helvetica", size=15, face="bold")) +
  theme(plot.subtitle=element_text(color="#505050")) +
  theme(plot.caption=element_text(color="#808080")) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.ticks.y=element_blank()) +
  labs(x="",y="Thousands", 
       title="Fewer Books, More of Every Other Medium",
       subtitle="DCPL resources to borrow, by type",
       caption="\nSource: IMLS") +
  facet_wrap(~ variable, ncol = 2, scale="free_y")
ggsave("/Users/katerabinowitz/Documents/DataLensDC/Website/DataLensDCsite/Images/DCPLholdingsSingle.png",width=6, height=6,dpi=100)

ggplot(dcLibC, aes(x=year, y=value, group=variable, colour=variable)) +
  geom_line() +
  scale_color_manual(values=c("#9BB899","#166678")) +
  scale_y_continuous(expand=c(0,0), limit=c(0,4500000),labels = function(x)x/1000000, breaks=c(1000000,2000000,3000000,4000000)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(hjust=0.6)) +
  theme(axis.text.y = element_text(hjust=1)) +
  theme(plot.background = element_rect(fill = '#EFF0F1',colour=NA))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=13)) +
  theme(axis.title.x = element_text(color="#505050")) +
  theme(axis.title.y = element_text(color="#505050")) +
  theme(plot.title=element_text(family="Helvetica", size=15, face="bold")) +
  theme(plot.subtitle=element_text(color="#505050")) +
  theme(plot.caption=element_text(color="#808080")) +
  annotate("text", x = 2011, y = 2500000, label = "Library visits", color="#166678") +
  annotate("text", x = 2012, y = 3450000, label = "Circulation",color="#9BB899") +
  labs(x="",y="Millions", 
       title="Circulation and Library Visits Double in 8 Years",
       subtitle="Annual Circulation and Library Visits",
       caption="\nSource: IMLS")
ggsave("/Users/katerabinowitz/Documents/DataLensDC/Website/DataLensDCsite/Images/DCPLvisits.png",width=6, height=5,dpi=100)
