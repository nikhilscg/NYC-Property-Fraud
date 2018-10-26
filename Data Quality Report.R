library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(rJava)
library(xlsx)
library(readxl)
options(scipen=999)

#NYdata = read.csv("NY property 1 million.csv",quote="")
#NYdata = read_excel("NY property 1 million.xlsx",sheetName="avroll")
##Read the dataset
NYdata = read_excel("NY property 1 million.xlsx",sheet = 1)
prop_data=NYdata
##look at the fields
summary(NYdata)
##if BLDGClass known then tax class can be generated
head(NYdata)
numrecords = nrow(NYdata)
numrecords

##look at each field one by one
length(unique(NYdata$RECORD)) ##is a unique code for each record
summary(NYdata$RECORD)

sum(is.na(NYdata$RECORD))
min(NYdata$RECORD)
summary(NYdata$RECORD)

length(unique(NYdata$BBLE))
sum(is.na(NYdata$BBLE))
min(NYdata$BBLE)
summary(NYdata$BBLE)

##separate the BORO from the BBLE data field
prop_data$BORO = substr(prop_data$BBLE,1,1)
prop_data$BORO=as.numeric(prop_data$BORO)
##add the BORO name
#initialize the vector to store the names
boro_name=vector()
for (j in 1:nrow(prop_data)) {
  
  if(prop_data$BORO[j]==1){
    boro_name[j]="MANHATTAN"
  }
  else if(prop_data$BORO[j]==2){
    boro_name[j]="BRONX"
  }
  else if(prop_data$BORO[j]==3){
    boro_name[j]="BROOKLYN"
  }
  else if(prop_data$BORO[j]==4){
    boro_name[j]="QUEENS"
  }
  else {
    boro_name[j]="STATEN ISLAND"
  }
}
prop_data = cbind(prop_data,boro_name)
prop_data$boro_name=as.character(prop_data$boro_name)
length(unique(NYdata$BLOCK))
sum(is.na(NYdata$BLOCK))
min(NYdata$BLOCK)
summary(NYdata$BLOCK)

prop_data%>%
  group_by(BLOCK)%>%
  summarize(num =n(),percent = n()/numrecords*100)%>%
  arrange(-num)%>%
  head(25) -> block
write.csv(block,"BLOCK.csv")
prop_data %>%
  group_by(BLOCK,boro_name)%>%
  summarize(number = n()) %>%
  arrange(-number)%>%
  head(25) -> block


length(unique(prop_data$BLOCK))
prop_data %>%
  ggplot(aes(BLOCK))+
  geom_histogram()

# LOT
length(unique(NYdata$LOT))
sum(is.na(NYdata$LOT))
min(NYdata$LOT)
summary(NYdata$LOT)
sd(prop_data$LOT)
prop_data %>%
  group_by(LOT)%>%
  summarize(number = n(),percent = n()*100/numrecords) %>%
  arrange(-number) %>%
  head(25) -> Lot
warnings()
write.csv(Lot,"LOT.csv")
prop_data%>%
  filter(LOT<2500)%>%
  ggplot(aes(LOT))+
  geom_histogram()


##EASEMENT
length(unique(prop_data$EASEMENT))
populated = 100*(1-(sum(!is.na(prop_data$EASEMENT))/numrecords))
100-populated
summary(prop_data$EASEMENT)
##make a bar chart
nrow(prop_data[!(is.na(prop_data$EASEMENT)),])
prop_data[!(is.na(prop_data$EASEMENT)),] %>%
  ggplot(aes(x=EASEMENT))+
  geom_bar()+
  ggtitle("# of Records per Easement Status")+
  ylab("# of Records")+
  coord_flip()

#OWNER
length(unique(prop_data$OWNER))
populated = 100*(1-(sum(is.na(prop_data$OWNER))/numrecords))
100-populated
summary(prop_data$EASEMENT)
##make a bar chart
nrow(prop_data[!(is.na(prop_data$EASEMENT)),])
prop_data[!(is.na(prop_data$OWNER)),] %>%
  group_by(OWNER)%>%
  summarize(number_prop=n(),percent = n()/numrecords*100)%>%
  arrange(-number_prop)%>%
  head(25) -> owner
write.csv(owner,"owner.csv")

  ggplot(aes(x=reorder(OWNER,number_prop),y=number_prop))+
  geom_bar(stat="identity")+
  ggtitle("Top 25 owners of property")+
  ylab("# of Records")

#BDLGCL
  length(unique(prop_data$BLDGCL))
  populated = 100*(1-(sum(is.na(prop_data$BLDGCL))/numrecords))
  populated
  100-populated
  summary(prop_data$BLDGCL)
prop_data[!(is.na(prop_data$BLDGCL)),] %>%
    group_by(BLDGCL)%>%
    summarize(number_prop=n())%>%
    arrange(-number_prop)%>%
    head(25)%>%
  ggplot(aes(x=reorder(BLDGCL,number_prop),y=number_prop))+
  geom_bar(stat="identity")+
  ggtitle("Top 25 Building Classes")+
  ylab("# of Records")+
  xlab("Building Class")
#TAx class
DLGCL
length(unique(prop_data$TAXCLASS))
populated = 100*(1-(sum(is.na(prop_data$TAXCLASS))/numrecords))
populated
100-populated
summary(prop_data$BLDGCL)
prop_data[!(is.na(prop_data$TAXCLASS)),] %>%
  group_by(TAXCLASS)%>%
  summarize(number_prop=n())%>%
  arrange(-number_prop)%>%
  head(25)%>%
  ggplot(aes(x=reorder(TAXCLASS,number_prop),y=number_prop))+
  geom_bar(stat="identity")+
  ggtitle("Top Tax Classes")+
  ylab("# of Records")+
  xlab("Tax Class")
##LTFront
prop_data$LTFRONT=as.numeric(prop_data$LTFRONT)
length(unique(prop_data$LTFRONT))
populated = 100*(1-(sum(is.na(prop_data$LTFRONT))/numrecords))
populated
100-populated
summary(prop_data$LTFRONT)
sd(prop_data$LTFRONT)

prop_data%>%
  filter(LTFRONT<100)%>%
  ggplot(aes(x="",y=LTFRONT))+
  geom_boxplot()

prop_data%>%
  filter(LTFRONT==0)%>%
  nrow()/numrecords*100
prop_data%>%
  filter(LTFRONT<70)%>%
  nrow()/numrecords*100
  #filter(!LTFRONT==0)%>%
  ggplot(aes(LTFRONT))+
  geom_bar()+
  scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="Lot Frontage", y="Count")

##LTDepth
prop_data$LTDEPTH=as.numeric(prop_data$LTDEPTH)
length(unique(prop_data$LTDEPTH))
populated = 100*(1-(sum(is.na(prop_data$LTDEPTH))/numrecords))
populated
100-populated
summary(prop_data$LTDEPTH)
sd(prop_data$LTDEPTH)

prop_data%>%
  filter(LTDEPTH==0)%>%
  nrow()
prop_data%>%
  filter(LTDEPTH<100)%>%
  ggplot(aes(x="",y=LTDEPTH))+
  geom_boxplot()

prop_data%>%
 filter(LTDEPTH<150)%>%
  filter(!LTDEPTH==0)%>%
  ggplot(aes(LTDEPTH))+
  geom_bar()+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="Lot Depth", y="Count")
prop_data%>%
  filter(LTDEPTH<150)%>%
  filter(!LTDEPTH==0)%>%
  ggplot(aes(LTDEPTH))+
  geom_bar()+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="Lot Depth", y="Count")+
  scale_y_continuous(breaks=seq(0,400000,100000))
prop_data%>%
  filter(LTDEPTH==100)%>%
  nrow()/numrecords*100


# FULLVAL
prop_data$STORIES=as.numeric(prop_data$STORIES)
length(unique(prop_data$STORIES))
populated = 100*(1-(sum(is.na(prop_data$STORIES))/numrecords))
populated
100-populated
summary(prop_data$STORIES)
sd(prop_data$STORIES,na.rm=TRUE)

prop_data%>%
  #filter(STORIES<100)%>%
  ggplot(aes(x="",y=STORIES))+
  geom_boxplot()

prop_data[!is.na(prop_data$STORIES),]%>%
  
  filter(STORIES<=10)%>%
  #nrow()/numrecords*100
  #filter(!STORIES==0 & !STORIES==100)%>%
  ggplot(aes(STORIES))+
  geom_bar()+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="STORIES", y="Count")+
  scale_x_continuous(breaks=seq(1,11,1))
prop_data%>%
  filter(STORIES<150)%>%
  filter(!STORIES==0)%>%
  ggplot(aes(STORIES))+
  geom_bar()+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="Lot Depth", y="Count")+
  scale_y_continuous(breaks=seq(0,400000,100000))
prop_data%>%
  filter(STORIES==0)%>%
  nrow()


##FULLVAL
prop_data$FULLVAL=as.numeric(prop_data$FULLVAL)
length(unique(prop_data$FULLVAL))
populated = 100*(1-(sum(is.na(prop_data$FULLVAL))/numrecords))
populated
100-populated
summary(prop_data$FULLVAL)
sd(prop_data$FULLVAL)
max(prop_data$FULLVAL)

prop_data%>%
  #filter(FULLVAL<100)%>%
  ggplot(aes(x="",y=FULLVAL))+
  geom_boxplot()


prop_data[!is.na(prop_data$FULLVAL),]%>%
  filter(FULLVAL==0)%>%
  nrow()
  
prop_data[!is.na(prop_data$FULLVAL),]%>%
  filter(FULLVAL<1000000)%>%
  
  
  filter(!FULLVAL==0)%>%
  nrow()/numrecords*100
  ggplot(aes(FULLVAL,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="FULLVAL", y="Count")+
  theme(legend.position = "none")
  #scale_x_continuous(breaks=seq(1,11,1))
prop_data%>%
  filter(FULLVAL<150)%>%
  filter(!FULLVAL==0)%>%
  ggplot(aes(FULLVAL))+
  geom_bar()+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="Lot Depth", y="Count")+
  scale_y_continuous(breaks=seq(0,400000,100000))

#AVLAND
prop_data$AVLAND=as.numeric(prop_data$AVLAND)
length(unique(prop_data$AVLAND))
populated = 100*(1-(sum(is.na(prop_data$AVLAND))/numrecords))
populated
100-populated
summary(prop_data$AVLAND)
sd(prop_data$AVLAND)
max(prop_data$AVLAND)

prop_data%>%
  #filter(AVLAND<100)%>%
  ggplot(aes(x="",y=AVLAND))+
  geom_boxplot()


prop_data[!is.na(prop_data$AVLAND),]%>%
  filter(AVLAND==0)%>%
  nrow()/numrecords*100

prop_data[!is.na(prop_data$AVLAND),]%>%
  filter(AVLAND<100000)%>%
  filter(!AVLAND==0)%>%
  nrow()/numrecords*100
  ggplot(aes(AVLAND,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="AVLAND", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))
prop_data%>%
  filter(AVLAND<150)%>%
  filter(!AVLAND==0)%>%
  ggplot(aes(AVLAND))+
  geom_bar()+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="Lot Depth", y="Count")+
  scale_y_continuous(breaks=seq(0,400000,100000))
#AVTOT
prop_data$AVTOT=as.numeric(prop_data$AVTOT)
length(unique(prop_data$AVTOT))
populated = 100*(1-(sum(is.na(prop_data$AVTOT))/numrecords))
populated
100-populated
summary(prop_data$AVTOT)
sd(prop_data$AVTOT)
max(prop_data$AVTOT)

prop_data%>%
  #filter(AVTOT<100)%>%
  ggplot(aes(x="",y=AVTOT))+
  geom_boxplot()


prop_data[!is.na(prop_data$AVTOT),]%>%
  filter(AVTOT==0)%>%
  nrow()/numrecords*100

prop_data[!is.na(prop_data$AVTOT),]%>%
  filter(AVTOT<100000)%>%
 
  filter(!AVTOT==0)%>%
  nrow()/numrecords*100
  ggplot(aes(AVTOT,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="AVTOT", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))
prop_data%>%
  filter(AVTOT<150)%>%
  filter(!AVTOT==0)%>%
  ggplot(aes(AVTOT))+
  geom_bar()+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="Lot Depth", y="Count")+
  scale_y_continuous(breaks=seq(0,400000,100000))

#EXLAND
prop_data$EXLAND=as.numeric(prop_data$EXLAND)
length(unique(prop_data$EXLAND))
populated = 100*(1-(sum(is.na(prop_data$EXLAND))/numrecords))
populated
100-populated
summary(prop_data$EXLAND)
sd(prop_data$EXLAND)
max(prop_data$EXLAND)

prop_data%>%
  #filter(EXLAND<100)%>%
  ggplot(aes(x="",y=EXLAND))+
  geom_boxplot()


prop_data[!is.na(prop_data$EXLAND),]%>%
  filter(EXLAND==0)%>%
  nrow()

prop_data%>%
  group_by(EXLAND)%>%
  summarize(num=n()/numrecords*100)%>%
  arrange(-num)%>%
  head(10)
prop_data[!is.na(prop_data$EXLAND),]%>%
  filter(EXLAND<10000)%>%
  #filter(!EXLAND==0)%>%
  ggplot(aes(EXLAND,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="EXLAND", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))
prop_data%>%
  filter(EXLAND<150)%>%
  filter(!EXLAND==0)%>%
  ggplot(aes(EXLAND))+
  geom_bar()+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="Lot Depth", y="Count")+
  scale_y_continuous(breaks=seq(0,400000,100000))

#EXTOT
prop_data$EXTOT=as.numeric(prop_data$EXTOT)
length(unique(prop_data$EXTOT))
populated = 100*(1-(sum(is.na(prop_data$EXTOT))/numrecords))
populated
100-populated
summary(prop_data$EXTOT)
sd(prop_data$EXTOT)
max(prop_data$EXTOT)

prop_data%>%
  #filter(EXTOT<100)%>%
  ggplot(aes(x="",y=EXTOT))+
  geom_boxplot()


prop_data[!is.na(prop_data$EXTOT),]%>%
  filter(EXTOT==0)%>%
  nrow()

prop_data[!is.na(prop_data$EXTOT),]%>%
  filter(EXTOT<25000)%>%
  #filter(!EXTOT==0)%>%
  ggplot(aes(EXTOT,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="EXTOT", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))
prop_data%>%
  group_by(EXTOT)%>%
  summarize(num=n()/numrecords*100)%>%
  arrange(-num)%>%
  head(10)

#EXCD1
prop_data$EXCD1=as.numeric(prop_data$EXCD1)
length(unique(prop_data$EXCD1))
populated = 100*(1-(sum(is.na(prop_data$EXCD1))/numrecords))
populated
100-populated
summary(prop_data$EXCD1)
sd(prop_data$EXCD1,na.rm=TRUE)
max(prop_data$EXCD1)

prop_data%>%
  #filter(EXCD1<100)%>%
  ggplot(aes(x="",y=EXCD1))+
  geom_boxplot()


prop_data[!is.na(prop_data$EXCD1),]%>%
  filter(EXCD1==0)%>%
  nrow()

prop_data[!is.na(prop_data$EXCD1),]%>%
  filter(EXCD1<25000)%>%
  filter(!EXCD1==0)%>%
  ggplot(aes(EXCD1,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="EXCD1", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))
prop_data[!is.na(prop_data$EXCD1),]%>%
  group_by(EXCD1)%>%
  summarize(num=n()/numrecords*100)%>%
  arrange(-num)%>%
  head(10)

#STADDR
#prop_data$STADDR=as.numeric(prop_data$STADDR)
length(unique(prop_data$STADDR))
populated = 100*(1-(sum(is.na(prop_data$STADDR))/numrecords))
populated
100-populated
summary(prop_data$STADDR)
sd(prop_data$STADDR,na.rm=TRUE)
max(prop_data$STADDR)

prop_data%>%
  #filter(STADDR<100)%>%
  ggplot(aes(x="",y=STADDR))+
  geom_boxplot()


prop_data[!is.na(prop_data$STADDR),]%>%
  filter(STADDR==0)%>%
  nrow()

prop_data[!is.na(prop_data$STADDR),]%>%
  filter(STADDR<25000)%>%
  filter(!STADDR==0)%>%
  ggplot(aes(STADDR,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="STADDR", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))
prop_data[!is.na(prop_data$STADDR),]%>%
  group_by(STADDR)%>%
  summarize(num=n(),percent=n()/numrecords*100)%>%
  arrange(-num)%>%
  head(25) -> address
write.csv(address,"address.csv")

#XIP
length(unique(prop_data$ZIP))
populated = 100*(1-(sum(is.na(prop_data$ZIP))/numrecords))
populated
100-populated
summary(prop_data$ZIP)
sd(prop_data$ZIP,na.rm=TRUE)
max(prop_data$ZIP)

prop_data%>%
  #filter(ZIP<100)%>%
  ggplot(aes(x="",y=ZIP))+
  geom_boxplot()


prop_data[!is.na(prop_data$ZIP),]%>%
  filter(ZIP==0)%>%
  nrow()

prop_data[!is.na(prop_data$ZIP),]%>%
  filter(ZIP<25000)%>%
  filter(!ZIP==0)%>%
  ggplot(aes(ZIP,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="ZIP", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))
prop_data[!is.na(prop_data$ZIP),]%>%
  group_by(ZIP)%>%
  summarize(num=n(),perent=n()/numrecords*100)%>%
  arrange(-num)%>%
  head(25) -> ZIP
write.csv(ZIP,"ZIP.csv")

#EXMPTCL
length(unique(prop_data$EXMPTCL))
populated = 100*(1-(sum(is.na(prop_data$EXMPTCL))/numrecords))
populated
100-populated
summary(prop_data$EXMPTCL)
sd(prop_data$EXMPTCL,na.rm=TRUE)
max(prop_data$EXMPTCL)

prop_data%>%
  #filter(EXMPTCL<100)%>%
  ggplot(aes(x="",y=EXMPTCL))+
  geom_boxplot()


prop_data[!is.na(prop_data$EXMPTCL),]%>%
  filter(EXMPTCL==0)%>%
  nrow()

prop_data[!is.na(prop_data$EXMPTCL),]%>%
  filter(EXMPTCL<25000)%>%
  filter(!EXMPTCL==0)%>%
  ggplot(aes(EXMPTCL,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="EXMPTCL", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))

##BLDFRONT
prop_data$BLDFRONT=as.numeric(prop_data$BLDFRONT)
length(unique(prop_data$BLDFRONT))
populated = 100*(1-(sum(is.na(prop_data$BLDFRONT))/numrecords))
populated
100-populated
summary(prop_data$BLDFRONT)
sd(prop_data$BLDFRONT,na.rm=TRUE)
max(prop_data$BLDFRONT)

prop_data%>%
  #filter(BLDFRONT<100)%>%
  ggplot(aes(x="",y=BLDFRONT))+
  geom_boxplot()


prop_data[!is.na(prop_data$BLDFRONT),]%>%
  filter(BLDFRONT==0)%>%
  nrow()/numrecords*100

prop_data[!is.na(prop_data$BLDFRONT),]%>%
  filter(BLDFRONT<30)%>%
  #filter(!BLDFRONT==0)%>%
  ggplot(aes(BLDFRONT,color="black"))+
  geom_histogram(bins=100)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="BLDFRONT", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))

prop_data[!is.na(prop_data$BLDFRONT),]%>%
  group_by(BLDFRONT)%>%
  summarize(num=n()/numrecords*100)%>%
  arrange(-num)%>%
  head(10)

#BLDDEPTH
prop_data$BLDDEPTH=as.numeric(prop_data$BLDDEPTH)
length(unique(prop_data$BLDDEPTH))
populated = 100*(1-(sum(is.na(prop_data$BLDDEPTH))/numrecords))
populated
100-populated
summary(prop_data$BLDDEPTH)
sd(prop_data$BLDDEPTH,na.rm=TRUE)
max(prop_data$BLDDEPTH)

prop_data%>%
  #filter(BLDDEPTH<100)%>%
  ggplot(aes(x="",y=BLDDEPTH))+
  geom_boxplot()


prop_data[!is.na(prop_data$BLDDEPTH),]%>%
  filter(BLDDEPTH==0)%>%
  nrow()/numrecords*100

prop_data[!is.na(prop_data$BLDDEPTH),]%>%
  filter(BLDDEPTH<200)%>%
  filter(!BLDDEPTH==0)%>%
  nrow()/numrecords*100
  ggplot(aes(BLDDEPTH,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="BLDDEPTH", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))

prop_data[!is.na(prop_data$BLDDEPTH),]%>%
  group_by(BLDDEPTH)%>%
  summarize(num=n()/numrecords*100)%>%
  arrange(-num)%>%
  head(10)

#AVLAND2
prop_data$AVLAND2=as.numeric(prop_data$AVLAND2)
length(unique(prop_data$AVLAND2))
populated = 100*(1-(sum(is.na(prop_data$AVLAND2))/numrecords))
populated
100-populated
summary(prop_data$AVLAND2)
sd(prop_data$AVLAND2,na.rm=TRUE)
max(prop_data$AVLAND2)

prop_data%>%
  #filter(AVLAND2<100)%>%
  ggplot(aes(x="",y=AVLAND2))+
  geom_boxplot()


prop_data[!is.na(prop_data$AVLAND2),]%>%
  filter(AVLAND2==0)%>%
  nrow()

prop_data[!is.na(prop_data$AVLAND2),]%>%
  filter(AVLAND2<70000)%>%
  nrow()/numrecords*100
  #filter(!AVLAND2==0)%>%
  ggplot(aes(AVLAND2,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="AVLAND2", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))

prop_data[!is.na(prop_data$AVLAND2),]%>%
  group_by(AVLAND2)%>%
  summarize(num=n()/numrecords*100)%>%
  arrange(-num)%>%
  head(10)
#AVTOT2
prop_data$AVTOT2=as.numeric(prop_data$AVTOT2)
length(unique(prop_data$AVTOT2))
populated = 100*(1-(sum(is.na(prop_data$AVTOT2))/numrecords))
populated
100-populated
summary(prop_data$AVTOT2)
sd(prop_data$AVTOT2,na.rm=TRUE)
max(prop_data$AVTOT2)

prop_data%>%
  #filter(AVTOT2<100)%>%
  ggplot(aes(x="",y=AVTOT2))+
  geom_boxplot()


prop_data[!is.na(prop_data$AVTOT2),]%>%
  filter(AVTOT2==0)%>%
  nrow()

prop_data[!is.na(prop_data$AVTOT2),]%>%
  filter(AVTOT2<400000)%>%
  #nrow()/numrecords*100
  #filter(!AVTOT2==0)%>%
  ggplot(aes(AVTOT2,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="AVTOT2", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))

prop_data[!is.na(prop_data$AVTOT2),]%>%
  group_by(AVTOT2)%>%
  summarize(num=n()/numrecords*100)%>%
  arrange(-num)%>%
  head(10)

#EXLAND2
prop_data$EXLAND2=as.numeric(prop_data$EXLAND2)
length(unique(prop_data$EXLAND2))
populated = 100*(1-(sum(is.na(prop_data$EXLAND2))/numrecords))
populated
100-populated

summary(prop_data$EXLAND2)
sd(prop_data$EXLAND2,na.rm=TRUE)
max(prop_data$EXLAND2)

prop_data%>%
  #filter(EXLAND2<100)%>%
  ggplot(aes(x="",y=EXLAND2))+
  geom_boxplot()


prop_data[!is.na(prop_data$EXLAND2),]%>%
  filter(EXLAND2==0)%>%
  nrow()

prop_data[!is.na(prop_data$EXLAND2),]%>%
  filter(EXLAND2<10000)%>%
  filter(!EXLAND2==2090)%>%
  ggplot(aes(EXLAND2,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="EXLAND2", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))

prop_data[!is.na(prop_data$EXLAND2),]%>%
  group_by(EXLAND2)%>%
  summarize(num=n()/numrecords*100)%>%
  arrange(-num)%>%
  head(10)
##EXTOT2
prop_data$EXTOT2=as.numeric(prop_data$EXTOT2)
length(unique(prop_data$EXTOT2))
populated = 100*(1-(sum(is.na(prop_data$EXTOT2))/numrecords))
populated
100-populated

summary(prop_data$EXTOT2)
sd(prop_data$EXTOT2,na.rm=TRUE)
max(prop_data$EXTOT2)

prop_data%>%
  #filter(EXTOT2<100)%>%
  ggplot(aes(x="",y=EXTOT2))+
  geom_boxplot()


prop_data[!is.na(prop_data$EXTOT2),]%>%
  filter(EXTOT2==0)%>%
  nrow()

prop_data[!is.na(prop_data$EXTOT2),]%>%
  filter(EXTOT2<5000)%>%
  #filter(!EXTOT2==2090)%>%
  ggplot(aes(EXTOT2,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="EXTOT2", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))

prop_data[!is.na(prop_data$EXTOT2),]%>%
  group_by(EXTOT2)%>%
  summarize(num=n())%>%
  arrange(-num)%>%
  head(10)

##EXCD2
prop_data$EXCD2=as.numeric(prop_data$EXCD2)
length(unique(prop_data$EXCD2))
populated = 100*(1-(sum(is.na(prop_data$PERIOD))/numrecords))
populated
100-populated

summary(prop_data$EXCD2)
sd(prop_data$EXCD2,na.rm=TRUE)
max(prop_data$EXCD2)

prop_data%>%
  #filter(EXCD2<100)%>%
  ggplot(aes(x="",y=EXCD2))+
  geom_boxplot()


prop_data[!is.na(prop_data$EXCD2),]%>%
  filter(EXCD2==0)%>%
  nrow()

prop_data[!is.na(prop_data$EXCD2),]%>%
  #filter(EXCD2<2000)%>%
  #filter(!EXCD2==2090)%>%
  ggplot(aes(EXCD2,color="black"))+
  geom_histogram(bins=200)+
  #scale_y_continuous(breaks=seq(0,125000,25000))+
  labs(x="EXCD2", y="Count")+
  theme(legend.position = "none")
#scale_x_continuous(breaks=seq(1,11,1))

prop_data[!is.na(prop_data$EXCD2),]%>%
  group_by(EXCD2)%>%
  summarize(num=n()/numrecords*100)%>%
  arrange(-num)%>%
  head(10)
