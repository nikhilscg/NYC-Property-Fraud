library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(rJava)
library(xlsx)
library(readxl)
library(sqldf)
options(scipen=999)

#NYdata = read.csv("NY property 1 million.csv",quote="")
#NYdata = read_excel("NY property 1 million.xlsx",sheetName="avroll")
##Read the dataset
NYdata = read_excel("NY property 1 million.xlsx",sheet = 1)
prop_data=NYdata
prop_data%>%
  mutate(BORO = substr(BBLE,0,1)) ->prop_data
##convert the data types of various coumns for analysis
prop_data$BORO = as.factor(prop_data$BORO)
prop_data$STORIES=as.numeric(prop_data$STORIES)
prop_data$BLDGCL=as.factor(prop_data$BLDGCL)
prop_data$TAXCLASS=as.factor(prop_data$TAXCLASS)
prop_data$BBLE=as.factor(prop_data$BBLE)
prop_data$LTFRONT=as.numeric(prop_data$LTFRONT)
prop_data$LTDEPTH=as.numeric(prop_data$LTDEPTH)
prop_data$BLDDEPTH=as.numeric(prop_data$BLDDEPTH)
prop_data$BLDFRONT=as.numeric(prop_data$BLDFRONT)
prop_data$ZIP=as.factor(prop_data$ZIP)


prop_data%>%
  group_by(TAXCLASS)%>%
  summarize(avg_stories=mean(STORIES,na.rm=T)) -> avg_stories






temp=prop_data

#sqldf("UPDATE prop_data SET STORIES = (SELECT avg_stories FROM avg_stories WHERE avg_stories.BORO=prop_data.BORO 
      #AND avg_stories.BLOCK=prop_data.BLOCK) WHERE STORIES = 'NA'")


prop_data%>%
  filter(is.na(STORIES)) %>%
  select(c(1,8,11)) -> NA_stories

NA_stories = left_join(NA_stories,avg_stories)
NA_stories=NA_stories[,-c(2,3)]


prop_data=temp



prop_data = left_join(prop_data,NA_stories,by=c("RECORD"="RECORD"))

prop_data%>%
  mutate(STORIES = if_else(is.na(STORIES), round(avg_stories,2), STORIES)) -> prop_data

prop_data%>%
  filter(is.na(STORIES))

######STORIES filled by avg by TAXCLASS#######

prop_data%>%
  group_by(BLOCK,BORO,ZIP)%>%
  summarize(zip = n()) %>%
  group_by(BLOCK,BORO) %>%
  summarize(max(zip))

###for ZIP Codes

temp_zip=prop_data[,c(3,4,19,31)]
##remove NAs
temp_zip%>%
  filter(!is.na(ZIP)) ->temp_zip

temp_zip=temp_zip[!duplicated(temp_zip[c("BORO", "BLOCK","LOT")]),]




for_zip=prop_data%>%
  filter(is.na(ZIP))

for_zip=for_zip[,c(1,3,4,19,31)]

for_zip=left_join(for_zip,temp_zip,by=c("BORO"="BORO","BLOCK"="BLOCK","LOT"="LOT"))

##join this back to prop data
temp=prop_data
for_join=for_zip[,c(1,6)]
prop_data=left_join(prop_data,for_join,by=c("RECORD"="RECORD"))

prop_data%>%
  mutate(ZIP=if_else(is.na(ZIP),ZIP.y,ZIP)) ->prop_data

prop_data%>%
  filter(is.na(ZIP))

for_zip%>%
  filter(is.na(ZIP.y))
##24216


prop_data=prop_data[,-c(32,33)]

###doing it for BORO and BLOCK

temp_zip=prop_data[,c(3,19,31)]

##remove NAs
temp_zip%>%
  filter(!is.na(ZIP)) ->temp_zip

temp_zip=temp_zip[!duplicated(temp_zip[c("BORO", "BLOCK")]),]




for_zip=prop_data%>%
  filter(is.na(ZIP))

for_zip=for_zip[,c(1,3,19,31)]

for_zip=left_join(for_zip,temp_zip,by=c("BORO"="BORO","BLOCK"="BLOCK"))

##join this back to prop data
temp=prop_data
for_join=for_zip[,c(1,5)]
prop_data=left_join(prop_data,for_join,by=c("RECORD"="RECORD"))

prop_data%>%
  mutate(ZIP=if_else(is.na(ZIP),ZIP.y,ZIP)) ->prop_data

prop_data%>%
  filter(is.na(ZIP))


###now we move one level up to BORO and repeat
prop_data=prop_data[,-32]


temp_zip=prop_data[,c(19,31)]
##remove NAs
temp_zip%>%
  filter(!is.na(ZIP)) ->temp_zip

temp_zip=temp_zip[!duplicated(temp_zip[c("BORO")]),]




for_zip=prop_data%>%
  filter(is.na(ZIP))

for_zip=for_zip[,c(1,19,31)]

for_zip=left_join(for_zip,temp_zip,by=c("BORO"="BORO"))

##join this back to prop data
temp=prop_data
for_join=for_zip[,c(1,4)]
prop_data=left_join(prop_data,for_join,by=c("RECORD"="RECORD"))

prop_data%>%
  mutate(ZIP=if_else(is.na(ZIP),ZIP.y,ZIP)) ->prop_data

prop_data%>%
  filter(is.na(ZIP))
prop_data=prop_data[,c(-32)]
##write out to CSV
write.csv(prop_data,"cleaned_NYdata.csv")

readdata = read_excel("cleaned_NYdata.xlsx",sheet = 1)
readdata%>%
  select(-X__1)->readdata
prop_data = readdata

##cleaning out the LOTFRONT,LOTDEPTH
##we first take the average values for both as per each building class
##then we add repalce the zeros with these average values

prop_data%>%
  filter(!(LTFRONT==0))%>%
  group_by(BLDGCL)%>%
  summarise(avg.lot.front=mean(LTFRONT)) -> lot.front

prop_data%>%
  filter(!(LTDEPTH==0))%>%
  group_by(BLDGCL)%>%
  summarise(avg.lot.depth=mean(LTDEPTH)) -> lot.depth

prop_data%>%
  filter(!(BLDFRONT==0))%>%
  group_by(BLDGCL)%>%
  summarise(avg.bld.front=mean(BLDFRONT)) -> bld.front

##find out the average building front by tax class
prop_data%>%
  filter(!(BLDFRONT==0))%>%
  group_by(TAXCLASS)%>%
  summarize(avg.bld.front=mean(BLDFRONT))->avg.bld.front

prop_data%>%
  filter(!(BLDDEPTH==0))%>%
  group_by(BLDGCL)%>%
  summarise(avg.bld.depth=mean(BLDDEPTH)) -> bld.depth

##find out the average building depth by tax class
prop_data%>%
  filter(!(BLDDEPTH==0))%>%
  group_by(TAXCLASS)%>%
  summarize(avg.bld.depth=mean(BLDDEPTH))->avg.bld.depth
############LOT FRONT###############

###now take out those records where ltfront =0
prop_data%>%
  filter(LTFRONT==0) -> temp.lot

temp.lot=temp.lot%>%
  select(RECORD,BLDGCL)

##now join the lot.front data to this as per the building class
temp.lot=left_join(temp.lot,lot.front)
temp.lot=temp.lot%>%
  select(RECORD,avg.lot.front)

##join this into prop_data
temp=prop_data
prop_data=left_join(prop_data,temp.lot)  

prop_data%>%
  mutate(LTFRONT=if_else(LTFRONT==0,avg.lot.front,LTFRONT))->prop_data
prop_data%>%
  select(-avg.lot.front) -> prop_data

############LOT DEPTH###############

###now take out those records where ltdepth =0
prop_data%>%
  filter(LTDEPTH==0) -> temp.lot

temp.lot%>%
  select(RECORD,BLDGCL) ->temp.lot

##now join the lot.depth data to this as per the building class
temp.lot=left_join(temp.lot,lot.depth)
temp.lot%>%
  select(RECORD,avg.lot.depth) ->temp.lot

##join this into prop_data
temp=prop_data

prop_data=left_join(prop_data,temp.lot)  

prop_data%>%
  mutate(LTDEPTH=if_else(LTDEPTH==0,avg.lot.depth,LTDEPTH))->prop_data
prop_data%>%
  select(-avg.lot.depth) -> prop_data


############BUILDING Front###############

###now take out those records where bldfront =0
prop_data%>%
  filter(BLDFRONT==0) -> temp.bld

temp.bld%>%
  select(RECORD,BLDGCL) ->temp.bld

##now join the bld.front data to this as per the building class
temp.bld=left_join(temp.bld,bld.front)
temp.bld%>%
  select(RECORD,avg.bld.front) ->temp.bld

##join this into prop_data
temp=prop_data
prop_data=left_join(prop_data,temp.bld)  

#prop_data%>%
  #filter(is.na(BLDFRONT))

prop_data%>%
  mutate(BLDFRONT=if_else(BLDFRONT==0,avg.bld.front,BLDFRONT))->prop_data
prop_data%>%
  select(-avg.bld.front) -> prop_data


##there are some building classes for which we do not have any average value of building front 
#so we will use TAXCLASS for them

##just check which are the records with 0 building front

prop_data%>%
  filter(is.na(BLDFRONT))%>%
  group_by(BLDGCL)%>%
  summarise(n())
##these records mostly belong to utility companies or Govt. properties

##extract those records where the building front is NA
##use the average building front by tax class and combine that to this dataset
##combine this new dataset into the original dataset
prop_data%>%
  filter(is.na(BLDFRONT))->temp.bld

temp.bld%>%
  select(RECORD,TAXCLASS) -> temp.bld


##add the average BLDfront to this
temp.bld=left_join(temp.bld,avg.bld.front)
temp.bld=temp.bld%>%
  select(RECORD,avg.bld.front)
##combine this back to the original dataset
temp=prop_data

prop_data = left_join(prop_data,temp.bld)

##update the building front column
prop_data%>%
  mutate(BLDFRONT=if_else(is.na(BLDFRONT),avg.bld.front,BLDFRONT))->prop_data

prop_data%>%
  select(-avg.bld.front)->prop_data



#######Building Depth#########

prop_data%>%
  filter(BLDDEPTH==0) -> temp.bld

temp.bld%>%
  select(RECORD,BLDGCL) ->temp.bld

##now join the bld.depth data to this as per the building class
temp.bld=left_join(temp.bld,bld.depth)
temp.bld%>%
  select(RECORD,avg.bld.depth) ->temp.bld

##join this into prop_data
temp=prop_data
prop_data=left_join(prop_data,temp.bld)  


prop_data%>%
  mutate(BLDDEPTH=if_else(BLDDEPTH==0,avg.bld.depth,BLDDEPTH))->prop_data

prop_data%>%
  select(-avg.bld.depth) -> prop_data

##prop_data%>%
#  filter(is.na(BLDDEPTH))

##there are some NAs for which we can find the bldclass
prop_data%>%
  filter(is.na(BLDDEPTH))%>%
  group_by(BLDGCL)%>%
  summarise(n())

###now we fill in the values for those remaining BLDDepths using taxclass, again they are all govt. utilities or property

prop_data%>%
  filter(is.na(BLDDEPTH))->temp.bld

temp.bld%>%
  select(RECORD,TAXCLASS) -> temp.bld

##add the average BLDdepth to temp.bld
temp.bld=left_join(temp.bld,avg.bld.depth)
temp.bld=temp.bld%>%
  select(RECORD,avg.bld.depth)
##combine this back to the original dataset
temp=prop_data

prop_data = left_join(prop_data,temp.bld)

##update the building depth column
prop_data%>%
  mutate(BLDDEPTH=if_else(is.na(BLDDEPTH),avg.bld.depth,BLDDEPTH))->prop_data

prop_data%>%
  select(-avg.bld.depth)->prop_data


##write out this dataset
#write.xlsx(prop_data,"cleaned_front_depth.xlsx",sheetName = "Sheet1")
write.csv(prop_data,"cleaned_front_depth.csv")

###add the new categorical cl=olumn for stories and zip3
prop_data = read_excel("Final/ny_property_cleaned_data.xlsx",sheet = 1)
stories = read_excel("Final/STORIESCATEGORY.xlsx",sheet = 1)
stories%>%
  select(RECORD,StoriesCategory) ->stories
prop_data = left_join(prop_data,stories)
prop_data$ZIP3 = substr(prop_data$ZIP,1,3)
prop_data$ZIP3=as.factor(prop_data$ZIP3)
write.csv(prop_data,"Final_filled_values.csv")
prop_data= read_excel("Final_filled_values.xlsx",sheet = 1)
prop_data%>%
  filter(BLDFRONT==0)
