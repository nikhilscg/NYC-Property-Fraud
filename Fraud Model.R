library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(rJava)
library(xlsx)
library(readxl)
library(sqldf)
library(h2o)
options(scipen=999)


prop_data = read_excel("Final_filled_values.xlsx")

newdata = prop_data

prop_data %>%
  mutate(LotArea = LTFRONT*LTDEPTH)%>%
  mutate(BLDArea = BLDFRONT*BLDDEPTH)%>%
  mutate(BLDVol = BLDArea*STORIES) -> newdata

newdata %>%
  mutate(FullVal1 = FULLVAL/LotArea, FullVal2 = FULLVAL/BLDArea,FullVal3 = FULLVAL/BLDVol) -> newdata

newdata %>%
  mutate(AVLand1 = AVLAND/LotArea, AVLand2 = AVLAND/BLDArea,AVLand3 = AVLAND/BLDVol) -> newdata

newdata %>%
  mutate(AVTot1 = AVTOT/LotArea, AVTot2 = AVTOT/BLDArea,AVTot3 = AVTOT/BLDVol) -> newdata


######## I AM HERE################

###initialize the fields FULLVAL4 etc. till AVTOT6

checkdata=newdata

checkdata$FullVal4 = 0
checkdata$FullVal5 = 0
checkdata$FullVal6 = 0
checkdata$AVLand4 = 0
checkdata$AVLand5 = 0
checkdata$AVLand6 = 0
checkdata$AVTot4 = 0
checkdata$AVTot5 = 0
checkdata$AVTot6 = 0


checkdata$FullVal7 = 0
checkdata$FullVal8 = 0
checkdata$FullVal9 = 0
checkdata$AVLand7 = 0
checkdata$AVLand8 = 0
checkdata$AVLand9 = 0
checkdata$AVTot7 = 0
checkdata$AVTot8 = 0
checkdata$AVTot9 = 0


checkdata$FullVal10 = 0
checkdata$FullVal11= 0
checkdata$FullVal12 = 0
checkdata$AVLand10 = 0
checkdata$AVLand11= 0
checkdata$AVLand12 = 0
checkdata$AVTot10 = 0
checkdata$AVTot11= 0
checkdata$AVTot12 = 0


checkdata$FullVal13 = 0
checkdata$FullVal14= 0
checkdata$FullVal15 = 0
checkdata$AVLand13 = 0
checkdata$AVLand14= 0
checkdata$AVLand15 = 0
checkdata$AVTot13 = 0
checkdata$AVTot14= 0
checkdata$AVTot15 = 0


checkdata$FullVal16 = 0
checkdata$FullVal17 = 0
checkdata$FullVal18 = 0
checkdata$AVLand16 = 0
checkdata$AVLand17 = 0
checkdata$AVLand18 = 0
checkdata$AVTot16 = 0
checkdata$AVTot17 = 0
checkdata$AVTot18 = 0


checkdata$FullVal19 = 0
checkdata$FullVal20 = 0
checkdata$FullVal21 = 0
checkdata$AVLand19 = 0
checkdata$AVLand20 = 0
checkdata$AVLand21 = 0
checkdata$AVTot19 = 0
checkdata$AVTot20 = 0
checkdata$AVTot21 = 0
checkdata$FullVal22 = 0
checkdata$FullVal23 = 0
checkdata$FullVal24 = 0
checkdata$AVLand22 = 0
checkdata$AVLand23 = 0
checkdata$AVLand24 = 0
checkdata$AVTot22 = 0
checkdata$AVTot23 = 0
checkdata$AVTot24 = 0

temp_data=checkdata

for(j in c(1:9)){
  temp_data=temp_data[,c(1:108)]
  print("Hello")
  
  temp=temp_data[,c(19,j+36)]
  
  colnames(temp)=c('ZIP','Value')
  zip_average=temp%>%
    group_by(ZIP)%>%
    summarize(mean=mean(Value))
  temp_data=left_join(temp_data,zip_average,by=c("ZIP"="ZIP"))
  temp_data[,j+45]=temp_data[,j+36]/temp_data[,109]
}

# sum(temp_data$FullVal5)
# sum(combined_data$FULLVAL5)
###BORO
for(j in c(1:9)){
  temp_data=temp_data[,c(1:108)]
  print("Hello")
  
  temp=temp_data[,c(31,j+36)]
  
  colnames(temp)=c('BORO','Value')
  zip_average=temp%>%
    group_by(BORO)%>%
    summarize(mean=mean(Value))
  temp_data=left_join(temp_data,zip_average)
  temp_data[,j+54]=temp_data[,j+36]/temp_data[,109]
}

#sum(is.na(temp_data$FullVal8))

##BLDClass
for(j in c(1:9)){
  temp_data=temp_data[,c(1:108)]
  print("Hello")
  
  temp=temp_data[,c(7,j+36)]
  
  colnames(temp)=c('BLDGCL','Value')
  zip_average=temp%>%
    group_by(BLDGCL)%>%
    summarize(mean=mean(Value))
  temp_data=left_join(temp_data,zip_average)
  temp_data[,j+63]=temp_data[,j+36]/temp_data[,109]
}
sum(is.na(temp_data$AVLand11))
sum(is.na(temp_data$FullVal11))


##by TAXCLASS

for(j in c(1:9)){
  temp_data=temp_data[,c(1:108)]
  print("Hello")
  
  temp=temp_data[,c(8,j+36)]
  
  colnames(temp)=c('TAXCLASS','Value')
  zip_average=temp%>%
    group_by(TAXCLASS)%>%
    summarize(mean=mean(Value))
  temp_data=left_join(temp_data,zip_average)
  temp_data[,j+72]=temp_data[,j+36]/temp_data[,109]
}

sum(is.na(temp_data$AVTot13))
temp_data$AVTot14
##BLOCK
for(j in c(1:9)){
  temp_data=temp_data[,c(1:108)]
  print("Hello")
  
  temp=temp_data[,c(3,j+36)]
  
  colnames(temp)=c('BLOCK','Value')
  zip_average=temp%>%
    group_by(BLOCK)%>%
    summarize(mean=mean(Value))
  temp_data=left_join(temp_data,zip_average)
  temp_data[,j+81]=temp_data[,j+36]/temp_data[,109]
}

sum(is.na(temp_data$AVTot17))
sum(is.na(temp_data$FullVal16))
##ZIP3
for(j in c(1:9)){
  temp_data=temp_data[,c(1:108)]
  print("Hello")
  
  temp=temp_data[,c(33,j+36)]
  
  colnames(temp)=c('ZIP3','Value')
  zip_average=temp%>%
    group_by(ZIP3)%>%
    summarize(mean=mean(Value))
  temp_data=left_join(temp_data,zip_average)
  temp_data[,j+90]=temp_data[,j+36]/temp_data[,109]
}
sum(is.na(temp_data$FullVal20))
temp_data$FullVal20
temp_data$StoriesCategory=as.factor(temp_data$StoriesCategory)
##Stories Category
for(j in c(1:9)){
  temp_data=temp_data[,c(1:108)]
  print("Hello")
  
  temp=temp_data[,c(32,j+36)]
  
  colnames(temp)=c('StoriesCategory','Value')
  zip_average=temp%>%
    group_by(StoriesCategory)%>%
    summarize(mean=mean(Value))
  temp_data=left_join(temp_data,zip_average)
  temp_data[,j+99]=temp_data[,j+36]/temp_data[,109]
}
sum(is.na(temp_data$FullVal23))


temp_data%>%
  select(-mean) -> temp_data
sd(temp_data$FullVal22)
Final_Expert_Variables = temp_data[,c(1,37:63,73:81,91:108)]
summary(Final_Expert_Variables)
##scale the variables
col=colnames(Final_Expert_Variables)[c(2:55)]
col
Final_Expert_Variables[col]=scale(Final_Expert_Variables[col])
sd(Final_Expert_Variables$FullVal22)

##PCA Analysis
prin_comp = prcomp(Final_Expert_Variables[col],scale.=F)

##get the variance explained
var_pca = prin_comp$sdev^2
var_exp = var_pca/sum(var_pca)


for_graph = data.frame(PCnum=c(1:54),var_expl = var_exp)
##add column with cumulative sums
for_graph$cumvar_exp = 0
for(i in c(1:54)){
  if (i==1){
    for_graph$cumvar_exp[i]=for_graph$var_expl[i]
  }
  else {
    for_graph$cumvar_exp[i]=for_graph$cumvar_exp[i-1]+for_graph$var_expl[i]
  }
  
}
ggplot(for_graph[1:15,],aes(x=PCnum,y=var_expl))+
  geom_bar(stat="identity")

ggplot(for_graph[1:15,],aes(x=PCnum,y=cumvar_exp))+
  geom_bar(stat="identity")

###so with 10 PCAs we are able to explain almost 97% 

##use the first 10 PCAs only

pc_matrix = data.frame(prin_comp$x)

##we require only the first 10 PCAs
for_scoring = pc_matrix[,1:10]
for_scoring = cbind(for_scoring,Final_Expert_Variables$RECORD)
colnames(for_scoring)[11]='RECORD'

###z scale the PCs again
col=colnames(for_scoring)[1:10]
for_scoring[col]=scale(for_scoring[col])
summary(for_scoring)

##check for fraud
for_scoring$score1 = abs(for_scoring$PC1)+abs(for_scoring$PC2)+abs(for_scoring$PC3)+abs(for_scoring$PC4)
+abs(for_scoring$PC5)+abs(for_scoring$PC6)+abs(for_scoring$PC7)+abs(for_scoring$PC8)+abs(for_scoring$PC9)
+abs(for_scoring$PC10)
for_scoring$score2 = (abs(for_scoring$PC1)^2+abs(for_scoring$PC2)^2+abs(for_scoring$PC3)^2+abs(for_scoring$PC4)^2
                      +abs(for_scoring$PC5)^2+abs(for_scoring$PC6)^2+abs(for_scoring$PC7)^2+abs(for_scoring$PC8)^2+abs(for_scoring$PC9)^2
                      +abs(for_scoring$PC10)^2)^0.5

for_scoring%>%
  arrange(-score1)%>%
  select(RECORD,score1) -> score1_data
for_scoring%>%
  arrange(-score2)%>%
  select(RECORD,score2)  -> score2_data


score1_data%>%
  filter(score1<5)%>%
  ggplot(aes(x=score1))+
  geom_histogram(bins = 100)
##bin my scoring data into quintiles...let us have 50 bins
score2_data$bin_num=0
score1_data$bin_num=0


##number of bins
bin_number = 1000

remainder=nrow(score2_data)%%bin_number
interval=nrow(score2_data)-remainder
iter=interval/bin_number

for(i in c(1:bin_number)){
  beg = (i-1)*iter+1
  end=i*iter
  score2_data$bin_num[beg:end]=bin_number+1-i
}
score2_data%>%
  filter(bin_num==0)%>%
  nrow()
score2_data%>%
  mutate(bin_num=if_else(bin_num==0,1,bin_num)) ->score2_data

###Score1
for(i in c(1:bin_number)){
  beg = (i-1)*iter+1
  end=i*iter
  score1_data$bin_num[beg:end]=bin_number+1-i
}
score1_data%>%
  filter(bin_num==0)%>%
  nrow()
score1_data%>%
  mutate(bin_num=if_else(bin_num==0,1,bin_num)) ->score1_data

score1_records = left_join(prop_data,score1_data)
score2_records = left_join(prop_data,score2_data)


score1_records%>%
  arrange(-score1)%>%
  head(20) -> top20_score1

score2_records%>%
  arrange(-score2)%>%
  head(20) -> top20_score2

write.csv(top20_score1,"top20_Manhattan.csv")
write.csv(top20_score2,"top20_Euclidean.csv")

###write out the datasets for the autoencoder
write.csv(for_scoring,"for_autoencoder.csv")
##write out the dataset for the 2 scores
write.csv(score1_records,"records_manhattan.csv")
write.csv(score2_records,"records_euclidean.csv")


###just checking the data

###score2_data is binned
###similarly score1_data can be binned
###add back the score2_data to the original dataset
fraud=prop_data

fraud=left_join(fraud,score2_data[,c('RECORD','score2','bin_num')])


######Build the Auto-Encoder######

auto_scoring = for_scoring%>%
  select(-RECORD,-score1,-score2)

##auto_scoring=Final_Expert_Variables%>%
# select(-RECORD)
###use the H2O package
local_h20=h2o.init()


###########I am here###############
###try out the blog video on H2o
prospath=system.file("extdata","prostate.csv",package="h2o")
prostate_df = read.csv(prospath)
prostate_df=prostate_df[,-1]
summary(prostate_df)

set.seed(1234)
random_splits <- runif(nrow(prostate_df))
train_df <- prostate_df[random_splits < .5,]
dim(train_df)

validate_df = prostate_df[random_splits>=0.5,]
dim(validate_df)

###run a random forest model

library(randomForest)
outcome_name = 'CAPSULE'
feature_names = setdiff(names(prostate_df),outcome_name)
set.seed(1234)

rf_model=randomForest(x=train_df[,feature_names],y=as.factor(train_df[,outcome_name]),
                      importance=TRUE, ntree=20, mtry=3)
validate_predictions = predict(rf_model,newdata=validate_df[,feature_names],type="prob")

library(pROC)
auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions[,2])
plot(auc_rf, print.thres = "best", main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')


feature_names
prostate.hex = as.h2o(train_df,destination_frame = "train.hex")
prostate.dl = h2o.deeplearning(x=feature_names, training_frame = prostate.hex,autoencoder = TRUE, reproducible=TRUE,
                               seed=1234,hidden=c(6,5,6), epochs = 50)

prostate.anamoly = h2o.anomaly(prostate.dl,prostate.hex,per_feature = FALSE)
head(prostate.anamoly)
error = as.data.frame(prostate.anamoly)
plot(sort(error$Reconstruction.MSE),main="Reconstruction Error")






##predictions
pred = h2o.predict(prostate.dl,train.df)
pred


list = c(2,5,3,4,10,32,333,20000,2,5,4,6,7,8,10,43,56,432,34,3,4,6,7,3,45)
list1=scale(list)
sd(list)
mean(list)
sd(list1)
mean(list1)
summary(list1)
###From here to do
summary(auto_scoring)
feature_NY = names(auto_scoring)
train_NY=as.h2o(auto_scoring, destination_frame = "train_NY")
NY_auto = h2o.deeplearning(x=feature_NY,training_frame = train_NY,
                           autoencoder = TRUE,
                           reproducible = TRUE,
                           seed=1234,
                           activation = "Tanh",
                           hidden = c(7,5,7),
                           epochs = 100)

?h2o.deeplearning
NY_anamoly = h2o.anomaly(NY_auto,train_NY,per_feature = FALSE)
error=as.data.frame(NY_anamoly)
head(error)
error$RECORD=prop_data$RECORD
error%>%
  arrange(-Reconstruction.MSE)%>%
  head(10)
plot(sort(error$Reconstruction.MSE))
error%>%
  arrange(-Reconstruction.MSE)%>%
  filter(Reconstruction.MSE>0.001) -> check_nikhil
head(12)



pred=h2o.predict(NY_auto,train_NY)
head(pred)
NY_pred = as.data.frame(pred)
###just checking
head(auto_scoring)
###take the difference of the 2 dataframes
NY_diff=auto_scoring-NY_pred
#NY_diff_sq=NY_diff*NY_diff
for(i in 1:ncol(NY_diff)){
  print(i)
  NY_diff_sq[,i]=NY_diff[,i]^2
}
NY_diff$scoreMan = abs(NY_diff$PC1)+abs(NY_diff$PC2)+abs(NY_diff$PC3)+abs(NY_diff$PC4)+
  abs(NY_diff$PC5)+abs(NY_diff$PC6)+abs(NY_diff$PC7)+abs(NY_diff$PC8)+abs(NY_diff$PC9)+abs(NY_diff$PC10)

NY_diff_sq$scoreEuc = (NY_diff_sq$PC1+NY_diff_sq$PC2+NY_diff_sq$PC3+NY_diff_sq$PC4+NY_diff_sq$PC5+NY_diff_sq$PC6+NY_diff_sq$PC7+NY_diff_sq$PC8+NY_diff_sq$PC9+NY_diff_sq$PC10)^0.5

NY_auto_scorer = NY_diff

NY_auto_scorer$RECORD = prop_data$RECORD

NY_auto_scorer%>%
  select(RECORD,scoreMan) -> NY_auto_scorer
NY_auto_scorer$scoreEuc = NY_diff_sq$scoreEuc

NY_auto_scorer%>%
  arrange(-scoreMan) %>%
  head(20) %>%
  write.csv("top20_autoencoder_manhattanScore.csv")

NY_auto_scorer%>%
  arrange(-scoreEuc)%>%
  head(20) %>%
  write.csv("top20_autoencoder_euclideanScore.csv")



##write out the larger files also
NY_auto_scorer%>%
  arrange(-scoreMan) %>%
  write.csv("AllRecords_Manhattan_Sorted.csv")

NY_auto_scorer%>%
  arrange(-scoreEuc)%>%
  write.csv("AllRecords_Euclidean_Sorted.csv")


h2o.shutdown()
Y

# pred = h2o.predict(NY_auto,train_auto_scoring)
# 
# pred
# pred.df=as.data.frame(pred)
# summary(pred.df)
# 
# pred.df$pred_score = pred.df$reconstr_PC1+pred.df$reconstr_PC2+pred.df$reconstr_PC3+pred.df$reconstr_PC4+
#   pred.df$reconstr_PC5+pred.df$reconstr_PC6+pred.df$reconstr_PC7
# +pred.df$reconstr_PC8+pred.df$reconstr_PC9+pred.df$reconstr_PC10
# 
# auto_scoring$or_score = auto_scoring$PC1+auto_scoring$PC2+auto_scoring$PC3+auto_scoring$PC4+auto_scoring$PC5+
#   auto_scoring$PC6+auto_scoring$PC7+auto_scoring$PC8+auto_scoring$PC9+auto_scoring$PC10
# 
# pred.df$or_score=auto_scoring$or_score
# pred.df$diff = pred.df$pred_score-pred.df$or_score


total_var = sum(prin_comp$sdev)
prin_comp$rotation[1:4,1:5]
dim(prin_comp$x)
biplot(prin_comp,scale=0)

