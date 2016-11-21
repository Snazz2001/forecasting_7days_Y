Preprocessing the data to get the Y compactment info
```{r}
library(readr)
require(ggplot2)
require(dplyr)
require(gbm)
library(lubridate)
library(forecast)
library(xts)
library(dummies)
library(parallel)
library(foreach)
library(doSNOW)

setwd("/Users/poorlyeric/data/Emirates/")

genCalFeat <- function(len=3003){
  library(lubridate)
  fltdays <- seq(as.Date("2009-01-01"),by=1,length.out=len)
  calfeat <- as.data.frame(fltdays)
  colnames(calfeat)[1] <- "date"
  calfeat$wday <- wday(calfeat$date)
  calfeat$month <- lubridate::month(calfeat$date)
  calfeat$monthday <- lubridate::mday(calfeat$date)
  #  calfeat$bookedlag28 <- dplyr::lag(calfeat$booked,28)
  #  calfeat$bookedlag35 <- dplyr::lag(calfeat$booked,35)
  calfeat$wday <- as.factor(calfeat$wday)
  calfeat$month <- as.factor(calfeat$month)
  calfeat$monthday <- as.factor(calfeat$monthday)
  calfeat$year <- lubridate::year(calfeat$date)
  weekind <- rep(1:(len/7),each=7)#to generate week index for each day
  calfeat$weekind <- weekind[1:len]
  calfeat <- calfeat %>% group_by(year,month) %>% mutate(month_len=n(),wdayindex=rep(1:5,each=7)[1:month_len])
  return(calfeat)
}

cal_feat <- genCalFeat()

gbmmodel <- function(flt1df2, test_size=30){
  require(gbm)
  data <- flt1df2[1:(nrow(flt1df2)-test_size),]
  data <- data[complete.cases(data),]
  set.seed(2015)
  # feats <- c("DCP8","DCP1","bookedlag28","wday2","wday3","wday4","wday5","wday6","wday7","month2","month3","month4","month5
  #               ","month6","month7","month8","month9","month10","month11","month12","wdayindex","D8_D28Now","D8_D6","D8_D6_mom","booked_28days_pred","deviation")
  feats <- c("DCP16","DCP1","bookedlag28","wday2","wday3","wday4","wday5","wday6","wday7","month2","month3","month4","month5","month6","month7","month8","month9","month10","month11","month12","wdayindex","D16_D23Now","D16_D9","D16_D9_mom","booked_7days_pred","bookedlag364","bookedlag7","bookedlag14","bookedlag21","deviation")
  feats <- intersect(colnames(data),feats)
  fitgbm <- gbm(as.formula(paste("booked~",paste(feats,collapse="+"))),data=data,distribution = 'gaussian',n.trees = 500,interaction.depth=2, shrinkage = 0.05)
  
  
  #  fitgbm <- gbm(booked~DCP8+DCP1+bookedlag28+wday2+wday3+wday4+wday5+wday6+wday7+month2+month3+month4+month5
  #               +month6+month7+month8+month9+month10+month11+month12+wdayindex+D8_D28Now+D8_D6+D8_D6_mom
  #              +booked_28days_pred+deviation,data=data,distribution = 'gaussian',n.trees = 500,interaction.depth=2, shrinkage = 0.05)
  return(fitgbm)
}

gbmmodel_month <- function(flt1df2, m){
  require(gbm)
  if(!m %in% flt1df2$month){
    return(-1)
  }
  test_start_year <- max(flt1df2$year[which(flt1df2$month==m)])
  test_month <- as.Date(paste(test_start_year,m,'1',sep = "-"))
  data <- flt1df2[which(flt1df2$FLTDATE<test_month),]
  train_month <- test_month - lubridate::years(2)
  if(min(flt1df2$FLTDATE<train_month)){
    data <- flt1df2[which(flt1df2$FLTDATE>=train_month),]
  }
  data <- data[complete.cases(data),]
  if(nrow(data)<120){
    return(-1)
  }
  set.seed(2015)
  # feats <- c("DCP8","DCP1","bookedlag28","wday2","wday3","wday4","wday5","wday6","wday7","month2","month3","month4","month5
  #               ","month6","month7","month8","month9","month10","month11","month12","wdayindex","D8_D28Now","D8_D6","D8_D6_mom","booked_28days_pred","deviation")
  feats <- c("DCP16","DCP1","bookedlag28","wday2","wday3","wday4","wday5","wday6","wday7","month2","month3","month4","month5","month6","month7","month8","month9","month10","month11","month12","wdayindex","D16_D23Now","D16_D9","D16_D9_mom","booked_7days_pred","bookedlag364","bookedlag7","bookedlag14","bookedlag21","deviation")
  feats <- intersect(colnames(data),feats)
  fitgbm <- gbm(as.formula(paste("BOOKING~",paste(feats,collapse="+"))),data=data,distribution = 'gaussian',n.trees = 500,interaction.depth=2, shrinkage = 0.05)
  
  return(fitgbm)
}

gbmpred_month <- function(flt1df2,m,fitgbm,flt_key){
  test_start_year <- max(flt1df2$year[which(flt1df2$month==m)])
  test_month <- as.Date(paste(test_start_year,m,'1',sep = "-"))
  data <- flt1df2 %>% filter(year==test_start_year,month==m)
  
  pred <- predict(fitgbm,newdata=data,n.trees=500,type="response")
  actual_pred <- data.frame(actual=data$BOOKING,date=data$FLTDATE,pred=round(pred))
  relerr <- with(actual_pred,mean((abs(actual-pred)/actual)))
  relerrsd <- with(actual_pred,sd((abs(actual-pred)/actual)))
  relerrmedian <- with(actual_pred,median((abs(actual-pred)/actual)))
  lro <- LRO %>% filter(FLT_key==flt_key)
  lro$date <- as.Date(lro$date)
  lro$DCP23 <- as.numeric(lro$DCP23)
  actual_pred <- actual_pred %>% left_join(lro) %>% mutate(syserr = abs(DCP16-actual)/actual)
  sys_err <- with(actual_pred,median(syserr,na.rm=TRUE))
  results <- list(relerr,actual_pred,relerrsd,relerrmedian,sys_err)
  return(results)
}

gbmpred <- function(flt1df2,test_size=30,fitgbm,flt_key){
  pred <- predict(fitgbm,newdata=flt1df2[(nrow(flt1df2)-test_size+1):nrow(flt1df2),],n.trees=500,type="response")
  actual_pred <- data.frame(actual=flt1df2$booked[(nrow(flt1df2)-test_size+1):nrow(flt1df2)],date=flt1df2$date[(nrow(flt1df2)-30+1):nrow(flt1df2)],pred=round(pred))
  relerr <- with(actual_pred,mean((abs(actual-pred)/actual)))
  relerrsd <- with(actual_pred,sd((abs(actual-pred)/actual)))
  relerrmedian <- with(actual_pred,median((abs(actual-pred)/actual)))
  lro <- LRO %>% filter(FLT_KEY==flt_key)
  lro$DCP23 <- as.numeric(lro$DCP23)
  actual_pred <- actual_pred %>% left_join(lro) %>% mutate(syserr = abs(DCP16-actual)/actual)
  sys_err <- with(actual_pred,median(syserr,na.rm=TRUE))
  results <- list(relerr,actual_pred,relerrsd,relerrmedian,sys_err)
  return(results)
}

cnames <- c("flt_no","ORGN","DSTN",'dep_date','cls_master_comp','rec_type',"DCP1","DCP2","DCP3","DCP4","DCP5","DCP6","DCP7","DCP8","DCP9","DCP10","DCP11","DCP12","DCP13","DCP14","DCP15","DCP16","DCP17","DCP18","DCP19","DCP20","DCP21","DCP22","DCP23")
data <- read_tsv("data/prms1000.csv",col_names = FALSE)
colnames(data) <- cnames
head(data)
data <- as.data.frame(data)
table(data$COMPARTMENT)
LRO <- data[data$rec_type=='LRO',]
LRO$flt_no <- as.numeric(LRO$flt_no)
LRO$DCP23 <- as.numeric(LRO$DCP23)
LRO <- LRO %>% mutate(FLT_key=paste(flt_no,ORGN,DSTN,sep='-')) %>% select(FLT_key,dep_date,DCP9,DCP16,DCP23)
head(LRO)
LRO <- LRO %>% rename(date=dep_date)
data <- data[data$rec_type=='LSS',]
head(data)
colnames(data)[length(data)] <- "BOOKING"
#derive the data
#data <- data[,c("FLTDATE","FLTNUM","ORGN","DSTN","DCP","DYPR","ADJCAP","BOOKING")]
#derive the capacity
#data <- data[,c("FLTDATE","FLTNUM","ORGN","DSTN","ADJCAP","DCP","BOOKING")]
colnames(data)[4] <- "FLTDATE"
colnames(data)[1] <- "FLTNUM"
data$FLTDATE <- as.Date(data$FLTDATE)
data$FLTNUM <- as.numeric(data$FLTNUM)
data <- data %>% filter(FLTNUM<1000)
head(data)
data <- data %>% mutate(FLT_key=paste(FLTNUM,ORGN,DSTN,sep='-')) 
#data <- data %>% mutate(LOADFACTOR=BOOKING/ADJCAP)
#data <- data %>% select(FLTDATE,FLT_key,DCP,LOADFACTOR) %>% arrange(FLT_key,FLTDATE)
#write.csv(data,"flth_load_factor.csv",row.names = FALSE)
#write.csv(data,"flt_cap.csv",row.names = FALSE)
###
#data_unique <- data[!duplicated(data),]
#data_unique$FLTNUM <- as.numeric(data_unique$FLTNUM)
#data_unique <- data_unique %>% filter(FLTNUM<1000)
#data_unique <- data_unique %>% mutate(FLT_key=paste(FLTNUM,ORGN,DSTN,sep='-')) 
#data_unique <- data_unique %>% arrange(FLT_key)
data %>% group_by(FLT_key) %>% summarize(occ_num = n()) %>% ggplot(aes(occ_num))+geom_histogram()
object.size(data)
#write.csv(data,'flthfull_Y_COMP.csv',row.names = FALSE)
flth <- data
```

Processing the data set, clean the data for passage number.
```{r}
#data <- read.csv("flthfull_Y_COMP.csv",header = TRUE,stringsAsFactors = FALSE)
# library(tidyr)
# ?spread
# data %>% filter(FLTNUM<1000) %>% mutate(FLT_key=paste(FLTNUM,ORGN,DSTN,sep='-')) %>% head()
# data <- data %>% filter(FLTNUM<1000) %>% mutate(FLT_key=paste(FLTNUM,ORGN,DSTN,sep='-'))
# data <- data %>% select(FLT_key,FLTDATE,DCP,BOOKING)
# as.Date(data$FLTDATE[1],"%d-%b-%y")
# data$FLTDATE <- as.Date(data$FLTDATE,"%d-%b-%y")
# data <- data  %>% arrange(FLT_key,FLTDATE,DCP) %>% spread(DCP,BOOKING)
# colnames(data)[3:21] <- paste0("DCP",1:19,sep="")
# head(data)  
# write.csv(data,'flthfull_Y_COMP_formatted.csv',row.names = FALSE)
```

Processing the data set, clean the data for passage number.
```{r}
#data <- read.csv("flth_load_factor.csv",header = TRUE,stringsAsFactors = FALSE)
# library(tidyr)
# ?spread
# data <- data %>% select(FLT_key,FLTDATE,DCP,LOADFACTOR)
# as.Date(data$FLTDATE[1],"%d-%b-%y")
# #data$FLTDATE <- as.Date(data$FLTDATE,"%d-%b-%y")
# data <- data  %>% arrange(FLT_key,FLTDATE,DCP) %>% spread(DCP,LOADFACTOR)
# colnames(data)[3:21] <- paste0("DCP",1:19,sep="")
# head(data)  
# write.csv(data,'flthfull_Y_COMP_LF_formatted.csv',row.names = FALSE)
```

To train the model and make prediction
```{r}
#data <- read.csv("flthfull_Y_COMP_LF_formatted.csv",stringsAsFactors = FALSE)
#for(i in 3:21){
#  data[,i] <- round(data[,i]*1000)
#}
results33lf <- data.frame(FLT_KEY=unique(flth$FLT_key), RelErr=rep(NA,length(unique(flth$FLT_key))),
                          RELErrSd=rep(NA,length(unique(flth$FLT_key))),
                          RELErrMedian=rep(NA,length(unique(flth$FLT_key))),
                          trainSize = rep(NA,length(unique(flth$FLT_key))),
                          SYSErrMedian=rep(NA,length(unique(flth$FLT_key))))
outputdata7 <- data.frame(actual=NA,date=NA,pred=0,FLT_key=NA,DCP9=0,DCP16=0,DCP23=0,syserr=0)

source('/Users/poorlyeric/code/8785OS_Code/Chap 8/Data Files/calendarHeat.R')
stock.data <- read.csv('/Users/poorlyeric/code/8785OS_Code/Chap 8/Data Files/google.csv')
library("chron")
calendarHeat(dates=ddd$FLTDATE,values=ddd$BOOKING,varname='Google Adjusted Close')
ddd <- flth %>% filter(FLT_key=='1-DXB-LHR') %>% select(FLTDATE,BOOKING) 
ddd2 <- flth %>% filter(FLT_key=='2-LHR-DXB') %>% select(FLTDATE,BOOKING)
write.csv(ddd,file='ddd.csv',row.names = FALSE)
write.csv(ddd2,file='ddd2.csv',row.names = FALSE)


colnames(data)[2] <- 'ORGN'
#colnames(flth)[4] <- "date"
flth <- data
head(flth)
colnames(flth)
#flth <- flth %>% rename(date=FLTDATE)
#flth$date <- as.Date(flth$date) 
flth$BOOKING <- as.numeric(flth$BOOKING)
str(flth)
rm(data)
m <- 9
#11,6,7,8,9
ptm <- proc.time()
#for(flt_key in results33lf$FLT_KEY[which(is.na(results33lf$trainSize))])
for(flt_key in results33lf$FLT_KEY[1:nrow(results33lf)])
{
  print(flt_key)
  flt1 <- flth[which(flth$FLT_key==flt_key),]
#  flt1df <- flt1 %>% dplyr::select(date,DCP1:DCP16,BOOKING)
  flt1df <- flt1 %>% dplyr::select(FLTDATE,DCP1:DCP16,BOOKING)
  flt1df$BOOKING <- as.numeric(flt1df$BOOKING)
  # flt1df[!complete.cases(flt1df),]
  #  flt1df <- tbl_df(flt1df)
  
 # colnames(flt1df)[ncol(flt1df)] <- 'booked'
 # flt1df$booked <- as.numeric(flt1df$booked)
  
  #if the first few data is missing, na.approx will not work and we just remove them
  i <- 1
  while(any(apply(flt1df[i,],1,is.na))&i<nrow(flt1df)){
    i <- i+1
  }
  if(i>1&i<nrow(flt1df)){
    flt1df <- flt1df[i:nrow(flt1df),]
  }else if(i==nrow(flt1df)){
    results33lf$trainSize[which(results33lf$FLT_KEY==flt_key)] = 0
    next
  }
  i <- nrow(flt1df)
  while(any(apply(flt1df[i,],1,is.na))){
    i <- i-1
  }
  if(i<nrow(flt1df))
  {
    flt1df <- flt1df[1:i,]
  }
  #clean the na data
  tmp <- is.na(apply(flt1df[,2:ncol(flt1df)],2,sum))
  colna <- names(tmp[tmp==TRUE])
  flt1df[,colna] <- sapply(colna, function(x) flt1df[,x] <- na.approx(flt1df[,x]))
  #  colna <- colnames(flt1df[is.na(apply(flt1df[,2:ncol(flt1df)],2,sum))])
  #  flt1df[,colna] <- sapply(colna, function(x) flt1df[,x] <- na.approx(flt1df[,x]))
  flt1df <- flt1df[complete.cases(flt1df),]
  if(nrow(flt1df)<1){
    next
  }
  
  
  #New features generate
  # flt1df <-  flt1df %>% inner_join(cal_feat,by=c("FLTDATE" = "date"))
  flt1df <- merge(flt1df, cal_feat, by.x="FLTDATE",by.y="date",all.x=TRUE)
  
  #below check the continuous of weekind
  weekind_min <- min(flt1df$weekind)
  weekind_max <- max(flt1df$weekind)
  weekind_missing <- seq(weekind_min,weekind_max)[!(weekind_min:weekind_max) %in% unique(flt1df$weekind)] 
  if(any(weekind_missing)){
    weekind_min <- max(weekind_missing)+1
    flt1df <- flt1df[flt1df$weekind>(weekind_min-1),]
    print('removing missing values')
  }
  
  if(nrow(flt1df)<150){
    results33lf$trainSize[which(results33lf$FLT_KEY==flt_key)] = nrow(flt1df)     
    next
  }
  results33lf$trainSize[which(results33lf$FLT_KEY==flt_key)] = nrow(flt1df)   
  #fltdays <- seq(as.Date("2009-01-01"),by=1,length.out=nrow(flt1df))
  #flt1df$wday <- wday(flt1df$date)
  #flt1df$date <- fltdays
  #flt1df$month <- lubridate::month(flt1df$date)
  #flt1df$monthday <- lubridate::mday(flt1df$date)
  fly_freq <- flt1df %>% group_by(year, month, wdayindex) %>% summarise(freq=n()) %>% ungroup() %>% select(freq)
  fly_freq <- sort(table(fly_freq),decreasing=TRUE)[1]
  ffreq <- as.numeric(names(fly_freq))
  flt1df$bookedlag28 <- as.numeric(dplyr::lag(flt1df$BOOKING,ffreq*4))
  flt1df$bookedlag7 <- as.numeric(dplyr::lag(flt1df$BOOKING,ffreq))
  flt1df$bookedlag14 <- as.numeric(dplyr::lag(flt1df$BOOKING,ffreq*2))
  flt1df$bookedlag21 <- as.numeric(dplyr::lag(flt1df$BOOKING,ffreq*3))
  # flt1df$bookedlag28 <- dplyr::lag(flt1df$booked,ffreq*4)
  # flt1df$bookedlag7 <- dplyr::lag(flt1df$booked,ffreq)
  # flt1df$bookedlag14 <- dplyr::lag(flt1df$booked,ffreq*2)
  # flt1df$bookedlag21 <- dplyr::lag(flt1df$booked,ffreq*3)
  
  
  #use the last 760 days for forecasting, i.e., using the more recent data
  #Due to the fact that we will test on several months, we will not check this now
 #  rec_size <- 760
 #  if(nrow(flt1df)>rec_size){
 # #   flt1df$bookedlag364 <- dplyr::lag(flt1df$booked,364)
 #    flt1df$bookedlag364 <- dplyr::lag(flt1df$BOOKING,364)
 #    start_ind <- nrow(flt1df) - rec_size + 1
 #    flt1df <- flt1df[start_ind:nrow(flt1df),]
 #  }
  
  
  # if(nrow(flt1df)>1000){
  #   flt1df$bookedlag364 <- dplyr::lag(flt1df$booked,364)
  # }
  # flt1df$bookedlag28 <- dplyr::lag(flt1df$booked,28)
  #flt1df$bookedlag35 <- dplyr::lag(flt1df$booked,35)
  #flt1df$wday <- as.factor(flt1df$wday)
  #flt1df$month <- as.factor(flt1df$month)
  #flt1df$monthday <- as.factor(flt1df$monthday)
  #flt1df$year <- lubridate::year(flt1df$date)
  #flt1df2 <- flt1df %>% group_by(year,month) %>% mutate(month_len=n(),wdayindex=rep(1:5,each=7)[1:month_len])
  flt1df2 <- flt1df
  flt1df2$D16_D23Now <- as.numeric(flt1df2$DCP16/flt1df2$bookedlag7)
  flt1df2$D16_D9 <- flt1df2$DCP16/flt1df2$DCP9
  D9_D16_sd <- apply(flt1df2[,c("DCP9","DCP10","DCP11","DCP12","DCP13","DCP14","DCP15","DCP16")],1,sd)
  flt1df2$D16_D9_mom <- flt1df2$D16_D9/D9_D16_sd
  
  #  flt1df2 <- flt1df2[complete.cases(flt1df2),] #new update
  #below we change it by filter out the NA cases
  weekind_min <- min(flt1df2$weekind[complete.cases(flt1df2)])
  #  weekind <- rep(1:round(nrow(flt1df2)/7),each=7)#to generate week index for each day
  #  flt1df2$weekind <- weekind[1:nrow(flt1df2)]
  
  print("entering one stp regression")
  
  
  #simple one step linear regression
  cl <- makeCluster(4)
  clusterExport(cl,c("flt1df2","weekind_min"))
  registerDoSNOW(cl)
  # lm_models <- foreach(i=6:floor(nrow(flt1df2)/7),.combine = 'c') %dopar%{#i is the weekind,start from 2 instead of 1 is due to bookedlag7 is NA for 1.
  #    list(Model=lm(booked~DCP8+bookedlag35, data=flt1df2[which(flt1df2$weekind==i),]))
  #  }
  
  #below use 7 instead of 6 is to avoid the case the starting weekind is not the whole week
  lm_models <- foreach(i=(weekind_min):(weekind_max-1),.combine = 'c') %dopar%{#i is the weekind,start from 2 instead of 1 is due to bookedlag7 is NA for 1.
    list(Model=lm(BOOKING~DCP16+bookedlag7, data=flt1df2[which(flt1df2$weekind==i),]))
  }
  # lm_models <- foreach(i=(weekind_min+7):(weekind_max-1),.combine = 'c') %dopar%{#i is the weekind,start from 2 instead of 1 is due to bookedlag7 is NA for 1.
  #   list(Model=lm(booked~DCP9+bookedlag28, data=flt1df2[which(flt1df2$weekind==i),]))
  # }
  # lm_models <- foreach(i=(weekind_min+7):(weekind_min+floor(nrow(flt1df2)/ffreq)),.combine = 'c') %dopar%{#i is the weekind,start from 2 instead of 1 is due to bookedlag7 is NA for 1.
  #   list(Model=lm(booked~DCP9+bookedlag28, data=flt1df2[which(flt1df2$weekind==i),]))
  # }
  # for(i in (weekind_min+7):(weekind_max-1))
  # {
  #   print(i)
  #   mmm <- lm(booked~DCP9+bookedlag28, data=flt1df2[which(flt1df2$weekind==i),])
  # }
  #from above, we built 116 models start from weekind 250, therefore the last model is 365 and the max weekind is 366
  #so we can use all models to predict one week after
  # for(tem in (weekind_min+7):(weekind_min+floor(nrow(flt1df2)/7))){
  #   print(tem)
  #   model=lm(booked~DCP8+bookedlag35, data=flt1df2[which(flt1df2$weekind==tem),])
  #   print(model)
  # }
  
  
  
  #prediction based on lm
  clusterExport(cl,"lm_models")
  #  book_pred <- foreach(i=1:(round(nrow(flt1df2)/7)-6),.combine = 'c') %dopar%{
  #    list(list(booked_lm_pred = round(predict(lm_models[[i]],newdata=flt1df2[which(flt1df2$weekind==(i+6)),]))))
  #  }
  book_pred <- foreach(i=1:(length(lm_models)),.combine = 'c') %dopar%{
    list(list(booked_lm_pred = round(predict(lm_models[[i]],newdata=flt1df2[which(flt1df2$weekind==(i+weekind_min)),]))))
  }
  # book_pred <- foreach(i=1:(length(lm_models)),.combine = 'c') %dopar%{
  #   list(list(booked_lm_pred = round(predict(lm_models[[i]],newdata=flt1df2[which(flt1df2$weekind==(i+7+weekind_min)),]))))
  # }
  
  
  #Add the prediction to the original data
  tmp <- lapply(book_pred,"[[","booked_lm_pred")
  book_pred <- unlist(tmp)
  pad_len <- nrow(flt1df2) - length(book_pred)
  book_pred <- c(rep(NA,pad_len),book_pred)#Cover missing value and training data
  #head(book_pred,20)
  flt1df2$booked_7days_pred <- book_pred
  
  #Add prediction deviation to the original data
  pred_resid <- lapply(lm_models,residuals)
  pred_resid_vec <- unlist(pred_resid)
  pred_resid_vec <- c(rep(NA,pad_len),pred_resid_vec)[1:nrow(flt1df2)]
  flt1df2$deviation <- pred_resid_vec
  
  stopCluster(cl)
  
  print("done one step regression")
  
  #One hot encoding
  wdaydummy <- dummy(flt1df2$wday)
  wdaydummy <- wdaydummy[,-c(1)]
  monthdummy <- dummy(flt1df2$month)
  head(monthdummy)
  monthdummy <- monthdummy[,-c(1)]
  flt1df2 <- cbind(as.data.frame(flt1df2),wdaydummy)
  flt1df2 <- cbind(as.data.frame(flt1df2),monthdummy)
  flt1df2$BOOKING <- as.numeric(flt1df2$BOOKING)
  flt1df2[,2:17] <- sapply(flt1df2[,2:17],as.numeric)
  # p <- ggplot(flt1df2,aes(x=date,y=booked,color=as.factor(wday)))+geom_line()+facet_grid(month~wday)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle(flt_key)
  # print(p)
  gm2 <- gbmmodel_month(flt1df2,m)
  if(!is.list(gm2)){
    next
  }
  gp2 <- gbmpred_month(flt1df2,m,gm2,flt_key)
  
  #gm2 <- gbmmodel(flt1df2)
  #gp2 <- gbmpred(flt1df2,test_size=30,gm2,flt_key)
  results33lf$RelErr[which(results33lf$FLT_KEY==flt_key)] <- gp2[[1]]
  results33lf$RELErrSd[which(results33lf$FLT_KEY==flt_key)] <- gp2[[3]]
  results33lf$RELErrMedian[which(results33lf$FLT_KEY==flt_key)] <- gp2[[4]]
  results33lf$SYSErrMedian[which(results33lf$FLT_KEY==flt_key)] <- gp2[[5]]
  outputdata7 <- rbind(outputdata7,gp2[[2]])
  print(paste(flt_key,gp2[[4]]))
  # results1m$RELErrSd[which(results1m$FLT_KEY==flt_key)] <- gp2[[2]]
}
proc.time()-ptm

r6 <- results33lf[complete.cases(results33lf),]
r6$win <- ifelse(r6$RELErrMedian<r6$SYSErrMedian,1,0)
mean(r6$win)
r6$id <- 1:nrow(r6)
head(r6)
colnames(r6)[4] <- 'GBMErrMedian'
library(tidyr)
r6 %>% select(id,GBMErrMedian,SYSErrMedian) %>% gather(Method,RelErr,-id) %>% ggplot(aes(x=RelErr,fill=Method))+geom_density(alpha=0.4)+xlim(c(0,1))+ggtitle("Density for 7 days forecasting on Test(Sep)")
sep %>% select(id,GBMErrMedian,SYSErrMedian) %>% gather(Method,RelErr,-id) %>% ggplot(aes(x=RelErr,fill=Method))+geom_density(alpha=0.4)+xlim(c(0,1))+ggtitle("Density for 7 days forecasting on Test(Sep)")

with(r6,t.test(GBMErrMedian,SYSErrMedian))
lapply(r6[,c("GBMErrMedian","SYSErrMedian")],median)
head(r6)
r6 %>% select(FLT_KEY,GBMErrMedian,SYSErrMedian,win) %>% write.csv("7daysforecasting.csv")
(
nov <- r6
oct <- r6

sep <- r6

aug <- r6
jul <- r6
jun <- r6

may <- r6
apr <- r6
mar <- r6
feb <- r6
jan <- r6
dec <- r6

nov$month <- 11
oct$month <- 10
sep$month <- 9
aug$month <- 8
jul$month <- 7
jun$month <- 6
may$month <- 5
apr$month <- 4
mar$month <- 3
feb$month <- 2
jan$month <- 1
dec$month <- 12

res_sum <- rbind(jan,feb,mar,apr,may,jun)
res_sum <- rbind(res_sum,jul,aug,sep,oct,nov,dec)
write.csv(res_sum,file='res_sum.csv',row.names=FALSE)

by(res_sum[,c("GBMErrMedian","SYSErrMedian")],res_sum$month,t.test)


)
output7 <- outputdata7[2:nrow(outputdata7),]
output7 <- output7 %>% separate(FLT_key,c("FLT_NO","ORGN","DSTN"),sep="-") 
output7 <- output7 %>% select(FLT_NO,ORGN,DSTN,date,actual,pred,DCP16) %>% rename(DEP_DATE=date,FINALBOOK=actual,GBM_PRED=pred,SYS_PRED=DCP16)
output7$DEP_DATE <- as.Date(output7$DEP_DATE)
#output7 <- output7 %>% rename(GBM_PRED_7DAY=GMB_PRED_7DAY)
write.csv(output7,file="7daysforecastDec.csv",row.names = FALSE)
output7 %>% mutate(GBM_Err=abs(GBM_PRED-FINALBOOK)/FINALBOOK,SYS_Err=abs(SYS_PRED-FINALBOOK)/FINALBOOK) %>% ggplot(aes(x=SYS_Err,y=GBM_Err))+geom_point(alpha=0.3)+stat_smooth(method='lm')+xlim(0,1)+ylim(0,1)+geom_abline(intercept = 0, slope = 1,col='red')+ggtitle("7 days forecasting for Test(Dec)")

output7$GBMErr <- abs(output7$FINALBOOK-output7$GBM_PRED)
output7$SYSErr <- abs(output7$FINALBOOK-output7$SYS_PRED)

output7$RES_SYSGBM <- output7$SYSErr - output7$GBMErr
output7 <- output7 %>% arrange(RES_SYSGBM)
output7$Outcome <- ifelse(output7$RES_SYSGBM>0,'Good','Bad')
output7$index <- 1:nrow(output7)

ggplot(output7[complete.cases(output7),],aes(x=index,y=RES_SYSGBM,fill=Outcome))+geom_bar(stat='identity',position='identity')+guides(fill=guide_legend(reverse=TRUE))

output28 <- read.csv("output28daysforecasting.csv",stringsAsFactors = FALSE)
str(output28)
as.Date(output28$DEP_DATE[1])
output28$DEP_DATE <- as.Date(output28$DEP_DATE)
output28 <- output28 %>% rename(GBM_PRED_28DAY=GBM_PRED,SYS_PRED_28DAY=SYS_PRED)
nrow(output28)
nrow(output7)
output7$FLT_NO <- as.integer(output7$FLT_NO)
output7 <- output7 %>% rename(FINALBOOKS=FINALBOOK)
output <- output7 %>% full_join(output28)
output %>% select(FLT_NO,ORGN,DSTN,DEP_DATE,FINALBOOKS,GMB_PRED_7DAY,GBM_PRED_28DAY,SYS_PRED,SYS_PRED_28DAY) %>% write.csv("legforecast7-28days.csv")

as.Date(output7$DEP_DATE[1])
%>%write.csv("output7daysforecasting.csv")
xxxx <- output7 %>% mutate(gbmerr=abs(GBM_PRED-FINALBOOK)/FINALBOOK,syserr=abs(SYS_PRED-FINALBOOK)/FINALBOOK) %>% group_by(FLT_NO,ORGN,DSTN) %>% summarise(GBM_M=median(gbmerr),SYS_M=median(syserr))
xxxx$win <- as.numeric(xxxx$GBM_M<xxxx$SYS_M)
mean(xxxx$win,na.rm=TRUE)

r4 <- results33lf[complete.cases(results33lf),]
r4$win <- ifelse(r4$RELErrMedian<r4$SYSErrMedian,1,0)
r4$id <- 1:nrow(r4)
colnames(r4)[4] <- 'GBMErrMedian'
r4 %>% select(id,GBMErrMedian,SYSErrMedian) %>% gather(Method,RelErr,-id) %>% ggplot(aes(x=RelErr,fill=Method))+geom_density(alpha=0.4)+xlim(c(0,1))
with(r4,t.test(GBMErrMedian,SYSErrMedian))
lapply(r4[,c("GBMErrMedian","SYSErrMedian")],median)
head(r4)
r4 %>% select(FLT_KEY,GBMErrMedian,SYSErrMedian,win) %>% write.csv("7daysforecasting.csv")


r3 <- results33lf[complete.cases(results33lf),]
r3$win <- ifelse(r3$RELErrMedian<r3$SYSErrMedian,1,0)
r3$id <- 1:nrow(r3)
r3 %>% select(id,GBMErrMedian,SYSErrMedian) %>% gather(Method,RelErr,-id) %>% ggplot(aes(x=RelErr,fill=Method))+geom_density(alpha=0.4)+xlim(c(0,1))
with(r3,t.test(RELErrMedian,SYSErrMedian))
colnames(r3)[4] <- "GBMErrMedian"
r3 %>% select(FLT_KEY,GBMErrMedian,SYSErrMedian,win) %>% write.csv("28daysforecasting.csv")

r2 <- results33lf[complete.cases(results33lf),]
r2$win <- ifelse(r2$RELErrMedian<r2$SYSErrMedian,1,0)
mean(r2$win)
r2 %>% select(id,RELErrMedian,SYSErrMedian) %>% gather(Method,RelErr,-id) %>% ggplot(aes(x=RelErr,fill=Method))+geom_density(alpha=0.4)+xlim(c(0,1))


r <- results33lf[complete.cases(results33lf),]

LRO$flt_no <- as.numeric(LRO$flt_no)
LRO <- LRO %>% mutate(FLT_KEY=paste(flt_no,ORGN,DSTN,sep='-')) 
colnames(LRO)[ncol(LRO)] <- "FLT_KEY"
LRO <- LRO %>% select(FLT_KEY,dep_date,DCP9,DCP10,DCP23) %>% rename(date=dep_date)
LRO$date <- as.Date(LRO$date)
LRO <- LRO %>% rename(FLT_KEY=FLT_key)

lro_result <- LRO %>% mutate(err=abs(dcp23-dcp9)/dcp23) %>% filter(!is.na(err)) %>% group_by(FLT_KEY) %>% summarise(ErrMedian=median(err))
head(results33lf)
results33lf_bak <- results33lf
results33lf$FLT_KEY <- as.character(results33lf$FLT_KEY)
result_fin <- results33lf %>% left_join(lro_result)
result_fin_com <- result_fin[complete.cases(result_fin),]
result_fin_com$id <- 1:nrow(result_fin_com)
result_fin_com <- result_fin_com %>% select(id,RELErrMedian,ErrMedian,trainSize) %>% rename(GBM=RELErrMedian,LRO=ErrMedian)
result_fin_com$size <- ifelse(result_fin_com$trainSize>1500,">1500",ifelse(result_fin_com$trainSize>1000,">1000",ifelse(result_fin_com$trainSize>500,">500","<=500")))
result_fin_com %>% mutate(win=as.numeric(LRO>GBM)) %>% group_by(size) %>% summarise(wins=sum(win),tot=n(),winrate=wins/n())
results33lf_bak <- results33lf
result_fin_com_bak <- result_fin_com


result_fin_com %>% select(id,GBM,LRO) %>% gather(Method,RelErr,-id) %>% ggplot(aes(x=id,y=RelErr,col=Method))+geom_line()



with(results33lf[complete.cases(results33lf),],mean(RelErr))
with(results33lf[complete.cases(results33lf),],mean(RELErrMedian))
ggplot(results33lf[complete.cases(results33lf),],aes(x=RelErr,y=RELErrMedian))+geom_point(alpha=0.4)
```

```{r}
library(dplyr)
flth_cor <- flth %>% select(FLT_key,date,DCP19)
flth_cor <- flth_cor[complete.cases(flth_cor),]
length(unique(flth_cor$FLT_key))
days <- seq(as.Date("2014-07-01"),by=1,length.out = 365)
days <- as.Date(days)
inval_flt_key <- flth_cor$FLT_key[!flth_cor$date %in% days]

flth_cor_1 <- flth_cor[flth_cor$date>=as.Date("2014-07-01")&flth_cor$date<as.Date("2015-07-01"),]
max(flth_cor_1$date)
flth_cor_sum <- flth_cor_1 %>% group_by(FLT_key) %>% summarise(num=n())
ggplot(flth_cor_sum,aes(x=num))+geom_histogram()
sum(flth_cor_sum$num==365)
val_flt_key <- flth_cor_sum$FLT_key[which(flth_cor_sum$num==365)]

flth_cor_2 <- flth_cor_1[which(flth_cor_1$FLT_key %in% val_flt_key),]
library(tidyr)
flth_cor_2 <- flth_cor_2 %>% arrange(FLT_key,date) %>% spread(date,DCP19) 
head(flth_cor_2)
x <- as.matrix(flth_cor_2[,2:ncol(flth_cor_2)])
rownames(x) <- flth_cor_2$FLT_key
x <- t(x)
dim(x)
M <- cor(x)
library(corrplot)
heatmap(M,Rowv = NA,Colv=NA,col=cm.colors(256),scale="column",margins=c(5,10))
corrplot(M,method="circle")

dt <- dist(t(x),method="euclidean")
clust = hclust(dt)
clust
```

```
FLT_NO,O,D,DEP_DATE,FinalBook,Forecast28

```
