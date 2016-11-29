# The following code is used to generate the features for leg forecasting
# Zheng Zhu
# 21/11/2016
####################
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

setwd("../Data/")

# calendar features
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

# Load the booking data
cnames <- c("FLTNUM","ORGN","DSTN",'FLTDATE','cls_master_comp','rec_type',"DCP1","DCP2","DCP3","DCP4","DCP5","DCP6","DCP7","DCP8","DCP9","DCP10","DCP11","DCP12","DCP13","DCP14","DCP15","DCP16","DCP17","DCP18","DCP19","DCP20","DCP21","DCP22","DCP23")
data <- read_tsv("prms1000.csv",col_names = FALSE)
colnames(data) <- cnames
head(data)
data_bak <- as.data.frame(data)
data <- subset(data, rec_type=='LSS')
head(data)
colnames(data)[length(data)] <- "BOOKING"

data$FLTDATE <- as.Date(data$FLTDATE)
data$FLTNUM <- as.numeric(data$FLTNUM)

# Remove  the non-sense data, ie FLTNUM>=1000
data <- data %>% filter(FLTNUM<1000)

# Generate the FLT_KEY
data <- data %>% mutate(FLT_key=paste(FLTNUM, ORGN, DSTN, sep="-"))

# get some idea of the data
data %>% group_by(FLT_key) %>% summarise(occ_num = n()) %>% ggplot(aes(occ_num))+geom_density()

object.size(data)

# save it to flth
flth <- data
flth <- flth %>% dplyr::rename(BOOKING=DCP23)

# full data frame to store the generate features
feat_df <- NA

results33lf <- data.frame(FLT_KEY=unique(flth$FLT_key), 
                          trainSize = rep(NA,length(unique(flth$FLT_key))))

for(flt_key in unique(flth$FLT_key)){
  flt1 <- flth %>% filter(FLT_key==flt_key)
  flt1df <- flt1 %>% dplyr::select(FLTDATE,DCP1:DCP8,BOOKING)
  flt1df$BOOKING <- as.numeric(flt1df$BOOKING)
  print(flt_key)
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
  #clean the na data by impute
  tmp <- is.na(apply(flt1df[,2:ncol(flt1df)],2,sum))
  colna <- names(tmp[tmp==TRUE])
  flt1df[,colna] <- sapply(colna, function(x) flt1df[,x] <- na.approx(flt1df[,x]))
  
  #remove the case with na
  flt1df <- flt1df[complete.cases(flt1df),]
  if(nrow(flt1df)<1){
    next
  }
  
  flt1df <- merge(flt1df, cal_feat, by.x="FLTDATE",by.y="date",all.x=TRUE)
  
  #below check the continuous of weekind
  weekind_min <- min(flt1df$weekind)
  weekind_max <- max(flt1df$weekind)
  weekind_missing <- seq(weekind_min,weekind_max)[!(weekind_min:weekind_max) %in% unique(flt1df$weekind)] 
  if(any(weekind_missing)){
    weekind_min <- max(weekind_missing)+1
    flt1df <- flt1df[flt1df$weekind>(weekind_min-1),]
    print('removing missing week')
  }  
  
  if(nrow(flt1df)<150){
    results33lf$trainSize[which(results33lf$FLT_KEY==flt_key)] = nrow(flt1df)     
    next
  }
  results33lf$trainSize[which(results33lf$FLT_KEY==flt_key)] = nrow(flt1df)  
  
  #Find the most popular flight frequency per week, Do we need to consider the date instead of using lag function??
  fly_freq <- flt1df %>% group_by(year, month, wdayindex) %>% summarise(freq=n()) %>% ungroup() %>% select(freq)
  fly_freq <- sort(table(fly_freq),decreasing=TRUE)[1]
  ffreq <- as.numeric(names(fly_freq))
  flt1df$bookedlag35 <- as.numeric(dplyr::lag(flt1df$BOOKING,ffreq*5))
  #  flt1df$bookedlag7 <- as.numeric(dplyr::lag(flt1df$BOOKING,ffreq))
  #  flt1df$bookedlag14 <- as.numeric(dplyr::lag(flt1df$BOOKING,ffreq*2))
  #  flt1df$bookedlag21 <- as.numeric(dplyr::lag(flt1df$BOOKING,ffreq*3))
  
  flt1df2 <- flt1df
  flt1df2$D8_D23Now <- as.numeric(flt1df2$DCP8/flt1df2$bookedlag35)
  flt1df2$D8_D6 <- ifelse(flt1df2$DCP6==0,flt1df2$DCP8,flt1df2$DCP8/flt1df2$DCP6)
  
  #D8_D1_sd <- apply(flt1df2[,c("DCP1","DCP2","DCP3","DCP4","DCP5","DCP6","DCP7","DCP8")],1,sd)
  D6_D8_sd <- apply(flt1df2[,c("DCP6","DCP7","DCP8")],1,sd)
  # D9_D16_sd <- apply(flt1df2[,c("DCP9","DCP10","DCP11","DCP12","DCP13","DCP14","DCP15","DCP16")],1,sd)
  flt1df2$D8_D6_mom <- ifelse(D6_D8_sd==0,flt1df2$D8_D6,flt1df2$D8_D6/D6_D8_sd)
  
  weekind_min <- min(flt1df2$weekind[complete.cases(flt1df2)])
  
  #simple one-step linear regression
  cl <- makeCluster(4)
  clusterExport(cl,c("flt1df2","weekind_min"))
  registerDoSNOW(cl)
  
  #below use 7 instead of 6 is to avoid the case the starting weekind is not the whole week
  lm_models <- foreach(i=(weekind_min):(weekind_max-1),.combine = 'c') %dopar%{#i is the weekind,start from 2 instead of 1 is due to bookedlag7 is NA for 1.
    list(Model=lm(BOOKING~DCP8+bookedlag35, data=flt1df2[which(flt1df2$weekind==i),]))
  }  
  
  #prediction based on lm
  clusterExport(cl,"lm_models")
  
  book_pred <- foreach(i=1:(length(lm_models)),.combine = 'c') %dopar%{
    list(list(booked_lm_pred = round(predict(lm_models[[i]],newdata=flt1df2[which(flt1df2$weekind==(i+weekind_min)),]))))
  }
  
  #Add the prediction to the original data
  tmp <- lapply(book_pred,"[[","booked_lm_pred")
  book_pred <- unlist(tmp)
  pad_len <- nrow(flt1df2) - length(book_pred)
  book_pred <- c(rep(NA,pad_len),book_pred)#Cover missing value and training data
  #head(book_pred,20)
  flt1df2$booked_33days_pred <- book_pred
  
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
  flt1df2[,2:24] <- sapply(flt1df2[,2:24],as.numeric)
  flt1df2$FLTKEY <- flt_key
  
  if(flt_key=="1-DXB-LHR"){
    feat_df <- flt1df2
  }
  else{
    feat_df <- bind_rows(feat_df, flt1df2)
  }
}

write.csv(feat_df, file="feat_33days_Y.csv",row.names = F)