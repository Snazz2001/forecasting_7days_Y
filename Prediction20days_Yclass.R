head(feat_df)

cnames <- c("FLTNUM","ORGN","DSTN",'FLTDATE','cls_master_comp','rec_type',"DCP1","DCP2","DCP3","DCP4","DCP5","DCP6","DCP7","DCP8","DCP9","DCP10","DCP11","DCP12","DCP13","DCP14","DCP15","DCP16","DCP17","DCP18","DCP19","DCP20","DCP21","DCP22","DCP23")
data <- read_tsv("prms1000.csv",col_names = FALSE)
colnames(data) <- cnames
head(data)
LRO <- data[data$rec_type=='LRO',]
LRO$FLTNUM <- as.numeric(LRO$FLTNUM)
LRO <- LRO %>% mutate(FLT_key=paste(FLTNUM,ORGN,DSTN,sep='-'))

LRO$FLTDATE <- as.Date(LRO$FLTDATE)
LRO$DCP11 <- as.numeric(LRO$DCP11)

countna <- function(x){
  sum(is.na(x))
}

gbmpred20_month <- function(flt1df2,fitgbm,m,flt_key,DoCompare=TRUE){
  test_start_year <- max(flt1df2$year[which(flt1df2$month==m)])
  test_month <- as.Date(paste(test_start_year,m,'1',sep = "-"))
  datatest <- flt1df2 %>% filter(year==test_start_year,month==m)
  
  pred <- predict(gbmmodel,newdata=datatest,n.trees=500,type="response")
  pred[which(pred<=0)] <- 1
  actual_pred <- data.frame(actual=datatest$BOOKING,date=datatest$FLTDATE,pred=round(pred))
  relerr <- with(actual_pred,mean((abs(actual-pred)/actual)))
  relerrsd <- with(actual_pred,sd((abs(actual-pred)/actual)))
  relerrmedian <- with(actual_pred,median((abs(actual-pred)/actual)))
  
  #  actual_pred <- actual_pred %>% left_join(lro) %>% mutate(syserr = abs(DCP4-actual)/actual) %>% select(actual,date,pred,FLT_KEY,DCP4,DCP9,DCP16,syserr)
  if(DoCompare){
    lro <- LRO %>% filter(FLT_key==flt_key) %>% select(FLT_key,FLTDATE,DCP11,DCP9,DCP16)
    actual_pred <- actual_pred %>% left_join(lro, by=c("date"="FLTDATE")) %>% mutate(syserr = abs(DCP11-actual)/actual) %>% select(actual,date,pred,FLT_key,DCP11,DCP9,DCP16,syserr)
    actual_pred <- actual_pred %>% mutate(gbmerr = abs(pred-actual)/actual)
    sys_err <- with(actual_pred,median(syserr,na.rm=TRUE))
    results <- list(relerr,actual_pred,relerrsd,relerrmedian,sys_err)
  }else{
    actual_pred <- actual_pred %>% mutate(gbmerr = abs(pred-actual)/actual)
    results <- list(relerr,actual_pred,relerrsd,relerrmedian)
  }
  return(results)
}

gbmmodel20_month <- function(flt1df2, m){
  require(gbm)
  require(lubridate)
  if(!m %in% flt1df2$month){
    return(-1)
  }
  test_start_year <- max(flt1df2$year[which(flt1df2$month==m)])
  test_month <- as.Date(paste(test_start_year,m,'1',sep='-'))
  data <- flt1df2[which(flt1df2$FLTDATE<test_month),]
  train_month <- test_month - lubridate::years(2)
  if(min(data$FLTDATE)<train_month){
    data <- data[which(data$FLTDATE>=train_month),]
  }
  nacount_col <- unlist(lapply(data, countna))
  nacol_names <- names(nacount_col)[which(nacount_col==dim(data)[1])]#remove the column with full na,i.e, some flights not fligh on particular day
  val_cnames <- setdiff(names(data),nacol_names)
  data <- data[,val_cnames]
  data <- data[complete.cases(data),]
  if(nrow(data)<120){
    return(-1)
  }
  set.seed(2015)
  # feats <- c("DCP8","DCP1","bookedlag28","wday2","wday3","wday4","wday5","wday6","wday7","month2","month3","month4","month5
  #               ","month6","month7","month8","month9","month10","month11","month12","wdayindex","D8_D28Now","D8_D6","D8_D6_mom","booked_28days_pred","deviation")
  feats <- c("DCP11","DCP1","bookedlag21","wday2","wday3","wday4","wday5","wday6","wday7","month2","month3","month4","month5
             ","month6","month7","month8","month9","month10","month11","month12","wdayindex","D11_D23Now","D11_D8","D11_D8_mom","booked_20days_pred","bookedlag364","deviation")
  feats <- intersect(colnames(data),feats)
  fitgbm <- gbm(as.formula(paste("BOOKING~",paste(feats,collapse="+"))),data=data,distribution = 'gaussian',n.trees = 500,interaction.depth=2, shrinkage = 0.05)
  
  
  #  fitgbm <- gbm(booked~DCP8+DCP1+bookedlag28+wday2+wday3+wday4+wday5+wday6+wday7+month2+month3+month4+month5
  #               +month6+month7+month8+month9+month10+month11+month12+wdayindex+D8_D28Now+D8_D6+D8_D6_mom
  #              +booked_28days_pred+deviation,data=data,distribution = 'gaussian',n.trees = 500,interaction.depth=2, shrinkage = 0.05)
  return(fitgbm)
}

list(relerr,actual_pred,relerrsd,relerrmedian)
flth_data <- read.csv('feat_20days_Y.csv', header = T, stringsAsFactors = FALSE)

perf_agg <- NA
perf_details <- NA

index <- 1
for(flt_key in unique(flth_data$FLTKEY)){
  print(flt_key)
  m <- 12
  flth_df <- subset(flth_data, FLTKEY==flt_key)
  flth_df$FLTDATE <- as.Date(flth_df$FLTDATE)
  gbmmodel <- gbmmodel20_month(flth_df, m)
  if(class(gbmmodel)!="numeric"){
    results <- gbmpred20_month(flth_df,gbmmodel,m,flt_key,DoCompare=TRUE)
    tmp <- data.frame(FLT_key=flt_key,month=m,relerr=results[[1]],relerrsd=results[[3]],relerrmedian=results[[4]])
    if(index==1){
      perf_agg <- tmp
      perf_details <- results[[2]]
    }else{
      perf_agg <- bind_rows(perf_agg,tmp)
      perf_details <- bind_rows(perf_details, results[[2]])
    }
  }
  else{
    print(paste(flt_key," not valid"))
  }
  index <- index + 1
}

summary.gbm(gbmmodel)