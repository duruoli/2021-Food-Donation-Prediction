# New script to organize data
install.packages("RODBC")
library(RJDBC)
library(odbc)
library(DBI)
library(tidyverse)
library(xts)
library(zoo) # for Centered Moving Average
library(lubridate)
library(stringr)
library(dplyr)
library(ggthemes)
library(readr)
library(forecast)
library(ggplot2)
library(anytime)
library(DataCombine)
library(e1071) # for Support vector regression
library(RODBC)

#Creating Dataset for only North Carolina Donors
NC_county <- filter(Masterfile_Donor, State == "NC")
NC_county$Posting_Date <- anytime(NC_county$Posting_Date)
NC_county$Period <- floor_date(NC_county$Posting_Date,"month")

NC_county <- NC_county[,c("County","Period", "Gross_Weight")] %>% 
  group_by(County, Period) %>%
  summarise(Amount = sum(Gross_Weight))

a <- list("Brunswick", "Carteret", "Columbus", "Craven", "Duplin", "Durham", 
          "Edgecombe", "Franklin", "Greene", "Halifax", "Harnett", "Johnston", "Lee", 
          "Lenoir", "Moore", "Nash", "New Hanover", "Onslow", "Pamlico", "Pender",
          "Pitt", "Richmond","Scotland", "Vance", "Wake", "Wayne", "Wilson")



Prediction <- data.frame(
  ModelName = character(),
  PostingDate = as.Date(character()),
  Period = character(),
  County = character(),
  Amount = double(),
  UOM = character()
)

Prediction__ <- data.frame(
  ModelName = character(),
  PostingDate = as.Date(character()),
  Period = character(),
  County = character(),
  Amount = double(),
  UOM = character()
)

#for loop for county level prediction
for (ctynindex in 1:length(a))  
{ 
  County <- filter(NC_county, County == a[ctynindex])
  Data_monthly <- County[,2:3]
  
  #time series data
  ts_monthly <- ts(Data_monthly$Amount, frequency = 12, start = c(2015,7))
  
  #time series train Dataset
  ts_train_1 <- window(ts_monthly, start = c(2015,7), end=c(2017,12))
  ts_train_2 <- window(ts_monthly, start = c(2015,8), end=c(2018,1))
  ts_train_3 <- window(ts_monthly, start = c(2015,9), end=c(2018,2))
  ts_train_4 <- window(ts_monthly, start = c(2015,10), end=c(2018,3))
  ts_train_5 <- window(ts_monthly, start = c(2015,11), end=c(2018,4))
  ts_train_6 <- window(ts_monthly, start = c(2015,12), end=c(2018,5))
  
  #time series test Dataset
  ts_test_1 <- window(ts_monthly, start = c(2018,1), end=c(2018,1))
  ts_test_2 <- window(ts_monthly, start = c(2018,2), end=c(2018,2))
  ts_test_3 <- window(ts_monthly, start = c(2018,3), end=c(2018,3))
  ts_test_4 <- window(ts_monthly, start = c(2018,4), end=c(2018,4))
  ts_test_5 <- window(ts_monthly, start = c(2018,5), end=c(2018,5))
  ts_test_6 <- window(ts_monthly, start = c(2018,6), end=c(2018,6))
  
  
  #naive forecasting method
  df_ts_monthly <- as.data.frame(ts_monthly)
  naive <- slide(df_ts_monthly, "x", NewVar = "Naive Forecast", slideBy = -1)
  naive_fc <- as.data.frame(naive[31:36,2])
  colnames(naive_fc) <- "naive_fc"
  
  #seasonal naive forecasting method
  snaive_fc <- as.data.frame(rbind(snaive(ts_train_1, h = 1)$mean,
                                   snaive(ts_train_2, h = 1)$mean,
                                   snaive(ts_train_3, h = 1)$mean,
                                   snaive(ts_train_4, h = 1)$mean,
                                   snaive(ts_train_5, h = 1)$mean,
                                   snaive(ts_train_6, h = 1)$mean))
  colnames(snaive_fc) <- "snaive_fc"
  
  #moving average forecasting method
  
  ma_fc <- data.frame(
    ma_fc = double()
  )
  ma_fc [1,] <- 0
  ma_fc [2,] <- 0
  
  for (j in 3:nrow(Data_monthly)){
    ma_avg <- ((Data_monthly[(j - 2),2]) + (Data_monthly[(j-1),2]))/2
    ma_fc[j,] <- ma_avg
  }
  
  ma_fc <- cbind(Data_monthly,ma_fc)
  ma_fc <- ma_fc[31:36,3]
  
  
  #ETS forecasting method
  ets_fc <- as.data.frame(rbind(forecast(ets(ts_train_1),h=1,PI = F,biasadj = T, simulate=T,bootstrap=TRUE)$mean,
                                forecast(ets(ts_train_2),h=1,PI = F,biasadj = T, simulate=T,bootstrap=TRUE)$mean,
                                forecast(ets(ts_train_3),h=1,PI = F,biasadj = T, simulate=T,bootstrap=TRUE)$mean,
                                forecast(ets(ts_train_4),h=1,PI = F,biasadj = T, simulate=T,bootstrap=TRUE)$mean,
                                forecast(ets(ts_train_5),h=1,PI = F,biasadj = T, simulate=T,bootstrap=TRUE)$mean,
                                forecast(ets(ts_train_6),h=1,PI = F,biasadj = T, simulate=T,bootstrap=TRUE)$mean))
  colnames(ets_fc) <- "ets_fc"
  
  #ARIMA model
  arima_fc <- as.data.frame(rbind(
    forecast(auto.arima(ts_train_1),h=1,biasadj = T, bootstrap=TRUE)$mean,
    forecast(auto.arima(ts_train_2),h=1,biasadj = T, bootstrap=TRUE)$mean,
    forecast(auto.arima(ts_train_3),h=1,biasadj = T, bootstrap=TRUE)$mean,
    forecast(auto.arima(ts_train_4),h=1,biasadj = T, bootstrap=TRUE)$mean,
    forecast(auto.arima(ts_train_5),h=1,biasadj = T, bootstrap=TRUE)$mean,
    forecast(auto.arima(ts_train_6),h=1,biasadj = T, bootstrap=TRUE)$mean
  ))
  colnames(arima_fc) <- "arima_fc"
  
  #SVR Modeling
  svr_fc <- data.frame()
  
  
  #SVR Data preparation
  x <- Data_monthly[1:35,2]
  y <- Data_monthly[2:36,2]
  Data_cbind <- cbind(x,y)
  names(Data_cbind)[1] <- "x"
  names(Data_cbind)[2] <- "y"
  
  # Train and Validation Data preparation
  TestData_1<- Data_cbind[1:29,]
  ValData_1 <- Data_cbind[30,]
  
  TestData_2 <- Data_cbind[1:30,]
  ValData_2 <- Data_cbind[31,]
  
  TestData_3 <- Data_cbind[1:31,]
  ValData_3 <- Data_cbind[32,]
  
  TestData_4 <- Data_cbind[1:32,]
  ValData_4 <- Data_cbind[33,]
  
  TestData_5 <- Data_cbind[1:33,]
  ValData_5 <- Data_cbind[34,]
  
  TestData_6 <- Data_cbind[1:34,]
  ValData_6 <- Data_cbind[35,]
  
  # 1st prediction month
  TestData <- TestData_1
  ValData <- ValData_1
  
  model<-svm(y~x, TestData)
  yhat<-predict(model,TestData)
  yhat1 <- predict(model, ValData)
  tuneResult<-tune(svm,y~x,data=TestData,ranges = list(gamma=seq(0,1,0.01), cost =2^(0:9)))
  tunedModel <- tuneResult$best.model
  tunedModely <- predict(tunedModel, TestData)
  tunedModely1 <- predict(tunedModel, ValData)
  svr_fc <- rbind(svr_fc,tunedModely1)
  names(svr_fc)[1] <- "svr_fc"
  
  # 2nd prediction month
  TestData <- TestData_2
  ValData <- ValData_2
  
  model<-svm(y~x, TestData)
  yhat<-predict(model,TestData)
  yhat1 <- predict(model, ValData)
  tuneResult<-tune(svm,y~x,data=TestData,ranges = list(gamma=seq(0,1,0.01), cost =2^(0:9)))
  tunedModel <- tuneResult$best.model
  tunedModely <- predict(tunedModel, TestData)
  tunedModely1 <- predict(tunedModel, ValData)
  svr_fc <- rbind(svr_fc,tunedModely1)
  names(svr_fc)[1] <- "svr_fc"
  
  # 3rd prediction month
  TestData <- TestData_3
  ValData <- ValData_3
  
  model<-svm(y~x, TestData)
  yhat<-predict(model,TestData)
  yhat1 <- predict(model, ValData)
  tuneResult<-tune(svm,y~x,data=TestData,ranges = list(gamma=seq(0,1,0.01), cost =2^(0:9)))
  tunedModel <- tuneResult$best.model
  tunedModely <- predict(tunedModel, TestData)
  tunedModely1 <- predict(tunedModel, ValData)
  svr_fc <- rbind(svr_fc,tunedModely1)
  names(svr_fc)[1] <- "svr_fc"
  
  # 4th prediction month
  TestData <- TestData_4
  ValData <- ValData_4
  
  model<-svm(y~x, TestData)
  yhat<-predict(model,TestData)
  yhat1 <- predict(model, ValData)
  tuneResult<-tune(svm,y~x,data=TestData,ranges = list(gamma=seq(0,1,0.01), cost =2^(0:9)))
  tunedModel <- tuneResult$best.model
  tunedModely <- predict(tunedModel, TestData)
  tunedModely1 <- predict(tunedModel, ValData)
  svr_fc <- rbind(svr_fc,tunedModely1)
  names(svr_fc)[1] <- "svr_fc"
  
  # 5th prediction month
  TestData <- TestData_5
  ValData <- ValData_5
  
  model<-svm(y~x, TestData)
  yhat<-predict(model,TestData)
  yhat1 <- predict(model, ValData)
  tuneResult<-tune(svm,y~x,data=TestData,ranges = list(gamma=seq(0,1,0.01), cost =2^(0:9)))
  tunedModel <- tuneResult$best.model
  tunedModely <- predict(tunedModel, TestData)
  tunedModely1 <- predict(tunedModel, ValData)
  svr_fc <- rbind(svr_fc,tunedModely1)
  names(svr_fc)[1] <- "svr_fc"
  
  # 6th prediction month
  TestData <- TestData_6
  ValData <- ValData_6
  
  model<-svm(y~x, TestData)
  yhat<-predict(model,TestData)
  yhat1 <- predict(model, ValData)
  tuneResult<-tune(svm,y~x,data=TestData,ranges = list(gamma=seq(0,1,0.01), cost =2^(0:9)))
  tunedModel <- tuneResult$best.model
  tunedModely <- predict(tunedModel, TestData)
  tunedModely1 <- predict(tunedModel, ValData)
  svr_fc <- rbind(svr_fc,tunedModely1)
  names(svr_fc)[1] <- "svr_fc"
  
  
  #Create average forecast from all forecast data set
  
  #all_fc <- cbind(Data_monthly[31:36,] ,ma_fc,naive_fc,snaive_fc,ets_fc,arima_fc,svr_fc)
  all_fc <- cbind(Data_monthly[31:36,],ma_fc,naive_fc,snaive_fc,ets_fc,arima_fc,svr_fc)
  all_fc$County <-  a[ctynindex]
  all_fc <- all_fc[,c(9,1:8)]
  Ensemble <- all_fc %>% mutate(ensemble_fc = ((all_fc$ma_fc/6) +
                                                 (all_fc$naive_fc/6) +
                                                 (all_fc$snaive_fc/6) +
                                                 (all_fc$ets_fc/6) +
                                                 (all_fc$arima_fc/6) +
                                                 (all_fc$svr_fc/6)))
  
  #MovingAverage Dataframe
  ModelName <- c("MovingAverage","MovingAverage","MovingAverage","MovingAverage",
                 "MovingAverage","MovingAverage")
  PostingDate <- c(today(),today(),today(),
                   today(),today(),today())
  Period <- c(as.character(Ensemble$Period[1]),as.character(Ensemble$Period[2]),
              as.character(Ensemble$Period[3]),as.character(Ensemble$Period[4]),
              as.character(Ensemble$Period[5]),as.character(Ensemble$Period[6]))
  County <- c(as.character(a[ctynindex]),as.character(a[ctynindex]),
              as.character(a[ctynindex]),as.character(a[ctynindex]),
              as.character(a[ctynindex]),as.character(a[ctynindex]))
  Amount <- c(Ensemble$ma_fc[1],Ensemble$ma_fc[2],
              Ensemble$ma_fc[3],Ensemble$ma_fc[4],
              Ensemble$ma_fc[5],Ensemble$ma_fc[6])
  UOM   <- c('lb','lb','lb',
             'lb','lb','lb')
  MovingAverage <- data.frame(ModelName,PostingDate,Period,
                              County,Amount,UOM)
  
  #Naive Dataframe
  ModelName <- c("Naive","Naive","Naive",
                 "Naive","Naive","Naive")
  PostingDate <- c(today(),today(),today(),
                   today(),today(),today())
  Period <- c(as.character(Ensemble$Period[1]),as.character(Ensemble$Period[2]),
              as.character(Ensemble$Period[3]),as.character(Ensemble$Period[4]),
              as.character(Ensemble$Period[5]),as.character(Ensemble$Period[6]))
  County <- c(as.character(a[ctynindex]),as.character(a[ctynindex]),
              as.character(a[ctynindex]),as.character(a[ctynindex]),
              as.character(a[ctynindex]),as.character(a[ctynindex]))
  Amount <- c(Ensemble$naive_fc[1],Ensemble$naive_fc[2],
              Ensemble$naive_fc[3],Ensemble$naive_fc[4],
              Ensemble$naive_fc[5],Ensemble$naive_fc[6])
  UOM   <- c('lb','lb','lb',
             'lb','lb','lb')
  Naive <- data.frame(ModelName,PostingDate,Period,
                      County,Amount,UOM)
  
  #SeasonalNaive Dataframe
  ModelName <- c("SeasonalNaive","SeasonalNaive","SeasonalNaive",
                 "SeasonalNaive","SeasonalNaive","SeasonalNaive")
  PostingDate <- c(today(),today(),today(),
                   today(),today(),today())
  Period <- c(as.character(Ensemble$Period[1]),as.character(Ensemble$Period[2]),
              as.character(Ensemble$Period[3]),as.character(Ensemble$Period[4]),
              as.character(Ensemble$Period[5]),as.character(Ensemble$Period[6]))
  County <- c(as.character(a[ctynindex]),as.character(a[ctynindex]),
              as.character(a[ctynindex]),as.character(a[ctynindex]),
              as.character(a[ctynindex]),as.character(a[ctynindex]))
  Amount <- c(Ensemble$snaive_fc[1],Ensemble$snaive_fc[2],
              Ensemble$snaive_fc[3],Ensemble$snaive_fc[4],
              Ensemble$snaive_fc[5],Ensemble$snaive_fc[6])
  UOM   <- c('lb','lb','lb',
             'lb','lb','lb')
  SeasonalNaive <- data.frame(ModelName,PostingDate,Period,
                              County,Amount,UOM)
  
  
  #ExponentialSmoothing Dataframe
  ModelName <- c("ExponentialSmoothing","ExponentialSmoothing","ExponentialSmoothing",
                 "ExponentialSmoothing","ExponentialSmoothing","ExponentialSmoothing")
  PostingDate <- c(today(),today(),today(),
                   today(),today(),today())
  Period <- c(as.character(Ensemble$Period[1]),as.character(Ensemble$Period[2]),
              as.character(Ensemble$Period[3]),as.character(Ensemble$Period[4]),
              as.character(Ensemble$Period[5]),as.character(Ensemble$Period[6]))
  County <- c(as.character(a[ctynindex]),as.character(a[ctynindex]),
              as.character(a[ctynindex]),as.character(a[ctynindex]),
              as.character(a[ctynindex]),as.character(a[ctynindex]))
  Amount <- c(Ensemble$ets_fc[1],Ensemble$ets_fc[2],
              Ensemble$ets_fc[3],Ensemble$ets_fc[4],
              Ensemble$ets_fc[5],Ensemble$ets_fc[6])
  UOM   <- c('lb','lb','lb',
             'lb','lb','lb')
  ExponentialSmoothing <- data.frame(ModelName,PostingDate,Period,
                                     County,Amount,UOM)
  
  #ARIMA Dataframe
  ModelName <- c("ARIMA","ARIMA","ARIMA",
                 "ARIMA","ARIMA","ARIMA")
  PostingDate <- c(today(),today(),today(),
                   today(),today(),today())
  Period <- c(as.character(Ensemble$Period[1]),as.character(Ensemble$Period[2]),
              as.character(Ensemble$Period[3]),as.character(Ensemble$Period[4]),
              as.character(Ensemble$Period[5]),as.character(Ensemble$Period[6]))
  County <- c(as.character(a[ctynindex]),as.character(a[ctynindex]),
              as.character(a[ctynindex]),as.character(a[ctynindex]),
              as.character(a[ctynindex]),as.character(a[ctynindex]))
  Amount <- c(Ensemble$arima_fc[1],Ensemble$arima_fc[2],
              Ensemble$arima_fc[3],Ensemble$arima_fc[4],
              Ensemble$arima_fc[5],Ensemble$arima_fc[6])
  UOM   <- c('lb','lb','lb',
             'lb','lb','lb')
  ARIMA <- data.frame(ModelName,PostingDate,Period,
                      County,Amount,UOM)
  
  #SVR Dataframe
  ModelName <- c("SVR","SVR","SVR",
                 "SVR","SVR","SVR")
  PostingDate <- c(today(),today(),today(),
                   today(),today(),today())
  Period <- c(as.character(Ensemble$Period[1]),as.character(Ensemble$Period[2]),
              as.character(Ensemble$Period[3]),as.character(Ensemble$Period[4]),
              as.character(Ensemble$Period[5]),as.character(Ensemble$Period[6]))
  County <- c(as.character(a[ctynindex]),as.character(a[ctynindex]),
              as.character(a[ctynindex]),as.character(a[ctynindex]),
              as.character(a[ctynindex]),as.character(a[ctynindex]))
  Amount <- c(Ensemble$svr_fc[1],Ensemble$svr_fc[2],
              Ensemble$svr_fc[3],Ensemble$svr_fc[4],
              Ensemble$svr_fc[5],Ensemble$svr_fc[6])
  UOM   <- c('lb','lb','lb',
             'lb','lb','lb')
  SVR <- data.frame(ModelName,PostingDate,Period,
                    County,Amount,UOM)
  
  
  #Ensemble Dataframe
  ModelName <- c("EnsembleShubhra","EnsembleShubhra","EnsembleShubhra",
                 "EnsembleShubhra","EnsembleShubhra","EnsembleShubhra")
  PostingDate <- c(today(),today(),today(),
                   today(),today(),today())
  Period <- c(as.character(Ensemble$Period[1]),as.character(Ensemble$Period[2]),
              as.character(Ensemble$Period[3]),as.character(Ensemble$Period[4]),
              as.character(Ensemble$Period[5]),as.character(Ensemble$Period[6]))
  County <- c(as.character(a[ctynindex]),as.character(a[ctynindex]),
              as.character(a[ctynindex]),as.character(a[ctynindex]),
              as.character(a[ctynindex]),as.character(a[ctynindex]))
  Amount <- c(Ensemble$ensemble_fc[1],Ensemble$ensemble_fc[2],
              Ensemble$ensemble_fc[3],Ensemble$ensemble_fc[4],
              Ensemble$ensemble_fc[5],Ensemble$ensemble_fc[6])
  UOM   <- c('lb','lb','lb',
             'lb','lb','lb')
  Ensemble <- data.frame(ModelName,PostingDate,Period,
                         County,Amount,UOM)
  
  
  #colnames(Ensemble)
  
  # create dataframe bound row and column wise 
  Prediction <- rbind(Prediction,MovingAverage,Naive,SeasonalNaive,
                      ExponentialSmoothing,ARIMA,SVR, Ensemble)
  Prediction_ <- cbind(MovingAverage,Naive,SeasonalNaive,
                      ExponentialSmoothing,ARIMA,SVR, Ensemble)
  Prediction__<-rbind(Prediction__,Prediction_)
  
}

Masterfile <- as.data.frame(dbGetQuery(con,
                                       "SELECT Source_No_,Posting_Date,Gross_Weight 
                                       FROM masterFY WHERE Source_Code = 'Donations' 
                                       AND Inventory_Posting_Group = 'DONATED'"))

#Converting Posting Data as Date Time
Masterfile$Posting_Date <- anytime(Masterfile$Posting_Date)

#Donor and daywise aggregate donation amount
Masterfile <- Masterfile %>% group_by(Source_No_,Posting_Date) %>% 
  summarise(Gross_Weight = sum(Gross_Weight))
Masterfile$Source_No_<-as.character(as.integer(trimws(Masterfile$Source_No_)))

#Merging Donation Master File and Donor Master File
Masterfile_Donor <- merge(Masterfile, Donor_final, by="Source_No_")

#Masterfile_Donor <- merge(Masterfile, Donor_final, by="Source_No_",all.x = TRUE )
#Creating Dataset for only North Carolina Donors
NC_county <- filter(Masterfile_Donor, State == "NC")

NC_county <- NC_county[,c("County","Posting_Date", "Gross_Weight")] %>% 
  mutate_if(is.factor, funs(factor(trimws(.))))  %>% 
  group_by(County, Posting_Date) %>% 
  summarise(Gross_Weight = sum(Gross_Weight))


## FEEED Project 
## Code to execute BSTS model 


County_results<-data.frame()
#library(DBI)
# list of counties
for (z in c("Brunswick","Carteret","Columbus","Craven","Duplin","Durham","Edgecombe","Franklin","Greene","Halifax","Harnett","Johnston","Lee",
            "Lenoir","Moore","Nash","New Hanover","Onslow","Pamlico","Pender","Pitt","Richmond","Scotland","Vance","Wake","Wayne","Wilson")){
  
  
  # to change csv to read from ncat server
  x<-NC_county
  y<-"month"
  #z<-"Wake"
  #args<-c(NC_county,"month","Wake")
  input<-as.data.frame(x)
  #input$Date <- as.Date(input$Date,format="%Y-%m-%d")
  grain<-as.character(y)
  county<-as.character(z)
  print(county)
  #print("done")
  input$month<-floor_date(as.Date(input$Posting_Date, "%Y/%m/%d"),unit="month")
  input$week<-floor_date(as.Date(input$Posting_Date, "%Y/%m/%d"),unit="week")
  #print("done_")
  if (grain=="month"){
    
    if (county!="all"){
      input<-input[input$County ==county,]
    }
    #print(input)
    input<-aggregate(x = input$Gross_Weight,                # Specify data column
                      by = list(input$month),              # Specify group indicator
                     FUN = sum)
    n<-nrow(input)
    low=1
    
    #print(n)
    n<-n-6-1
    for (i in 1:6)
    {
      n=n+1
      #print(n)
      input_time_series<-xts(as.integer(input[low:n,2]),order.by=as.Date(input[low:n,1],"%Y-%m-%d"))
      input_test_series<-xts(as.integer(input[(n+1),2]),order.by=as.Date(input[(n+1),1],"%Y-%m-%d"))
      # FEEED_MA_month<-sma(input_time_series[,1])
      # predict_ma_month<-FEEED_MA_month$forecast[1]
      ss<-list()
      ss<- AddLocalLinearTrend(ss, y=input_time_series[,1])
      ss <- AddSeasonal(ss, y=input_time_series[,1], nseasons = 12)
      bsts.model <- bsts(input_time_series[,1], state.specification = ss, niter = 3000, ping=0,seed=30)
      p.bsts.model <- predict.bsts(bsts.model, horizon = 1)
      # print(p.bsts.model$mean[1])
      de<- list("BSTS",as.character(today()),as.character(as.Date(index(input_test_series[1,1]),format="Y-m-d")),as.character(county),p.bsts.model$mean[1],"lb")
      County_results<-rbind(County_results,de)
    }}
  if (grain=="week"){
    if (county!="all"){
      input<-input[input$County ==county,]
    }
    
    input<-aggregate(x = input$Gross_Weight,                # Specify data column
                     by = list(input$week),              # Specify group indicator
                     FUN = sum)
    n<-nrow(input)
    input_time_series<-xts(as.integer(input[1:n,2]),order.by=as.Date(input[1:n,1],"%Y-%m-%d"))
    
    ss<-list()
    ss<- AddLocalLinearTrend(ss, y=input_time_series[,1])
    ss <- AddSeasonal(ss, y=input_time_series[,1], nseasons = 52)
    bsts.model<-bsts(input_time_series[,1],niter=1000,state.specification = ss,ping=0,seed=60)
    #FEEED_MA_month<-sma(input_time_series[,1],h=(i-13))
    # predict_ma_month<-FEEED_MA_month$forecast[1]
    #  }
  }
  
}
# generate column names for dataframe 
col_nam <- list("ModelName", "PostingDate","Period", "County","Amount","UOM")
#x <- list("Date", "Actual Quantity", "BSTS")
colnames(County_results) <- col_nam
Prediction___<-rbind(Prediction,County_results)
Prediction____<-cbind(Prediction__,County_results)
Prediction_ensemble <- data.frame(
     ModelName = character(),
       PostingDate = as.Date(character()),
       Period = character(),
      County = character(),
      Amount = double(),
      UOM = character()
  )


colnames(Prediction____)[47]="BSTS Prediction"
colnames(Prediction____)[41]="Ensemble Prediction"

Prediction____$average_ensemble<-(as.numeric(as.character(Prediction____$`Ensemble Prediction`)) + as.numeric(as.character(Prediction____$`BSTS Prediction`)))/2
Prediction____$ModelName_new<-rep("Ensemble",162)

Prediction_ensemble<-rbind(Prediction_ensemble,Prediction____[c(50,44,45,46,48,49)])

colnames(Prediction_ensemble) <- col_nam
Prediction_ensemble_<-as.data.frame(Prediction_ensemble[,2:5])
Prediction___<-rbind(Prediction,Prediction_ensemble)

