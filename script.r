source('./r_files/flatten_HTML.r')


############### Library Declarations ###############

libraryRequireInstall = function(packageName, ...)
{
  if(!require(packageName, character.only = TRUE)) 
    warning(paste("*** The package: '", packageName, "' was not installed ***", sep=""))
}

libraryRequireInstall("ggplot2");
libraryRequireInstall("plotly")
libraryRequireInstall("lubridate")
libraryRequireInstall("prophet")
libraryRequireInstall("dplyr")
libraryRequireInstall("scales")

#RVIZ_IN_PBI_GUIDE:BEGIN:Added to enable user parameters   

############ User Parameters #########
# Set of parameters from GUI


#PBI_PARAM Confidence level line
#Type:numeric, Default: 0.75 , Range:[0,1], PossibleValues:NA, Remarks: GUI input is predefined set of values
conf1 = "Forecast Next 30 Days"
if(exists("settings_forecast_params_conf1")){
  conf1 = settings_forecast_params_conf1
}
conf2 = "Linear"
if(exists("settings_forecast_params_conf2")){
  conf2 = settings_forecast_params_conf2
}
plot1 = "line"
if(exists("settings_plot_params_plot1")){
  plot1 = settings_plot_params_plot1
}
xaxis1 = "Date"
if(exists("settings_axis_params_xaxis1")){
  xaxis1 = settings_axis_params_xaxis1
}
yaxis1 = "Value"
if(exists("settings_axis_params_yaxis1")){
  yaxis1 = settings_axis_params_yaxis1
}

prep_forecast_data_frame <- function(forecast,pf_df_summarised,trend_type,model_format) {
  fdata<-forecast[forecast$is_forecast,c('ds','yhat','trend')]
  
  colnames(fdata)<-c("Invoice_Date","Order_Value_prime",'trend')
  fdata$is_forecast_plot<-TRUE
  fdata$is_forecast_table<-TRUE
  #print('prepdata ')
  colnames(pf_df_summarised)<-c("Invoice_Date","Order_Value_prime") 
  pf_df_summarised$is_forecast_plot<-FALSE
  pf_df_summarised$is_forecast_table<-FALSE
  frc<-forecast[,c('ds','trend')]
  if(model_format=="Forecast till End of the Month" | model_format=="Forecast Next 30 Days"){
    #add mean of weekly component
    fdata$trend<-fdata$trend+mean(forecast$additive_terms[1:7])
    frc$trend<-frc$trend+mean(forecast$additive_terms[1:7])
  } else if(model_format=="Forecast Next 12 Months "){
    fdata$trend<-fdata$trend+mean(forecast$additive_terms[1:12])
    frc$trend<-frc$trend+mean(forecast$additive_terms[1:12])
    
  } else if(model_format=="Forecast Next Quarter"){
    fdata$trend<-fdata$trend+mean(forecast$additive_terms[1:4])
    frc$trend<-frc$trend+mean(forecast$additive_terms[1:4])
    
  }
  frc$ds<-as.Date(frc$ds)
  pf_df_summarised<-merge(pf_df_summarised,frc,by.x = "Invoice_Date",by.y = "ds",all.x = TRUE,all.y = FALSE)
  
  
  padd_plot<-tail(pf_df_summarised,1)
  padd_plot$is_forecast_plot<-TRUE
  padd_plot$is_forecast_table<-FALSE
  
  if(model_format=="Forecast till End of the Month"){
    this_month_start=tail(fdata$Invoice_Date,1)
    day(this_month_start)<-1
    pf_df_summarised$is_forecast_table[pf_df_summarised$Invoice_Date>=this_month_start]=TRUE
    
  }
  
  forecasted_data<- rbind(pf_df_summarised,padd_plot,fdata)
  #forecasted_data$category<-t
  forecasted_data$trend_type<-trend_type
  forecasted_data$model_format<-model_format
  
  
  return(forecasted_data)
} 


############################End of helper functions 


######################
# Reading data source 
######################
validPlot = TRUE
validData = TRUE
validPlot2 = TRUE
if(!exists("valuefield"))
   {
     validData = FALSE
     validPlot = FALSE
     text = "Value field is required"
     p =ggplot() + 
       annotate("text", x = 4, y = 25, size=4, label = text) + 
       theme_void()
     p =ggplotly(p) %>%
        config(displaylogo = FALSE)
}
if(!exists("datefield"))
   {
     validData = FALSE
     validPlot = FALSE
     text = "Date field is required \n (Format m/d/yyyy)"
     p =ggplot() + 
       annotate("text", x = 4, y = 25, size=4, label = text) + 
       theme_void()
     p =ggplotly(p) %>%
        config(displaylogo = FALSE)
}
if(validData)
{
  validPlot = TRUE
  dataset = cbind(datefield, valuefield)
  Invoice_Date = mdy(dataset[,1])
  errorDate = mean(is.na(Invoice_Date))
  if(errorDate>0.3)
  {
    validPlot = FALSE
     text = "Error in Date field \n Format required is m/d/yyyy"
     p =ggplot() + 
       annotate("text", x = 4, y = 25, size=4, label = text) + 
       theme_void()
     p =ggplotly(p) %>%
        config(displaylogo = FALSE)
  }
  Order_Value_prime = dataset[,2]
  Order_Value = data.frame(Invoice_Date, Order_Value_prime)
  Order_Value$Order_Value_prime<-gsub("[$]","", Order_Value$Order_Value_prime)
  Order_Value$Order_Value_prime<-as.numeric(gsub(',', '', Order_Value$Order_Value_prime))
  Order_Value$Invoice_Date<-as.Date(Invoice_Date,format = "%d-%m-%Y")
  errorOrder = mean(is.na(Order_Value$Order_Value_prime))
  if(errorOrder>0.3)
  {
    validPlot = FALSE
     text = "Error in Value field,\n Value should be decimal or whole number"
     p =ggplot() + 
       annotate("text", x = 4, y = 25, size=4, label = text) + 
       theme_void()
     p =ggplotly(p) %>%
        config(displaylogo = FALSE)
  }
  
}
if(validPlot)
{
  Order_Value = Order_Value %>% group_by(Invoice_Date) %>% summarize("Order_Value_prime" = sum(Order_Value_prime)) %>% as.data.frame
  Order_Value<- Order_Value[order(Order_Value$Invoice_Date),]

  list_model_formats<-conf1
  trend_model <- conf2

  ######################
  #cleanin data 
  #######################
  pf_df_summarised_daily<-Order_Value
  # removing na
  pf_df_summarised_daily<-pf_df_summarised_daily[!is.na(pf_df_summarised_daily$Order_Value_prime),]


  #summary(pf_df_summarised$Order_Value_prime)

  #remove weekend data
  pf_df_summarised_daily$wday<-wday(pf_df_summarised_daily$Invoice_Date)
  pf_df_summarised_daily<-pf_df_summarised_daily[pf_df_summarised_daily$wday!=7,]
  pf_df_summarised_daily<-pf_df_summarised_daily[pf_df_summarised_daily$wday!=1,]

  #summary(pf_df_summarised$Order_Value_prime)



  # Removing outlier 99%tile 
  threshold_outlier_u<-quantile(Order_Value$Order_Value_prime,0.99,na.rm = TRUE)
  threshold_outlier_l<-quantile(Order_Value$Order_Value_prime,0.01,na.rm = TRUE)

  pf_df_summarised_daily<-pf_df_summarised_daily[pf_df_summarised_daily$Order_Value_prime<threshold_outlier_u,]
  pf_df_summarised_daily<-pf_df_summarised_daily[pf_df_summarised_daily$Order_Value_prime>threshold_outlier_l,]

  #summary(pf_df_summarised$Order_Value_prime)
  ####
  #groupings 
  # 1.All - group sum all by date
  # 2.OTC - select only OTC and group sum

  forecasted_data_master<- NULL


  #t="ALL"
  # we do to categories 

  pf_df_summarised_category_m<-pf_df_summarised_daily


  #summary(pf_df_summarised$Order_Value_prime)
  ###########################
  # Data Cleaning  
  ###########################


  pf_df_summarised_category<-pf_df_summarised_category_m
  pf_df_summarised_category<-pf_df_summarised_category %>% group_by(Invoice_Date) %>% summarize(Order_Value_prime = sum(Order_Value_prime)) %>% as.data.frame
  #sort by invoice date
  pf_df_summarised_category<-pf_df_summarised_category[order(pf_df_summarised_category$Invoice_Date),]

  ######
  #if there is missing data more than a month skip that
  if(max(Order_Value$Invoice_Date)-max(pf_df_summarised_category$Invoice_Date)>30){
    print(paste('skipping ' ,pl,'due to less data points. Max date in parent level',max(pf_df_summarised_category$Invoice_Date)))
    next
  }


  ###########################
  # Group and summarise data 
  ###########################
  # 1. last 90 -> next 30
  # 2. last 90 -> till end of montht
  # 
  # #bar chart
  # 3. quater data -> 1 quater
  # 4. monthly -> next 12 month

  # list_model_formats<-c("Forecast Next 12 Months")
  #model_format<-"Forecast Next Quarter"
  for (model_format in list_model_formats) { 
    if(model_format=="Forecast Next 30 Days"|model_format=="Forecast till End of the Month"){
      pf_df_summarised<-tail(pf_df_summarised_category[c("Invoice_Date" , "Order_Value_prime")],90)
      
    } else if(model_format=="Forecast Next 12 Months"|model_format=="Forecast Next Quarter") {
      
      pf_df_summarised<-pf_df_summarised_category
      pf_df_summarised$month = lubridate::month(pf_df_summarised$Invoice_Date)
      pf_df_summarised$year = year(pf_df_summarised$Invoice_Date)
      
      # view data
      #head(pf_df_summarised)
      
      #if number of days in the last month is less than 21 working days, remove that month 
      this_month <- tail(pf_df_summarised,1)
      
      if(length(pf_df_summarised$Invoice_Date[(pf_df_summarised$month==this_month$month) &(pf_df_summarised$year==this_month$year)])<21) {
        pf_df_summarised<-pf_df_summarised[!((pf_df_summarised$month==this_month$month) &(pf_df_summarised$year==this_month$year)),]
      }
      #tail(pf_df_summarised)
      
      
      # summarize data as needed
      pf_df_summarised<-pf_df_summarised %>% group_by(year,month) %>% summarize(Order_Value_prime = sum(Order_Value_prime)) %>% as.data.frame
      if(nrow(pf_df_summarised)<12)
      {
        validPlot2 = FALSE
        text = "Not enough data points"
        p =ggplot() + 
          annotate("text", x = 4, y = 25, size=4, label = text) + 
          theme_void()
        p =ggplotly(p) %>%
          config(displaylogo = FALSE)
        break
      }
      
      pf_df_summarised$Invoice_Date=as.Date(paste(as.character(pf_df_summarised$year),as.character(pf_df_summarised$month),'01',sep='-'))
      pf_df_summarised<-pf_df_summarised[,c("Invoice_Date","Order_Value_prime")]
      
    } 
    
    ###########################
    # Model Building 
    ###########################
    #set colume to ds and y . 
    #ds column should be YYYY-MM-DD for a date, or YYYY-MM-DD HH:MM:SS for a timestamp
    
    colnames(pf_df_summarised)<-c("ds","y")
    #pf_df_summarised$y<-log(pf_df_summarised$y)
    data_for_model<-pf_df_summarised
    
    
    for( trend_type in trend_model){
      
      
      if(model_format=="Forecast till End of the Month"){
        pf_df_summarised<-data_for_model
        if(trend_type=="Weighted"){
          m <- prophet(pf_df_summarised,changepoint.prior.scale = 0.8,n.changepoints = 40,yearly.seasonality=FALSE,daily.seasonality=FALSE)
        } else {
          m <- prophet(pf_df_summarised,n.changepoints =1,yearly.seasonality=FALSE,daily.seasonality=FALSE)
        }
        
        future <- make_future_dataframe(m, periods = 50)
        this_month <-month(max(Order_Value$Invoice_Date))
        future$month<-month(future$ds)
        end_month<-max(future$ds[future$month==this_month])
        
        future$wday<-wday(future$ds)
        future<-future[future$wday!=7,]
        future<-future[future$wday!=1,]
        future<-future[future$ds<=end_month,]
        
        
        if(sum(future$ds>max(pf_df_summarised$ds))==0){
          
          month(end_month)<-month(end_month)+1
          future <- make_future_dataframe(m, periods = 50)
          future$month<-month(future$ds)
          
          future$wday<-wday(future$ds)
          future<-future[future$wday!=7,]
          future<-future[future$wday!=1,]
          future<-future[future$ds<=end_month,]
        }
        future<-as.data.frame(future[,c(1)])
        colnames(future)<-c("ds")
        
        #tail(future)
        
        #predict 
        forecast <- predict(m, future)
        forecast$is_forecast<-FALSE
        forecast$is_forecast[forecast$ds>max(pf_df_summarised$ds)]<-TRUE
        
        #plot_forecast_plotly(paste(t,"Forecast till End of the Month"))
        print("Forecast till End of the Month")
        
      } else if (model_format=="Forecast Next 30 Days") {
        pf_df_summarised<-data_for_model
        if(trend_type=="Weighted"){
          m <- prophet(pf_df_summarised,changepoint.prior.scale = 0.8,n.changepoints = 40,yearly.seasonality=FALSE,daily.seasonality=FALSE)
        } else {
          m <- prophet(pf_df_summarised,n.changepoints =1,yearly.seasonality=FALSE,daily.seasonality=FALSE)
        }
        
        future <- make_future_dataframe(m, periods = 50)
        future$wday<-wday(future$ds)
        future<-future[future$wday!=7,]
        future<-future[future$wday!=1,]
        future<-as.data.frame(head(future[,c(1)],120))
        colnames(future)<-c("ds")
        #tail(future)
        
        #predict
        forecast <- predict(m, future)
        forecast$is_forecast<-FALSE
        forecast$is_forecast[forecast$ds>max(pf_df_summarised$ds)]<-TRUE
        
        #plot_forecast_plotly(paste(t,"Forecast Next 30 days "))
        print("Forecast Next 30 days ")
        
      } else if (model_format=="Forecast Next 12 Months"|model_format=="Forecast Next Quarter") {
        #quarterly forecst is prepared as aggregate of monthly forecast 
        pf_df_summarised<-data_for_model
        if(trend_type=="Weighted"){
          m <- prophet(pf_df_summarised,changepoint.prior.scale = 0.3,n.changepoints =4,
                      seasonality.prior.scale = 0.25,
                      #seasonality.mode ='multiplicative',
                      weekly.seasonality=FALSE,daily.seasonality=FALSE)
          
          
        } else {
          m <- prophet(pf_df_summarised,n.changepoints =1,
                      #seasonality.mode ='multiplicative',
                      seasonality.prior.scale = 0.25,
                      weekly.seasonality=FALSE,daily.seasonality=FALSE)
        }
        
        future <- make_future_dataframe(m, periods = 12, freq = 'month')
        #tail(future)
        
        #predict
        
        forecast <- predict(m, future)
        forecast$is_forecast<-FALSE
        forecast$is_forecast[forecast$ds>max(pf_df_summarised$ds)]<-TRUE
        
        if (model_format=="Forecast Next Quarter") {
          #aggregate montly to get quarterl 
          forecast_m<-forecast[,c("ds","yhat",'trend','additive_terms')]
          forecast_m$year = year(forecast_m$ds)
          forecast_m$quarter<-quarter(forecast_m$ds, with_year = FALSE, fiscal_start = 1)
          # view data
          #head(forecast_m)
          forecast<-forecast_m %>% group_by(year,quarter) %>% summarize(yhat = sum(yhat),trend=sum(trend),additive_terms=sum(additive_terms)) %>% as.data.frame
          
          forecast$ds<-as.Date(paste(as.character(forecast$year),as.character(forecast$quarter*3),'01',sep='-'))
          month(forecast$ds)<-month(forecast$ds)+1
          day(forecast$ds)<-day(forecast$ds)-1
          forecast<-forecast[,c("ds","yhat",'trend','additive_terms')]
          forecast$is_forecast<-FALSE
          max_date<-max(pf_df_summarised$ds)
          forecast$is_forecast[forecast$ds>max_date]<-TRUE
          year(max_date)<-year(max_date)+1
          forecast<-forecast[forecast$ds<=max_date,]
          pf_df_summarised$year = year(pf_df_summarised$ds)
          pf_df_summarised$quarter<-quarter(pf_df_summarised$ds, with_year = FALSE, fiscal_start = 1)
          # view data
          #head(pf_df_summarised)
          
          #if number of days in the last quarter is less than 65 working days, remove that quarter 
          this_quarter <- tail(pf_df_summarised,1)
          
          if(length(pf_df_summarised$ds[(pf_df_summarised$quarter==this_quarter$quarter) &(pf_df_summarised$year==this_quarter$year)])<65) {
            pf_df_summarised<-pf_df_summarised[!((pf_df_summarised$quarter==this_quarter$quarter) &(pf_df_summarised$year==this_quarter$year)),]
          }
          
          # pf_df<-pf_df[(pf_df$year<this_quarter$year),]
          #tail(pf_df)
          # load library
          
          # summarize data as needed
          pf_df_summarised<-pf_df_summarised %>% group_by(year,quarter) %>% summarize(y = sum(y)) %>% as.data.frame
          
          pf_df_summarised$ds=as.Date(paste(as.character(pf_df_summarised$year),as.character(pf_df_summarised$quarter*3),'01',sep='-'))
          month(pf_df_summarised$ds)<-month(pf_df_summarised$ds)+1
          day(pf_df_summarised$ds)<-day(pf_df_summarised$ds)-1
          
          
          
          
          pf_df_summarised<-pf_df_summarised[,c("ds","y")]
          #plot_forecast_plotly(paste(t,"Forecast Quaterly Order Value"))
          print("Forecast Quaterly Order Value")
          
        } else {
          #plot_forecast_plotly(paste(t,"Forecast Monthly Order Value")) 
          print("Forecast Monthly Order Value") 
        }
        
      }
      
      forecasted_data<-prep_forecast_data_frame(forecast,pf_df_summarised,trend_type,model_format)
      forecasted_data_master<-rbind(forecasted_data_master,forecasted_data)
      
      print(paste("-I- Finished modeling ", model_format,trend_type))
    }
  }







  # rm(end_month,fdata,forecast,forecast_m,forecasted_data,future,list_model_formats,m,max_date,model_format,Order_Value,
  #    pf_df_summarised,pf_df_summarised_category ,pf_df_summarised_daily,
  #    t,this_month,this_quarter,threshold_outlier_l,threshold_outlier_u,types ,padd_plot,data_for_model,pdata,pf_df_summarised_category_m  )

  # end_time <- Sys.time()
  # end_time - start_time


  #########
  # fore testing 
 if(validPlot2) 
 {
  for (model_format in list_model_formats) {
    for( trend_type in trend_model){
      pdata<-forecasted_data_master[forecasted_data_master$trend_type==trend_type & forecasted_data_master$model_format==model_format,]
      #pdata$is_forecast_plot = pdata[pdata$is_forecast_plot =="TRUE",3] = "Forecast"
      #pdata$is_forecast_plot = pdata[pdata$is_forecast_plot =="FALSE",3] = "Input"
      pdata = transform(pdata, legend1 = ifelse(is_forecast_plot==TRUE, "Forecast", "Input"))
      pdata = transform(pdata, legend2 = ifelse(is_forecast_table==TRUE, "Forecast", "Input"))
      if(plot1 == "bar")
      {
      if(list_model_formats == "Forecast Next 12 Months"|list_model_formats =="Forecast Next Quarter")
      {
        pp<- ggplot() +
          geom_bar(data = pdata[!duplicated(pdata$Invoice_Date),], 
                  aes(x = Invoice_Date, y = Order_Value_prime , fill = legend1),
                  stat = "identity", width = 20 )+
          geom_line(data = pdata[!duplicated(pdata$Invoice_Date),], aes(x=Invoice_Date, y=trend, color = "trend"))+
          labs(title=paste(model_format,trend_type)) + xlab(xaxis1) + ylab(yaxis1)+
          scale_y_continuous(labels = scales::comma) + theme(legend.title=element_blank())+
          scale_fill_discrete(labels = c("Input", "Forecast"))+
          scale_colour_manual("", labels = c("trend"),
                              breaks = c("trend"),
                              values = c("black"))
      }
      else
      {
          pp<- ggplot() +
          geom_bar(data = pdata[!duplicated(pdata$Invoice_Date),], 
                  aes(x = Invoice_Date, y = Order_Value_prime , fill = legend1),
                  stat = "identity", width = 1 )+
          geom_line(data = pdata[!duplicated(pdata$Invoice_Date),], aes(x=Invoice_Date, y=trend, color = "trend"))+
          labs(title=paste(model_format,trend_type)) + xlab(xaxis1) + ylab(yaxis1)+
          scale_y_continuous(labels = scales::comma) + theme(legend.title=element_blank())+
          scale_fill_discrete(labels = c("Input", "Forecast"))+
          scale_colour_manual("", labels = c("trend"),
                              breaks = c("trend"),
                              values = c("black"))
      }
      }
      else
      {
        pp<-ggplot(pdata, aes(x=Invoice_Date, y=Order_Value_prime,color=legend1)) +
          geom_line()+labs(title=paste(model_format,trend_type))+
          geom_line(aes(x=Invoice_Date, y=trend,color='trend')) + xlab(xaxis1) +
          ylab(yaxis1)  + scale_y_continuous(labels = scales::comma) + 
          theme(legend.title=element_blank())
      }
      
      #pdata<-forecasted_data_master[forecasted_data_master$category==t & forecasted_data_master$trend_type==trend_type & forecasted_data_master$model_format==model_format,]
      # pp<-ggplot(pdata, aes(x=Invoice_Date, y=Order_Value_prime,color=is_forecast_plot)) +geom_line()+labs(title=paste(t,model_format,trend_type))+geom_line(aes(x=Invoice_Date, y=trend,color='trend')) + xlab("Invoice Date") + ylab("Order Value in $")  + scale_y_continuous(labels = scales::comma) + theme(legend.title=element_blank())
      pp<-pp + theme(
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.2, linetype = 'dotted',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.15, linetype = 'dotted',
                                        colour = "lightgrey"))
      fig = plotly::plot_ly(type="table",domain = list(x=c(0.75,1),
                                                      y=c(0,1)),header=list(values=list(xaxis1,yaxis1),  align = c("center", "center"),
                                                                            line = list(width = 1, color = 'black'),
                                                                            fill = list(color = c("cornflowerblue", "cornflowerblue")),
                                                                            font = list(family = "Arial", size = 15, color = "white")), 
                            cells=list(values=list(pdata[pdata$is_forecast_table=="TRUE",]$Invoice_Date,format( round(as.numeric(pdata[pdata$is_forecast_table=="TRUE",]$Order_Value_prime)), big.mark = ',')),  align = c("center", "center"),
                                      line = list(color = "black", width = 1),
                                      font = list(family = "Arial", size = 15, color = c("black")), height = 28))
      
      r<- plotly::ggplotly(pp)
      r <- r %>%
      layout(
        images = list(
          list(source = "https://media-exp1.licdn.com/dms/image/C510BAQHrtPR7taPSyQ/company-logo_200_200/0?e=1609372800&v=beta&t=0tEgRDkeB9vsRXfsWf5c0EhiVQOvfGjrnzPo4n_CiPg",
               xref = "paper",
               yref = "paper",
               x= 1.38,
               y= 0.15,
               sizex = 0.2,
               sizey = 0.2,
               opacity = 1
          )))
      p<-plotly::subplot(r,fig, widths = c(0.75,0.25), titleX = T, titleY = T) %>%
        config(displaylogo = FALSE)
    } }
 }
}



####################################################

############# Create and save widget ###############
#p = ggplotly(g);
internalSaveWidget(p, 'out.html');
####################################################
