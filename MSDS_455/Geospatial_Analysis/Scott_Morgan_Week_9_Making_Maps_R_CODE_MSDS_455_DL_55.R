#CLASS: PREDICT 455 Data Visualization

#Section: 55

#Name: Scott Morgan

#Assignment: Week 9. Individual Assignment 5: Making Maps

#Link to data: https://datascienceplus.com/linear-regression-in-python-predict-the-bay-areas-home-prices/

#1. INITIAL SETUP - SETTING WORKSPACE AND INSTALL/LOAD PACKAGES ----------------------------------------------------------

  #1.1 Clear workspace----
  rm(list=ls())
  
  #1.2 Set working directory and file name----
  
  #Set User name for Windows Machine
  windowsUser='smorgan'
  # windowsUser='smm25'
  # windowsUser='Scott'
  
  #Set working directory
  setwd(paste("C:/Users/",windowsUser,"/Desktop/455/Assignments/Maps/data_source",sep=""))
  
  #File paths and nsf
  wdfilepathname<-getwd()

  #1.3 Install and load packages----
  # install.packages(c(
  # "BAS"
  # ,"broom"
  # ,"dplyr"
  # ,"ggmap"
  # ,"ggplot2"
  # ,"ggthemes"
  # ,"glmnet"
  # ,"grid"
  # ,"gridExtra"
  # ,"lm.beta"
  # ,"maptools"
  # ,"MASS"
  # ,"plyr"
  # ,"png"
  # ,"readr"
  # ,"reshape2"
  # ,"rgeos"
  # ,"scales"
  # ,"tidyverse"), dependencies = TRUE)
  # 
  
  library(readr)
  library(ggplot2)
  library(ggmap)
  library(maptools)
  library(ggthemes)
  library(rgeos)
  library(broom)
  library(dplyr)
  library(plyr)
  library(grid)
  library(gridExtra)
  library(reshape2)
  library(scales)
  library(MASS)
  library(glmnet)
  library(BAS)
  library(lm.beta)
  library(tidyverse)
  library(png)
  library(sqldf)

  #1.4 Set Plot Theme ----
  plotTheme <- function(base_size = 12) {
    theme(
      text = element_text( color = "black"),
      plot.title = element_text(size = 30,colour = "black"),
      plot.subtitle = element_text(size=22, face="italic"),
      plot.caption = element_text(hjust=0),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_line("grey80", size = 0.5),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey80", color = "white"),
      strip.text = element_text(size=12),
      axis.title = element_text(size=20),
      axis.text = element_text(size=14),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.title = element_text(colour = "black", face = "italic"),
      legend.text = element_text(colour = "black", face = "italic"),
      legend.position = "top")
  }
  
  #1.5 Set Map Theme ----
  mapTheme <- function(base_size = 12) {
    theme(
      text = element_text( color = "black"),
      plot.title = element_text(size = 22,colour = "black"),
      plot.subtitle=element_text(face="italic"),
      plot.caption=element_text(hjust=0),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_line("grey80", size = 0.1),
      strip.text = element_text(size=12),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey80", color = "white"),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.title = element_text(colour = "black", face = "italic"),
      legend.text = element_text(colour = "black", face = "italic"))
  }
  
  #1.6 Download info about neighborhood shapes----
    
    #Define the URL of the zipped shapefile
    URL <- "https://github.com/simonkassel/Visualizing_SF_home_prices_R/raw/master/Data/SF_neighborhoods.zip"
    # Download the shapefile to your working directory
    download.file(URL, "SF_neighborhoods.zip")
    #Unzip
    unzip("SF_neighborhoods.zip")
    #Read into R as a spatial polygons data frame, we will use this
    neighb <- readShapePoly("SF_neighborhoods")
    
  #1.7 Map Preparation Download basemap that fit the SF geographic boundaries of the data----
    
    #Define boundaries in latitude and longitude
     bbox <- neighb@bbox
    # 
    # #Download the basemap
     basemap<-get_stamenmap(bbox = c(left = -122.5164, bottom = 37.7066, right = -122.3554, top = 37.8103), maptype = c("toner-lite"), zoom = 13)
    # 
    # #Add theme to map, save down for reference and set object----
     bmMap <- ggmap(basemap) + mapTheme()
     ggsave("basemap.png", bmMap, width = 6, height = 6, device = "png")
    
    #Set map ----
    googlemap <- readPNG("basemap.png")
  
#2. DATA PREPARATION AND PROCESSING------------------------------------------------------------------------

  #2.1 Import Source Data and Set Datatypes----
    data <- read_csv("mydata.csv", col_types = cols(PID = col_number(),Address = col_character(), 
                                                BedroomAbvGr = col_number(), DateSold = col_date(format = "%m/%d/%Y"), 
                                                FullBath = col_number(), Latitude = col_number(), Longitude = col_number(), 
                                                LotArea = col_number(), MoSold = col_integer(), 
                                                YrSold = col_integer(), finishedsqft = col_number(), 
                                                price = col_number(), totalrooms = col_number(), 
                                                zestimate = col_number(), zindexvalue = col_number()))
    data=as.data.frame(data)
    sf_raw=data
    sf_raw=sf_raw[(sf_raw$YrSold)> 2008,]
    sf=sf_raw
    # plot(sf$YrSold,sf$price)
    # summary(sf)

  #2.2 Convert Sale Year as a categorical variable----
    sf$YrSold <- as.factor(sf$YrSold)
    sf$MoSold <- as.factor(sf$MoSold)
    # plot(sf$YrSold,sf$price)
   
  #2.3 Remove outliers; anything 2.5 st. deviations from the mean----
   sf <- sf[which(sf$price < mean(sf$price) + (2.5 * sd(sf$price))), ]
   

#3. EXHIBIT 1:EXPLORATORY DATA ANALYSIS ----------------------------------------------------------

  # 3.1.Overall home price per year boxplot ----
    exhibit_1_boxplot_sales_year<- ggplot(sf, aes(x=YrSold, y=price, fill=YrSold)) + 
    geom_boxplot(color = "grey50",outlier.size = 2.5,lwd=0.2) +
    stat_boxplot(geom = "errorbar",size=1.0) +
    xlab("Sale Price($)") + ylab("Count") +
    scale_fill_brewer(type = "seq" #https://ggplot2.tidyverse.org/reference/scale_brewer.html ; Good resoruce
                       ,palette = "Blues") +
    stat_summary(fun.y=mean, geom="point", size=2, col="black", fill="white") +
    plotTheme() + 
    theme(legend.position="none") +
    scale_y_continuous(labels = comma) +
    labs(x="Year",y="Sale Price($)",title="EXHIBIT 1: DISTRIBUTION OF SAN FRANCISCO HOME PRICES",
         subtitle="Sales Prices for the Period 2003 - 2016",
         caption="Source: San Francisco Office of the Assessor-Recorder, Zillow")
    #Plot
    exhibit_1_boxplot_sales_year

    #Save
    ggsave("exhibit_1_boxplot_sales_year.png", exhibit_1_boxplot_sales_year, width = 17, height =11, device = "png")

  ########################################################################################
  # #Not Used##
  ########################################################################################
  # # # Sales Prices Per Year
  #    prices_mapped_by_year <- ggmap(basemap) +
  #    geom_point(data = sf, aes(x = Longitude, y = Latitude, color = price),
  #               size = .25, alpha = 0.6) +
  #    facet_wrap(~YrSold, scales = "fixed", ncol = 2) +
  #    coord_map() +
  #    mapTheme() + theme(legend.position = c(.85, .25)) +
  #    scale_color_gradientn("Sale Price",
  #                          colors = palette_8_colors,
  #                          labels = scales::dollar_format(prefix = "$")) +
  #    labs(title="DISTRIBUTION OF HOME PRICES IN SAN FRANCISCO",
  #         subtitle="Sales Prices (2003 - 2016)",
  #         caption="Source: San Francisco Office of the Assessor-Recorder")
  #  prices_mapped_by_year
  #  ggsave("exhibit_xx_point map.png", prices_mapped_by_year, width = 17, height = 11, device = "png")
  
  # home_value_hist <- ggplot(sf, aes(price)) + 
  #   geom_histogram(fill=palette_1_colors) +
  #   xlab("Sale Price($)") + ylab("Count") +
  #   scale_fill_manual(values=palette_1_colors) +
  #   plotTheme() + 
  #    labs(x="Sale Price($)", y="Count", title="Distribution of San Francisco Home Prices",
  #        subtitle="Nominal prices (2007 - 2016)", 
  #        caption="Source: San Francisco Office of the Assessor-Recorder, Zillow") +
  #   scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma)
  # 
  # #plot
  # home_value_hist
  # # And saving it to the working directory:
  # ggsave("plot1_histogram.png", home_value_hist, width = 17, height = 11, device = "png")

#4.MODEL GENERATION ----------------------------------------------------------
  
  #4.1 Take original data and clean----
  mydata=sf_raw
  mydata=as.data.frame(mydata)
  sf.clean.all=na.omit(mydata)
  sf.clean.all$YrSold <- as.integer(sf.clean.all$YrSold)
  sf.clean.all$MoSold <- as.integer(sf.clean.all$MoSold)
  # View(sf.clean.all)
  # plot(mydata$YrSold,mydata$price)
  #Data partitions
  set.seed(123)
  n = dim(sf.clean.all)[1]
  inTrain = sample(1:n,size = round(0.7*n))
  inValid = sample(1:n,size = round(0.3*n))
  
  sf_train  = sf.clean.all[inTrain,]
  sf_test  = sf.clean.all[-inTrain,]
  sf_validation  = sf.clean.all[inValid,]
  # View(sf_train)
  
  #4.2 Variable transformations----
  sf_train <- sf_train %>% mutate(
    log_price = log(price)
  )
  # Do same to test and validation data
  sf_test <- sf_test %>% mutate(
    log_price = log(price)
  )
  sf_validation <- sf_validation %>% mutate(
    log_price = log(price)
  )
  
  #4.3 Base Model ----
  model.full =lm(log_price ~ .-Longitude-Latitude-price-PID-Address-DateSold
                 ,data = sf_train)
  
  
  #4.4 BIC----
   model.step.bic <- 
     stepAIC(model.full, direction = "backward", k=log(834), trace = 0)
   model.bic <- eval(model.step.bic$call)
    
  #4.5 AIC----
   model.step.aic <- 
     stepAIC(model.full, direction = "backward", k=2, trace = 0)
   model.aic <- eval(model.step.aic$call)
  
  
  #4.6 Choose model based on r2----
    summary(model.full)$r.squared
    summary(model.bic)$r.squared
    summary(model.aic)$r.squared
   model.chosen <- model.aic
  
  #4.7 Testing the full model----
  sf_test$predicted.log.price <- predict.lm(model.chosen, sf_test)
  sf_test <- sf_test %>% mutate(
    res.log_price = log_price - predicted.log.price,
    predicted.price = price / exp(res.log_price),
    residual = price - predicted.price
  )

#5. FINALIZING MODEL----
  # model.comparison <- compare.std.coef()
  #5.1 Select Final----
  model.final = model.chosen

  sf_train$predicted.log.price <- model.final$fitted.values
  sf_train <- sf_train %>% mutate(
    res.log_price = log_price - predicted.log.price,
    predicted.price = price / exp(res.log_price),
    residual = price - predicted.price
  )


  #5.2 Final Model 
  sf_train$predicted.log.price <- predict.lm(model.final, sf_train)
  sf_train <- sf_train %>% mutate(
    res.log_price = log_price - predicted.log.price,
    predicted.price = price / exp(res.log_price),
    residual = price - predicted.price
  )
  #5.3 Final Model Validation----
  sf_validation$predicted.log.price <- predict.lm(model.final, sf_validation)
  sf_validation <- sf_validation %>% mutate(
    res.log_price = log_price - predicted.log.price,
    predicted.price = price / exp(res.log_price),
    residual = price - predicted.price
  )
  
  #Test Set
  sf_test$predicted.log.price <- predict.lm(model.final, sf_test)
  sf_test <- sf_test %>% mutate(
    res.log_price = log_price - predicted.log.price,
    predicted.price = price / exp(res.log_price),
    residual = price - predicted.price
  )
  
#6. VALUATION ----------------------------------------------------------
  #Which homes differ greatly from the predicted price? Subset data
  homes_large_resids_raw  <-
    rbind(sf_train %>% dplyr::select(PID, price, predicted.price, res.log_price,YrSold,Address,Longitude,Latitude),
          sf_test %>% dplyr::select(PID, price, predicted.price, res.log_price,YrSold,Address,Longitude,Latitude),
          sf_validation %>% 
            dplyr::select(PID, price, predicted.price, res.log_price,YrSold,Address,Longitude,Latitude)) %>%
    filter(abs(res.log_price)>0.3) %>%
    arrange(desc(abs(res.log_price)))
  
  homes_large_resids_raw=as.data.frame(homes_large_resids_raw)
  
  #remove dots in names as sql dataframes dont like them. Replace with _
  colnames(homes_large_resids_raw) <- gsub("\\.","_",colnames(homes_large_resids_raw))
  
  #Filter for more recent transactions with sql query
  homes_large_resids=sqldf(
    "
   SELECT 
         PID
        ,price 
        ,predicted_price
        ,CASE 
          WHEN predicted_price < price THEN 'Over-Valued'
          WHEN predicted_price = price THEN 'Fairly Valued'
          WHEN predicted_price > price THEN 'Under-Valued'
        ELSE 'N/A'
        END AS Valuation
        ,res_log_price
        ,YrSold
        ,Address
        ,Longitude
        ,Latitude
    FROM homes_large_resids_raw
    WHERE YrSold = '2016'
 "
)
  
  #Export to CSV to check
   # write.csv(homes_large_resids, file = "large_residuals.csv")
  
#7. EXHIBIT 2: MAPPING PREDICTIONS ----------------------------------------------------------
  #Mapping - Be patient
  attach(homes_large_resids)
  predictions_mapped <- ggmap(basemap) +
  plotTheme(plotTheme) + 
  geom_point(aes(x = Longitude
                 ,y = Latitude
                 ,color=cut_width(sign(res_log_price),1)
                 ,size=abs(res_log_price))
                 ,data = homes_large_resids) +
  scale_color_discrete(name="Over/Under-Valued"
                       ,labels=c("Over-Valued"
                                 ,"Under-Valued")) +
  scale_size_continuous(name="Magnitude of Mis-pricing By Log Price Error") +
  labs(x="Longitude", y="Latitude"
       , title="EXHIBIT 2: STATE OF SF REAL ESTATE MARKET",
       subtitle="Model Valuation (2016). Larger Magnitutde of Over-Valuation", 
       caption="Source: San Francisco Office of the Assessor-Recorder, Proprietary Internal Sources")
  
  #Plot
  predictions_mapped
  
  #Save
  ggsave("exhibit_2_predictions_map.png", predictions_mapped, width = 17, height = 11, device = "png")

#8 CLEAR ENVIRONMENT WORKSPACE ----
  dev.off()
  rm(list=ls())
  
  
  
  

     