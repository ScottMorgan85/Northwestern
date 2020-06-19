#ShinyR App------------

###################################################
#SETUP: LIBRARY----
###################################################

require(shiny)
require(readr)
require(shinyBS)
require(shinydashboard)
require(shinythemes)
require(shinyalert)
require(rsconnect)
require(randomNames)
require(lubridate)
require(mailR)
require(knitr)
require(gbm)
require(xgboost)
require(shinyjs)
require(DT)
require(htmlTable)
require(V8)
require(png)

###################################################
#SETUP: SET VARIABLES----
###################################################

# setwd("C://Users//Scott Morgan//Desktop//Capstone//") Change as needed
# getwd()

#VARIABLES: SET PREDICTION THRESHOLDHOLDS----
isFraudPredThreshold= 0.5

#VARIABLES: EMAIL ALERTS----
addressFrom=c('xyz@gmail.com')
addressTo=c("xyz@gmail.com")

#VARIABLES: TEXT ALERT PHONE LIST----
textAlertList<-c(
  '5555555555@vtext.com'
)

#  # Verizon: number@vtext.com
#  # AT&T: number@txt.att.net
#  # other carriers: https://20somethingfinance.com/how-to-send-text-messages-sms-via-email-for-free/


#VARIABLES: EMAIL ALERTS----
subjectEmailIsFraud="WARNING: Possible Fraud Detected"
subjectEmailIsNOTFraud="SUCCESS:Transaction Pending"
subjectTexttIsFraud="WARNING: Possible Fraud Detected"

#SET JAVASCRIPT FUNCTION TO REFRESH PAGE ----
jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page


#IMPORT MODEL AS RDS OBJECT----
model=readRDS(file = 'XGBoostModel.RDS',.GlobalEnv)

#IMPORT TEST DATA FRAME WITH RESULTS----
Data.Test<-read.csv("./Data/Data.Test.csv")
Data.Test<-as.data.frame(Data.Test)
df=Data.Test
df=df[!df$oldbalanceOrg %in% c(0), ]
rm(Data.Test)

#GENERATE RANDOM NAMES AND HARDCODED FOR DROP DOWN----
demoNames <- data.frame(Name = c("Dr. Rumack",
                                 "Roger Murdock",
                                 "Ted Striker",
                                 "Rex Cramer",
                                 "Clarence Oveur",
                                 "Father O'Flanagan",
                                 "Norman Alexander Gibbs",
                                 "Al White",
                                 "Donald Wedding"
))

namesList<-randomNames(89,
                       which.names="both",
                       name.order="first.last",
                       name.sep=" ",
                       sample.with.replacement=TRUE,
                       return.complete.data=FALSE)
namesList<-as.data.frame(namesList)
colnames(namesList)<-'Name'
namesList<-rbind(demoNames,namesList)
namesList[] <- lapply(namesList, as.character)


###################################################
#SHINY APP: USER INTERFACE----
###################################################

ui <- dashboardPage(
        dashboardHeader(disable = TRUE),
        dashboardSidebar(disable = TRUE),
        dashboardBody(
    
          fluidRow(
            valueBoxOutput("sourceOldBalanceOrg"),
            valueBoxOutput("savings"),
            valueBoxOutput("creditcard")),
          
          fluidRow(
            box(
              solidHeader = TRUE,background = "black",
                selectInput("nothing", "Outbound: Recipient/ Inbound: Sender ",choices = namesList),
                numericInput("amount",label="Enter Amount (kr)",min = 0,value = 0),
                  br(),
                  useShinyalert(),
                actionButton("Run_model", "Outbound: Cashout/Transfer",icon = icon("arrow-right")),
                  br(),
                  br(),
                actionButton("request", "Inbound: Request for Payment",icon = icon("arrow-left")),
                  br(),
                  br(),
                  useShinyjs(),                                           # Include shinyjs in the UI
                  extendShinyjs(text = jsResetCode),                      # Add the js code to the page
                actionButton("reset_button", "Refresh: New Transcaction",icon = icon("refresh"))
            )),
          fluidRow(
            mainPanel("(FOR TESTING//REMOVE FOR FINAL DEPLOYMENT) Predicted Value: Not Fraud (0) or Fraud (1)",
                      verbatimTextOutput('summary'))
          )))

###################################################
#SHINY APP: SERVER SIDE----
###################################################


server<- function(input,output,session){
  
  #GENERATE A RANDOM SINGLE TEST ROW FOR SCORING----
  shinyTranscactionData=df[sample(nrow(df), 1), ]
  shinyTranscactionData
  
  
  #GENERATE SAVINGS AND CREDIT CARD BALANCING FOR LOOKS----
  savings=sample(75143:1534124,1,replace=T)
  creditcard=sample(176:5768,1,replace=T)
  
  #VALUE BOX FOR CHECKING ACCOUNT----
  output$sourceOldBalanceOrg <- renderValueBox({
    valueBox(
      prettyNum(shinyTranscactionData$oldbalanceOrg,big.mark=","),
      "Member Checking Account (Ending 1234)",
      icon = icon("credit-card")
      ,color = "green"
    )
  })
  
  #VALUE BOX FOR SAVINGS ACCOUNT----
  output$savings <- renderValueBox({
    valueBox(
      prettyNum(savings,big.mark=","),
      "Member Share Savings Account (Ending 4321) ",
      icon = icon("credit-card")
      ,color = "purple"
    )
  })
  
  #VALUE BOX FOR SAVINGS ACCOUNT----
  output$creditcard <- renderValueBox({
    valueBox(
      prettyNum(creditcard,big.mark=","),
      "Member Visa Credit Card (Ending 9999)",
      icon = icon("credit-card")
      ,color = "red"
    )
  })
  
  #CREATE DATA FRAME FOR RECEIVING TEST INPUTS FROM USER INTERFACE FOR SCORING----
  test <- reactive({
    req(input$amount)
    
    # validate(
    #     need(input$amount != "", "Please input an amount."))
    
    #ALL INDEPENDENT VARIABLES----
    step <- as.numeric(shinyTranscactionData$step)
    amount <- as.numeric(input$amount)
    oldbalanceOrg <- as.numeric(shinyTranscactionData$oldbalanceOrg)
    newbalanceOrig <- as.numeric(shinyTranscactionData$newbalanceOrig)
    oldbalanceDest <- as.numeric(shinyTranscactionData$oldbalanceDest)
    newbalanceDest <- as.numeric(shinyTranscactionData$newbalanceDest)
    isFlaggedFraud<-as.numeric(shinyTranscactionData$isFlaggedFraud)
    typeCASH_OUT<-as.numeric(shinyTranscactionData$typeCASH_OUT)
    typeTRANSFER<-as.numeric(shinyTranscactionData$typeTRANSFER)
    
    #COMBINING NUMERIC FIELDS INTO DATAFRAME----
    test <- cbind(step,amount,oldbalanceOrg,newbalanceOrig,oldbalanceDest,newbalanceDest,isFlaggedFraud,typeCASH_OUT,typeTRANSFER)
    test <- as.data.frame(test)
    
    #EMPLY FIELD FOR DEPENDENT VARIABLES---
#    test$isFraud <- ""
#    test$isFraud<-as.numeric(test$isFraud)
#    test
    })
  
  #CREATE DATA FRAME FOR RECEIVING TEST INPUTS FROM USER INTERFACE FOR SCORING----
  exportTable <- reactive({
    req(input$amount)
    req(input$nothing)
    
    # validate(
    #     need(input$amount != "", "Please input an amount."))
    
    #ALL INDEPENDENT VARIABLES----
    nothing <- as.character(input$nothing)
    amount <- as.numeric(input$amount)

    #COMBINING NUMERIC FIELDS INTO DATAFRAME----
    exportTable <- cbind(nothing,amount)
    exportTable <- as.data.frame(exportTable)
    
  })
  
  
  #ACTION BUTTON FOR RUNNING MODEL FOR LIVE SCORING----
  # pred <- eventReactive(input$Run_model,{predict(model,type='response',newdata = as.matrix(test()))},ignoreNULL = FALSE)
  
  
  pred <-  eventReactive(input$Run_model,{
    if((input$amount == 0) & (input$amount>shinyTranscactionData$oldbalanceOrg)) {
      showModal(
        modalDialog(
          tags$p("Request Failed. Please correct the accordingly"),
          title = "ERROR FAILED"
        )
      )
    }
    req((input$amount) > 0 & (input$amount<=shinyTranscactionData$oldbalanceOrg),na.rm = TRUE)
        {predict(model,type='response',newdata = as.matrix(test()))}
    
  })

  
  #PAGE REFRESH----
  observeEvent(input$reset_button, {js$reset()})
  
##################################################  
# IN APP ALERTS-------------------------------  
##################################################
  #  Outbound: Cashout/Transfer ALERTS----  
  observeEvent(input$Run_model,
               if
                    (input$amount==0)
                      {shinyalert("REQUEST FAILED", "No amount entered. Please try again.", type = "warning")}
               else if 
                    (input$amount>shinyTranscactionData$oldbalanceOrg) 
                      {shinyalert("REQUEST FAILED", "Insufficient funds. Please try again", type = "warning")})
  
# Inbound: Request for Payment ALERTS----
  observeEvent(input$request,
               if
                    (input$amount==0)
                      {shinyalert("REQUEST FAILED", "No amount entered. Please try again.", type = "warning")}
               else if
                     (input$amount> 0)
                       {shinyalert("PENDING", "Your request for payment has been submitted and is pending.", type = "success")})
 
  
#Predicitve Model ALERTS----
  observeEvent(pred(),
                   if
                       (pred()<isFraudPredThreshold)
                                    {shinyalert("SUCCESS", "Your transaction has been submitted and is pending.",
                                    type = "success")}
                  else if
                       (pred()>isFraudPredThreshold)
                                    {shinyalert("ALERT","Possible fraudulent transaction detected. Please check your registered device to authorize.",
                                    type = "error")})
                    
#,callbackJS = "location.reload()

##################################################  
# EMAIL ALERTS-------------------------------  
###################################################
  
  #EMAIL ALERTS:IS FRAUD -----
  observeEvent(pred(),
              if
                 (pred()>isFraudPredThreshold & 
                 (input$amount<=shinyTranscactionData$oldbalanceOrg))  {

                     # x <- head(mtcars)
                   
                     x <- exportTable()
                     x <-data.frame(x)
                     colnames(x)<-c('Name','Amount')

                     y <- htmlTable(x, rnames = FALSE)
                     
                     logoImage = '<img src="/img.png" width = "500" height="80">'
                     emailButtonImage = '<img src="/email_button.png" width = "250" height="56">'
                     
                     
                     html_body <- paste0(
                       # logoImage,
                                         "<h2> Customer Protection Services</h2>
                                         <p>Dear Valued Member,
                                         <br>
                                         <br>
                                        For your protection and security reasons, your Member Checking
                                        Account (Ending 1234) has been locked. Please tell us if you or someone you
                                        authorized used this account for the following transaction:</p>",
                                         y,
                                         # emailButtonImage,
                                         "<p>We kindly ask you not reply to this e-mail but instead 
                                          contact us securely either by using the above action buttons or logging into
                                          your Shirley mobile app. Thank you.
                                         <br>
                                         <br>Regards,
                                         <br>
                                         <br>Your Shirley Team at Surely Detect Fraud</p>
                                         ")
                     

                     send.mail(from = addressFrom,
                               to = addressTo,
                               subject = subjectEmailIsFraud,
                               body = html_body,
                               html = TRUE,
                               smtp = list(host.name = "smtp.gmail.com", port = 465,
                                           user.name = addressFrom, passwd = "<PASSWORD>",
                                           ssl = TRUE),
                               authenticate = TRUE,
                               send = TRUE)})
  
  # #EMAIL ALERTS: NOT FRAUD -----
  # observeEvent(pred(),
  #              if
  #                 (pred()<=isFraudPredThreshold & 
  #                 (input$amount<=shinyTranscactionData$oldbalanceOrg))  {
  #                
  #                    send.mail(from = addressFrom,
  #                            to = c("XYZ@gmail.com"),
  #                            subject =subjectEmailIsNOTFraud,
  #                            body = "TRANSACTION PENDING",
  #                            smtp = list(host.name = "smtp.gmail.com", port = 465,
  #                                        user.name = addressFrom, passwd = "PASSWORD",
  #                                        ssl = TRUE),
  #                            authenticate = TRUE,
  #                            send = TRUE)})
  
  ##################################################  
  # TEXT ALERTS-------------------------------  
  ###################################################
    
  #TEXT MESSAGE ALERTS: IS FRAUD----
  # observeEvent(pred(),
  #               if
  #                  (pred()>isFraudPredThreshold)  {
  # 
  #                     send.mail(from = addressFrom,
  #                           to = textAlertList,
  #                           subject = subjectTexttIsFraud,
  #                           body = "Please log into your Shirley App to review recent activity.",
  #                           smtp = list(host.name = "smtp.gmail.com", port = 465,
  #                                       user.name = addressFrom, passwd = "PASSWORD",
  #                                       ssl = TRUE),
  #                           authenticate = TRUE,
  #                           send = TRUE)})
  # 
  #RENDER LIVESCORES FOR QA----
  output$summary <- renderPrint(pred()) #QA // REMOVE FOR FINAL
}

shinyApp(ui=ui, server=server)
