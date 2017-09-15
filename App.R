library(shiny)
library(MASS)
require(zoo)
library(reshape2)
library(plyr)
library(dplyr)
library(data.table)
library(caret)
library(caretEnsemble)
#library(kernlab)
require(randomForest)
require(doParallel)
library(nnet) 
library(bnlearn)
#library(Rgraphviz)
library(shinydashboard)
library(csvread)
library(arules)
#library(ggplot2)
options(error=NULL)
library(shinyjs)

##

ui <- dashboardPage(
  dashboardHeader(title = "HPE Healthcare Demo"),
  dashboardSidebar( width= 150,
                    sidebarMenu(
                      menuItem("HAR", tabName= "tabHAR", icon=icon("dashboard")),
                      menuItem("BAYES", tabName= "tabBayes", icon=icon("dashboard"), 
                               menuSubItem("Bayes 1", tabName= "tabBayes1", icon= icon("dashboard")),
                               menuSubItem("Bayes 2", tabName= "tabBayes2", icon= icon("dashboard"))),
                   
                      menuItem("Signature",
                               menuSubItem("HPE"),
                               menuSubItem("#QTpro"))
                      
                    )),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName="tabHAR",
              fluidPage(
                tags$style(type="text/css",
                           ".recalculating {opacity: 1.0;}"
                ),
                fluidRow(
                  column(4,
                         box(title= strong("Inputs"), status = "primary", width= 10,
                             
                             sliderInput("num1", "Accel_X:", min = -5, max = 5,  value = 0, step = .01),
                             sliderInput("num2", "Accel_Y:", min = -5, max = 5,  value = 0, step = .01),
                             sliderInput("num3", "Accel_Z:", min = -5, max = 5,  value = 0, step = .01),
                             sliderInput("g1", "GYRO_X:", min = -400, max = 400,  value = 0, step = .01),
                             sliderInput("g2", "GYRO_Y:", min = -400, max = 400,  value = 0, step = .01),
                             sliderInput("g3", "GYRO_Z:", min = -400, max = 400,  value = 0, step = .01),
                             sliderInput("a1", "ALT_RATE:", min = -100, max = 100,  value = 0, step = 1),
                             sliderInput("a2", "ALT_STEPPING_GAIN:", min = 0, max = 4500,  value = 0, step = 1),
                             sliderInput("a3", "ALT_STEPPING_GAIN_LOSS:", min = 0, max = 4500,  value = 0, step = 1),
                             sliderInput("a4", "ALT_STEPPING_ASCENDED:", min = 0, max = 1000,  value = 0, step = 1),
                             sliderInput("a5", "ALT_STEPPING_DESCENDED:", min = 0, max = 1000,  value = 0, step = 1),
                             sliderInput("a6", "ALT_TOTAL_GAIN:", min = 0, max = 15000,  value = 0, step = 1),
                             sliderInput("a7", "ALT_TOTAL_LOSS:", min = 0, max = 15000,  value = 0, step = 1)
                             
                             
                         )),
                  column(8,
                         fluidRow(
                           box(title= "PREDICTIONS", status= "success", solidHeader = TRUE, width=6,
                               textOutput("text"),
                               textOutput("text1"),
                               textOutput("text2"),
                               strong(h3(textOutput("text3")))
                               
                           ),
                           
                           box(title= "ACCEL", status= "warning", solidHeader = TRUE, width=6,
                               plotOutput("plot1")
                               
                           )),#fluidrow
                         
                         fluidRow(
                           box(title= "GYRO", status= "warning", solidHeader = TRUE, width=6,
                               plotOutput("plot2")
                               
                           ),
                           box(title= "ALTI", status= "warning", solidHeader = TRUE,width=6,
                               plotOutput("plot3")
                               
                           ))#fluidrow
                         
                  ) #column 
                  
                )) #fluidrow #fluidpage
      ),
      
      tabItem(tabName= "tabBayes1",
              
              fluidPage(
                tags$style(type="text/css",
                           ".recalculating {opacity: 1.0;}"
                ),
                fluidRow( 
                  column(3,
                         box(title= strong("Inputs"), status = "primary", width =12,
                             
                             
                             radioButtons("ACT1", "Activity:", 
                                          c("Sitting" = "SITTING",
                                            "Walking"= "WALKING",
                                            "Walking Up" = "WALKING_UP",
                                            "Walking Down" = "WALKING_DOWN",
                                            "NA"= "NA",
                                            "Probability" = "P"), 
                                          selected = "P"), 
                             br(),     
                             radioButtons("HR1", "Heart Rate:", 
                                          c("Out of Bounds (low)" = 0,
                                            "Low (64-72)" = "[64,72]",
                                            "Medium (73-75)" = "(72,75]",
                                            "Medium-High (76-79)" = "(75,79]",
                                            "High (80-108)" = "(79,108]",
                                            "Out of Bounds (high)"= 0,
                                            "NA"= "NA",
                                            "Probability" = "P"),
                                          selected = "[64,72]"),
                             br(),
                             
                             radioButtons("RR1", "Respiratory Interval:", 
                                          c("Out of Bounds (low)" = 0,
                                            "Low (.0332-.647)" = "[0.0332,0.647]",
                                            "Medium (.648-.763)" = "(0.647,0.763]",
                                            "Medium-High (.764-.846)" = "(0.763,0.846]",
                                            "High (.847-1.48)" = "(0.846,1.48]",
                                            "Out of Bounds (high)"= 0,
                                            "NA"= "NA",
                                            "Probability" = "P"),
                                          selected = "[0.0332,0.647]"),
                             br(),
                             
                             radioButtons("GSR1", "Galvanic Skin Response:", 
                                          c("Out of Bounds (low)" = 0,
                                            "Low (390-1400)" = "[390,1.4e+03]",
                                            "Medium (1401-4270)" = "(1.4e+03,4.27e+03]",
                                            "Medium-High (4271-9160)" = "(4.27e+03,9.16e+03]",
                                            "High (9161-340000)" = "(9.16e+03,3.4e+05]",
                                            "Out of Bounds (high)"= 0,
                                            "NA"= "NA", 
                                            "Probability" = "P"),
                                          selected = "[390,1.4e+03]"),
                             br(),
                             
                             radioButtons("SKN1", "Skin Temperature:", 
                                          c("Out of Bounds (low)" = 0,
                                            "Low (29.1-29.8)" = "[29.1,29.8]",
                                            "Medium (29.9-30.7)" = "(29.8,30.7]",
                                            "Medium-High (30.8-31.3)" = "(30.7,31.3]",
                                            "High (31.4-31.5)" = "(31.3,31.5]",
                                            "Out of Bounds (high)"= 0,
                                            "NA"= "NA",
                                            "Probability" = "P"),
                                          selected = "[29.1,29.8]")
                         )),
                  
                  
                  column(9,
                         fluidRow(
                           box(title= "Probabilities (INTRA-metric)", status= "warning", solidHeader = TRUE, width =12,
                               column(8,
                                      plotOutput("probPlot")), 
                               column(4,
                                      box(status= "primary", width = 12,
                                          radioButtons("loopProb", "Select View:", 
                                                       c("ACTIVITY" = 1,
                                                         "GALVANIC SKIN RESPONSE" = 2,
                                                         "SKIN TEMPERATURE" = 3,
                                                         "HEART RATE" = 4,
                                                         "RESPIRATORY RATE INTERVAL" = 5), selected = 1 ))
                                      
                               ) #column
                           )))))), #box #row
      ###############################################
      
      tabItem(tabName= "tabBayes2",
              
              fluidPage(
                useShinyjs(),
                tags$style(type="text/css",
                           ".recalculating {opacity: 1.0;}"
                ),
                fluidRow( 
                  column(3,
                         box(title= strong("Inputs"), status = "primary", width =12,
                             
                             
                             radioButtons("ACT", "Activity:", 
                                          c("Sitting" = "SITTING",
                                            "Walking"= "WALKING",
                                            "Walking Up" = "WALKING_UP",
                                            "Walking Down" = "WALKING_DOWN",
                                            "NA"= "NA",
                                            "Probability" = "P"), 
                                          selected = "P"), 
                             br(),     
                             radioButtons("HR", "Heart Rate:", 
                                          c("Out of Bounds (low)" = 0,
                                            "Low (64-72)" = "[64,72]",
                                            "Medium (73-75)" = "(72,75]",
                                            "Medium-High (76-79)" = "(75,79]",
                                            "High (80-108)" = "(79,108]",
                                            "Out of Bounds (high)"= 0,
                                            "NA"= "NA",
                                            "Probability" = "P"),
                                          selected = "[64,72]"),
                             br(),
                             
                             radioButtons("RR", "Respiratory Interval:", 
                                          c("Out of Bounds (low)" = 0,
                                            "Low (.0332-.647)" = "[0.0332,0.647]",
                                            "Medium (.648-.763)" = "(0.647,0.763]",
                                            "Medium-High (.764-.846)" = "(0.763,0.846]",
                                            "High (.847-1.48)" = "(0.846,1.48]",
                                            "Out of Bounds (high)"= 0,
                                            "NA"= "NA",
                                            "Probability" = "P"),
                                          selected = "[0.0332,0.647]"),
                             br(),
                             
                             radioButtons("GSR", "Galvanic Skin Response:", 
                                          c("Out of Bounds (low)" = 0,
                                            "Low (390-1400)" = "[390,1.4e+03]",
                                            "Medium (1401-4270)" = "(1.4e+03,4.27e+03]",
                                            "Medium-High (4271-9160)" = "(4.27e+03,9.16e+03]",
                                            "High (9161-340000)" = "(9.16e+03,3.4e+05]",
                                            "Out of Bounds (high)"= 0,
                                            "NA"= "NA", 
                                            "Probability" = "P"),
                                          selected = "[390,1.4e+03]"),
                             br(),
                             
                             radioButtons("SKN", "Skin Temperature:", 
                                          c("Out of Bounds (low)" = 0,
                                            "Low (29.1-29.8)" = "[29.1,29.8]",
                                            "Medium (29.9-30.7)" = "(29.8,30.7]",
                                            "Medium-High (30.8-31.3)" = "(30.7,31.3]",
                                            "High (31.4-31.5)" = "(31.3,31.5]",
                                            "Out of Bounds (high)"= 0,
                                            "NA"= "NA",
                                            "Probability" = "P"),
                                          selected = "[29.1,29.8]")
                         )),
                  
                  
                  column(9,
                         fluidRow(  
                           box(title= "Probabilities (INTER-metric) Grid", status= "warning", solidHeader = TRUE, width=12,
                               
                               column(8,
                                      plotOutput("probPlot2")), 
                               column(4,
                                      fluidRow(
                                        box(status= "primary", width = 12,
                                            
                                            sliderInput("sleepy", "Speed (Zero for fastest):", min=0, max=5, value = 0, step=.01),
                                            
                                            sliderInput("TIME", "Time Window (Note: Curently only for show):", min = 0, max = 500,  value = 1, step = 1),
                                            
                                            actionButton("loopy", label = "START"),
                                            
                                            actionButton("loopyStop", label = "PAUSE"),
                                            
                                            actionButton("loopyResume", label = "RESUME"),
                                            
                                            actionButton("loopyRestart", label = "RESET")
                                            
                                        )),
                                      fluidRow(
                                        valueBoxOutput("counter", width=12)
                                        
                                      )
                                      
                               )  #   
                               
                               
                               
                           ) #big box
                         ), #row
                         fluidRow(
                           
                           box(title= "SLIDING AGGREGATE NOVELTY RATE & INDIVIDUAL EVENT VALUES GRID", status= "danger", solidHeader=TRUE, width=12, 
                               fluidRow(
                                 column(8,
                                        plotOutput("novplot")  
                                        
                                 ),
                                 column(4,
                                        fluidRow(
                                          br(),
                                          br(),
                                          box(status = "primary", width=12,
                                              sliderInput("thresh", "Event Probability Threshold:", min=0, max=1, value = .15, step=.005)
                                          )),
                                        
                                        fluidRow(
                                          br(),
                                          valueBoxOutput("rates", width=12)
                                          
                                        )
                                        
                                        
                                 )), #column #fluidrow
                               
                               fluidRow(
                                 
                                 column(6,
                                        
                                        plotOutput("novplothr")
                                        
                                 ),#column 
                                 
                                 column(6,
                                        
                                        plotOutput("novplotrri")
                                        
                                 ) #column
                                 
                               ),#fluidrow
                               
                               fluidRow(
                                 
                                 column(6,
                                        
                                        plotOutput("novplotgsr")
                                        
                                 ),#column 
                                 
                                 column(6,
                                        
                                        plotOutput("novplotskn")
                                        
                                 ) #column
                                 
                               ) #fluidrow
                           )#box
                           
                         ))  #fluidrow #column
                  
                  
                  
                ) )) #fluidrow  #fluidpage #tabitem
      
     
      
    ) #tab items
  )#dashboard body
)

###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################

server <- function(input, output, session) {
  
  
  Q <- reactive(predict(model.Accel, newdata = data.frame(ACCEL_X = input$num1,ACCEL_Y = input$num2, ACCEL_Z = input$num3)))
  S <- reactive(predict(model.Gyro, newdata = data.frame(GYRO_X = input$g1, GYRO_Y = input$g2, GYRO_Z = input$g3)))
  TE <- reactive(predict(model.Alti, newdata = data.frame(ALT_RATE = input$a1, ALT_STEPPING_GAIN = input$a2, ALT_STEPPING_GAIN_LOSS = input$a3, ALT_STEPPING_ASCENDED = input$a4, ALT_STEPPING_DESCENDED = input$a5, ALT_TOTAL_GAIN = input$a6, ALT_TOTAL_LOSS = input$a7)))
  
  #ACCEL
  output$text <- renderText({
    
    paste("Accel Prediction:", Q()) 
    
  }) 
  
  #GYRO
  output$text1 <- renderText({
    
    paste("Gyro Prediction:", S() ) 
    
  }) 
  
  #ALTI
  output$text2 <- renderText({
    
    paste("Alti Prediction:", TE() ) 
    
  }) 
  
  #FINAL
  output$text3 <- renderText({
    
    paste("OVERALL PREDICTION:", as.character(predict(model.ensemble, newdata = data.frame(results.Accel = Q(), results.Gyro = S(), results.Alti = TE())))) 
    
  }) 
  
  
  #histogram accel
  output$plot1 <- renderPlot({
    
    barplot(c(ACCEL_X = input$num1,ACCEL_Y = input$num2, ACCEL_Z = input$num3), col = c("violet", "orange", "blue"))  
    
  }) 
  
  
  #histogram gyro
  output$plot2 <- renderPlot({
    
    barplot(c(GYRO_X = input$g1, GYRO_Y = input$g2, GYRO_Z = input$g3), col = c("violet", "orange", "blue"))  
    
  }) 
  
  
  #histogram alti
  output$plot3 <- renderPlot({
    
    barplot(c(ALT_RATE = input$a1, ALT_STEPPING_GAIN = input$a2, ALT_STEPPING_GAIN_LOSS = input$a3, ALT_STEPPING_ASCENDED = input$a4, ALT_STEPPING_DESCENDED = input$a5, ALT_TOTAL_GAIN = input$a6, ALT_TOTAL_LOSS = input$a7), col = c("violet", "orange", "blue", "green", "red", "turquoise", "yellow"))  
    
  }) 
  
  
  
  
  #bayes stuff
  observe({
    input$ACT1
    input$HR1
    input$RR1
    input$GSR1
    input$SKN1
    
    isolate({
      output$probPlot <- renderPlot({
        
        
        
        eve <<- reactive({
          
          allinput <<- data.frame(Activity = input$ACT1, HR_RATE = input$HR1, RRI_INTERVAL = input$RR1, GSR_RESISTANCE = input$GSR1, SKTMP_TEMPERATURE = input$SKN1, stringsAsFactors = FALSE)
          
          allinputs <<- allinput[which(!(allinput %in% "NA"))] 
          allinputs <<- allinputs[which(!(allinputs %in% c("P")))] 
          eventss<<- paste("(", names(allinputs), "=='",
                           sapply(allinputs[1,], as.character), "')",
                           sep = "", collapse = " & ")
          
          
          probinputs <<- allinput[which(allinput %in% c("P"))]
          binPick <<- groupPick(probinputs) 
          allcpquery <<- ldply(.data = binPick, .fun = function(x){
            eventz <<- paste("(", names(probinputs), "=='", as.character(x), "')", sep = "")
            queryresults <<- cpquery(fitted, event = eval(parse(text = eventz)), evidence = eval(parse(text = eventss)) )
            return(queryresults)
          })
          
          qresults<<- t(allcpquery)
          colnames(qresults) <- binPick
          
          max_val <<- which.is.max(qresults)
          text_col <<- c(rep("black", length(qresults)))
          text_col[max_val] <<- "red"
          
          return(qresults)
          
          
          
        }) 
        
        
        barp <- barplot(c(eve()),ylim =c(0,1),col=c("blue", "violet", "orange", "green"), names.arg= colnames(eve()), main = "PROBABILITIES", ylab= "Probability")
        text(barp, y= eve()+.05, round(eve(), 2), col = text_col)
        #text(barp, y= eve()+.05, round(eve(), 2))
        
        
      })
      
    })})    
  
  
  
  #########################################################################################################
  ##########################################################################################################
  
  
  
  observeEvent(input$loopy,{
    
    disable("loopy")
    
    maxIter <- 500
    
    
    vals<- reactiveValues(counter=1, pausecounter=1, novdetect = c(0), novplotsave = c(0), lloppy = 0, vlinesHR = c(0), vlinesGSR = c(0),vlinesSKN = c(0), vlinesRRI = c(0))
    
    
    
    
    
    
    output$counter <- renderValueBox({
      valueBox(
        
        paste0(round(vals$counter/maxIter * 100, 1), "%"),"PROGRESS:",
        icon= icon("list") ,col= "purple" 
      )
    })
    
    
    
    
    observe({ 
      
      
      isolate({
        
        threshinput <- input$thresh
        
        ii <- vals$counter
        updateSliderInput(session, "TIME","Time Window (Note: Curently only for show):", min = 0, max = maxIter,  value = ii, step = 1)
        
        
        if(input$loopProb == 1){
          updateRadioButtons(session, "ACT1", "Activity:", 
                             c("Sitting" = "SITTING",
                               "Walking"= "WALKING",
                               "Walking Up" = "WALKING_UP",
                               "Walking Down" = "WALKING_DOWN",
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected = "P")
          #### temporary code for second plot
          
          
          updateRadioButtons(session, "GSR1", "Galvanic Skin Response:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (390-1400)" = "[390,1.4e+03]",
                               "Medium (1401-4270)" = "(1.4e+03,4.27e+03]",
                               "Medium-High (4271-9160)" = "(4.27e+03,9.16e+03]",
                               "High (9161-340000)" = "(9.16e+03,3.4e+05]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA", 
                               "Probability" = "P"),
                             selected =  as.character(data[ii,2]) )
          
          updateRadioButtons(session, "SKN1", "Skin Temperature:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (29.1-29.8)" = "[29.1,29.8]",
                               "Medium (29.9-30.7)" = "(29.8,30.7]",
                               "Medium-High (30.8-31.3)" = "(30.7,31.3]",
                               "High (31.4-31.5)" = "(31.3,31.5]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  as.character(data[ii,3]) )
          
          updateRadioButtons(session, "HR1","Heart Rate:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (64-72)" = "[64,72]",
                               "Medium (73-75)" = "(72,75]",
                               "Medium-High (76-79)" = "(75,79]",
                               "High (80-108)" = "(79,108]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  as.character(data[ii,4]) )
          
          updateRadioButtons(session, "RR1", "Respiratory Interval:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (.0332-.647)" = "[0.0332,0.647]",
                               "Medium (.648-.763)" = "(0.647,0.763]",
                               "Medium-High (.764-.846)" = "(0.763,0.846]",
                               "High (.847-1.48)" = "(0.846,1.48]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  as.character(data[ii,5]) )
        }
        
        if(input$loopProb == 2){
          updateRadioButtons(session, "ACT1", "Activity:", 
                             c("Sitting" = "SITTING",
                               "Walking"= "WALKING",
                               "Walking Up" = "WALKING_UP",
                               "Walking Down" = "WALKING_DOWN",
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected = as.character(data[ii,1]))
          
          updateRadioButtons(session, "GSR1", "Galvanic Skin Response:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (390-1400)" = "[390,1.4e+03]",
                               "Medium (1401-4270)" = "(1.4e+03,4.27e+03]",
                               "Medium-High (4271-9160)" = "(4.27e+03,9.16e+03]",
                               "High (9161-340000)" = "(9.16e+03,3.4e+05]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA", 
                               "Probability" = "P"),
                             selected =  "P" )
          
          updateRadioButtons(session, "SKN1", "Skin Temperature:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (29.1-29.8)" = "[29.1,29.8]",
                               "Medium (29.9-30.7)" = "(29.8,30.7]",
                               "Medium-High (30.8-31.3)" = "(30.7,31.3]",
                               "High (31.4-31.5)" = "(31.3,31.5]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  as.character(data[ii,3]) )
          
          updateRadioButtons(session, "HR1","Heart Rate:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (64-72)" = "[64,72]",
                               "Medium (73-75)" = "(72,75]",
                               "Medium-High (76-79)" = "(75,79]",
                               "High (80-108)" = "(79,108]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  as.character(data[ii,4]) )
          
          updateRadioButtons(session, "RR1", "Respiratory Interval:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (.0332-.647)" = "[0.0332,0.647]",
                               "Medium (.648-.763)" = "(0.647,0.763]",
                               "Medium-High (.764-.846)" = "(0.763,0.846]",
                               "High (.847-1.48)" = "(0.846,1.48]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  as.character(data[ii,5]) )
        }
        
        if(input$loopProb == 3){
          updateRadioButtons(session, "ACT1", "Activity:", 
                             c("Sitting" = "SITTING",
                               "Walking"= "WALKING",
                               "Walking Up" = "WALKING_UP",
                               "Walking Down" = "WALKING_DOWN",
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected = as.character(data[ii,1]))
          
          updateRadioButtons(session, "GSR1", "Galvanic Skin Response:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (390-1400)" = "[390,1.4e+03]",
                               "Medium (1401-4270)" = "(1.4e+03,4.27e+03]",
                               "Medium-High (4271-9160)" = "(4.27e+03,9.16e+03]",
                               "High (9161-340000)" = "(9.16e+03,3.4e+05]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA", 
                               "Probability" = "P"),
                             selected =  as.character(data[ii,2]) )
          
          updateRadioButtons(session, "SKN1", "Skin Temperature:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (29.1-29.8)" = "[29.1,29.8]",
                               "Medium (29.9-30.7)" = "(29.8,30.7]",
                               "Medium-High (30.8-31.3)" = "(30.7,31.3]",
                               "High (31.4-31.5)" = "(31.3,31.5]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  "P" )
          
          updateRadioButtons(session, "HR1","Heart Rate:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (64-72)" = "[64,72]",
                               "Medium (73-75)" = "(72,75]",
                               "Medium-High (76-79)" = "(75,79]",
                               "High (80-108)" = "(79,108]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  as.character(data[ii,4]) )
          
          updateRadioButtons(session, "RR1", "Respiratory Interval:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (.0332-.647)" = "[0.0332,0.647]",
                               "Medium (.648-.763)" = "(0.647,0.763]",
                               "Medium-High (.764-.846)" = "(0.763,0.846]",
                               "High (.847-1.48)" = "(0.846,1.48]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  as.character(data[ii,5]) )
        }
        
        if(input$loopProb == 4 ){
          updateRadioButtons(session, "ACT1", "Activity:", 
                             c("Sitting" = "SITTING",
                               "Walking"= "WALKING",
                               "Walking Up" = "WALKING_UP",
                               "Walking Down" = "WALKING_DOWN",
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected = as.character(data[ii,1]))
          
          updateRadioButtons(session, "GSR1", "Galvanic Skin Response:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (390-1400)" = "[390,1.4e+03]",
                               "Medium (1401-4270)" = "(1.4e+03,4.27e+03]",
                               "Medium-High (4271-9160)" = "(4.27e+03,9.16e+03]",
                               "High (9161-340000)" = "(9.16e+03,3.4e+05]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA", 
                               "Probability" = "P"),
                             selected =  as.character(data[ii,2]) )
          
          updateRadioButtons(session, "SKN1", "Skin Temperature:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (29.1-29.8)" = "[29.1,29.8]",
                               "Medium (29.9-30.7)" = "(29.8,30.7]",
                               "Medium-High (30.8-31.3)" = "(30.7,31.3]",
                               "High (31.4-31.5)" = "(31.3,31.5]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  as.character(data[ii,3]) )
          
          updateRadioButtons(session, "HR1","Heart Rate:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (64-72)" = "[64,72]",
                               "Medium (73-75)" = "(72,75]",
                               "Medium-High (76-79)" = "(75,79]",
                               "High (80-108)" = "(79,108]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  "P" )
          
          updateRadioButtons(session, "RR1", "Respiratory Interval:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (.0332-.647)" = "[0.0332,0.647]",
                               "Medium (.648-.763)" = "(0.647,0.763]",
                               "Medium-High (.764-.846)" = "(0.763,0.846]",
                               "High (.847-1.48)" = "(0.846,1.48]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  as.character(data[ii,5]) )
        }
        
        if(input$loopProb == 5){
          updateRadioButtons(session, "ACT1", "Activity:", 
                             c("Sitting" = "SITTING",
                               "Walking"= "WALKING",
                               "Walking Up" = "WALKING_UP",
                               "Walking Down" = "WALKING_DOWN",
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected = as.character(data[ii,1]))
          
          updateRadioButtons(session, "GSR1", "Galvanic Skin Response:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (390-1400)" = "[390,1.4e+03]",
                               "Medium (1401-4270)" = "(1.4e+03,4.27e+03]",
                               "Medium-High (4271-9160)" = "(4.27e+03,9.16e+03]",
                               "High (9161-340000)" = "(9.16e+03,3.4e+05]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA", 
                               "Probability" = "P"),
                             selected =  as.character(data[ii,2]) )
          
          updateRadioButtons(session, "SKN1", "Skin Temperature:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (29.1-29.8)" = "[29.1,29.8]",
                               "Medium (29.9-30.7)" = "(29.8,30.7]",
                               "Medium-High (30.8-31.3)" = "(30.7,31.3]",
                               "High (31.4-31.5)" = "(31.3,31.5]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  as.character(data[ii,3]) )
          
          updateRadioButtons(session, "HR1","Heart Rate:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (64-72)" = "[64,72]",
                               "Medium (73-75)" = "(72,75]",
                               "Medium-High (76-79)" = "(75,79]",
                               "High (80-108)" = "(79,108]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  as.character(data[ii,4]) )
          
          updateRadioButtons(session, "RR1", "Respiratory Interval:", 
                             c("Out of Bounds (low)" = 0,
                               "Low (.0332-.647)" = "[0.0332,0.647]",
                               "Medium (.648-.763)" = "(0.647,0.763]",
                               "Medium-High (.764-.846)" = "(0.763,0.846]",
                               "High (.847-1.48)" = "(0.846,1.48]",
                               "Out of Bounds (high)"= 0,
                               "NA"= "NA",
                               "Probability" = "P"),
                             selected =  "P" )
        }
        
        updateRadioButtons(session, "ACT", "Activity:", 
                           c("Sitting" = "SITTING",
                             "Walking"= "WALKING",
                             "Walking Up" = "WALKING_UP",
                             "Walking Down" = "WALKING_DOWN",
                             "NA"= "NA",
                             "Probability" = "P"),
                           selected = as.character(data[ii,1]))
        
        updateRadioButtons(session, "GSR", "Galvanic Skin Response:", 
                           c("Out of Bounds (low)" = 0,
                             "Low (390-1400)" = "[390,1.4e+03]",
                             "Medium (1401-4270)" = "(1.4e+03,4.27e+03]",
                             "Medium-High (4271-9160)" = "(4.27e+03,9.16e+03]",
                             "High (9161-340000)" = "(9.16e+03,3.4e+05]",
                             "Out of Bounds (high)"= 0,
                             "NA"= "NA", 
                             "Probability" = "P"),
                           selected =  as.character(data[ii,2]))
        
        updateRadioButtons(session, "SKN", "Skin Temperature:", 
                           c("Out of Bounds (low)" = 0,
                             "Low (29.1-29.8)" = "[29.1,29.8]",
                             "Medium (29.9-30.7)" = "(29.8,30.7]",
                             "Medium-High (30.8-31.3)" = "(30.7,31.3]",
                             "High (31.4-31.5)" = "(31.3,31.5]",
                             "Out of Bounds (high)"= 0,
                             "NA"= "NA",
                             "Probability" = "P"),
                           selected =  as.character(data[ii,3]) )
        
        updateRadioButtons(session, "HR","Heart Rate:", 
                           c("Out of Bounds (low)" = 0,
                             "Low (64-72)" = "[64,72]",
                             "Medium (73-75)" = "(72,75]",
                             "Medium-High (76-79)" = "(75,79]",
                             "High (80-108)" = "(79,108]",
                             "Out of Bounds (high)"= 0,
                             "NA"= "NA",
                             "Probability" = "P"),
                           selected =  as.character(data[ii,4]) )
        
        updateRadioButtons(session, "RR", "Respiratory Interval:", 
                           c("Out of Bounds (low)" = 0,
                             "Low (.0332-.647)" = "[0.0332,0.647]",
                             "Medium (.648-.763)" = "(0.647,0.763]",
                             "Medium-High (.764-.846)" = "(0.763,0.846]",
                             "High (.847-1.48)" = "(0.846,1.48]",
                             "Out of Bounds (high)"= 0,
                             "NA"= "NA",
                             "Probability" = "P"),
                           selected =  as.character(data[ii,5]) )
        
        
        
        allin<<- data[ii,c(1,4,5,2,3)]
        #allin <<- data.frame(Activity = isolate(input$ACT), HR_RATE = isolate(input$HR), RRI_INTERVAL = isolate(input$RR), GSR_RESISTANCE = isolate(input$GSR), SKTMP_TEMPERATURE = isolate(input$SKN), stringsAsFactors = FALSE)
        #allin[which(allin %in% "P")] <<- as.character(data[isolate(vals$counter), which(allin %in% "P")])
        
        allinp1 <<- allin[2:5]  
        evi1<<- paste("(", names(allinp1), "=='",
                      sapply(allinp1[1,], as.character), "')",
                      sep = "", collapse = " & ")
        event1 <<- paste("(", names(allin[1]), "=='", as.character(allin[,1]), "')", sep = "")
        queryresult1 <<- cpquery(fitted, event = eval(parse(text = event1)), evidence = eval(parse(text = evi1)) )
        
        
        allinp2<<- allin[c(1,3:5)]
        evi2<<- paste("(", names(allinp2), "=='",
                      sapply(allinp2[1,], as.character), "')",
                      sep = "", collapse = " & ")
        event2 <<- paste("(", names(allin[2]), "=='", as.character(allin[,2]), "')", sep = "")
        queryresult2 <<- cpquery(fitted, event = eval(parse(text = event2)), evidence = eval(parse(text = evi2)) )
        
        
        allinp3<<- allin[c(1,2,4,5)]
        evi3<<- paste("(", names(allinp3), "=='",
                      sapply(allinp3[1,], as.character), "')",
                      sep = "", collapse = " & ")
        event3 <<- paste("(", names(allin[3]), "=='", as.character(allin[,3]), "')", sep = "")
        queryresult3 <<- cpquery(fitted, event = eval(parse(text = event3)), evidence = eval(parse(text = evi3)) )
        
        
        allinp4<<- allin[c(1,2,3,5)]
        evi4<<- paste("(", names(allinp4), "=='",
                      sapply(allinp4[1,], as.character), "')",
                      sep = "", collapse = " & ")
        event4 <<- paste("(", names(allin[4]), "=='", as.character(allin[,4]), "')", sep = "")
        queryresult4 <<- cpquery(fitted, event = eval(parse(text = event4)), evidence = eval(parse(text = evi4)) )
        
        
        allinp5<<- allin[c(1,2,4,5)]
        evi5<<- paste("(", names(allinp5), "=='",
                      sapply(allinp5[1,], as.character), "')",
                      sep = "", collapse = " & ")
        event5 <<- paste("(", names(allin[5]), "=='", as.character(allin[,5]), "')", sep = "")
        queryresult5 <<- cpquery(fitted, event = eval(parse(text = event5)), evidence = eval(parse(text = evi5)) )
        
        
        qres<<- c(queryresult1, queryresult2, queryresult3, queryresult4, queryresult5)
        
        barcolors <<-c("blue","blue", "blue", "blue", "blue") 
        whichcols <<- which(qres < threshinput)
        
        barcolors[whichcols]<<- "red"
        
        
        
        
        output$probPlot2 <- renderPlot({
          
          
          barp2 <- barplot(qres, ylim =c(0,1),col=barcolors, names.arg= c(as.character(originalData[vals$counter,1]), paste("HR", as.character(originalData[vals$counter,4]), sep=" "), paste("RRI", as.character(originalData[vals$counter,5]), sep=" "), paste("GSR",as.character(originalData[vals$counter,2]),sep=" "), paste("SKN", as.character(originalData[vals$counter,3]), sep=" ")), main = "Probailities of Individual Events Given All Others", ylab= "Probability")
          text(x= barp2, y= qres+.05, round(qres, 2), col = "dark red")
          
          
        })
        
        
        
        if(queryresult2 < threshinput){
          vals$vlinesHR <- c(vals$vlinesHR, ii)
        }
        if(queryresult3 < threshinput){
          vals$vlinesRRI <- c(vals$vlinesRRI, ii)
        }
        if(queryresult4 < threshinput){
          vals$vlinesGSR <- c(vals$vlinesGSR, ii)
        }
        if(queryresult5 < threshinput){
          vals$vlinesSKN <- c(vals$vlinesSKN, ii)
        }
        
        
        
        if(length(qres[which(qres < as.numeric(threshinput))])>0){
          
          addit<- c(1)
          
        }
        if(length(qres[which(qres < as.numeric(threshinput))])==0){
          
          addit<- c(0)
        }
        
        vals$novdetect <- c(vals$novdetect, addit)
        
        
        if(ii >= 50){
          
          
          novydetect <- vals$novdetect[(ii-48):(ii+1)]
          novVal <<- round((sum(novydetect) / 50) * 100, 1)
          
          if(novVal >= 50){
            novValcol <<- c("red")
            novValicon <<- c("exclamation-triangle")
          }
          if(novVal < 50){
            novValcol <<- c("green")
            novValicon <<- c("thumbs-up")
          }
          
          
          
          vals$novplotsave <- c(vals$novplotsave, novVal)
          output$rates <- renderValueBox({
            
            valueBox( 
              
              paste0(novVal, "%"),"CURRENT RATE:",
              icon= icon(novValicon) ,col= novValcol 
            )
            
            
            
          })
          
          output$novplot <- renderPlot({
            
            plot(x= 49:(48+length(vals$novplotsave)), y=vals$novplotsave, type = "l", col="red", lwd=2 , xlab="Time", ylab= "Rate", main= "NOVELTY RATE", ylim= c(0,100))
          })
          
          
          
          
        }
        
        
        
        
        if(vals$pausecounter <= 50){
          ylimits <<- 1
        }
        
        if(vals$pausecounter > 50){
          ylimits <<- ii - 50
        }
        
        hr <- originalData[1:ii, 4]
        rri <- originalData[1:ii, 5]
        gsr <- originalData[1:ii, 2]
        skn <- originalData[1:ii, 3]
        
        
        
        output$novplothr <- renderPlot({
          
          plot(hr, type = "l", col="red", lwd=3 , xlab="Time", ylab= "HR", main= "Heart Rate", ylim = c(30,180))
          abline(v= vals$vlinesHR, col= "gray94", lwd=2, lty= 2)
          lines(hr, type = "l", col="red", lwd=3)
        })
        
        output$novplotrri <- renderPlot({
          
          plot(rri, type = "l", col="red", lwd=3 , xlab="Time", ylab= "RRI", main= "Respiratory Rate Interval", ylim = c(0,2))
          abline(v= vals$vlinesRRI, col= "gray94", lwd=2, lty= 2)
          lines(rri, type = "l", col="red", lwd=3)
        })
        
        output$novplotgsr <- renderPlot({
          
          plot(gsr, type = "l", col="red", lwd=3 , xlab="Time", ylab= "GSR", main= "Galvanic Skin Response", ylim= c(3000,7000))
          abline(v= vals$vlinesGSR, col= "gray94", lwd=2, lty= 2)
          lines(gsr, type = "l", col="red", lwd=3)
        })
        
        output$novplotskn <- renderPlot({
          
          plot(skn, type = "l", col="red", lwd=3 , xlab="Time", ylab= "SKN", main= "Skin Temperature", ylim = c(29.5,31.5))
          abline(v= vals$vlinesSKN, col= "gray94", lwd=2, lty= 2)
          lines(skn, type = "l", col="red", lwd=3)
        })
        
        
        
        
        vals$counter <- vals$counter + 1
        vals$pausecounter <- vals$pausecounter + 1
        
        Sys.sleep(as.numeric(input$sleepy))
        
        
        
        
        
        
      })
      
      
      
      
      
      
      
      if(input$loopyStop== input$loopy){
        
        maxIter <- 0
        
      } 
      
      
      if(input$loopyResume == input$loopyStop){
        maxIter <- 500
        
      }
      
      if(input$loopyStop > input$loopyResume){
        maxIter <- 0
        
      }
      
      
      if(input$loopyRestart > isolate(vals$lloppy)){
        
        vals$lloppy <- isolate(vals$lloppy) + 1
        vals$counter <- 1
        vals$pausecounter<- 1
        vals$novdetect <- 0
        vals$novplotsave <- 0
        vals$vlinesHR <- 0
        vals$vlinesGSR <- 0
        vals$vlinesSKN <- 0
        vals$vlinesRRI <- 0
        
        
        
      } 
      
      if(input$loopyRestart <= isolate(vals$lloppy)){
        if (isolate(vals$counter) < maxIter){
          invalidateLater(0, session) }
      }
      
      
      
      
      
      
      
    })  
    
    
    
    
    
    
    
    
  }) 
  
  ##############################################################
  
} #end server code



shinyApp(ui=ui, server=server)


#QTpro
#QTpro
##HPE
##HPE

