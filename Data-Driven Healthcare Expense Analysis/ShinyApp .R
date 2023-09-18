
# loading the packages

library(tidyverse)
library(imputeTS)
library(caret)
library(kernlab)
library(ggplot2)
library(rpart)
library(dplyr)
library(caret)

# getting the csv data from the url
hmo <- read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv")
# looking at the structure
str(hmo)

# repairing the missing values
hmo$bmi <- na_interpolation(hmo$bmi)
hmo$hypertension <- na_interpolation(hmo$hypertension)

# creating a column in hmo called expensive
hmo$expensive <- as.factor(hmo$cost >= 4775)

# creating the age category
hmo$agecategory[18<=hmo$age & hmo$age<=34] <- "Young-adults"
hmo$agecategory[35<=hmo$age & hmo$age<=50] <- "Middle-aged"
hmo$agecategory[51<=hmo$age & hmo$age<=66] <- "older-adults"

# creating the bmi category
hmo$bmicategory[hmo$bmi < 18 ] <- "Underweight"
hmo$bmicategory[hmo$bmi >= 18 & hmo$bmi < 25  ] <- "Healthy"
hmo$bmicategory[hmo$bmi >= 25 & hmo$bmi < 30  ] <- "Overweight"
hmo$bmicategory[hmo$bmi >= 30 & hmo$bmi < 40  ] <- "Obese"
hmo$bmicategory[hmo$bmi >= 40 & hmo$bmi < 65  ] <- "Extremely Obese"

# creating the training set and the testing set
set.seed(687)

trainList <- createDataPartition(y=hmo$expensive,p=.67,list=FALSE)
training <- hmo[trainList,]
testing <- hmo[-trainList,]

# training the model
svm.model1 <- train(expensive ~ age+bmi+children+smoker+location+exercise+education_level+location_type+married+yearly_physical+gender+hypertension, 
                    data = training,
                    method = "svmRadial",
                    trControl=trainControl(method = "none"),
                    preProcess = c("center", "scale"))

svmPred1 <- predict(svm.model1, testing)



# loading the shiny packages
library(shiny)
library(shinydashboard)


ui <-dashboardPage(
        dashboardHeader(title = "Healthcare Cost"),
        dashboardSidebar(
          sidebarMenu(id = "sidebar",
                      menuItem("Dataset", tabName = "data", icon = icon("file-csv")),
                      menuItem("Result", tabName = "res", icon = icon("chart-simple")),
                      menuItem("Confusion Matrix", tabName = "conmat", icon = icon("chart-simple")),
                      menuItem("Sensitivity", tabName = "sen", icon = icon("chart-simple")),
                      menuItem("Map", tabName = "map", icon = icon("chart-column")),
                      menuItem("Boxplot", tabName = "bp", icon = icon("chart-column")),
                      menuItem("Histograms", tabName = "hist", icon = icon("chart-column")),
                      menuItem("Scatter Plots", tabName = "scatterplot", icon = icon("chart-column"))
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(tabName = "data", 
                    tabBox(id="t1", width = 12,
                           tabPanel("About", icon=icon("address-card"),
                                    fluidRow(
                                      column(width = 12, tags$p("Insert your test file down and open result section to view results")),
                                      column(width = 12, fileInput("file1", "Choose CSV File", accept=c('text/csv', 
                                                                                                        'text/comma-separated-values,text/plain', 
                                                                                                        '.csv'))
                                      )
                                            )
                                    ), 
                           tabPanel("Data", tableOutput("dataT"), icon = icon("table")), 
                           tabPanel("Structure", verbatimTextOutput("structure"), icon=icon("uncharted"))
                           )
                    
            ),
            
            tabItem(tabName = "res",
                    tabBox(id="t2", width= 12,
                           tabPanel("Result",verbatimTextOutput("result"), icon=icon("uncharted"),
                                    fluidRow(
                                      column(width = 12, tags$p("TRUE means this person has a higher cost and vice versa")),
                                      column(width = 12, tags$p("People who are smoker and not active exercising having higher cost")))
                           
                    ))),
            
            tabItem(tabName = "conmat",
                    tabBox(id="t3", width= 12,
                           tabPanel("Matrix",verbatimTextOutput("mat"), icon=icon("uncharted"))
                           
                    )),
            tabItem(tabName = "sen",
                    tabBox(id="t4", width= 12,
                           tabPanel("Sensitivity",verbatimTextOutput("Sens"), icon=icon("uncharted"),
                                    fluidRow(
                                      column(width = 12, tags$p("Insert solution file to get sensitivity")),
                                      column(width = 12, fileInput("file2", "Choose CSV File", accept=c('text/csv', 
                                                                                                        'text/comma-separated-values,text/plain', 
                                                                                                        '.csv')))
                                             
                                            )
                                     )
                           )
                   ),
            
            tabItem(tabName = "map",
                    tabBox(id="t5", width= 12,
                           tabPanel("Map",plotOutput("map"), icon=icon("chart-pie"),
                                    fluidRow(
                                      column(width = 12, tags$p("")))
                           
                    ))),
            
            tabItem(tabName = "bp",
                    tabBox(id="t6", width= 12,
                           tabPanel("state", plotOutput("st"),icon=icon("chart-column")),
                           tabPanel("age", plotOutput("ag"),icon=icon("chart-column")),
                           tabPanel("bmi", plotOutput("bm"),icon=icon("chart-column")),
                           tabPanel("exercise",plotOutput("ex"), icon=icon("chart-column")),
                           tabPanel("smoker",plotOutput("sm"), icon=icon("chart-column")),
                           tabPanel("children number", plotOutput("ch"),icon=icon("chart-column")),
                           tabPanel("hypertension", plotOutput("hy"),icon=icon("chart-column")),
                           tabPanel("gender",plotOutput("ge"), icon=icon("chart-column")),
                           tabPanel("married",plotOutput("ma"),icon=icon("chart-column")),
                           tabPanel("locationtype",plotOutput("lo"),icon=icon("chart-column")),
                           tabPanel("education",plotOutput("ed"), icon=icon("chart-column")),
                           tabPanel("physical",plotOutput("ph"), icon=icon("chart-column"))
                           
                           
                    )),
            tabItem(tabName = "hist",
                    tabBox(id="t7", width= 12,
                           tabPanel("cost",plotOutput("coh"), icon=icon("chart-pie")),
                           tabPanel("smoker_average",plotOutput("smh"), icon=icon("chart-pie")),
                           tabPanel("smoker_percentage",plotOutput("smph"), icon=icon("chart-pie")),
                           tabPanel("state", plotOutput("sth"),icon=icon("chart-pie")),
                           tabPanel("exercise", plotOutput("exh"),icon=icon("chart-pie")),
                           tabPanel("age_average",plotOutput("agh"),icon=icon("chart-pie")),
                           tabPanel("age_percentage",plotOutput("agph"),icon=icon("chart-pie")),
                           tabPanel("bmi",plotOutput("bmh"),icon=icon("chart-pie"))
                           
                           
                    )),
            tabItem(tabName = "scatterplot",
                    tabBox(id="t8", width= 12,
                           tabPanel("age",plotOutput("spab"), icon=icon("chart-pie"))
                    ))
          )
        )
)
        




server <- function(input, output){

  # upload a csv
  mydata <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath)
  })
  
  mydata2 <- reactive({
    file <- input$file2
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath)
  })
  
  # output the data, predict result, and structure
        output$dataT <- renderTable({
        req(mydata)
        mydata()
      })
      output$result <- renderPrint({
        svm_t <- predict(svm.model1 ,mydata(), type="raw")
        svm_t
      })
      output$structure <- renderPrint({
        str(mydata())
      })
      
      
    # box plot by state
      output$st <- renderPlot({
        box_state <- ggplot(hmo)+aes(x=cost,y=location)+geom_boxplot()
        box_state
        })
    
    # box plot by age
      output$ag <- renderPlot({
        box_age <- ggplot(hmo)+aes(x=cost,y=agecategory)+geom_boxplot()
        box_age
      })
      
    # box plot by bmi
      output$bm <- renderPlot({
        box_bmi <- ggplot(hmo)+aes(x=cost,y=bmicategory)+geom_boxplot()
        box_bmi
      })
      
    # box plot by exercise
      output$ex <- renderPlot({
        box_exercise <- ggplot(hmo) + aes(x=cost,y=exercise)+geom_boxplot()
        box_exercise
      })
    
    # box plot by smoker
      output$sm <- renderPlot({
        box_smoker <- ggplot(hmo) + aes(x=cost,y=smoker)+geom_boxplot()
        box_smoker
      })
      
    # box plot by children number
      output$ch <- renderPlot({
        box_children <- ggplot(hmo)+aes(x=as.factor(children),y=cost)+geom_boxplot()
        box_children
      })
      
    # box plot by hypertension
      output$hy <- renderPlot({
        box_hypertension <- ggplot(hmo)+aes(x=as.factor(hypertension),y=cost)+geom_boxplot()
        box_hypertension
      })  
    
    # box plot by gender
      output$ge <- renderPlot({
        box_gender <- ggplot(hmo) + aes(x=cost,y=gender)+geom_boxplot()
        box_gender
      })
      
    # box plot by marriage
      output$ma <- renderPlot({
        box_married <- ggplot(hmo) + aes(x=cost,y=married)+geom_boxplot()
        box_married
      })
      
    # box plot by location type
      output$lo <- renderPlot({
        box_locationtype <- ggplot(hmo) + aes(x=cost,y=location_type)+geom_boxplot()
        box_locationtype
      })
      
    # box plot by education level
      output$ed <- renderPlot({
        box_education_level <- ggplot(hmo) + aes(x=cost,y=education_level)+geom_boxplot()
        box_education_level
      })
      
    # box plot by yearly physical
      output$ph <- renderPlot({
        box_physical <- ggplot(hmo) + aes(x=cost,y=yearly_physical)+geom_boxplot()
        box_physical
      })
      
    # confulsionMatrix
      output$mat <- renderPrint({
       confusionMatrix(svmPred1,testing$expensive)
      })
      
    # hist of the cost
      output$coh <- renderPlot({
        bar_cost <- hist(hmo$cost)
        bar_cost
      })
      
    # histogram by smoker
      output$smh <- renderPlot({
        smomean <- hmo %>%
          group_by(smoker) %>%
          summarise(mean_cost = mean(cost))
        
        bar_smo <- ggplot(smomean, aes(x=smoker, y=mean_cost, fill=smoker)) + geom_bar(stat="identity")+theme_minimal() + theme(legend.position = "none")+
          xlab("Smoker")+ylab("Mean Cost")+ ggtitle("Visualizing mean cost with smoker or non-smoker")  
        bar_smo
      })
      
    # hist by smoker percentage
      output$smph <- renderPlot({
        SMOKER <- hmo[hmo$smoker=="yes",]
        expensivePercentageSMOKER <- sum(SMOKER$expensive=="TRUE")/nrow(SMOKER)
        
        NON_SMOKER <- hmo[hmo$smoker=="no",]
        expensivePercentageNON_SMOKER <- sum(NON_SMOKER$expensive=="TRUE")/nrow(NON_SMOKER)
        
        
        SmokerX <- c("SMOKER","NON_SMOKER")
        SmokerExpensivePercentage <- c(expensivePercentageSMOKER,
                                       expensivePercentageNON_SMOKER)
        
        SmokerexpensivePercentageDF <- data.frame(SmokerX,SmokerExpensivePercentage)
        
        bar12 <- ggplot(SmokerexpensivePercentageDF, aes(x=SmokerX, y=SmokerExpensivePercentage)) + geom_bar(stat="identity") 
        bar12
      })
      
      
      
    # hist by state    
      output$sth <- renderPlot({
        statemean <- hmo %>%
          group_by(location) %>%
          summarise(mean_cost = mean(cost))
        
        bar_state <- ggplot(statemean, aes(x=location, y=mean_cost, fill=location)) +
          geom_bar(stat="identity")+theme_minimal() + geom_jitter(width=0.15)+
          theme(axis.text.x = element_text(angle = 45, hjust=1)) + theme(legend.position = "none")+
          xlab("States")+ylab("Mean Cost")+ ggtitle("Visualizing mean cost with states")
        bar_state
      })
      
    # hist by age
      output$agh <- renderPlot({
        agemean <- hmo %>%
          group_by(agecategory) %>%
          summarise(mean_cost = mean(cost))
        bar_age <- ggplot(agemean, aes(x=agecategory, y=mean_cost, fill=agecategory)) + geom_bar(stat="identity")+theme_minimal() + theme(legend.position = "none")+
          xlab("Age categories")+ylab("Mean Cost")+ ggtitle("Visualizing mean cost with age categories")  
        bar_age})
        
    # hist by age percentage
      output$agph <- renderPlot({
        YA <- hmo[hmo$agecategory=="Young-adults",]
        expensivePercentageYA <- sum(YA$expensive=="TRUE")/nrow(YA)
        
        MidA <- hmo[hmo$agecategory=="Middle-aged",]
        expensivePercentageMidA <- sum(MidA$expensive=="TRUE")/nrow(MidA)
        
        OA <- hmo[hmo$agecategory=="older-adults",]
        expensivePercentageOA <- sum(OA$expensive=="TRUE")/nrow(OA)
        
        AgeX <- c("YA","MidA","OA")
        AgeexpensivePercentage <- c(expensivePercentageYA,
                                    expensivePercentageMidA,
                                    expensivePercentageOA)
        
        AgeexpensivePercentageDF <- data.frame(AgeX,AgeexpensivePercentage)
        
        bar11 <- ggplot(AgeexpensivePercentageDF, aes(x=AgeX, y=AgeexpensivePercentage)) + geom_bar(stat="identity") 
        bar11
      })
      
    # hist by bmi
      output$bmh <- renderPlot({
        bmimean <- hmo %>%
          group_by(bmicategory) %>%
          summarise(mean_cost = mean(cost))
        bar_bmi <- ggplot(bmimean, aes(x=bmicategory, y=mean_cost, fill=bmicategory)) + geom_bar(stat="identity")+theme_minimal() + theme(legend.position = "none")+
          xlab("Bmi categories")+ylab("Mean Cost")+ ggtitle("Visualizing mean cost with bmi categories")  
        bar_bmi
      })
      
      
      
    # hist by exercise
      output$exh <- renderPlot({
        exemean <- hmo %>%
          group_by(exercise) %>%
          summarise(mean_cost = mean(cost))
        bar_exe <- ggplot(exemean, aes(x=exercise, y=mean_cost, fill=exercise)) + geom_bar(stat="identity")+theme_minimal() + theme(legend.position = "none")+
          xlab("Exercise")+ylab("Mean Cost")+ ggtitle("Visualizing mean cost with exercise")  
        bar_exe
      })
      
  # map
      output$map <- renderPlot({
        
        stateHmo <- hmo %>%
          group_by(location) %>%
          summarise(meanCost = mean(cost))
        
        stateHmo <- data.frame(stateHmo)
        stateHmo$state <- tolower(stateHmo$location)
        
        us <- map_data("state")
        mergeHmo <- merge(stateHmo,us,by.x="state",by.y="region",all.x=T)
        mergeHmo <- mergeHmo %>% arrange(order)
        mapHmo <- ggplot(mergeHmo) +
          geom_polygon(color="black",aes(x=long,y=lat,group=group,fill=meanCost)) +
          coord_map()
        mapHmo
      })
      
    # sensitivity
      output$Sens <- renderPrint({
        expensive_predicted <- predict(svm.model1 ,mydata(), type="raw")
        actual_expensive <- mydata2()$expensive 
        conf_Mar <- table(expensive_predicted, actual_expensive)
        sensitivity(conf_Mar)
      })
      
    # scatter plot
      output$spab <- renderPlot({
        scatterplot_bmi <- ggplot(hmo, aes(x=bmi, y=cost)) + geom_point()
        scatterplot_bmi
      })
  }

shinyApp(ui, server)
