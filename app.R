#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(dplyr)
library(stringr)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Female Enrollment Rate of Education"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput(inputId = "year",
                     label="Time Span",
                     min = 1986,
                     max = 2018,
                     value = c(1986,2018))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput(outputId = "ggplot")
      )
   )
)

# Define server logic required to draw a ggplot
server <- function(input, output) {
setwd("C:/Users/huang/Desktop/midterm_project_615")
data<-read.csv("education_gender.csv",header = TRUE)
n<-colnames(data)
nn<-n[5:37]
data1<-gather(data,year,value,nn)
data2<-unite(data1,"varname",Series,Series.Code, sep="-",remove = TRUE)
data3<-spread(data2,varname,value)
data3$year<-as.numeric(str_sub(data3$year,2,5))
a<-c("country_name","country_code","year","pre_primary_both","pre_primary_female","primary_female","secondary_both","secondary_female","tertiary_both","tertiary_female")
colnames(data3)<-a
data4<-data3%>%group_by(year)%>%mutate(total_pre_edu_ratio=(sum(as.numeric(pre_primary_female)))/(sum(as.numeric(pre_primary_both))),total_secondary_ratio=(sum(as.numeric(secondary_female)))/(sum(as.numeric(secondary_both))),total_tertiary_ratio=(sum(as.numeric(tertiary_female)))/(sum(as.numeric(tertiary_both))))

data5<-data4%>%mutate(pre_edu_ratio=as.numeric(pre_primary_female)/as.numeric(pre_primary_both),secondary_ratio=as.numeric(secondary_female)/as.numeric(secondary_both),tertiary_ratio=as.numeric(tertiary_female)/as.numeric(tertiary_both))
  total<-data5[,c(3,11:13)]
  totalrev<-gather(total,totaledu,ratio,-year)
  
  output$ggplot <- renderPlot({
    
  df1 <- reactive(subset(totalrev,totalrev$year<input$year[2] & totalrev$year>input$year[1]))
  test <- (df1[,"year"])
  p1 <- ggplot() + aes(test)
  p1+geom_point(data=df1(),aes(year,ratio,color=totaledu))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

