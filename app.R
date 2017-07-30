#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes) 
library(animation)
library(latticeExtra)
library(lubridate)
library(ggbeeswarm)
library(gridExtra)
library(data.table)
library(viridis)
library(ggbeeswarm)
library(choroplethr)
library(choroplethrMaps)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Climate change in US continent"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Choose your interested States and Year"),
      selectInput("State_name",
                  label="Your interested State",
                  choices = c("New York","California", "Florida", "Illinois", "Texas"),
                  selected = "New York"), 
      
      sliderInput("Year_num",
                  "Interested Year",
                  min = 1743,
                  max = 2013,
                  value = 2000,
                  step = 1), 
      
      sliderInput("Year_range", "Range of year:",
                  min = 1743, max = 2013, value = c(1850,2012), step = 1),
      helpText("Note: while the plot view will show only be updated"),
      submitButton("Update View")
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Ave by Year", plotOutput("aveTplot_1")), 
        tabPanel("Ave by Month", plotOutput("aveTplot_2")),
        tabPanel("cool and hot", plotOutput("aveTplot_3")),
        tabPanel("Temp variation", plotOutput("aveTplot_4")),
        tabPanel("Heat map", plotOutput("aveTplot_5")),
        tabPanel("Temp change", plotOutput("aveTplot_6")),
        tabPanel("Spatial Temp", plotOutput("aveTplot_7"))
      )
    )
  )
)
)

# Define server logic required to draw a histogram
setwd("E:\\Study\\Applied analytics\\4336\\ToolBox Assignment")
## Main data treatment
cData = read.csv("climate-change-earth-surface-temperature-data/GlobalLandTemperaturesByState.csv")
##Then I choose the United States, remove Hawaii and Alaska, and I separate the date into Year, Month and Day.
cData %>%
  filter(Country=="United States")  %>%
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) ->cData
cData<-na.omit(cData)

cData %>%
  filter(State!="Hawaii" & State!="Alaska") -> cData1
# Remove na's
cData1 = na.omit(cData1)

server <- shinyServer(function(input, output) {
#1. PLOT 1
    # show the temperature variation of different states
  output$aveTplot_1 <- renderPlot({
    cData1 %>% 
      filter(State==input$State_name) %>%
      filter(Year> input$Year_range[1] & Year<input$Year_range[2]) %>%
      group_by(Year) %>% 
      summarise(Temp = mean(AverageTemperature)) ->cData2
    # draw the plots of state temperature
    ggplot(data=cData2,aes(x=Year, y=Temp))+ geom_point(aes(colour=Temp))+stat_smooth(method = "loess")+ggtitle(paste("Average Temperature 1743-2013 in", input$State_name))
  })
  
#2. PLOT 2  
  # extracting US's average temperature month-wise & year_wise
    # month-on-month change in US's average temperature from 1743 to 2013
  output$aveTplot_2 <- renderPlot({
    US_temp_1<- cData1 %>%
      filter(State==input$State_name) %>%
      filter(Year> input$Year_range[1] & Year<input$Year_range[2]) %>%
      group_by(Year,Month) %>%
      summarise(ave_Temp=mean(AverageTemperature))
    US_temp_1$date = paste0("01-",as.character(US_temp_1$Month),"-",as.character(US_temp_1$Year))
    US_temp_1$date = as.Date(US_temp_1$date, "%d-%m-%Y")
    ggplot(data=US_temp_1, aes(x = date, y = ave_Temp)) + geom_line() + geom_smooth(method="lm",size = 2) +
      xlab("Years") + ylab("Average Monthly Temperature") + 
      theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13)) +
      ggtitle(paste("Average Monthly Temperature Trend of the hottest and coolest states in",input$State_name, "in Year", input$Year_range[1], "-",input$Year_range[2]))
  })
  
#3. PLOT3  
  output$aveTplot_3 <- renderPlot({
    # State hottest and coolest temperature trend
    state_temp<-cData1 %>%
      group_by(State, Year, Month) %>%
      summarise(avg_Temp = mean(AverageTemperature)) %>%
      filter(Month ==1)
    
    state_temp= as.data.table(state_temp)
    state_temp= state_temp[!is.na(avg_Temp),]  
    
    # State-wise comparison of average January month average temperature, for the years 1950 and 2013
    ggplot(data=state_temp[state_temp$Year == input$Year_num,], aes(x = reorder(State, avg_Temp), y = avg_Temp, group = as.factor(Year), colour = as.factor(Year))) +
      labs(colour = "Year") + 
      theme(legend.title = element_text(size = 13, face="bold")) +
      theme(legend.text = element_text(size = 13)) +
      geom_line(size = 1) + xlab("Coldest to Hottest States") + ylab("Average Temperature") + 
      theme(axis.title = element_text(size = 15, face = "bold"), axis.text.y = element_text(size = 13), axis.text.x = element_blank()) +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(colour = "gray25"), 
            panel.border = element_blank())+
      ggtitle(paste("State-wise Change in Temperature in", input$Year_num)) + 
      theme(plot.title = element_text(size = 13, lineheight=.8, face="bold"))
  })

#4. PLOT 4
# Temperature variation and comparison with 2013
  output$aveTplot_4 <- renderPlot({
  cData7<-cData1  %>% 
    filter( Year==input$Year_num | Year==1854 | Year==1900 | Year==1950 | Year==2013)
  cData7$Year<-as.factor(cData7$Year)
  ggplot(data=cData7, aes(x=Year, y=AverageTemperature,color=AverageTemperature)) + 
    geom_quasirandom(size=5) + scale_colour_viridis(option = "C")+
    ggtitle(paste("Temperature variation in", input$Year_num)) + 
    theme(plot.title = element_text(size = 13, lineheight=.8, face="bold"))
})

#5. PLOT 5
  output$aveTplot_5 <- renderPlot({
    cData8 <- cData1 %>%
      filter(State ==input$State_name) %>%
      filter(Year> input$Year_range[1] & Year<input$Year_range[2]) %>%
      group_by(State, Month) %>%
      summarise(avg_Temp = mean(AverageTemperature))
    
    ggplot(cData8, aes(x = Month, y = State, fill = avg_Temp, frame = State)) +
      geom_tile(color = "white", size = 0.1) +
      scale_fill_gradient(name = "Average Temperature",low = "white", high = "red") +
      coord_equal() +
      labs(x = "Months", y = "", title = "Average Temp in the selected year range") +
      theme(axis.ticks = element_blank()) +
      theme(axis.text = element_text(size = 14)) +
      theme(plot.title = element_text(size = 15)) +
      theme(legend.title = element_text(size = 15)) +
      theme(legend.text = element_text(size = 10))+
      ggtitle(paste("Heat map for", input$State_name))+ 
      theme(plot.title = element_text(size = 13, lineheight=.8, face="bold"))
})

#6. PLOT 6
  # Temperature variation
  output$aveTplot_6 <- renderPlot({ 
  cData6<-cData1  %>% 
    filter(Year> input$Year_range[1] & Year<input$Year_range[2]) %>%
    filter(State==input$State_name)
  
  cData6$Month<-as.factor(cData6$Month)
  ggplot(data=cData6, aes(x=Month, y=AverageTemperature,color=AverageTemperature))+
    geom_quasirandom() + scale_colour_viridis()+
    ggtitle(paste("Temperature variation in", input$State_name, "in Year", input$Year_range[1], "-",input$Year_range[2])) + 
    theme(plot.title = element_text(size = 13, lineheight=.8, face="bold"))
  
  })  

#7. PLOT 7
  # Spatial temperature distribution
  output$aveTplot_7 <- renderPlot({
    ## I organize the data to get a data frame for a state choropleth and print the maps for 1850 and 2013.
    # Changing Georgia (State)
    cData$State <- as.character(cData$State)
    cData$State[cData$State=="Georgia (State)"] <- "Georgia"
    cData$State<- as.factor(cData$State)                    
    
    # select columns of interest
    cData %>% 
      select(Year,AverageTemperature,State) %>%
      group_by(Year,State) %>%
      summarise(value=mean(AverageTemperature))-> cData4
    
    #Data frame must have a column named region (all lower case) and another one value.
    colnames(cData4)[2]<- "region"
    cData4$region<-tolower(cData4$region)
    
    cData4 %>%
      filter(Year==input$Year_num) -> cData4_1
    cData4_1<-cData4_1[,2:3]
    
   
    print(state_choropleth(cData4_1,
                           title=paste("Land Temperature in Year", input$Year_num), 
                           num_colors = 8,
                           legend="Degrees"),reference_map=TRUE)
    
  })
  
    
}) # For input output main function

# Run the application 
shinyApp(ui = ui, server = server)

