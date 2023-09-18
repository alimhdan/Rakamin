library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)

url="https://raw.githubusercontent.com/alimhdan/Rakamin/main/Study%20Case%20-%20Data.csv"
data <-read.csv(url,sep=";")
data$first_contact=as.Date(data$first_contact,"%m/%d/%Y")
data$checkout_date=as.Date(data$checkout_date,"%m/%d/%Y")
data$paid_date=as.Date(data$paid_date,"%m/%d/%Y")

#ui
ui<- dashboardPage(
  dashboardHeader(title = "PBI Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        title = "Overview Dashboard",
        width = 12,
        infoBoxOutput(outputId = "transaksi",
                      width = 4),
        infoBoxOutput(outputId = "umur",
                      width = 4),
        infoBoxOutput(outputId = "lt",
                      width = 4)
      )
    ),
    fluidRow(
      box(
        width = 4,
        plotlyOutput(outputId = "kota")),
      box(
        width = 4,
        plotlyOutput(outputId = "pkrj")),
      box(
        width = 4,
        plotlyOutput(outputId = "chn")),
      ),
    fluidRow(
      box(
        width = 12,
        plotlyOutput(outputId = "plot"))
    )
  )
)



#server
server <- function(input,output){
  output$transaksi<-renderInfoBox({
    n=count(data)
    infoBox(
      title = "TRANSACTION",
      subtitle = "Number of Transaction",
      value = n,
      icon = icon("money-check"),
      color = "green",
      fill= TRUE
    )
  })
  output$umur<-renderInfoBox({
    modus <- function(x) {
      uniqx <- unique(x)
      uniqx[which.max(tabulate(match(x, uniqx)))]
    }
    mod=modus(data$age)
    infoBox(
      title = "AGE (Years)",
      subtitle = "Mode of Age",
      value = mod,
      icon = icon("user"),
      color = "red",
      fill= TRUE
    )
  })
  output$lt<-renderInfoBox({
    lt=data$paid_date-data$first_contact
    nlt=ceiling(mean(lt))
    infoBox(
      title = "Lead Time (Days)",
      subtitle = "Average Lead Time (First Contact Until Paid)",
      value = nlt,
      icon = icon("clock"),
      color = "blue",
      fill= TRUE
    )
  })
  output$kota<-renderPlotly({
    city = data %>% count(city)
    
    #Brachart
    plot_ly(city,
            x=~n,
            y=~city,
            style = "bar",
            color = I("#98FB98")) %>%
      layout(yaxis=list(title="City",categoryorder="total ascending"),
             xaxis=list(title = "Count"),
             title = "Bar Chart of City")
  })
  output$pkrj<-renderPlotly({
    occ=data %>% count(occupation)
    #Brachart
    plot_ly(occ,
            x=~n,
            y=~occupation,
            style = "bar",
            color = I("#E9967A")) %>%
      layout(yaxis=list(title="Occupation",categoryorder="total ascending"),
             xaxis=list(title = "Count"),
             title = "Bar Chart of Occupation")
  })
  output$chn<-renderPlotly({
    channel=data %>% count(channel)
    #Brachart
    plot_ly(channel,
            x=~n,
            y=~channel,
            style = "bar",
            color = I("#B0E0E6")) %>%
      layout(yaxis=list(title="Channel",categoryorder="total ascending"),
             xaxis=list(title = "Count"),
             title = "Bar Chart of Channel")
  })
  output$plot<-renderPlotly({
    week0 = as.Date(cut(data$first_contact,
                        breaks = "week",
                        start.on.monday = T))
    
    data_week0 = data %>% mutate(Week = week0)
    data_week00 = data_week0 %>% count(Week)
    
    week1 = as.Date(cut(data$checkout_date,
                        breaks = "week",
                        start.on.monday = T))
    
    data_week1 = data %>% mutate(Week = week1)
    data_week11 = data_week1 %>% count(Week)
    
    week2 = as.Date(cut(data$paid_date,
                        breaks = "week",
                        start.on.monday = T))
    
    data_week2 = data %>% mutate(Week = week2)
    data_week22 = data_week2 %>% count(Week)
    pertama=full_join(data_week00,data_week11,by="Week")
    kedua=full_join(pertama,data_week22,by="Week")
    kedua1<- kedua %>% mutate(n.x = ifelse(is.na(n.x)==T,0,n.x),
                              n.y = ifelse(is.na(n.y)==T,0,n.y),
                              n = ifelse(is.na(n)==T,0,n))
    fig <- plot_ly(kedua1, type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~Week, y = ~n.x,name="First Contact",fill = "tozeroy",color="#FA8072")%>%
      add_trace(x=~Week, y=~n.y,name="Checkout",fill = "tozeroy",color="#FFA500") %>%
      add_trace(x=~Week,y=~n,name="Paid",fill = "tozeroy",color="#32CD32") %>%
      layout(title = "Transaction Graph per Week",
             showlegend = T,
             yaxis=list(title="Count"))
    
    fig
  })
}


#running App
shinyApp(ui,server)


