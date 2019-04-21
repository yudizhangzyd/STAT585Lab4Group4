library(tidyverse)
library(ggrepel)
library(shiny)
library(lubridate)

iowaliquor <- unzip("../data/story-sales.zip", junkpaths = T, exdir = "data")
iowaliquor <- read_csv("../data/Iowa_Liquor_Sales-Story.csv")

iowaliquor <- iowaliquor %>% 
    mutate(longlat = str_extract(`Store Location`, pattern = ".*?\\(.*?\\)"),
           lat = as.numeric(str_replace(longlat, "\\((.*?),\\s*.*\\)", "\\1")),
           long = as.numeric(str_replace(longlat, ".*?,\\s*(.*)\\)", "\\1"))) %>%
    select(-longlat) %>%
    dplyr::filter(lat < 42.3, long > -93.8) %>%
    dplyr::filter(lat > 41.8, long < -93.2) %>%
    mutate(City = tolower(City))

storedata <- iowaliquor %>%
    mutate(`Store Name` = tolower(`Store Name`)) %>%
    group_by(`Store Name`) %>%
    dplyr::filter(!is.na(`Sale (Dollars)`)) %>%
    summarise(n = n(), 
              volume_liters = sum(`Volume Sold (Liters)`),
              sale_dollars = sum(`Sale (Dollars)`), 
              long = mean(long), 
              lat = mean(lat),
              City = City[1]) %>%
    ungroup() %>%
    arrange(desc(n))

iowaliquor <- iowaliquor %>% mutate(Date = lubridate::mdy(Date))%>% 
  mutate(Date = format(as.Date(Date), "%Y"))

storedatayear <- iowaliquor %>%
  group_by(City,`Date`) %>% 
  dplyr::filter(!is.na(`Sale (Dollars)`)) %>%
  summarise(n = n(), 
            volume_liters = sum(`Volume Sold (Liters)`),
            sale_dollars = sum(`Sale (Dollars)`))%>%
  ungroup() 

ui <- navbarPage(
  theme = "yeti",
  tags$title(" "),
  
  div(
    tags$header(p("Spatial snd temporal analysis of liquor sale in Iowa", style="font-size:40px"),
                p("group 4", style="font-size:30px")),
    align = "center", style="color:#ffffff; background-color: #4d728d"),
  
  tabPanel("Temporal",
           sidebarLayout(sidebarPanel(
             selectInput("city", "City",
                         choices = c("ames", "story city",
                                     "nevada", "huxley",
                                     "slater", "cambridge",
                                     "maxwell", "colo", "roland"), 
                         selected = "ames")),
             mainPanel(tabsetPanel(
               tabPanel("Sales_dollars_by_year_and_city",
                        plotOutput("Sales_dollars_by_year_and_city")),
               tabPanel("Sales_volume_by_year_and_city",
                        plotOutput("Sales_volume_by_year_and_city")))))),
  
  tabPanel("Spatial",
           sidebarLayout(sidebarPanel(
             selectInput("city", "City",
                         choices = c("ames", "story city",
                                     "nevada", "huxley",
                                     "slater", "cambridge",
                                     "maxwell", "colo", "roland"), 
                         selected = "ames")),
             mainPanel(
               tabsetPanel(
                 tabPanel("Stroy County Liquor Sales", 
                          plotOutput("Stroy_County_Liquor_Sales")),
                 tabPanel("City Liquor Sales",
                          plotOutput("City_Liquor_Sales")),
                 tabPanel("City Top Five Liquor Sales' Stores",
                          plotOutput("City_Liquor_Sales_T5")),
                 tabPanel("City Liquor Sales Income in Each Stores",
                          plotOutput("City_Liquor_Sales_income"))
                 ))
           ))
)

server <- function(input, output) {
  
  output$Sales_dollars_by_year_and_city <- renderPlot({
    storedatayear %>% dplyr::filter(City == input$city) %>%
      ggplot(aes(x = Date, y = sale_dollars)) + 
      geom_point() + 
      ggtitle(paste0(input$city, "Sale dollars by year and city"))
    
  })
  
  output$Sales_volume_by_year_and_city <- renderPlot({
    storedatayear %>% dplyr::filter(City == input$city) %>%
      ggplot(aes(x = Date, y = volume_liters)) + 
      geom_point() + 
      ggtitle(paste0(input$city, "Sale volume by year and city"))
    
  })
 
   output$Stroy_County_Liquor_Sales <- renderPlot({
    story <- map_data("county") %>%
      dplyr::filter(subregion == "story")
    story %>%
      ggplot() + 
      geom_path(aes(x = long, y = lat, group = group)) + 
      geom_count(data = iowaliquor, aes(x = long, y =lat, color = City), alpha = 0.5) +
      ggtitle("Story County Liquor Sales Volume") + 
      theme_light()
  })
  
  output$City_Liquor_Sales <- renderPlot({
    ggplot() +
      geom_count(data = iowaliquor %>% dplyr::filter(City == input$city), 
                 aes(x = long, y =lat), alpha = 0.5, color = "pink2") +
      ggtitle(paste0(input$city, "Liquor Sales Volume"))
  })
  
  output$City_Liquor_Sales_T5 <- renderPlot({
    ggplot(data = iowaliquor %>% dplyr::filter(City == input$city),
           aes(x = long, y =lat)) +
      geom_count(alpha = 0.5, color = "pink2") +
      geom_label_repel(data = storedata %>% dplyr::filter(City == input$city) %>% head(n = 5), 
                       aes(label = `Store Name`), 
                       hjust = 1, vjust = -7) +
      ggtitle(paste0(input$city, "#observations and top five stores"))
  })
  
  output$City_Liquor_Sales_income <- renderPlot({
    storedata %>% dplyr::filter(City == input$city) %>%
      ggplot(aes(x = long, y = lat, size = sale_dollars)) + 
      geom_point() + 
      ggtitle(paste0(input$city, "Store and Income"))
  })
}

shinyApp(ui, server)

