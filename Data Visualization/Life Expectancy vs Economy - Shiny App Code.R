
# Importing necessary packages
library(readr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rsconnect)
library(quantmod)
library(plotly)

# Country Metadata data processing
country_metadata <- read_csv("country-metadata.csv")
country_metadata <- country_metadata %>% rename("Code" = "Country Code")
country_metadata <- country_metadata %>% select(Code, Region)

# Life Expectancy Data Processing
life_expectancy <- read_csv('life-expectancy-at-birth.csv')
life_expectancy <- life_expectancy %>% rename(
  "Country" = "Country Name",
  "Code" = "Country Code")
life_expectancy <- inner_join(life_expectancy, country_metadata, by = "Code")
life_expectancy <- life_expectancy %>% select(Country, Code, Region, `1990`:`2017`)
life_expectancy <- life_expectancy %>% gather(`1990`:`2017`, key = "Year", value = "Life.Expectancy")
life_expectancy$Life.Expectancy <- round(life_expectancy$Life.Expectancy, 2)

life_expectancy_region_year <- life_expectancy %>% group_by(Region, Year) %>% 
  summarise(Life.Expectancy.Region = round(mean(Life.Expectancy, na.rm = TRUE), 2))
life_expectancy_region_year <- life_expectancy_region_year[-which(is.na(life_expectancy_region_year$Region)),]
life_expectancy_region_year <- spread(life_expectancy_region_year, key = Region, value = Life.Expectancy.Region)

# GDP data processing
gdp <- read_csv('gdp.csv')
gdp <- gdp %>% rename(
        "Country" = "Country Name",
        "Code" = "Country Code")
gdp <- inner_join(gdp, country_metadata, by="Code")
gdp <- gdp %>% select(Country, Code, Region, `1990`:`2017`)
gdp <- gdp %>% gather(`1990`:`2017`, key = "Year", value = "GDP")
gdp$GDP <- round(gdp$GDP, 2)

gdp_region_year <- gdp %>% group_by(Region, Year) %>% summarise(GDP.Region = round(mean(GDP, na.rm = TRUE), 2))
gdp_region_year <- gdp_region_year[-which(is.na(gdp_region_year$Region)),]
gdp_region_year <- spread(gdp_region_year, key = Region, value = GDP.Region)

# GDP Per Capita data processing
gdp_per_capita <- read_csv('gdp-per-capita.csv')
gdp_per_capita <- gdp_per_capita %>% rename(
    "Country" = "Country Name",
    "Code" = "Country Code")
gdp_per_capita <- inner_join(gdp_per_capita, country_metadata, by = "Code")
gdp_per_capita <- gdp_per_capita %>% select(Country, Code, Region, `1990`:`2017`)
gdp_per_capita <- gdp_per_capita %>% gather(`1990`:`2017`, key = "Year", value = "GDP.Per.Capita")
gdp_per_capita$GDP.Per.Capita <- round(gdp_per_capita$GDP.Per.Capita, 2)

gdp_per_capita_region <- gdp_per_capita %>% group_by(Region) %>% 
  summarise(GDP.Per.Capita.Region = round(mean(GDP.Per.Capita, na.rm = TRUE), 2))
gdp_per_capita_region <- gdp_per_capita_region[-which(is.na(gdp_per_capita_region$Region)),]

gdp_per_capita_region_year <- gdp_per_capita %>% group_by(Region, Year) %>% 
  summarise(GDP.Per.Capita.Region = round(mean(GDP.Per.Capita, na.rm = TRUE), 2))
gdp_per_capita_region_year <- gdp_per_capita_region_year[-which(is.na(gdp_per_capita_region_year$Region)),]
gdp_per_capita_region_year <- spread(gdp_per_capita_region_year, key = Region, value = GDP.Per.Capita.Region)

# GDP PPP data processing
gdp_ppp <- read_csv('gdp-per-capita-ppp.csv')
gdp_ppp <- gdp_ppp %>% rename(
  "Country" = "Country Name",
  "Code" = "Country Code")
gdp_ppp <- inner_join(gdp_ppp, country_metadata, by="Code")
gdp_ppp <- gdp_ppp %>% select(Country, Code, Region, `1990`:`2017`)
gdp_ppp <- gdp_ppp %>% gather(`1990`:`2017`, key = "Year", value = "GDP.PPP")
gdp_ppp$GDP.PPP <- round(gdp_ppp$GDP.PPP, 2)

gdp_ppp_region_year <- gdp_ppp %>% group_by(Region, Year) %>% summarise(GDP.PPP.Region = round(mean(GDP.PPP, na.rm = TRUE), 2))
gdp_ppp_region_year <- gdp_ppp_region_year[-which(is.na(gdp_ppp_region_year$Region)),]
gdp_ppp_region_year <- spread(gdp_ppp_region_year, key = Region, value = GDP.PPP.Region)

# Disable Header
header <- dashboardHeader(disable = TRUE)

# Disable Sidebar
sidebar <- dashboardSidebar(disable = TRUE)

# Create first row of dashboard
introRow <- fluidRow(
  column(6,
         # Dashboard introduction text
         valueBoxOutput("dashboardIntro", width = "100%")
  ),
  column(6,
         fixedRow(
           column(4,
                  # Slider Input for selecting the year
                  sliderInput("Year", "Regional Average Life Expectancy:",
                              min = 1990, max = 2017,
                              value = 2017, step = 1,
                              animate = animationOptions(interval = 500, loop = FALSE), width = '100%')
           ),
           column(4,
                  box(
                    # Region 1 Life Expectancy Box
                    background = "maroon",
                    width = "100%", height = 37,
                    textOutput("lifeExpectancyRegion1")
                  )
           ),
           column(4,
                  box(
                    # Region 2 Life Expectancy Box
                    background = "fuchsia",
                    width = "100%", height = 37,
                    textOutput("lifeExpectancyRegion2")
                  )
           ),
           column(4,
                  box(
                    # Region 3 Life Expectancy Box
                    background = "blue",
                    width = "100%", height = 37,
                    textOutput("lifeExpectancyRegion3")
                  )
           ),
           column(4,
                  box(
                    # Region 4 Life Expectancy Box
                    background = "green",
                    width = "100%", height = 37,
                    textOutput("lifeExpectancyRegion4")
                  )
           ),
           column(4,
                  box(
                    # Region 5 Life Expectancy Box
                    background = "yellow",
                    width = "100%", height = 37,
                    textOutput("lifeExpectancyRegion5")
                  )
           ),
           column(4,
                  box(
                    # Region 6 Life Expectancy Box
                    background = "orange",
                    width = "100%", height = 37,
                    textOutput("lifeExpectancyRegion6")
                  )
           ),
           column(4,
                  box(
                    # Region 7 Life Expectancy Box
                    background = "red",
                    width = "100%", height = 37,
                    textOutput("lifeExpectancyRegion7")
                  )
           )
         )
  )
)

#
mapAndLineChartRow <- fluidRow(
  box(
    # Average GDP By Region Line Chart
    background = "light-blue",
    plotlyOutput("averageGDPByRegionChart", height = "283px"),
    width = 6
  ),
  box(
    # Life Expectancy World Map
    background = "light-blue",
    plotlyOutput("lifeExpectancyMap", height = "283px"),
    width = 6
  )
)

lineChartsRow <- fluidRow(
  box(
    # Average GDP per Capita By Region Line Chart
    background = "light-blue",
    plotlyOutput("averageGDPPerCapitaByRegionChart", height = "283px"),
    width = 6
  ),
  box(
    # Average GDP PPP By Region Line Chart
    background = "light-blue",
    plotlyOutput("averageGDPPPPByRegionChart", height = "283px"),
    width = 6
  )
)

# Body of the Dashboard
body <- dashboardBody(introRow, mapAndLineChartRow, lineChartsRow,
                      tags$head(tags$style(HTML(
                              '.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}'
                          ))))

# UI part of the Dashboard
ui <- dashboardPage(title = 'Life Expectancy v/s Economy: A study on affects of Economy on Life Expectancy', header, 
                    sidebar, body)

# Create the Server for the Dashboard
server <- function(input, output) {

  # Adding the text for Dashboard Introduction
  output$dashboardIntro <- renderValueBox({
    valueBox(
      tags$p(style = "font-size: 15px;", "Life Expectancy v/s Economy"),
      HTML(paste(tags$p(style = "font-size: 13px;",
            "Life Expectancy measures the mean age of death of a population and is a key factor of health of population. ",
            "During industrialisation, the life expectancy improved faster in industrialised countries ",
            "than the remaining world. This suggests that economy change is directlty proportional to life expectancy ",
            "and this study supports the argument. ",
            "In this study, we can check the average regional life expectancy, using the Input Slider on the right ",
            "and compare it different economic indentifiers (GDP, GDP/Capita & GDP PPP) ",
            "to find which identifier supports the relation. ",
            "After comparing, GDP per Capita is found to be directly proportional to Life Expectancy. ",
            "Hence, our study confirms that Life Expectancy is directly proportional to GDP per Capita."))),
      icon = icon("stats",lib='glyphicon'),
      color = "light-blue")
  })

  # Region 1 Life Expectancy Box text update
  output$lifeExpectancyRegion1 <- renderText({
    paste0(names(life_expectancy_region_year)[2], ": ", 
           life_expectancy_region_year[which(life_expectancy_region_year$Year == input$Year),]$`East Asia & Pacific`)
  })
  
  # Region 2 Life Expectancy Box text update
  output$lifeExpectancyRegion2 <- renderText({
    paste0(names(life_expectancy_region_year)[3], ": ", 
           life_expectancy_region_year[which(life_expectancy_region_year$Year == input$Year),]$`Europe & Central Asia`)
  })
  
  # Region 3 Life Expectancy Box text update
  output$lifeExpectancyRegion3 <- renderText({
    paste0(names(life_expectancy_region_year)[4], ": ", 
           life_expectancy_region_year[which(life_expectancy_region_year$Year == input$Year),]$`Latin America & Caribbean`)
  })
  
  # Region 4 Life Expectancy Box text update
  output$lifeExpectancyRegion4 <- renderText({
    paste0(names(life_expectancy_region_year)[5], ": ", 
           life_expectancy_region_year[which(life_expectancy_region_year$Year == input$Year),]$`Middle East & North Africa`)
  })
  
  # Region 5 Life Expectancy Box text update
  output$lifeExpectancyRegion5 <- renderText({
    paste0(names(life_expectancy_region_year)[6], ": ", 
           life_expectancy_region_year[which(life_expectancy_region_year$Year == input$Year),]$`North America`)
  })
  
  # Region 6 Life Expectancy Box text update
  output$lifeExpectancyRegion6 <- renderText({
    paste0(names(life_expectancy_region_year)[7], ": ", 
           life_expectancy_region_year[which(life_expectancy_region_year$Year == input$Year),]$`South Asia`)
  })
  
  # Region 7 Life Expectancy Box text update
  output$lifeExpectancyRegion7 <- renderText({
    paste0(names(life_expectancy_region_year)[8], ": ", 
           life_expectancy_region_year[which(life_expectancy_region_year$Year == input$Year),]$`Sub-Saharan Africa`)
  })

  # Plot Life Expectancy Map
  output$lifeExpectancyMap <- renderPlotly({
    l <- list(width = 0.5)

    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )

    plot_geo(
      data = filter(life_expectancy, Year == input$Year)) %>%
      add_trace(
        z = ~Life.Expectancy, color = ~Life.Expectancy, colors = 'Blues',
        text = ~Country, locations = ~Code, marker = list(line = l)
      ) %>%
      colorbar(title = "Life Expectancy (years)") %>%
      layout(
        title = paste0('Life Expectancy (',input$Year, ') (Source:',
                       '<a href="http://api.worldbank.org/v2/en/indicator/SP.DYN.LE00.IN?downloadformat=csv">',
                       'The World Bank Data</a>)'),
        geo = g
      ) %>% 
      config(displayModeBar = FALSE)
  })

  # Plot GDP Line Chart
  output$averageGDPByRegionChart <- renderPlotly({
    plot_ly(gdp_region_year, x = ~Year) %>%
      add_lines(y = ~`East Asia & Pacific`, hoverinfo = 'text',name = "East Asia & Pacific", line = list(color = "maroon")) %>%
      add_lines(y = ~`Europe & Central Asia`, name = "Europe & Central Asia", line = list(color = "fuchsia")) %>%
      add_lines(y = ~`Latin America & Caribbean`, name = "Latin America & Caribbean", line = list(color = "blue")) %>%
      add_lines(y = ~`Middle East & North Africa`, name = "Middle East & North Africa", line = list(color = "green")) %>%
      add_lines(y = ~`North America`, name = "North America", line = list(color = "yellow")) %>%
      add_lines(y = ~`South Asia`, name = "South Asia", line = list(color = "orange")) %>%
      add_lines(y = ~`Sub-Saharan Africa`, name = "Sub-Saharan Africa", line = list(color = "red")) %>%
      layout(
        title = paste0('Average Regional GDP (Source:',
                       '<a href="http://api.worldbank.org/v2/en/indicator/NY.GDP.MKTP.CD?downloadformat=csv">',
                       'The World Bank Data</a>)'),
        xaxis = list(
          rangeslider = list(type = "date")
        ),
        yaxis = list(title = "GDP $ (B:Billion, T:Trillion)")
      ) %>% config(displayModeBar = FALSE)
  })

  # Plot GDP per Capita Line Chart
  output$averageGDPPerCapitaByRegionChart <- renderPlotly({
    plot_ly(gdp_per_capita_region_year, x = ~Year) %>%
      add_lines(y = ~`East Asia & Pacific`, hoverinfo = 'text',name = "East Asia & Pacific", line = list(color = "maroon")) %>%
      add_lines(y = ~`Europe & Central Asia`, name = "Europe & Central Asia", line = list(color = "fuchsia")) %>%
      add_lines(y = ~`Latin America & Caribbean`, name = "Latin America & Caribbean", line = list(color = "blue")) %>%
      add_lines(y = ~`Middle East & North Africa`, name = "Middle East & North Africa", line = list(color = "green")) %>%
      add_lines(y = ~`North America`, name = "North America", line = list(color = "yellow")) %>%
      add_lines(y = ~`South Asia`, name = "South Asia", line = list(color = "orange")) %>%
      add_lines(y = ~`Sub-Saharan Africa`, name = "Sub-Saharan Africa", line = list(color = "red")) %>%
      layout(
        title = paste0('Average Regional GDP per Capita (Source:',
                       '<a href="http://api.worldbank.org/v2/en/indicator/NY.GDP.PCAP.CD?downloadformat=csv">',
                       'The World Bank Data</a>)'),
        xaxis = list(
          rangeslider = list(type = "date")
        ),
        yaxis = list(title = "GDP/Capita $ (k:Thousand)")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Plot GDP PPP Line Chart
  output$averageGDPPPPByRegionChart <- renderPlotly({
    plot_ly(gdp_ppp_region_year, x = ~Year) %>%
      add_lines(y = ~`East Asia & Pacific`, hoverinfo = 'text',name = "East Asia & Pacific", line = list(color = "maroon")) %>%
      add_lines(y = ~`Europe & Central Asia`, name = "Europe & Central Asia", line = list(color = "fuchsia")) %>%
      add_lines(y = ~`Latin America & Caribbean`, name = "Latin America & Caribbean", line = list(color = "blue")) %>%
      add_lines(y = ~`Middle East & North Africa`, name = "Middle East & North Africa", line = list(color = "green")) %>%
      add_lines(y = ~`North America`, name = "North America", line = list(color = "yellow")) %>%
      add_lines(y = ~`South Asia`, name = "South Asia", line = list(color = "orange")) %>%
      add_lines(y = ~`Sub-Saharan Africa`, name = "Sub-Saharan Africa", line = list(color = "red")) %>%
      layout(
        title = paste0('Average Regional GDP PPP (Source:',
                       '<a href="http://api.worldbank.org/v2/en/indicator/NY.GDP.PCAP.PP.CD?downloadformat=csv">',
                       'The World Bank Data</a>)'),
        xaxis = list(
          rangeslider = list(type = "date")
        ),
        yaxis = list(title = "GDP PPP $ (k:Thousand)")
      ) %>%
      config(displayModeBar = FALSE)
  })
}

# Run the Shiny Dashboard
shinyApp(ui = ui, server = server)
