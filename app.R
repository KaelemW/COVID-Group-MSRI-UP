#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shiny)
library(plotly)
library(leaflet)
library(tidyverse)
library(sf)
library(highcharter)
library(shinydashboard)
library(scales)
library(tigris)
library(htmltools)

options(tigris_use_cache = TRUE)

ca_svi_map <- read_csv("ca_svi_map.csv")
covid_filtered_gg <- read_csv("covid_data_gg.csv")
vax_cases_svi <- read_csv("vax_cases_svi.csv") |> select(-1)


# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "The Association of Social Vulnerability with COVID-19", titleWidth = 500),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
    # Row 1: Header + Text
    fluidRow(
      box(
        width = 12,
        title = NULL,
        solidHeader = FALSE,
        status = "primary",
        h3("Background and Purpose"),
        p("This dashboard explores the relationship between Social Vulnerability Index (SVI) factors and the spread of COVID-19 across counties in California. This study highlights the existence of disparities in Covid-19 cases and vaccination rates within socially vulnerable communities. Our models demonstrates a strong, statistically significant positive correlation between racial and ethnic minority status and COVID-19 cases. Specifically, this subcategory of the SVI showed the highest positive correlation. This finding suggests that communities with a higher proportion of racial and ethnic minorities experienced a greater number of COVID-19 cases. Conversely, a notable negative correlation between household characteristics (another SVI subcategory) and COVID-19 vaccination rates. This suggests that communities characterized by a higher proportionof certain household characteristics (e.g., age, disabilities, single-parent households, etc.,)experienced lower COVID-19 vaccination rates.")
      )
    ),
    # Row 2: Time series and SVI scatter plot
    fluidRow(
      box(
        width = 6,
        selectInput(
          "selected_county", 
          "County:",
          choices = c(sort(unique(covid_filtered_gg$County))),
          selected = "Los Angeles County"
        ),
        plotlyOutput("plot1", height = 400)
      ),
      box(
        width = 6,
        selectInput('dep', "Choose an SVI subcategory percentile:",
                    choices = c('Racial and Ethnic Minority Status','Household Characteristics', 'Housing Type and Transportation', 'Socioeconomic Status', 'Social Vulnerability Index')),
        selectInput('plot_typ', "Choose a response variable:",
                    choices = c('Covid-19 Cases', 'Covid-19 Vaccinations')),
        plotlyOutput("plot2", height = 325)
      )
    ),
    
    # Row 3: SVI Map
    fluidRow(
      box(
        width = 12,
        leafletOutput("svi_map", height = 375),
        selectInput("theme", "Select Theme:",
                    choices = c("SVI" = "svi", "SES" = "ses", 
                                "HHC" = "hhc", "REMS" = "rems", 
                                "HTT" = "htt"),
                    selected = "svi")
      )
    ),
    #Row 4
    fluidRow(
      box(
        width = 12,
        title = NULL,
        solidHeader = FALSE,
        status = "primary",
        h3("Resources and Ways to Help"),
        
        p("Volunteer opportunities in LA for health justice:"),
        tags$a(href = "https://volunteer.laworks.com/search", 
               "https://volunteer.laworks.com/search", target = "_blank"),
        br(), br(),
        
        p("Engaging Communities of Color to Promote Health Equity:"),
        tags$a(href = "https://www.chcs.org/resource/engaging-communities-of-color-topromote-health-equity-five-lessons-from-new-york-based-health-care-organizations/", 
               "https://www.chcs.org/resource/engaging-communities-of-color-topromote-health-equity-five-lessons-from-new-york-based-health-care-organizations/", 
               target = "_blank"),
        br(), br(),
        
        p("Top 10 ways to improve health and health equity:"),
        tags$a(href = "https://www.americanprogress.org/article/top-10-ways-to-improve-health-and-health-equity/", 
               "https://www.americanprogress.org/article/top-10-ways-to-improve-health-and-health-equity/", 
               target = "_blank")
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  filtered_data_reactive <- reactive({
    req(covid_filtered_gg)
    covid_filtered_gg %>% filter(County == input$selected_county)
  })
  
  output$plot1 <- renderPlotly({
    plot_data <- filtered_data_reactive()
    plot_data$Date <- as.Date(plot_data$Date)
    
    gg <- ggplot(data = plot_data, aes(x = Date)) +
      geom_line(aes(y = new_cases, color = 'COVID Cases'), size = 1) +
      geom_line(aes(y = Vaccinations, color = 'Vaccinations'), size = 1) +
      scale_color_manual(
        name = "Legend",
        values = c('COVID Cases' = "steelblue", 'Vaccinations' = "coral2")
      ) +
      labs(
        title = paste("COVID-19 Cases and Vaccinations in", input$selected_county),
        x = 'Date', y = 'Total'
      ) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      scale_y_continuous(labels = comma) +
      theme_minimal()
    
    ggplotly(gg)
  })
  
  output$plot2 <- renderPlotly({
    req(vax_cases_svi)
    
    # Get y values
    if(input$plot_typ == 'Covid-19 Vaccinations'){
      y <- vax_cases_svi$vacs_100k
      ylab <- 'Sum of Vax/100k'
    } else {
      y <- vax_cases_svi$sum_cases_p100k
      ylab <- 'Sum of Cases/100k'
    }
    
    # Get x values
    xlab <- input$dep
    x <- switch(input$dep,
                'Racial and Ethnic Minority Status' = vax_cases_svi$rems,
                'Household Characteristics' = vax_cases_svi$hhc,
                'Housing Type and Transportation' = vax_cases_svi$htt,
                'Socioeconomic Status' = vax_cases_svi$ses,
                'Social Vulnerability Index' = vax_cases_svi$svi
    )
    
    # Create plot
    p <- ggplot(vax_cases_svi, aes(x = x, y = y, label = County)) +
      geom_point(color = "darkblue") +
      xlab(xlab) +
      ylab(ylab) +
      theme_gray()
    
    ggplotly(p)
  })
  
  output$svi_map <- renderLeaflet({
    
    req(input$theme)
    
    selected_values <- ca_svi_map[[input$theme]]
    
    color_palette <- colorNumeric(
      palette = "YlOrRd",
      domain = selected_values,
      na.color = "transparent"
    )
    
    mytext <- paste0(
      "County: ", ca_svi_map$NAMELSAD, "<br/>",
      toupper(input$theme), " Rate: ", round(selected_values, 3)
    ) %>% lapply(htmltools::HTML)
    
    leaflet(ca_svi_map) %>%
      addTiles() %>%
      addControl(paste(toupper(input$theme),"Rating: 2020"), position = "topleft") %>%
      addPolygons(
        fillColor = ~color_palette(selected_values),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = color_palette,
        values = selected_values,
        opacity = 0.9,
        title = paste(toupper(input$theme), "Rating Based on CDC"),
        position = "bottomleft"
      )
  })
}










# Run the application 
shinyApp(ui = ui, server = server)
