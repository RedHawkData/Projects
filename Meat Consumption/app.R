library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(DT)
library(plotly)
library(scales)

LOCATION_DESC <- sort(unique(annual_monthly_chicken_comparison$LOCATION_DESC))
STATISTICCAT_DESC <- sort(unique(annual_monthly_chicken_comparison$STATISTICCAT_DESC))

ui <- fluidPage(
    titlePanel("Chicken Production by State"),
    
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(6,
                       actionButton("toggle_states", "Select/Deselect All States"),
                       checkboxGroupInput(
                           "checkbox_filter", 
                           "Select State:",
                           choices = LOCATION_DESC
                       ),
                       style = "padding-right: 20px;"
                ),
                column(6, 
                       selectInput(
                           "STATISTICCAT_DESC_filter", 
                           "Select Classification:",
                           choices = STATISTICCAT_DESC
                       )
                )
            )
        ),
        
        mainPanel(
            fluidRow(
                column(
                    12, 
                    h2('Chicken Production by State'),
                    plotlyOutput("filtered_plot"),
                    p("The head count is a reflection of 'Placement', which means the chicks were brought to a growing plant before they are later moved to their final position I.E. broiler, hen, etc.")
                )
            )
        )
    )
)

server <- function(input, output, session){
    
    # Track whether all states are selected
    all_selected <- reactiveVal(FALSE)
    
    observeEvent(input$toggle_states, {
        if (all_selected()) {
            updateCheckboxGroupInput(session, "checkbox_filter", selected = character(0))
            all_selected(FALSE)
        } else {
            updateCheckboxGroupInput(session, "checkbox_filter", selected = LOCATION_DESC)
            all_selected(TRUE)
        }
    })
    
    filtered_data <- reactive({
        annual_monthly_chicken_comparison %>% 
            filter(
                LOCATION_DESC %in% input$checkbox_filter,
                STATISTICCAT_DESC %in% input$STATISTICCAT_DESC_filter
            ) %>% 
            arrange(YEAR)
    })
    
    output$filtered_plot <- renderPlotly({
        data <- filtered_data()
        
        p <- data %>% 
            ggplot(aes(
                x = YEAR, 
                y = VALUE, 
                color = FREQ_DESC,
                text = paste0("Year: ", YEAR,
                              "<br>Quantity: ", comma(VALUE),
                              "<br>State: ", LOCATION_DESC)
            )) +
            facet_wrap(~ LOCATION_DESC) +
            geom_point(size = 0.1) +
            labs(x = "Year", y = "Head Count")
        
        plotly::ggplotly(p, tooltip = "text")
    })
}

shinyApp(ui, server)
