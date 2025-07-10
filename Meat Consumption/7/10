library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(DT)
library(plotly)
library(scales)


#notes i need to add
# For All anamials i only used the REFERENCE_PERIOD_DESC == "ANNUAL"
# For All animals I used 
#       mutate(STATISTICCAT_DESC = ifelse(
                                        # grepl("CONDEMNED", STATISTICCAT_DESC, ignore.case = TRUE), 
                                        # "CONDEMNED", 
                                        # STATISTICCAT_DESC)) %>% 
#       Which makes it to where all items with "CONDEMNED were aggergated to just show CONDEMNED.
#       I don't know if this acceptable practice but the numbers seemed to be justified and closely matched USDA data.
#       Regardless, if there is an exaggeration in condemned animals, it's only through double counting 
# For cows, i removed all "SHIPMENTS IN" from the STATISTICCAT_DESC column


#data <- annual_chicken
data <- all_cattle

STATE_NAME <- sort(unique(data$STATE_NAME))
STATISTICCAT_DESC <- sort(unique(data$STATISTICCAT_DESC))
YEAR <- sort(unique(data$YEAR))
#creating a uniform color scheme for the STATISTICCAT_DESC
state_color <- RColorBrewer::brewer.pal(n = length(STATISTICCAT_DESC), name = "Set3")
names(state_color) <- STATISTICCAT_DESC

ui <- fluidPage(
    
    #This is the interactive map
    
    titlePanel("Chicken Production by State"),
    tabsetPanel(
        tabPanel("Map",
            sidebarLayout(
                sidebarPanel(
                    fluidRow(
                        column(6, 
                               selectInput(
                                   "STATISTICCAT_DESC_filter_panel1", 
                                   "Select Classification:",
                                   choices = STATISTICCAT_DESC
                               )
                        ),
                        column(6,
                               selectInput(
                                   "YEAR_panel1", 
                                   "Select Year:",
                                   choices = YEAR,
                                   selected = (YEAR[length(YEAR)])
                               ),
                               style = "padding-right: 20px;"
                        )
                    )
                ),
                
                mainPanel(
                    fluidRow(
                        column(
                            12, 
                            h2('Chicken Production by State'),
                            plotlyOutput("map_plot_panel1"),
                            
                            uiOutput("other_states_info_panel1"),
                            
                            p("The head count is a reflection of 'Placement', 
                      which means the chicks were brought to a growing plant.
                      They are later moved to their final position I.E. broiler, hen, etc.")
                        ),
                        column(
                            12, 
                            h2('Chicken Production Table'),
                            DTOutput("meat_table_panel1"),
                            

                            p("The head count is a reflection of 'Placement', 
                      which means the chicks were brought to a growing plant.
                      They are later moved to their final position I.E. broiler, hen, etc.")
                        )
                    )
                )
            )
            
        ),
    
    #This in the interactive (x,y) graph
        tabPanel( "Scatter Plot",
            sidebarLayout(
                sidebarPanel(
                    fluidRow(
                        column(6,
                               actionButton("toggle_states_panel2", "All States"),
                               checkboxGroupInput(
                                   "action_button_panel2",
                                   "Select State:",
                                   choices = STATE_NAME,
                                   selected = "ALABAMA"
                               ),
                               style = "padding-right: 40px;"
                        ),
                        column(6, 
                               checkboxGroupInput(
                                   "STATISTICCAT_DESC_filter_panel2", 
                                   "Select Classification:",
                                   choices = STATISTICCAT_DESC,
                                   selected = "PLACEMENTS"
                               )
                        )
                    )
                ),
                
                mainPanel(
                    fluidRow(
                        column(
                            12,
                            h2('Chicken Production by State'),
                            plotlyOutput("scatter_plot_panel2"),
                            p("The head count is a reflection of 'Placement',
                      which means the chicks were brought to a growing plant.
                      They are later moved to their final position I.E. broiler, hen, etc.")
                        )
                    )
                )
            )
        )
            
    )
    
)

server <- function(input, output, session){
    
  
  #filtered data for panel 1  
    filtered_data_map_panel1 <- reactive({
        data %>% 
            filter(
                STATISTICCAT_DESC %in% input$STATISTICCAT_DESC_filter_panel1,
                YEAR %in% input$YEAR_panel1
            ) %>% 
            arrange(YEAR)
    })
    
    output$map_plot_panel1 <- renderPlotly({
        data <- filtered_data_map_panel1()
            
            plot_geo(data, locationmode = 'USA-states') %>%
            add_trace(
                z =  ~VALUE,
                locations = ~STATE_ABB,
                text = ~paste(
                    "State:", STATE_NAME,
                    "<br>VALUE:", comma(VALUE),
                    "<br>Year:", YEAR
                ),
                hoverinfo = "text",
                colors = "Blues",
                marker = list(line = list(color = 'black', width = 0.5))
            ) %>%
            colorbar(title = "Head Count") %>%
            layout(
                geo = list(
                    scope = 'usa',
                    projection = list(type = 'albers usa'),
                    showlakes = TRUE,
                    lakecolor = toRGB('white')
                ),
                margin = list(l = 0, r = 0, t = 0, b = 0)
            )
    })
    
    output$meat_table_panel1 <- renderDT({
        df <- filtered_data_map_panel1() %>%
                      arrange(desc(VALUE)) %>% 
                      ungroup() %>% 
                      select(
                          STATE_NAME,
                          VALUE,
                          YEAR,
                          STATISTICCAT_DESC) %>% 
                      mutate(VALUE = comma(VALUE)) 

        
    })
    
    output$other_states_info_panel1 <- renderUI({
        other_data <- filtered_data_map_panel1() %>%
            filter(STATE_NAME == "OTHER STATES")
        
        if (nrow(other_data) > 0) {
            val <- comma(sum(other_data$VALUE))
            val_class <- other_data
            HTML(paste0("<strong>Other States Total for</strong> ", input$STATISTICCAT_DESC_filter_panel1, " :" , val))
        } else{
            HTML(paste("<strong>Other States Total:</strong> NA "))
        }
    })
    
    
    
    
    # Filtered data for panel 2
    # Track whether all states are selected
    all_selected <- reactiveVal(FALSE)

    observeEvent(input$toggle_states_panel2, {
        if (all_selected()) {
            updateCheckboxGroupInput(session, "action_button_panel2", selected = character(0))
            all_selected(FALSE)
        } else {
            updateCheckboxGroupInput(session, "action_button_panel2", selected = STATE_NAME)
            all_selected(TRUE)
        }
    })
    filtered_data_map_panel2 <- reactive({
        data %>%
            filter(
                STATISTICCAT_DESC %in% input$STATISTICCAT_DESC_filter_panel2,
                STATE_NAME %in% input$action_button_panel2
            ) %>%
            arrange(YEAR)
    })
    
    output$scatter_plot_panel2 <- renderPlotly({
    
    data <- filtered_data_map_panel2()
    p <- data %>% 
        ggplot()+
        geom_point(
            aes(x = YEAR, 
                y = VALUE, 
                color = STATISTICCAT_DESC,
                text = paste0("Year: ",YEAR,
                              "<br>State: ", STATE_NAME,
                              "<br>Value", comma(VALUE),
                              "<br>Classification", STATISTICCAT_DESC)
            )
        ) +
        scale_color_manual(values = state_color)
    ggplotly(p, tooltip = "text")
    
    })
    
}

shinyApp(ui, server)
