``` r
library(shiny)
library(tidyverse)
library(DT)

options(max.print = 100)

set.seed(1)
norm_sample <- round(rnorm(1000, mean = 10, sd = 2 ))
pois_sample <- round(rpois(1000, lambda = 3))
unif_sample <- round(runif(1000, 0, 10))

ui <- fluidPage(
  
#normal distribution for UI
  titlePanel("Normal Distribution"),
  sidebarLayout(
    sidebarPanel(
      numericInput("norm", "Sample Size", 20, min = 1, max = 1000),
      checkboxInput("normSampleMean", "Sample Mean"),
      checkboxInput("randomVariablesList", "List Numbers"),
      checkboxInput("normalSD", "Show Standard Deviation on Graph"),
      uiOutput("normalValues")
    ),
    mainPanel(
      plotOutput("normalPlot"),
      textOutput("normalList")
    )
  ),
#poisson distribution for UI
  titlePanel("Poisson Distribution"),
  sidebarLayout(
    sidebarPanel(
      numericInput("pois", "Sample Size", 20, min = 1, max = 1000)
    ),
    mainPanel(
      plotOutput("poisPlot")  
    )
  ),
#uniform distribution for UI
  titlePanel("Uniform Distribution"),
  sidebarLayout(
    sidebarPanel(
      numericInput("unif", "Sample Size", value = 20, min = 1, max = 1000)
    ), 
    mainPanel(
      plotOutput("unifPlot")
      
    )
  )
)









server <- function(input, output) {
  #Normal Distribution server information
  
  output$normalPlot <- renderPlot({
     normalPlot <-  tibble(
        collect = norm_sample[1:input$norm]
      ) %>% ggplot() +
      geom_histogram(
          binwidth = 1, 
          aes(collect))+
        geom_segment(
          aes(
            x = norm_sample[input$norm] - 0.5,
            y = sum(collect == norm_sample[input$norm]),
            xend = norm_sample[input$norm] + 0.5,
            yend = sum(collect == norm_sample[input$norm]),
            colour = "red",
            linewidth = 1
            )
          )+
        scale_x_continuous(
          limits = c(0,20)
        )+
      labs(
        x = "Random Variable",
        y = "Count"
      )+
      theme(
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)
      )+
      geom_vline(xintercept = mean(norm_sample)
      )
     
     sampleMean <- mean(norm_sample[1:input$norm])
     sampleSD <- sd(norm_sample[1:input$norm])
     
      if(input$normSampleMean){
       normalPlot = normalPlot+ 
         geom_vline(
           xintercept = sampleMean,
           colour = "red")
      }
     
      if(input$normalSD){
        normalPlot = normalPlot + 
          geom_curve(
            x = sampleMean,
            xend = sampleMean + sampleSD,
            y = 0,
            yend = 0
          )+
          geom_curve(
            x = sampleMean + sampleSD,
            xend = sampleMean + 2* sampleSD,
            y = 0,
            yend = 0
          )
      }
     
     print(normalPlot)
      })
  
  output$normalList <- renderPrint({
    if (input$randomVariablesList){
    holder <- norm_sample[1:input$norm]
    noquote(paste(holder, collapse = ", "))
    } 
  })
  
  output$normalValues <- renderText({
    HTML(
      paste(
        "Mean:", round(mean(norm_sample),2),"<br>",
        "Standard Deviation:", round(sd(norm_sample),2), "<br>",
        "Sample Mean:", round(mean(norm_sample[1:input$norm]), 2), "<br>",
        "Sample Standard Deviation:",round(sd(norm_sample[1:input$norm]),2)
        )
      )
  })
  
  #Poisson Distribution server information
  output$poisPlot <- renderPlot({
    tibble(
      pcollect = pois_sample[1:input$pois]) %>% 
      ggplot() +
      geom_histogram(
        binwidth = 1, 
        aes(pcollect))+
      geom_segment(
        aes(
          x = pois_sample[input$pois] - 0.5,
          y = sum(pcollect == pois_sample[input$pois]),
          xend = pois_sample[input$pois] + 0.5,
          yend = sum(pcollect == pois_sample[input$pois]),
          colour = "red",
          linewidth = 1
        )
      )+
      scale_x_continuous(
        limits = c(0,20)
      )+
      labs(
        x = "Random Variable",
        y = "Count"
      )+
      theme(
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)
      )
  })
  
  #Uniform Distribution server information
  output$unifPlot <- renderPlot({
    tibble(
      ucollect = unif_sample[1:input$unif]) %>% 
      ggplot() +
      geom_histogram(
        binwidth = 1, 
        aes(ucollect))+
      geom_segment(
        aes(
          x = unif_sample[input$unif] - 0.5,
          y = sum(ucollect == unif_sample[input$unif]),
          xend = unif_sample[input$unif] + 0.5,
          yend = sum(ucollect == unif_sample[input$unif]),
          colour = "red",
          linewidth = 1
        )
      )+
      scale_x_continuous(
        limits = c(0,10)
      )+
      labs(
        x = "Random Variable",
        y = "Count"
      )+
      theme(
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)
      )
  })
}
 
shinyApp(ui = ui, server = server)
```
