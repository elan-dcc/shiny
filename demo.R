#' Demo of a Shiny dashboard
#' Date: 31-10-2024
#' @author Lisette de Schipper

# install.packages("shinydashboard")
# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("highcharter")
library(shinydashboard)
library(ggplot2)
library(plotly)
library(highcharter)

color_five <- c("#a3a9cc", "#8e8ec0", "#9079b7", "#7b5f89", "#624a6d")
color_two <- c("#9adbf9", "#f79ad6")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
  navbarPage("Demo",
                 tabPanel("Grafieken",
                          fluidRow(tabBox(
                            selected = "Inkomen",
                            width = 12,
                            tabPanel("Inkomen", plotOutput("bloeddruk_inkomen")),
                            tabPanel("Geslacht", plotlyOutput("bloeddruk_geslacht")),
                            tabPanel("Ethniciteit", highchartOutput("bloeddruk_ethniciteit"))
                          ))),
             navbarMenu("Example link",
                        tabPanel("Example sublink", highchartOutput("bubble")),
                        tabPanel("Example sublink 2", 
                                 fluidRow(
                                   column(width = 6, "Jaar", br(), selectInput("year", NULL, choices = seq(2018, 2020), width = "100%"))
                                 ),
                                 fluidRow(
                                   column(width = 12, highchartOutput("linechart")))
                            ))),
                 tags$footer(HTML("
                    <!-- Footer -->
                           <footer class='page-footer font-large indigo'>
                           2024 © <a href='https://healthcampusdenhaag.nl/nl/'>Health Campus Den Haag</a>
                           </footer>")))

server <- function(input, output) {
  output$bloeddruk_inkomen <- renderPlot({
    data <- data.frame(inkomen = c(40, 20, 50, 70, 90),
                       kwintiel = c("1e kwintiel (laagste)", "2e kwintiel", "3e kwintiel", "4e kwintiel", "5e kwintiel (hoogste)"))
    ggplot(data, aes(x= kwintiel, y = inkomen, fill = kwintiel )) +
      geom_col() +
      scale_fill_manual(values = color_five ) +
      theme(legend.position="none")
  })
  
  output$bloeddruk_geslacht <- renderPlotly(
    plot1 <- plot_ly(x = c("Vrouw", "Man"), y = c(59, 71), type = 'bar',
                     color = c("Vrouw", "Man"), colors = color_two)  %>%
      config(displayModeBar = FALSE)
  )
  
  
  output$bloeddruk_ethniciteit <- renderHighchart({
    data <- data.frame(percentage = c(1,1,1, 30, 30, 20,70, 70, 80),
                       type = factor(rep(c("Landelijk ziekenhuiscijfer", "Regionaal ziekenhuiscijfer", "Regionaal huisartsencijfer"), 3)),
                       leeftijdsgroep = factor(rep(c("18-39", "40-69", ">=70"), 3)))
    highchart() %>% hc_add_series(
      data, 
      "column",
      hcaes(x = leeftijdsgroep, y = percentage, group = type)
    ) %>%
      hc_xAxis(categories = data$leeftijdsgroep)
  })

  output$bubble <- renderHighchart({
    data <- data.frame(percentage = c(30, 70, 80),
                       leeftijdsgroep = c("Nederlands", "Surinaams", "Overig"))
    hchart(
      data, 
      "packedbubble",
      hcaes(name = leeftijdsgroep, value = percentage, group = leeftijdsgroep)
    )
  })
  
  output$linechart <- renderHighchart({
    
    data <- data.frame(y = c(runif(n = 3 *length(seq(1, 20)), min=1, max=20), 
                                 runif(n = 3 * length(seq(1, 20, 0.5)), min=5, max=50)),
                       x = rep(c(seq(1, 20), seq(1, 20, 0.5)), 3),
                       group = rep(c(rep(1, length(seq(1,20))), 
                                 rep(2, length(seq(1,20,0.5)))), 3),
                       year = c(rep(2018, length(seq(1,20)) + length(seq(1,20,0.5))),
                                rep(2019, length(seq(1,20)) + length(seq(1,20,0.5))),
                                rep(2020, length(seq(1,20)) + length(seq(1,20,0.5))))
                       )

    hchart(
      data[ data$year == input$year,], 
      "area",
      hcaes(x = x, y = y, group = group)
    ) %>% hc_add_dependency("plugins/multicolor_series.js")
  })
}

shinyApp(ui, server)