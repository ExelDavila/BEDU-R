#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
dataframe <- read.csv("datos/match.data.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Postwork08"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h2("Gráfica de barras"), 
            selectInput("op", "Seleccione",
                        choices = c("Goles visita","Goles casa"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Plots",h3(textOutput("output_text")),
                         plotOutput("output_plot")
                ),
                tabPanel("Graficas Postwork 3",  #Pestaña de imágenes
                         img( src = "pw3-1.png", 
                              height = 400, width = 450),
                         img( src = "pw3-2.png", 
                              height = 400, width = 450),
                         img( src = "pw3-3.png", 
                              height = 400, width = 450)
                ),
                tabPanel("Data Table", dataTableOutput("data_table")),
                tabPanel("Factores de ganancia",  #Pestaña de imágenes
                         img( src = "grafica1.png", 
                              height = 350, width = 550),
                         img( src = "grafica2.png", 
                              height = 350, width = 550)
                )
            
             
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Agregando el data table
    output$data_table <- renderDataTable({dataframe}, 
                                         options = list(aLengthMenu = c(10,20,50),
                                                        iDisplayLength = 5))
    
    output$output_text <- renderText(input$op)
    #Gráficas  
    colname <- reactive(switch (input$op,
        "Goles casa" = dataframe$home.score,
        "Goles visita" = dataframe$away.score
    ))
    #output$output_plot <- renderPlot(hist(colname(),breaks = seq(0,10.1), main="Frecuencia de goles", xlab=input$op))
    output$output_plot <- renderPlot(ggplot(dataframe)+aes(colname())+geom_histogram(breaks=seq(0,7,1))+facet_wrap("away.team")+xlab(input$op)+ggtitle("Frecuencia de goles"))
}

# Run the application 
shinyApp(ui = ui, server = server)
