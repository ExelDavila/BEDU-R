## app.R ##


library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

Netflix <- read.csv("netflix_titles.csv", stringsAsFactors = FALSE)
#LECTURA DE ARCHIVOS POPULARIDAD NETFLIX EN PAISES A LO LARGO DE LOS AÑOS
netflixAlemania <- read.csv("netflix_Alemania.csv")
netflixCanada <- read.csv("netflix_Canada.csv")
netflixChina <- read.csv("netflix_China.csv")
netflixEua <- read.csv("netflix_EUA.csv")
netflixFrancia <- read.csv("netflix_Francia.csv")
netflixItalia <- read.csv("netflix_Italia.csv")
netflixMexico <- read.csv("netflix_Mexico.csv")
netflixMundo <- read.csv("netflix_Mundo.csv")
netflixReinoUnido <- read.csv("netflix_reinoUnido.csv")
dimension <- dim(netflixAlemania)[1]
#Agregamos código de país
netflixAlemania$pais <- rep("Alemania", dimension)
netflixCanada$pais <- rep("Canada", dimension)
netflixChina$pais <- rep("China", dimension)
netflixEua$pais <- rep("EUA", dimension)
netflixFrancia$pais <- rep("Francia", dimension)
netflixItalia$pais <- rep("Italia", dimension)
netflixMexico$pais <- rep("Mexico", dimension)
netflixMundo$pais <- rep("Mundo", dimension)
netflixReinoUnido$pais <- rep("Reino Unido", dimension)
#Unimos dataframes en uno solo
netflixGeneral <- rbind(netflixAlemania,netflixCanada,netflixChina,netflixEua,netflixFrancia,netflixItalia,netflixMexico,netflixMundo,netflixReinoUnido)
#Modificamos formato de fecha
netflixGeneral$Day <- rep("01", dim(netflixGeneral)[1])
netflixGeneral <- as.data.frame(unite(netflixGeneral, Fecha, c(1,4),sep="-",remove=TRUE))
netflixGeneral <- mutate(netflixGeneral, Fecha = as.Date(Fecha, "%Y-%m-%d"))

#Esta parte es el anÃ¡logo al ui.R
ui <- 
    
    fluidPage(
         
    dashboardPage(skin = "red", 
        
    dashboardHeader(title = "Análisis de Netflix"),
    
    dashboardSidebar(
       
         sidebarMenu(
            menuItem("Popularidad general Netflix", tabName = "popularidadGeneral", icon = icon("dashboard")),
            menuItem("Popularidad por país", tabName = "popularidadDetallada", icon = icon("area-chart")),
            menuItem("Predccion popularidad mundial", tabName = "prediccionMundo", icon = icon("area-chart")),
            menuItem("Peliculas a lo largo del tiempo", tabName = "STPeliculas", icon = icon("area-chart")),
            menuItem("Paises con mas peliculas", tabName = "TOP10-CantPel", icon = icon("area-chart")),
            menuItem("Generos Populares", tabName = "GenerosPopulares", icon = icon("area-chart")),
            menuItem("Generos Populares por Pais", tabName = "GenerosPopularesPais", icon = icon("area-chart")),
            menuItem("Datos Popularidad Netflix", tabName = "data_tablePopularidad", icon = icon("table")),
            menuItem("Datos Titulos Netflix", tabName = "data_tableTitulos", icon = icon("table"))
         )
    ),
    
    dashboardBody(

        tabItems(
            
            # POPULARIDAD GENERAL
            tabItem(tabName = "popularidadGeneral",
                    fluidRow(
                        titlePanel("Popularidad de Netflix de 2011 a 2021"), 
                       # box(
                        #     title = "Controls",
                         #    sliderInput("meses", "Número de meses:", 1, 117, 5)
                        #,width = 100),
                        box(plotOutput("general1", height = 300,width = 800),plotOutput("general2", height = 900,width = 800)),
                       
                        
                    )
            ),
       
            # POPULARIDAD DETALLADA
               tabItem(tabName = "popularidadDetallada", 
               fluidRow(
                   titlePanel(h3(textOutput("output_text"))),
                   selectInput("opPais", "Selecciona el país", 
                               choices = c("Alemania","Canada","China","EUA","Francia","Italia","Mexico","Mundo","Reino Unido")),
                  #box(plotOutput("popularidadDetallada", height = 300, width = 800),plotOutput("popularidadDetalladaSt"),plotOutput("popularidadDetalladaStJunta") ),
                  box(plotOutput("popularidadDetallada", height = 300, width = 800), plotOutput("graf1"),
                      plotOutput("graf2"))
               )
               ),
            
            tabItem(tabName = "data_tablePopularidad",
                    fluidRow(        
                        titlePanel(h3("Datos Popularidad Netflix")),
                        dataTableOutput ("data_tablePopularidad")
                    )
            ),
            tabItem(tabName = "data_tableTitulos",
                    fluidRow(        
                        titlePanel(h3("Datos Titulos Netflix")),
                        dataTableOutput ("data_tableTitulos")
                    )
            ),
            # PREDICCION POPULARIDAD
            tabItem(tabName = "prediccionMundo",
                    fluidRow(
                        titlePanel("Prediccion de popularidad mundial Netflix 2024"), 
                        box(plotOutput("prediccionMundo",width=800),plotOutput("correlogramaAjuste",width=800)),
                    )
            ),
            tabItem(tabName = "STPeliculas", 
                    fluidRow(
                        titlePanel(h3("CANTIDAD DE PELICULAS A LO LARGO DEL TIEMPO")),
                        box(plotOutput("grafSTPeliculas", height = 500),plotOutput("grafSTPeliculasDes"))
                    )
            ),
            tabItem(tabName = "TOP10-CantPel", 
                    fluidRow(
                        titlePanel(h3("TOP 10 - Paises con mayor cantidad de peliculas")),
                        box(plotOutput("grafCantidad"))
                    )
            ),
            tabItem(tabName = "GenerosPopulares", 
                    fluidRow(
                        titlePanel(h3("TOP 10 - Generos mas populares por año")),
                        selectInput("year", "Selecciona el año", 
                                    choices = c(2014:2020)),
                        box(plotOutput("plotGeneros"))
                    )
            ),
            tabItem(tabName = "GenerosPopularesPais", 
                    fluidRow(
                        titlePanel(h3("TOP 10 - Generos mas populares por pais")),
                        selectInput("pais", "Selecciona el pais", 
                                    choices =c("Germany","Canada","China","United States","France","Italy","Mexico","United Kingdom")),
                        box(plotOutput("plotGenerosPais"),plotOutput("grafPais"))
                    )
            )
            
            
            )
            
    )
)
)

#De aquÃ­ en adelante es la parte que corresponde al server

server <- function(input, output) {
library(ggplot2)
    
    
    #Gráfico Popularidad Netflix General
    output$general1 <- renderPlot({
        ggplot(netflixGeneral, aes(x=rep(Fecha[1:dimension],9), y=Popularidad, color=pais)) + 
            geom_line()+geom_point()+ labs(x = "Fecha",y = "Porcentaje de popularidad", title="Popularidad Netflix")+ scale_x_date(labels = date_format("%Y-%m"))
        })
    #Gráfico Popularidad Netflix General Separado
    output$general2 <- renderPlot({
        ggplot(netflixGeneral, aes(x=rep(Fecha[1:dimension],9), y=Popularidad, color=pais)) + 
            geom_line()+geom_point()+facet_wrap("pais")+ labs(x = "Fecha",y = "Porcentaje de popularidad", title="Popularidad Netflix")+ scale_x_date(labels = date_format("%Y-%m"))
    })
    
    
    # Gráficas de popularidad detalladas
    
    output$output_text <- renderText(paste("Popularidad de",input$opPais))
    output$output_text1 <- renderText(paste("TOP de",input$op1)) #BRIS
    datos <- reactive(filter(netflixGeneral,pais==input$opPais))
    output$popularidadDetallada <- renderPlot({ 
        ggplot(datos(), aes(x=Fecha, y=Popularidad)) + 
            geom_line()+geom_point()+ labs(x = "Fecha",y = "Porcentaje de popularidad", title="Popularidad Netflix")+ scale_x_date(labels = date_format("%Y-%m"))
        })   
    graf1 <- reactive(switch (input$opPais,
                            "Alemania"="11.png","Canada"="21.png","China"="31.png","EUA"="41.png","Francia"="51.png","Italia"="61.png","Mexico"="71.png","Mundo"="81.png","Reino Unido"="91.png"
    ))
    graf2 <- reactive(switch (input$opPais,
                              "Alemania"="12.png","Canada"="22.png","China"="32.png","EUA"="42.png","Francia"="52.png","Italia"="62.png","Mexico"="72.png","Mundo"="82.png","Reino Unido"="92.png"
    ))
    graf3 <- reactive(switch (input$op1,
                              "Alemania"="12.png","Canada"="22.png","China"="32.png","EUA"="42.png","Francia"="52.png","Italia"="62.png","Mexico"="72.png","Mundo"="82.png","Reino Unido"="92.png"
    ))
    grafYears<-reactive(switch (input$year,
                                "2020"="2020","2019"="2019","2018" = "2018", "2017"= "2017", "2016"="2016", "2015" = "2015", "2014"="2014"))
    plotGenerosPais<-reactive(switch (input$pais,
                               "Alemania"="Germany","Canada"="Canada","China"="China","EUA"="United States","Francia"="France","Italy"="Italia","Mexico"="Mexico","Reino Unido"="United Kingdom"))
    output$graf1 <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(file.path('./images/',graf1()))
        # Return a list containing the filename
        list(src = filename,width = 569,
             height = 484)
    }, deleteFile = FALSE)
    output$graf2 <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(file.path('./images/',graf2()))
        # Return a list containing the filename
        list(src = filename,width = 800,
             height = 400)
    }, deleteFile = FALSE)
    #Ajuste serie de tiempo popularidad mundial Netflix
    filtro = "Mundo"
    datos2 <- filter(netflixGeneral,pais==filtro)
    #Serie de tiempo MUndo
    stMundo <- ts(datos2[,2],start = c(2011,6), freq=12)
    # Función para buscar un buen modelo ARIMA
    get.best.arima <- function(x.ts, maxord = c(1, 1, 1, 1, 1, 1)){
        best.aic <- 1e8
        n <- length(x.ts)
        for(p in 0:maxord[1])for(d in 0:maxord[2])for(q in 0:maxord[3])
            for(P in 0:maxord[4])for(D in 0:maxord[5])for(Q in 0:maxord[6])
            {
                fit <- arima(x.ts, order = c(p, d, q),
                             seas = list(order = c(P, D, Q),
                                         frequency(x.ts)), method = "CSS")
                fit.aic <- -2*fit$loglik + (log(n) + 1)*length(fit$coef)
                if(fit.aic < best.aic){
                    best.aic <- fit.aic
                    best.fit <- fit
                    best.model <- c(p, d, q, P, D, Q)
                }
            }
        list(best.aic, best.fit, best.model)
    }
    # obtención del mejor Modelo ARIMA
    best.arima.elec <- get.best.arima(log(stMundo),
                                      maxord = c(2, 2, 2, 2, 2, 2))
    best.fit.elec <- best.arima.elec[[2]]  # Modelo
    best.arima.elec[[3]] # Tipo de modelo (órdenes)
    best.fit.elec
    best.arima.elec[[1]] # AIC
    ###
    # ACF para residuales del ajuste
    output$correlogramaAjuste <- renderPlot({ acf(resid(best.fit.elec), main = "Correlograma de los residuales del ajuste")
    })
    ###
    # Predicción de 3 años de popularidad Netflix en el mundo
    pr <- predict(best.fit.elec, 36)$pred 
    output$prediccionMundo <- renderPlot({ ts.plot(cbind(window(stMundo, start = 2011),
                                                         exp(pr)), col = c("blue", "red"), xlab = "Año",title="Predicicón de popularidad de Netflix en el mundo", ylab="Popularidad Netflix en el Mundo")
    })
    output$graf3 <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(file.path('./images/',graf2()))
        # Return a list containing the filename
        list(src = filename,width = 800,
             height = 400)
    }, deleteFile = FALSE)
    output$grafCantidad <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(file.path('./images/',"CantidadPel.png"))
        # Return a list containing the filename
        list(src = filename,width = 569 ,
             height = 484)
    }, deleteFile = FALSE)
    output$grafSTPeliculas <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(file.path('./images/',"STPeliculas.png"))
        # Return a list containing the filename
        list(src = filename,width = 678 ,
             height = 484)
    }, deleteFile = FALSE)
    output$grafSTPeliculasDes <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(file.path('./images/',"STPeliculasDes.png"))
        # Return a list containing the filename
        list(src = filename,width = 678 ,
             height = 484)
    }, deleteFile = FALSE)
    output$plotGeneros <- renderPlot({
        if(!is.null(input$year)){
           
                yearInput <- input$year
            
            
            tabla_year<-select(Netflix,listed_in,date_added)
            tabla_year<-mutate(tabla_year, year = format(as.Date(date_added, format= "%B %d, %Y"),"%Y"))
         
            tabla_year<-subset.data.frame(tabla_year,year==yearInput)
          
            tabla_year<-tabla_year %>% group_by(listed_in)
            
            tabla_year2<-tabla_year %>% summarise(Cantidad = n())
            
            
            tabla.gen.year<-tabla_year2[1:10,]
            ggplot(tabla.gen.year, aes(x = reorder(listed_in,-Cantidad), y = Cantidad)) + 
                geom_col(colour = "pink", fill= "pink")+
                 theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                labs(x = "Genero",y = "Cantidad de peliculas")
        }
        
    })
    
    output$plotGenerosPais <- renderPlot({
        if(!is.null(input$pais)){
            
            paisInput <- input$pais
            
            
            tabla_year<-select(Netflix,listed_in,date_added,country)
         
            tabla_year<-mutate(tabla_year, year = format(as.Date(date_added, format= "%B %d, %Y"),"%Y"))
           
            tabla_year<-subset.data.frame(tabla_year,grepl(paisInput,country))
           
            #tabla_year<-tabla_year[tabla_year$listed_in != '',]
            tabla_year<-tabla_year %>% group_by(listed_in)
            tabla_pais<-tabla_year %>% summarise(Cantidad = n())
            
            tabla_pais<-arrange(tabla_pais,desc(Cantidad))

            tabla.gen.pais<-tabla_pais[1:10,]

            ggplot(tabla.gen.pais, aes(x = reorder(listed_in,-Cantidad), y = Cantidad)) + 
                geom_col(colour = "pink", fill= "pink")+
                theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                labs(x = "Genero",y = "Cantidad de peliculas")
        }
        
    })

    #Data Popularidad Table 
    output$data_tablePopularidad <- renderDataTable( {netflixGeneral}, 
                                          options = list(aLengthMenu = c(5,25,50),
                                                         iDisplayLength = 5)
        )
    #Data Titulos Table 
    output$data_tableTitulos <- renderDataTable( {Netflix}, 
                                                     options = list(aLengthMenu = c(5,25,50),
                                                                    iDisplayLength = 5)
    )
  
    
}

shinyApp(ui, server)