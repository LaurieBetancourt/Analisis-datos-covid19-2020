library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(rnaturalearth)
library(rgeos)
library(sf) 
library(ggplot2)
library(lubridate)
library(plotly)
library(dygraphs)
library(xts)
library(rnaturalearthdata)



ui <- fluidPage(

    
    dashboardPage(
        
        ####--Color
        skin = "red",
        
        ######Titulo de la pagina
        dashboardHeader(title="COVID-19"),
        
        
        ######Aqui esta el Menú
        dashboardSidebar(
            sidebarMenu(
                menuItem("Inicio",tabName = "inicio",icon =icon("home") ),
                menuItem("Datos",tabName = "datos", icon = icon("table")),
                menuItem("Gráficas", tabName = "grafica",icon = icon("chart-bar")),
                menuItem("Mapa",tabName = "mapa", icon = icon("map")),
                dateInput("date1","Fecha de inicio:", value = "2020-01-04"),
                dateInput("date2", "Fecha Fin:", value = today()),
                uiOutput("pais"),
                checkboxInput("logscale", "Log Y", value = FALSE)
                
            )
            
        ), # <-- fin DASHBOAR SIDEBAR
        
        
        dashboardBody(
            
            tabItems(
                
                #---Seccion INICIO
                tabItem( tabName = "inicio", 
                         
                         h1("INICIO",align="center", style= "background-color:#DB4E4E;padding:15px;border-radius:10px"),
                         
                         br(),
                         
                         p(" Los Datos que se analizarán en este documento, procede de la compilación hecha por usuarios de 
                         https://www.kaggle.com/imdevskp/corona-virus-report.",br() ,br(), 
                         "La fecha del análisis empieza el 10 de Mayo de 2020, utilizando la versión número 102 recopilada en la web anterior.
                         Los datos de igual manera estan recolectados hasta esa fecha."
                           ,style="text-align:justify;color:black;background-color:#F8F8F8;padding:15px;border-radius:10px"),
                         
                         br(),
                         
                         
                         h3("¿Cuál es el Propósito de esta App?",align="center"),
                         
                         br(),
                         
                         p("Esta patología ha llevado a la sociedad a una situación de nerviosismo, alerta 
                       e incluso pánico a nivel mundial. Entre las principales consecuencias 
                       de este fenómeno, se ha podido evidenciar, una volatilidad en el comercio, disminución en el turismo 
                       e incluso la reducción en el precio del petróleo.",br(),br(),"Estos efectos han sido potencializados por la velocidad del 
                       flujo de información en redes sociales que desencadenan lo que se conoce como infodemia, en el cuál la información 
                       se propaga muy velozmente, gracias al alto nivel conectividad entre individuos, lo que le permite llegar a 
                       cualquier parte del mundo en segundos, desencadenando cambios importantes en el comportamiento de los individuos 
                       y de la sociedad como conjunto.",br(), br(), "Por esta razón es imperativo que el usuario en redes esté informado y tenga 
                       una perspectiva objetiva de la dinámica de la evolución de esta pandemia.",
                           style="text-align:justify;color:black;background-color:#F8F8F8;padding:15px;border-radius:10px"),
                         
                         br(),
                         
                         h3("¿Qué analizaremos?",align="center"),
                         
                         br(),
                         
                         p("El comportamiento histórico de la pandemia provocada
                            por el COVID-19, así como las actualizaciones que se vayan realizando de los datos.",
                           style="text-align:justify;color:black;background-color:#F8F8F8;padding:15px;border-radius:10px"),
                         
                         br(),
                         
                         
                         h2("Información acerca del virus COVID-19",align="center"),
                         
                         br(),
                         
                         h3("¿Qué es un coronavirus?",align="center"),
                         
                         br(),
                         
                         p("Los coronavirus son una extensa familia de virus que pueden causar enfermedades 
                            tanto en animales como en humanos. En los humanos, se sabe que varios coronavirus 
                            causan infecciones respiratorias que pueden ir desde el resfriado común hasta enfermedades 
                            más graves, como el síndrome respiratorio de Oriente Medio (MERS) y el síndrome 
                            respiratorio agudo severo (SRAS).", 
                           style="text-align:justify;color:black;background-color:#F8F8F8;padding:15px;border-radius:10px"),
                         
                         br(),
                         
                         h3("¿Qué es el COVID-19?",align="center"),
                         
                         br(),
                         
                         p("El COVID‑19 es la enfermedad infecciosa causada por el coronavirus que se ha 
                            descubierto más recientemente. Tanto este nuevo virus como la enfermedad que 
                            provoca eran desconocidos antes de que estallara el brote en Wuhan (China) 
                            en diciembre de 2019. Actualmente la COVID‑19 es una pandemia que afecta a 
                            muchos países de todo el mundo.",
                           style="text-align:justify;color:black;background-color:#F8F8F8;padding:15px;border-radius:10px")
                         
                         
                         
                         
                ), #<--FIN TabItem INICIO
                
                
                
                
                # ----Seccion Datos
                tabItem(tabName = "datos",
                        
                        h1("DATOS",align="center", style= "background-color:#DB4E4E;padding:15px;border-radius:10px"),
                        
                        br(),
                        
                        p("En esta sección se encuentra la información de la base de datos utilizada para el análisis, la cual esta
                        segmentada por país"
                          ,style="text-align:justify;color:black;background-color:#F8F8F8;padding:15px;border-radius:10px"),
                        
                        br(),
                  
                        tableOutput("TablaDatos")
                      
                ),
                
                
                
                
                #---Seccion GRAFICA
                tabItem(tabName = "grafica",
                        
                        h1("GRÁFICAS", align="center", style= "background-color:#DB4E4E;padding:15px;border-radius:10px"),
                        
                        br(),
                        
                        p(" En esta sección se encuentran dos gráficas. Cada una cambiará de acuerdo con el país. Además, 
                          La primer gráfica puede visualizarse en modo normal y logarítmico."
                          ,style="text-align:justify;color:black;background-color:#F8F8F8;padding:15px;border-radius:10px"),
                        
                        h3("Tipo de Casos por País",align="center"),
                    
                        plotOutput("CasosGrafica"),
                        
                        br(),
                        
                        h3("Gráfico Interactivo",align="center"),
                        
                        br(),
                        
                        dygraphOutput("CasosGraficaInteractivo")
                        
                        
                    ), #<--FIN TabItem GRAFICAS
                
                
            
                #---Seccion MAPA---
                tabItem(tabName = "mapa",
                        
                        h1("MAPA",align="center", style= "background-color:#DB4E4E;padding:15px;border-radius:10px"),
                        
                        br(),
                        
                        p("En esta sección se encuentra la concentración geográfica de los Casos Confirmados.
                          Los datos estan actualizados a la fecha del 10 de Mayo del 2020"
                          ,style="text-align:justify;color:black;background-color:#F8F8F8;padding:15px;border-radius:10px"),
                        
                    
                        plotOutput("MapaCasos")
                    
                        
                    
                    ) #<--FIN TabItem Mapa
                
                
            )# <-- FIN TAMITEMS
            
            
        )#<--FIN dashboarBODY
        
        
    ) # <--- FIN DASHBOAR PAGE
   
)

#### ----------------------------------------- INICIO SERVER -------------------------------------------
server <- function(input, output, session) {
    
    mydata <- reactive({
        
        datos <- read.csv("covid_19_clean_complete.csv")

    #----Cambiar Nombre a Columnas
    colnames(datos) = c("Provincia_Estado",
                         "Pais_Region",
                         "Latitud", #Norte+ o Sur-
                         "Longitud", # Este+ u Oeste-
                         "Fecha",
                         "Casos_Confirmados",
                         "Casos_Muertos",
                         "Casos_Recuperados")
    
    
    ### --- Manipulacion de Fechas---
    datos$Provincia_Estado = factor(datos$Provincia_Estado)
    datos$Pais_Region = factor(datos$Pais_Region)
    datos$Fecha = as.Date(datos$Fecha, format = "%m/%d/%y")
    
    
    ###----Crear Columna NUEVA de 'Casos Enfermos'---
    datos <- datos %>%
        mutate(Casos_Enfermos = Casos_Confirmados - Casos_Muertos - Casos_Recuperados)
    
    
    #### --------------------------- MAPA -------------------
    
    #---Base datos geograficos
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    ##------Correccion nombre de paises 'datos' y 'world'
    
    #--Pais Estados Unidos
    datos$Pais_Region = factor(datos$Pais_Region, levels = c(levels(datos$Pais_Region), "United States"))
    datos [ datos$Pais_Region == "US",]$Pais_Region = "United States"
    
    #--Pais Republica Democratica del Congo
    datos$Pais_Region = factor(datos$Pais_Region, levels = c(levels(datos$Pais_Region), "Dem. Rep. Congo"))
    datos [ datos$Pais_Region == "Congo (Kinshasa)",]$Pais_Region = "Dem. Rep. Congo"
    
    #--Pais Republica Congo
    datos$Pais_Region = factor(datos$Pais_Region, levels = c(levels(datos$Pais_Region), "Congo"))
    datos [ datos$Pais_Region == "Congo (Brazzaville)",]$Pais_Region = "Congo" 
    
    #--Pais Sudan Sur
    datos$Pais_Region = factor(datos$Pais_Region, levels = c(levels(datos$Pais_Region), "S. Sudan"))
    datos [ datos$Pais_Region == "South Sudan",]$Pais_Region = "S. Sudan"
    
    #--Pais Republica Centroafricana
    datos$Pais_Region = factor(datos$Pais_Region, levels = c(levels(datos$Pais_Region), "Central African Rep."))
    datos [ datos$Pais_Region == "Central African Republic",]$Pais_Region = "Central African Rep."
    
    
    #---Diseño MAPA 'Casos Confirmados'
    world %>%
        inner_join(datos,by=c("name"="Pais_Region")) %>%
        filter(Fecha == dmy("10-05-2020")) %>%
        
        ggplot() + 
        geom_sf(color="black", aes(fill= Casos_Confirmados)) + 
        scale_fill_viridis_c(option = "plasma",trans = "sqrt") +
        xlab("Longitud") + ylab("Latitud") + 
        ggtitle("Mapa del Mundo", subtitle = "COVID 19")
    
    
         
            
            return(datos %>% filter(between(Fecha, input$date1, input$date2)))
            
    }) ####--------------------------- FIN MyData-------              
            
            
            
            
    
    ####----------------------------------------------- INICIO OUTPUTS --------------------------------------------
    
    
    #### 1) -----Seleccion Pais
    
    output$pais <- renderUI({
        
        countries <- mydata() %>%
            select(Pais_Region) %>%
            arrange(Pais_Region) %>%
            unique()
        selectInput("pais", "Selecciona el País: ", choices = countries)
        
    })
    
    
    
    
    output$TablaDatos <- renderTable({
        
        mydata() %>% filter(Pais_Region == input$pais)
    })
    
    
    
    
    
    #### 2) ----------------Datos 'casos confirmados' a mostrar 
    
    output$MapaCasos <- renderPlot({
        
        #---Diseño MAPA 'Casos Confirmados'
        world %>%
            inner_join(mydata(),by=c("name"="Pais_Region")) %>%
            filter(Fecha == dmy("10-05-2020")) %>%
            
            ggplot() + 
            geom_sf(color="black", aes(fill= Casos_Confirmados)) + 
            scale_fill_viridis_c(option = "plasma",trans = "sqrt") +
            xlab("Longitud") + ylab("Latitud") + 
            ggtitle("Mapa del Mundo", subtitle = "COVID 19")
        
        
        
        
        
    }) #<--FIN OUTPLOT 'casos confirmados'----------
    
    
    

    
    
    
    
    #### 3)--------------Evolucion 'Confirmados' vs 'Muertos' vs 'Recuperados' en el Mundo -------
            
    output$CasosGrafica <- renderPlot({
        
        ####----------------- ANALISIS DE DATOS TEMPORAL ----------
        
        datos_por_fecha = aggregate(
            cbind( Casos_Confirmados, Casos_Muertos, Casos_Recuperados) ~ Fecha,
            data = mydata() %>% filter(Pais_Region == input$pais),
            FUN = sum  
        )
        
        datos_por_fecha$Casos_Enfermos = datos_por_fecha$Casos_Confirmados -
            datos_por_fecha$Casos_Muertos - datos_por_fecha$Casos_Recuperados
        
        
        #-- Definir variable log Y
        
        logy = ""
        lims = c(0, max(datos_por_fecha$Casos_Confirmados))
        
        if(input$logscale) {
            logy = "y"
            datos_por_fecha %>%
                filter(Casos_Confirmados > 0)
            lims = c(1, max(datos_por_fecha$Casos_Confirmados))
        }
        
        
        #--- Grafica
        plot(Casos_Confirmados ~ Fecha, data = datos_por_fecha, col = "#29CCED", type = "l",
             ylim = lims,
             main = paste0("Casos Confirmados por Dia en  ", input$pais), 
             xlab = "Fecha", ylab = "Número de Personas",
             log = logy, lwd = 2)
        
        lines(Casos_Muertos ~ Fecha, data = datos_por_fecha, col="#BA513A", lwd = 2)
        lines(Casos_Recuperados ~ Fecha, data = datos_por_fecha, col = "#F59D1D", lwd = 2)
        
        #--- Leyenda
        legend("topleft", c("Confirmados", "Muertos", "Recuperados"), 
               col = c("#29CCED","#BA513A","#F59D1D"), lwd = 4)
        
        
        
    }) # <--- FIN OUTPLOT Evolucion------------------
    
    
    
    
    
    
    
    
    
    #### 4) ------------ Evolucion Graficos Interactivos -------------
    output$CasosGraficaInteractivo <- renderDygraph({

        datos_por_fecha = aggregate(
            cbind( Casos_Confirmados, Casos_Muertos, Casos_Recuperados) ~ Fecha,
            data = mydata() %>% filter(Pais_Region == input$pais),
            FUN = sum  
        )
        
        datos_por_fecha$Casos_Enfermos = datos_por_fecha$Casos_Confirmados -
            datos_por_fecha$Casos_Muertos - datos_por_fecha$Casos_Recuperados
        
        datos_por_fecha_ts <- xts(x = datos_por_fecha[,2:5],
                                  order.by = datos_por_fecha$Fecha)
        
        dygraph(datos_por_fecha_ts) %>%
            dyOptions(labelsUTC = TRUE, labelsKMB = TRUE,
                      fillGraph = TRUE, fillAlpha = .05,
                      drawGrid = FALSE,
                      colors = c("#0A1E6B","#DE4F48","#F59B30","#663BDA")) %>%
            dyRangeSelector() %>%
            dyCrosshair(direction = "vertical") %>%
            dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2,
                        hideOnMouseOut = FALSE)
        
   
        
    }) #### ------- FIN Evolucion Graficos Interactivos -------
    
    
    

    
    
    
    
    
    
    } # <----FIN SERVER

# Run the application 
shinyApp(ui = ui, server = server)
