library(plotly)
library(shiny)
library(shinydashboard)

# Definimos la interfaz de usuario para el proyecto de Crimen
shinyUI({
  
  ###############
  # H E A D E R # 
  # Encabezado  #
  ###############
  
  dbHeader <- dashboardHeader(title = "El crimen en México.",
                              titleWidth="400px",
                              tags$li(a(href = 'https://cee.colmex.mx/',
                                        img(src = 'https://cee.colmex.mx/images/logos/Logo_CEE_.png',
                                            title = "CEE", height = "25px"),
                                        style = "padding-top:0px; padding-bottom:0px;padding-right:15px; padding-left:15px;"),
                                      class = "dropdown"),
                              tags$li(a(href = 'https://www.colmex.mx',
                                        img(src = 'https://www.colmex.mx/assets/colmex-5f07a8b09f687e8cbf6e08b130e55a2a1c621c39bc4fa927cacfa2026b2bfa3c.png',
                                            title = "Colmex", height = "25px"),
                                        style = "padding-top:0px; padding-bottom:0px;padding-right:15px; padding-left:15px;"),
                                      class = "dropdown"), 
                              tags$li(a(href = 'https://github.com/SusanaAlmeidaM/VisualizacionCrimen',
                                        img(src = 'https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg',
                                            title = "GitHub", height = "25px"),
                                        style = "padding-top:0px; padding-bottom:0px;padding-right:15px; padding-left:15px;"),
                                      class = "dropdown"))
  
                              
                              
#################
# S I D E B A R #
# Barra lateral #  
#################

  sidebar <- dashboardSidebar(
    width = 170, 
    
    sidebarMenu(
      menuItem("Nivel Estatal", tabName = "tabNIVEL_ESTATAL"), 
      menuItem("Nivel Municipal", tabName = "tabNIVEL_MUNICIPAL"),
      menuItem("El crimen en el cine", tabName = "tabCINE")
    )
    
  )
  

############
# B O D Y  #
# Cuerpo   #
###########

body <- dashboardBody(
  
  #####################################
  # Estilo y colores del contenido!!! #
  ####################################
  #####################################
  # Estilo y colores del contenido!!! #
  ####################################
  tags$head(tags$style(
    HTML("@import url('https://fonts.googleapis.com/css?family=Karla|Source+Serif+Pro|Ultra');
         .content-wrapper {
         background-color: white !important;}
         label, input, button, select { 
         font-family: 'Karla', sans-serif;
         font-color = #606060;
         font-size: 14pt;
         }
         
         body {
         background-color: white;
         style= width: 80%;
         }
         p {
         text-indent: 0px;
         font-family: 'Karla', sans-serif;
         font-size: 12pt;
         color: #606060;
         text-align:justify;
         padding-top: 5px;
         padding-botton: 10px;
         padding-right:190px;
         padding-left:70px;
         
         }
         .main-header .logo {
         font-family: 'Ultra', serif;
         font-size: 28px;
         background-color:white;
         padding-top:0px; 
         padding-bottom:0px;
         }
         
         .main-sidebar {
         background-color: white !important;
         }
         
         
         .container {
         position: relative;
         width: 50%;
         }
         
         .image {
         opacity: 1;
         display: block;
         width: 100%;
         height: auto;
         transition: .5s ease;
         backface-visibility: hidden;
         }
         
         .middle {
         transition: .5s ease;
         opacity: 0;
         position: absolute;
         top: 50%;
         left: 50%;
         transform: translate(-50%, -50%);
         -ms-transform: translate(-50%, -50%)
         }
         
         .container:hover .image {
         opacity: 0.3;
         }
         
         .container:hover .middle {
         opacity: 1;
         }
         
         .text {
         background-color: black;
         color: white;
         font-size: 14px;
         padding: 16px 32px;
         }
         
         .box.box-solid.box-primary>.box-header {
         color:#606060;
         background:white;
         background-color:#f0f0f5;
         font-family: 'Karla', sans-serif;
         text-align:center;
         }
         
         
         .box.box-solid.box-warning>.box-header {
         color:#606060;
         background:white;
         background-color:#f0f0f5;
         font-family: 'Karla', sans-serif;
         text-align:center;
         }
         
         .box.box-solid.box-primary {
         color:#606060;
         background-color:white;
         border-color:white !important;
         }
         
         .box.box-solid.box-warning {
         color:#606060;
         background-color:white;
         border-color:white;
         }
         
         li {
         font-family: 'Karla', sans-serif;                    
         font-size: 16px;
         margin-top: 15px;
         padding-top: 0px;
         padding-botton: 0px;
         border:1px;
         line-height: 1.7;
         font-color:#606060;
         
         }
         li span {
         font-family: 'Karla', sans-serif;
         font-size: 15px;
         font-color:#606060;
         
         }
         ul {
         list-style-type: none;
         }
         
         h2 {
         font-family:'Ultra', serif;
         font-weight: 500;
         line-height: 1.1;
         color: 	#000000;
         font-size: 22pt;
         text-align:center;
         padding-right:190px;
         padding-left:70px;
         }
         "))
    ),
  
    
  tabItems(

    ######################################  
    tabItem("tabCINE",
            h2("Devolverle la voz a lo que se vuelve cifra"),
            br(),
            p("Selección de películas mexicanas que retratan las diversas vertientes del crimen.", style= "text-align:center" ),
            
            p("Trata de personas", style= "text-align:center"),
            tags$div(
              class = "container",
              tags$img(src="las_elegidas.jpg", style = "alt:Avatar; class:image; 
                       display: block; width:550px; height:250px;  opacity: 1;
                       transition: .5s ease;
                       backface-visibility: hidden;"),
              tags$div(class = "middle",
                       tags$div(class = "text", "Las elegidas de David Pablos (2016)")
              )),
            br(),
            tags$div(
              class = "container",
              tags$img(src="esclava.jpg", style = "alt:Avatar; class:image; 
                       display: block; width:550px; height:250px;  opacity: 1;
                       transition: .5s ease;
                       backface-visibility: hidden;"),
              tags$div(class = "middle",
                       tags$div(class = "text", "Esclava de Amat Escalante (2015)")
              )),
            br(),
            
            p("Feminicidios", style= "text-align:center"),
            tags$div(
              class = "container",
              tags$img(src="bajo_la_sal.jpg", style = "alt:Avatar; class:image; 
                       display: block; width:550px; height:250px;  opacity: 1;
                       transition: .5s ease;
                       backface-visibility: hidden;"),
              tags$div(class = "middle",
                       tags$div(class = "text", "Bajo la sal de  Mario Muñoz (2008)")
              )),
            br(),
            tags$div(
              class = "container",
              tags$img(src="backyard.jpg", style = "alt:Avatar; class:image; 
                       display: block; width:550px; height:250px;  opacity: 1;
                       transition: .5s ease;
                       backface-visibility: hidden;"),
              tags$div(class = "middle",
                       tags$div(class = "text", "Backyard de Carlos Carrera (2009)")
              )),
            br(),
            
            p("Violencia de género", style= "text-align:center;"),
            tags$div(
              class = "container",
              tags$img(src="batallas_intimas.jpg", style = "alt:Avatar; class:image; 
                       display: block; width:550px; height:250px;  opacity: 1;
                       transition: .5s ease;
                       backface-visibility: hidden;"),
              tags$div(class = "middle",
                       tags$div(class = "text", "Batallas íntimas de Lucía Gajá (2017)")
              )),
            br(),
            tags$div(
              class = "container",
              tags$img(src="perfume.jpg", style = "alt:Avatar; class:image; 
                       display: block; width:550px; height:250px;  opacity: 1;
                       transition: .5s ease;
                       backface-visibility: hidden;"),
              tags$div(class = "middle",
                       tags$div(class = "text", "Perfume de violetas de Maryse Sistach (2001)
                                ")
                       )),
            br(),
            tags$div(
              class = "container",
              tags$img(src="miss_bala.jpg", style = "alt:Avatar; class:image; 
                       display: block; width:550px; height:250px;  opacity: 1;
                       transition: .5s ease;
                       backface-visibility: hidden;"),
              tags$div(class = "middle",
                       tags$div(class = "text", "Miss Bala de Gerardo Naranjo (2011)")
              )),
            br(),
            
            p("Crimen organizado", style= "text-align:center;"),
            tags$div(
              class = "container",
              tags$img(src="heli.jpg", style = "alt:Avatar; class:image; 
                       display: block; width:550px; height:250px;  opacity: 1;
                       transition: .5s ease;
                       backface-visibility: hidden;"),
              tags$div(class = "middle",
                       tags$div(class = "text", "Heli de Amat Escalante (2013)")
              )),
            br(),
            tags$div(
              class = "container",
              tags$img(src="tempestad.jpg", style = "alt:Avatar; class:image; 
                       display: block; width:550px; height:250px;  opacity: 1;
                       transition: .5s ease;
                       backface-visibility: hidden;"),
              tags$div(class = "middle",
                       tags$div(class = "text", "Tempestad de Tatiana Huezo (2017)")
              )),
            br(),
            tags$div(
              class = "container",
              tags$img(src="la_libertad.jpg", style = "alt:Avatar; class:image; 
                       display: block; width:550px; height:250px;  opacity: 1;
                       transition: .5s ease;
                       backface-visibility: hidden;"),
              tags$div(class = "middle",
                       tags$div(class = "text", "La libertad del diablo de Everardo González (2016)")
              )),
            br(),
            
            p("Violencia familiar", style= " text-align:center;"),
            tags$div(
              class = "container",
              tags$img(src="el_sandwich.png", style = "alt:Avatar; class:image; 
                       display: block; width:550px; height:250px;  opacity: 1;
                       transition: .5s ease;
                       backface-visibility: hidden;"),
              tags$div(class = "middle",
                       tags$div(class = "text", "El sándwich de Marina de Carlos Cuarón (2014)")
              )),
            br(),
            
            p("Inseguridad", style= "text-align:center;"),
            tags$div(
              class = "container",
              tags$img(src="la_zona.jpg", style = "alt:Avatar; class:image; 
                       display: block; width:550px; height:250px;  opacity: 1;
                       transition: .5s ease;
                       backface-visibility: hidden;"),
              tags$div(class = "middle",
                       tags$div(class = "text", "La zona de Rodrigo Plá (2007)")
              )),
            br(),
            tags$div(
              class = "container",
              tags$img(src="todo_poder.jpg", style = "alt:Avatar; class:image; 
                       display: block; width:550px; height:250px;  opacity: 1;
                       transition: .5s ease;
                       backface-visibility: hidden;"),
              tags$div(class = "middle",
                       tags$div(class = "text", "Todo el poder de Fernando Sariñana (1999)")
              )),
            br(),
            
            p("Secuestro", style= "text-align:center;"),
            tags$div(
              class = "container",
              tags$img(src="dias_gracia.jpg", style = "alt:Avatar; class:image; 
                       display: block; width:550px; height:250px;  opacity: 1;
                       transition: .5s ease;
                       backface-visibility: hidden;"),
              tags$div(class = "middle",
                       tags$div(class = "text", "Secuestro: Días de gracias de Everardo Gout (2011) ")
              ))
            
            ),
    
    tabItem("tabNIVEL_ESTATAL", 
            h2("Nivel estatal"),
            
            fluidPage(
              fluidRow(
                br(),
                column(8, p(" El", span("Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)", style = "color:#000000"), "es el encargado de reportar y clasificar mensualmente los crímenes. 
                            Esta base de datos existe desde 2015 y reporta únicamente los crímenes denunciados o aquellos bajo investigación por las autoridades de procuración de justicia del estado. 
                            Se clasifican los delitos por el bien jurídico afectado; existen siete categorías: 
                            crímenes contra la vida y la integridad, la libertad personal, la libertad y la seguridad sexual, el patrimonio, la familia, la sociedad y otros. ", style = "padding-right:0px; padding-left:60px")),
                column(2,tags$li(a(href = 'https://www.gob.mx/sesnsp/acciones-y-programas/incidencia-delictiva-del-fuero-comun-nueva-metodologia?state=published',
                                   img(src = 'https://www.ordenadorpolitico.com/wp-content/uploads/2017/11/1-268.jpg',style = "width:260px;height:110px; padding-left:0px; padding-right:40px; padding-top:0px")), style = "list-style-type: none")))
                ),
            
            fluidPage(
              fluidRow(
                column(2,tags$li(a(href='http://economicsandpeace.org/reports/',
                                   img(src = 'https://upload.wikimedia.org/wikipedia/commons/thumb/3/3d/Institute_for_Economics_and_Peace_Logo.svg/220px-Institute_for_Economics_and_Peace_Logo.svg.png', style = "padding-right: 0px; padding-left:60px; padding-top:0px; padding-bottom:0px; width:180px;height:100px")), style = "list-style-type: none")),
                
                br(),
                column(8, p("Por otro lado,", span("The Institute for Economics and Peace (IEP)", style = "color:#000000"), "es una organización internacional dedicada a construir métricas del nivel de paz y su impacto en la economía de los países. 
                            La paz es definida como “la ausencia de violencia o miedo a la violencia”. Desde 2013 dicha institución publica un reporte anual
                            sobre el índice de Paz en México, el cual incluye datos a nivel estatal. El",span("índice de Paz México (IPM)", style = "color:#000000"),"está conformado por siete indicadores:", style = "padding-right:0px; padding-left:0px;")))
              
                ),
            
            
            tags$div(tags$ul(
              tags$li(tags$span(" • Homicidios: tasa de homicidios por cada 100, 000 habitantes", style = "padding-top:10px; padding-bottom:0px;padding-right:190px; padding-left:70px")),
              tags$li(tags$span(" • Delitos con violencia: tasa de delitos con violencia por cada 100, 000 habitantes" ,style = "padding-top:10px; padding-bottom:0px;padding-right:190px; padding-left:70px")),
              tags$li(tags$span(" • Delitos cometidos con armas de fuego: tasa de delitos con violencia por cada 100, 000 habitantes", style = "padding-top:10px; padding-bottom:0px;padding-right:190px; padding-left:70px")),
              tags$li(tags$span(" • Encarcelamiento: número de personas encarceladas al año por cada 100, 000 habitantes (fuente: INEGI)", style = "padding-top:10px; padding-bottom:0px;padding-right:190px; padding-left:70px")),
              tags$li(tags$span(" • Financiamiento de las fuerzas policiales: financiamiento del gobierno federal a
                                los estados para la Seguridad Pública.", style = "padding-top:10px; padding-bottom:0px;padding-right:190px; padding-left:70px")),
              tags$li(tags$span(" • Delincuencia organizada: número de extorsiones, delitos contra la salud y secuestros por cada 100, 000 habitantes", style = "padding-top:10px; padding-bottom:0px;padding-right:190px; padding-left:70px")),
              tags$li(tags$span(" • Eficiencia del sistema judicial: proporción de sentencias por homicidio respecto al total de homicidios registrados (fuente INEGI)", style = "padding-top:10px; padding-bottom:0px;padding-right:190px; padding-left:70px"))),
              
              br(),
              
              p("Todos los indicadores reciben una calificación entre 1 y 5 (1: mayor paz y 5: menor paz). Después de calcular la calificación de cada indicador, 
                se aplican ponderadores a cada uno de los indicadores con el fin de calcular la calificación final.")
              ),
            p("El mapa siguiente presenta los datos más relevantes del reporte del índice de Paz México por año (2015-2018). En los paneles inferiores se muestran los datos del SESNSP separados por crímenes que atentan contra la persona y contra la propiedad. 
              También se incluyen las gráficas sobre el nivel de pobreza y de rezago educativo del estado seleccionado.  "),
            br(),
            
            box(title = "Introduzca Año de Consulta", status = 'primary', solidHeader = FALSE, width = 12, collapsible = TRUE, 
                # Contenido
                selectInput(inputId = "Anio_Estatal", label = "Seleccione Año", choices = c(2015, 2016, 2017, 2018))
            ), 
            fluidPage(
              fluidRow(
                column(12, box(title = "Mapa de la República Mexicana", status = 'primary', solidHeader = TRUE, width = 12, collapsible = TRUE, 
                               # Contenido
                               shinycssloaders::withSpinner(leafletOutput("MAPA", height = 500)) 
                )
                )
              )
            ), 
            
            fluidPage(
              fluidRow(
                column(6, box(title = "Crímenes a la persona a Nivel Estatal", status = 'primary', solidHeader = TRUE, width = 12, collapsible = TRUE, 
                              withSpinner(plotlyOutput("dona", width = "100%")) 
                )
                ), 
                column(6, box(title = "Pobreza", status = 'primary', solidHeader = TRUE, width = 12, collapsible = TRUE, 
                              withSpinner(plotlyOutput("Pobreza", width = "100%")) 
                )
                )
              ), 
              fluidRow(
                column(5, withSpinner(plotlyOutput("TS", width = "50%"))), 
                column(6, box(title = "Rezago", status = 'primary', solidHeader = TRUE, width = 12, collapsible = TRUE, 
                              withSpinner(plotlyOutput("rezago"))), offset = 1
                )
              )
            ),
            
            p("Glosario", style= "font-size: 24pt; text-align:center; font-family:'Source Serif Pro', serif;"),
            tags$div(tags$ul(
              tags$li(tags$span("Lesiones: incluye los daños o perjuicios dolosos y culposos realizados con arma de fuego, arma blanca u otro elemento.")),
              tags$li(tags$span("Homicidios: incluye asesinatos dolosos y culposos incluyendo la categoría de feminicidios. También, de esta parte se excluyeron los datos de la categoría de Aborto.")),
              tags$li(tags$span("Abuso sexual: en esta categoría se incluyen los crímenes de hostigamiento sexual, violación simple o equiparada (i.e. a personas incapacitadas para resistir física o psíquicamente el acto) e incesto.")),
              tags$li(tags$span("Secuestros: aquí se incluyen secuestros extorsivos, secuestros con calidad de rehén, secuestro para causar daño, secuestro exprés, tráfico de menores y rapto. ")),
              tags$li(tags$span("Violencia familiar: se refiere a los casos denunciados de violencia familiar y violencia de género en todas sus modalidades distintas a la violencia")),
              tags$li(tags$span("Robos de vehículos: comprende robos (con y sin violencia) de automóviles,  motocicletas y embarcaciones.")),
              tags$li(tags$span("Robos a transeúnte: comprende robos (con y sin violencia) a transeúntes en vías públicas y en espacios abiertos al público.")),
              tags$li(tags$span("Robo en transporte: comprende robos (con y sin violencia) en transporte público individual, colectivo y en transporte individual. ")),
              tags$li(tags$span("Pobreza: “Una persona se encuentra en situación de pobreza cuando tiene al menos una carencia social (en los seis indicadores de rezago educativo, acceso a servicios de salud,
                                acceso a la seguridad social, calidad y espacios de la vivienda, servicios básicos en la vivienda y acceso a la alimentación) y su ingreso es insuficiente para adquirir los bienes
                                y servicios que requiere para satisfacer sus necesidades alimentarias y no alimentarias” (CONEVAL). Promedio nacional: 43.6%")),
              tags$li(tags$span("Rezago educativo: “Se considera que una persona  está en situación de rezago educativo si i) tiene de tres a 15 años y no cuenta con la educación básica obligatoria ni asiste 
                                a un centro de educación formal; ii) nació antes de 1982 y no cuenta con el nivel de educación obligatoria vigente en el momento en que debía haberla cursado (primaria completa);
                                o nació a partir de 1982 y no cuenta con el nivel de educación obligatoria (secundaria completa)”. (CONEVAL). Promedio nacional: 17.4% "))
              
              
              ))
              ), 
    
    
  
    
    tabItem("tabNIVEL_MUNICIPAL", 
            h2("Nivel Municipal"),
            br(),
           
            p("El Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública también cuenta con una base de datos de crímenes denunciados desde 2015 a nivel municipio. En la siguiente figura
              se pueden explorar las cifras sobre el número de delitos cometidos a nivel municipal, así como su tipo y subtipo. "),
            br(), 
            
            box(title = "Selecciona los datos de interés ", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                # CONTENIDO:
                fluidPage(
                  fluidRow(
                    column(4, selectInput(inputId = "Anio", label = "Año: ", choices = levels(as.factor(D$Año)))), 
                    column(4, selectInput(inputId = "Estado", label = "Estado: ", choices = levels(D$Entidad))),
                    column(4, selectInput(inputId = "Municipio", label = "Municipio: ", choices = levels(D$Municipio)))
                  )  
                )
                
            ),
            
            fluidPage(
              fluidRow(
                column(6, box(title = "Delitos cometidos a nivel municipal ", status = 'primary', solidHeader = TRUE, width = 12,
                              # CONTENIDO: 
                              shinycssloaders::withSpinner(highchartOutput("tm"))
                              # , 
                              # textOutput("text") 
                )
                )
                ,
                column(6, box(title = "Subtipo y modalidad de delito ", status = 'warning', solidHeader = TRUE, width = 12, align= 'center',
                              # Contenido
                              shinycssloaders::withSpinner(highchartOutput("HistDelito"))
                ))
              )
            ),
            
            br(),
            
            fluidPage(
              fluidRow(
                column(9, p("Por otro lado, INEGI dispone de datos de homicidios a nivel municipio desde 1990. Esta es la base que se utilizó para realizar el siguiente análisis econométrico. ", style = "padding-right:0px; padding-left:60px")),
                column(1,tags$li(a(href = 'http://www.inegi.org.mx/sistemas/olap/proyectos/bd/continuas/mortalidad/defuncioneshom.asp?s=est',
                                   img(src = 'https://upload.wikimedia.org/wikipedia/commons/thumb/8/89/INEGI.png/230px-INEGI.png',style = "width:100px;height:80px; padding-left:0px; padding-right:40px; padding-bottom:15px")), style = "list-style-type: none")))
            ),
            
            
            br(),
            h2("Análisis espacial"),
            br(),
            
            p("Índice de Moran local univariado", style= "font-size: 20pt; text-align:center; color:black; font-family:'Source Serif Pro', serif;"),
      
            p("El  índice  de  Moran  es  una" ,span("medida de autocorrelación", style = "color:#000000"), "espacial  que  tiene  como  objetivo  encontrar  similitudes  entre  algunas  variables  en  espacios  cercanos.  
              Cuando  este  índice  tiene  un  valor  positivo  (negativo)  y  significativo 
              se  puede  inferir  que  existe  algún  tipo  de  relación  espacial  positiva  (negativa)  entre  los  valores  de  la  variable  en  cuestión."),
            
            
            fluidPage(
              fluidRow(
                column(12, sliderInput(inputId = "Anios_graficas", label = "Seleccione Año", min = 1990, max = 2017, value = 1990, step = 1, round = T, animate = T, timeFormat = "%Y", sep = ""))
              )
            ), 
            
            fluidPage(
              fluidRow(
                column(5, withSpinner(imageOutput("cluster", width = "auto"))), 
                column(7, box(title = "Coeficiente de Moran por año", width = 12, solidHeader = T, status = 'warning', 
                              withSpinner(plotlyOutput("moran"))))
              )
            ), 
            
            
           
            br(),
            p("Índice de Moran local bivariado", style= "font-size: 20pt; text-align:center; color:black;font-family: 'Source Serif Pro', sans-serif;"),
            
            p("El" ,span("Índice de Moran bivariado", style = "color:#000000"), "nos permite relacionar el número de homicidios en un municipio específico, con los niveles alcanzados por sus vecinos en un momento previo en el tiempo.
              Esto permite inferir cuáles municipios se han mantenido violentos a través del tiempo "),
            p("Cada municipio se clasifica en una de las siguientes cuatro condiciones:"),
            
            p(" •	", span("Municipio en resilencia", style = "color:#000000"),": se caracterizan por mantener bajos niveles de homicidios (por debajo de la media de la variable Z(t)), a pesar de que su rezago espacio-temporal los tenga altos (por encima de la media de la variable WZ(t-i))"),
            p(" •	", span("Municipio en exclusión", style = "color:#000000"),": que experimenta altos niveles de homicidios y su rezago espacio-temporal tiene bajos niveles de esta variable. "),
            p(" •	", span("Municipio en trampa de violencia", style = "color:#000000"),": cuyos niveles de homicidios y rezago espacio-temporal son altos"),
            p(" •	", span("Municipio en prosperidad", style = "color:#000000"),": con bajos niveles de violencia tanto en su ubicación, como en su rezago espacio-temporal"),
            
            fluidPage(
              fluidRow(
                column(5, selectInput(inputId = 'sPeriodo', 
                                       label = "Seleccione un periodo", 
                                       choices = c("1990 a 2004", "1990 a 2017", "2004 a 2017")), 
                                       imageOutput("clusterII")
                                       ), 
                column(7, br(), br(), br(), br(), box(title = "Clusters por Periodos de tiempo", status = 'primary', solidHeader = FALSE, width = 12, collapsible = TRUE,  
                                                          textOutput("Periodo"), br(),
                                                          withSpinner(plotlyOutput("moranII")))
                              )
                
                )
            ), 
            
            # fluidPage(
            #   fluidRow(
            #     column(12, box(title = "Coeficiente de Moran por Periodo", status = 'primary', solidHeader = FALSE, width = 12, collapsible = TRUE,
            #                    textOutput("Periodo"),
            #                    withSpinner(plotlyOutput("moranII")))
            #     )
            #   )
            # ),
            # 
            
            
            br(),
            p("Matrices de Markov", style= "font-size: 20pt; text-align:center; color:black; font-family: 'Source Serif Pro', sans-serif;"),
            p("Las matrices de Markov nos permitieron analizar la dinámica de los homicidios entre 1990 y 2017 a nivel municipal empleando la clasificación anterior para cada uno de los estados de la cadena. "),
            br(),
            img(src='univariado.png', style = "float:center; width:1150px;height:220px; padding-right:190px;padding-left:70px;"),
            br(),
            br(),
            p("Una propiedad importante de la matriz de transiciones de Markov es que se puede calcular la probabilidad de que, dado que una entidad se encuentra en el estado i en el presente, se encuentre en el estado j en el transcurso de dos períodos. "),
            br(),
            
            img(src='bivariado.png', style = "class: center; width:1150px;height:220px; padding-right:190px;padding-left:70px;"),
            br(),
            br(),
            p("Notamos que los resultados muestran la existencia de un patrón centro-periferia en la prevalencia de la violencia. Los estados de Baja California, Sonora, Sinaloa, Durango, el norte de Chihuahua y la región que conforman Michoacán, Guerrero, 
              Ciudad de México, Morelos y Tlaxcala se caracterizan por permanecer en una situación de trampa espacial de violencia que parece haber persistido durante casi tres décadas. No obstante, es alentador saber que los resultados del Índice de Moran parecen
              indicar que la persistencia de las trampas espaciales de violencia ha disminuido a nivel general a través del tiempo. "),
            br(),
            br(),
            br()
            
            
            
            )
    
    ##### __________________________ ######
)#FIN DEL TABITEMS
  )

###################
# DASHBOARD PAGE  #
##################

dashboardPage(skin = 'black', dbHeader, sidebar = sidebar, body = body)

})
