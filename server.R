#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  output$TS <- renderPlotly({
    if(is.null(pol_of_click$clickedShape)){
      k <- 1
    } else {
      k <- as.numeric(pol_of_click$clickedShape)
    }
    
    Propiedad <- read_xlsx(paste0("www/Bases/Propiedad/", Todos_estados[k], "_propiedad.xlsx"))
    
    xaxis <- list(title = "",
                  showline = TRUE,
                  showgrid = FALSE,
                  showticklabels = TRUE,
                  linecolor = 'rgb(204, 204, 204)',
                  linewidth = 2,
                  autotick = FALSE,
                  ticks = 'outside',
                  tickcolor = 'rgb(204, 204, 204)',
                  tickwidth = 2,
                  ticklen = 5,
                  tickfont = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgb(82, 82, 82)'))
    yaxis <- list(title = "",
                  showgrid = FALSE,
                  zeroline = FALSE,
                  showline = FALSE,
                  showticklabels = FALSE)
    
    
    
    margin <- list(autoexpand = FALSE,
                   l = 120,
                   r = 100,
                   t = 110)
    
    Casa1 <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.05,
      y = Propiedad$Casa[1],
      xanchor = 'right',
      yanchor = 'middle',
      text = ~paste('Casas robadas: ', Casa[1]),
      font = list(family = "Courier New, monospace",
                  size = 9,
                  color = '#7f7f7f'),
      showarrow = FALSE)
    
    Vehiculo1 <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.05,
      y = Propiedad$Vehiculo[1],
      xanchor = 'right',
      yanchor = 'middle',
      text = ~paste('Vehículos robados:', Vehiculo[1]),
      font = list(family = "Courier New, monospace",
                  size = 9,
                  color = '#7f7f7f'),
      showarrow = FALSE)
    
    
    Transeunte1 <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.05,
      y = Propiedad$Transeunte[1],
      xanchor = 'right',
      yanchor = 'middle',
      text = ~paste('Robos a transeuntes:', Transeunte[1]),
      font = list(family = "Courier New, monospace",
                  size = 9,
                  color = '#7f7f7f'),
      showarrow = FALSE)
    
    Transporte1 <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.05,
      y = Propiedad$Transporte[1],
      xanchor = 'right',
      yanchor = 'middle',
      text = ~paste('Robos en transporte:', Transporte[1]),
      font = list(family = "Courier New, monospace",
                  size = 9,
                  color = '#7f7f7f'),
      showarrow = FALSE)
    
    Negocio1 <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.05,
      y = Propiedad$Negocio[1],
      xanchor = 'right',
      yanchor = 'middle',
      text = ~paste('Robos a negocios:', Negocio[1]),
      font = list(family = "Courier New, monospace",
                  size = 9,
                  color = '#7f7f7f'),
      showarrow = FALSE)
    
    Casa2 <- list(
      xref = 'paper',
      x = 0.95,
      y = Propiedad$Casa[4],
      xanchor = 'left',
      yanchor = 'middle',
      text = paste(Propiedad$Casa[4]),
      font = list(family = "Courier New, monospace",
                  size = 9,
                  color = '#7f7f7f'),
      showarrow = FALSE)
    
    Vehiculo2 <- list(
      xref = 'paper',
      x = 0.95,
      y = Propiedad$Vehiculo[4],
      xanchor = 'left',
      yanchor = 'middle',
      text = paste(Propiedad$Vehiculo[4]),
      font = list(family = "Courier New, monospace",
                  size = 9,
                  color = '#7f7f7f'),
      showarrow = FALSE)
    
    Transeunte2 <- list(
      xref = 'paper',
      x = 0.95,
      y = Propiedad$Transeunte[4],
      xanchor = 'left',
      yanchor = 'middle',
      text = paste(Propiedad$Transeunte[4]),
      font = list(family = "Courier New, monospace",
                  size = 9,
                  color = '#7f7f7f'),
      showarrow = FALSE)
    
    Transporte2 <- list(
      xref = 'paper',
      x = 0.95,
      y = Propiedad$Transporte[4],
      xanchor = 'left',
      yanchor = 'middle',
      text = paste(Propiedad$Transporte[4]),
      font = list(family = "Courier New, monospace",
                  size = 9,
                  color = '#7f7f7f'),
      showarrow = FALSE)
    
    Negocio2 <- list(
      xref = 'paper',
      x = 0.95,
      y = Propiedad$Negocio[4],
      xanchor = 'left',
      yanchor = 'middle',
      text = paste(Propiedad$Negocio[4]),
      font = list(family = "Courier New, monospace",
                  size = 9,
                  color = '#7f7f7f'),
      showarrow = FALSE)
    
    t <- list(
      family = "Courier New, monospace",
      size = 11,
      color ='#7f7f7f')
    
    names(Propiedad)[1] <- "x"
    
    p <- plot_ly(Propiedad, x = ~x) %>%
      add_trace(y = ~Casa, type = 'scatter', mode = 'lines', line = list(color = '#6495ED', width = 4), hoverinfo = 'text', text = ~paste0('Ocurrencia: ', Propiedad$Casa, " casos"))  %>%
      add_trace(y = ~Vehiculo, type = 'scatter', mode = 'lines', line = list(color = '#008B8B', width = 4), hoverinfo = 'text', text = ~paste0('Ocurrencia: ', Propiedad$Vehiculo, " casos")) %>%
      add_trace(y = ~Transeunte, type = 'scatter', mode = 'lines', line = list(color = '#87CEFA', width = 4), hoverinfo = 'text', text = ~paste0('Ocurrencia: ', Propiedad$Transeunte, " casos")) %>%
      add_trace(y = ~Transporte, type = 'scatter', mode = 'lines', line = list(color = '#66CDAA', width = 4), hoverinfo = 'text', text = ~paste0('Ocurrencia: ', Propiedad$Transporte, " casos")) %>%
      add_trace(y = ~Negocio, type = 'scatter', mode = 'lines', line = list(color = '#0000FF', width = 4), hoverinfo = 'text', text = ~paste0('Ocurrencia: ', Propiedad$Negocio, " casos")) %>%
      add_trace(x = ~c(x[1], x[4]), y = ~c(Casa[1], Casa[4]), type = 'scatter', mode = 'markers', marker = list(color = '#6495ED', size = 8)) %>%
      add_trace(x = ~c(x[1], x[4]), y = ~c(Vehiculo[1], Vehiculo[4]), type = 'scatter', mode = 'markers', marker = list(color = '#008B8B', size = 8)) %>%
      add_trace(x = ~c(x[1], x[4]), y = ~c(Transeunte[1], Transeunte[4]), type = 'scatter', mode = 'markers', marker = list(color = '#87CEFA', size = 8)) %>%
      add_trace(x = ~c(x[1], x[4]), y = ~c(Transporte[1], Transporte[4]), type = 'scatter', mode = 'markers', marker = list(color = '#66CDAA', size = 8)) %>%
      add_trace(x = ~c(x[1], x[4]), y = ~c(Negocio[1], Negocio[4]), type = 'scatter', mode = 'markers', marker = list(color = '#0000FF', size = 8)) %>%
      layout(title = paste0("Crimenes contra la propiedad en ", Todos_estados[k]), font=t, xaxis = xaxis, yaxis = yaxis, margin = margin,
             autosize = FALSE,
             showlegend = FALSE,
             annotations = Casa1) %>%
      layout(annotations = Vehiculo1) %>%
      layout(annotations = Transeunte1) %>%
      layout(annotations = Transporte1) %>%
      layout(annotations = Negocio1) %>%
      layout(annotations = Casa2) %>%
      layout(annotations = Vehiculo2) %>%
      layout(annotations = Transeunte2) %>%
      layout(annotations = Transporte2) %>%
      layout(annotations = Negocio2)
    
    p %>%
      config(displayModeBar = F) %>%
      layout(showlegend = FALSE)
    
  })
  
  output$rezago <- renderPlotly({
    if(is.null(pol_of_click$clickedShape)){
      j <- 1
    } else {
      j <- as.numeric(pol_of_click$clickedShape)
    }
    
    t <- list(
      family = "Courier New, monospace",
      size = 12,
      color ='black')
    
    
    titulo <- list(
      family = "Courier New, monospace",
      size = 12,
      color ='black')
    
    colores <- c(rep('#FFCCCC', 32))
    
    colores[which(as.numeric(j) == Edu$CVE_EDO)] <- '#FF6666'
    Edu$x
    
    colores
    rezago <- plot_ly(Edu, x =Edu$x, y =Edu$Educacion, type = 'bar', 
                      marker = list(color = colores),
                      text = ~paste0('Población con rezago educativo en'," ", x,": ", Educacion, '%'),
                      hoverinfo ='text') %>%
      layout(title = 'Rezago educativo por entidad federativa',
             font =titulo,
             xaxis = list(
               title = "",
               bargap = 1.2,
               tickfont = t),
             yaxis = list(
               title = 'porcentaje',
               automargin = TRUE,
               titlefont = t,
               tickfont = t),
             legend = list(x = 0, y = 1, bgcolor = 'rgb(107, 107, 107)', bordercolor = 'rgba(255, 255, 255, 0)'))
    rezago
    
    
  })
  
  output$Pobreza <- renderPlotly({
    
    if(is.null(pol_of_click$clickedShape)){
      j <- 1
    } else {
      j <- as.numeric(pol_of_click$clickedShape)
    }
    
    t <- list(
      family = "Courier New, monospace",
      size = 13,
      color ='black')
    
    
    titulo <- list(
      family = "Courier New, monospace",
      size = 12,
      color ='black')
    
    colores <- c(rep('#CCFFCC', 32))
    colores[which(as.numeric(j) == Pobreza$CVE_EDO)] <- '#66CDAA'
    colores
    
    pobre <- plot_ly(Pobreza, x =Pobreza$x, y =Pobreza$Pobreza, type = 'bar', 
                     marker = list(color = colores),
                     text = ~paste0('Población en situación de pobreza en'," ", x,": ", Pobreza, '%'),
                     hoverinfo ='text') %>%
      layout(title = 'Pobreza por entidad federativa',
             font =titulo,
             xaxis = list(
               title = "",
               bargap = 1.2,
               tickfont = t),
             yaxis = list(
               title = 'porcentaje',
               automargin = TRUE,
               titlefont = t,
               tickfont = t),
             legend = list(x = 0, y = 1, bgcolor = 'rgb(107, 107, 107)', bordercolor = 'rgba(255, 255, 255, 0)'))
    pobre
    
  })
  
  output$Periodo <- renderText({
    paste0("Periodo de análisis = ", as.character(input$sPeriodo))
  })
  
  output$moranII <- renderPlotly({
    
    pal <- c("red", "darkblue")
    
    if(input$sPeriodo == "1990 a 2017"){
      
      f <- list(
        family = "Book antiqua, monospace",
        size = 18,
        color = "black"
      )
      
      x <- list(
        title = "Número de homicidios 2017 estandarizado",
        titlefont = f
      )
      y <- list(
        title = "Rezago del número de homicidios<br> 1990 estandarizado",
        titlefont = f
      )
      
      airq <- data %>% 
        filter(!is.na(data$LAG_1990))
      fit <- lm(data$LAG_1990 ~ data$STD_2017, data = airq)
      plot_ly(data = data, x = data$STD_2017, y = data$LAG_1990,color = data$id_col, colors = pal, text = ~paste("Municipio", data$NOM_MUN,'<br>Homicidios:<br>Hombres:', data$homman_2017, '<br>Mujeres:', data$hommuj_2017))%>%
        layout( title = paste("Dimensión espacio temporal del número <br>de homicidios en 2017<br><br><br>Índice de Moran:",0.1444),titlefont = f,xaxis = x, yaxis = y,showlegend = T)%>% 
        add_markers(y = ~data$LAG_1990)%>% 
        add_lines(x = ~data$STD_2017, y = fitted(fit),showlegend = F) %>%
        config(displayModeBar = F) %>%
        layout(showlegend = FALSE)
      
    } else if(input$sPeriodo == "1990 a 2004"){
      
      f <- list(
        family = "Book antiqua, monospace",
        size = 18,
        color = "black"
      )
      
      x <- list(
        title = "Número de homicidios 2004 estandarizado",
        titlefont = f
      )
      y <- list(
        title = "Rezago del número de homicidios<br> 1990 estandarizado",
        titlefont = f
      )
      
      airq <- data %>% 
        filter(!is.na(data$LAG_1990))
      fit <- lm(data$LAG_1990 ~ data$STD_2004, data = airq)
      plot_ly(data = data, x = data$STD_2004, y = data$LAG_1990,color = data$id_col, colors = pal, text = ~paste("Municipio", data$NOM_MUN,'<br>Homicidios:<br>Hombres:', data$homman_2004, '<br>Mujeres:', data$hommuj_2004))%>%
        layout( title = paste("Dimensión espacio temporal del número de homicidios en 2004<br><br><br>Índice de Moran:", 0.3008),titlefont = f,xaxis = x, yaxis = y,showlegend = T) %>% 
        add_markers(y = ~data$LAG_1990)%>% 
        add_lines(x = ~data$STD_2004, y = fitted(fit),showlegend = F) %>%
        config(displayModeBar = F) %>%
        layout(showlegend = FALSE)
      
    } else {
      
      f <- list(
        family = "Book antiqua, monospace",
        size = 18,
        color = "black"
      )
      
      x <- list(
        title = "Número de homicidios 2017 estandarizado",
        titlefont = f
      )
      y <- list(
        title = "Rezago del número de homicidios<br> 2004 estandarizado",
        titlefont = f
      )
      
      airq <- data %>% 
        filter(!is.na(data$LAG_2004))
      fit <- lm(data$LAG_2004 ~ data$STD_2017, data = airq)
      plot_ly(data = data, x = data$STD_2017, y = data$LAG_2004,color = data$id_col, colors = pal, text = ~paste("Municipio", data$NOM_MUN,'<br>Homicidios:<br>Hombres:', data$homman_2017, '<br>Mujeres:', data$hommuj_2017))%>%
        layout( title = paste("Dimensión espacio temporal del número<br> de homicidios en 2017<br><br><br>Índice de Moran:",0.1678),titlefont = f,xaxis = x, yaxis = y,showlegend = T) %>% 
        add_markers(y = ~data$LAG_2004)%>% 
        add_lines(x = ~data$STD_2017, y = fitted(fit),showlegend = F) %>%
        config(displayModeBar = F) %>%
        layout(showlegend = FALSE)
    }
    
  })
  
  output$moran <- renderPlotly({
    anio <- as.character(input$Anios_graficas)
    data1 <- data[c(names(data)[stringr::str_detect(names(data), pattern = anio)], "id_col", "NOM_MUN")] %>% 
      filter(!is.na(data[,2]))
    pal <- c("red", "darkblue")
    
    f <- list(
      family = "Book antiqua, monospace",
      size = 18,
      color = "black"
    )
    x <- list(
      title = paste0("Número de homicidios ", anio, " estandarizado"),
      titlefont = f
    )
    y <- list(
      title = paste0("Rezago del numero de homicidios<br>", anio, " estandarizado"),
      titlefont = f
    )
    
    fit <- lm(data1[,2] ~ data1[,1])
    fit
    
    p <- plot_ly(data = data1, x = data1[,1], y = data1[,2], color = data1$id_col, 
                 colors = pal, text = ~paste("Municipio", data1$NOM_MUN,'<br>Homicidios:<br>Hombres:', 
                                             data1[,3], '<br>Mujeres:', data1[,4]))%>%
      layout(title = paste("Autocorrelaciones espaciales del número<br> de homicidios en", anio, "<br><br><br>Índice de Moran:",M[as.numeric(anio)-1989]),titlefont = f,xaxis = x, yaxis = y,showlegend = T) %>%
      add_markers(y = ~data1[,2])%>% 
      add_lines(x = ~data1[,1], y = fitted(fit), showlegend = F) %>%
      config(displayModeBar = F) %>%
      layout(showlegend = FALSE)
    p
  })
  
  output$cluster <- renderImage({
    anyo <<- input$Anios_graficas
    print(anyo)
    list(src = paste0("www/mapas_png/clus_", as.character(anyo), ".png"),
         contentType = 'image/png',
         height = 450
    )}, deleteFile = FALSE)
  
  output$clusterII <- renderImage({
    if (input$sPeriodo == "1990 a 2017"){
      src1 <- "www/mapas_png/clus_1990_2017.png"
    } else if (input$sPeriodo == "1990 a 2004") {
      src1 <- "www/mapas_png/clus_1990_2004.png"
    } else if (input$sPeriodo == "2004 a 2017") {
      src1 <- "www/mapas_png/clus_2004_2017.png"
    }
    
    print(anyo)
    list(src = src1,
         contentType = 'image/png',
         height = 420
    )}, deleteFile = FALSE)
  
  
  
  
  # output$cluster_periodo <- renderImage({
  #   anyo <<- input$Anios_graficas
  #   print(anyo)
  #   list(src = paste0("www/mapas_png/clus_", as.character(anyo), ".png"),
  #        contentType = 'image/png',
  #        height = 450
  #   )}, deleteFile = FALSE)
  # 
  # 
  
  # Objetos Hoja Estatal 
  
  pol_of_click <- reactiveValues(clickedShape = NULL)
  
  observeEvent(input$MAPA_shape_click, 
               {
                 pol_of_click <- input$pol_of_click
                 p <- input$MAPA_shape_click$id
                 #print(p)
                 pol_of_click$clickedShape <<- input$MAPA_shape_click$id
                 print(pol_of_click$clickedShape)
                 print(class(pol_of_click$clickedShape))
               })
  
  output$MAPA <- renderLeaflet({
    
    if(input$Anio_Estatal == 2015){
      # Filtros
      filtro <- c(9,13,17,21, 25)
    } else if(input$Anio_Estatal == 2016){
      filtro <- c(10,14,18,22, 26)
    } else if(input$Anio_Estatal == 2017){
      filtro <- c(11,15,19,23, 27)
    } else {
      filtro <- c(12,16,20,24, 28)
    }
    
    mapx <- mapa[,c(1:8,filtro)]
    nombres <- c(names(mapa)[1:8], "Crimen", "Paz", "Hom", "Violencia", "Organizado", "geometry")
    names(mapx) <- nombres
    
    #Ahora ponemos las etiquetas
    etiqueta <- paste0("<strong>Entidad: </strong>", 
                       mapx$ENTIDAD, 
                       "<br><strong>Crímenes totales en 2018: </strong>", 
                       mapx$Crimen,
                       "<br><strong>Homicidios*: </strong>",
                       mapx$Hom,
                       "<br><strong>Delitos con violencia*: </strong>",
                       mapx$Violencia,
                       "<br><strong>Crímenes de la delincuencia organizada*: </strong>",
                       mapx$Organizado,
                       "<br><strong>*</strong><small>calificación del índice de Paz México 2018 (IPM)</small>")%>% lapply(htmltools::HTML)
    
    label <- sprintf( "<strong>%s</strong>",
                      mapx$ENTIDAD) %>% lapply(htmltools::HTML)
    
    
    my_palette = c(RColorBrewer::brewer.pal(5, "PuRd")[c(1,2,3,4,5)])
    #Trazamos el mapa
    
    leaflet(data=mapx)  %>%
      addProviderTiles("Stamen.Watercolor") %>%
      setView(lng = -102.16920324, lat= 23.81986595, zoom=4.5) %>%
      addPolygons(fillColor = ~qpal(mapx[['Paz']]), 
                  fillOpacity = 0.9, 
                  smoothFactor = 0.9, 
                  color = "black",
                  weight = 0.5,
                  label = label,
                  popup = etiqueta,
                  layerId = mapx$CVE_EDO, 
                  highlightOptions = highlightOptions(color = "white",
                                                      weight =1.5 ,
                                                      fillOpacity = 0.7,
                                                      bringToFront = TRUE)) %>% 
      addLegend(colors = c(my_palette),
                labels=c("Alto nivel de paz", "Nivel medio-alto de paz ","Nivel medio de paz","Nivel medio-bajo de paz ", "Bajo nivel de paz"),
                values = ~mapx$Paz, 
                opacity = 1, 
                position = "bottomleft", 
                title = "<b>Nivel de Paz<b>")
  })
  
  output$dona <- renderPlotly({
    
    # TIPO DE LETRA
    t <- list(
      family = "Courier New, monospace",
      size = 11,
      color ='#7f7f7f')
    # labels
    etiqueta<- c('Homicidios denunciados:', "Lesiones denunciadas:", 'Secuestros denunciados:', 'Casos de abuso sexual denunciados:', 'Violencia familiar denunciada:')
    # Paleta de colores
    my_palette <- c('#bc5090','#003f5c','#58508d','#ff6361','#ffa600')
    
    if(is.null(pol_of_click$clickedShape)){
      i <- 1
    } else {
      i <- as.numeric(pol_of_click$clickedShape)
    }
    print(paste0("i = ", i))
    G01 <- Personas %>%
      filter(Anio == as.numeric(input$Anio_Estatal) & CVE_ENT == i)
    G01 <- as.data.frame(t(G01[,1:5]))
    
    
    G01 <- Personas %>%
      filter(Anio == as.numeric(input$Anio_Estatal) & CVE_ENT == i)
    G01 <- as.data.frame(t(G01[,1:5]))
    G01
    
    
    valores <- G01$V1
    
    Graph <- G01  %>%
      plot_ly(labels= etiqueta, values = valores,
              opacity=0.9,
              marker = list(colors = my_palette),
              insidetextfont = list(color = 'white')) %>%
      add_pie(hole = 0.6) %>%
      layout(title = paste0("Crimen en ", Todos_estados[i],  " (", as.character(input$Anio_Estatal), ") "),  showlegend = F,
             font=t,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      config(displayModeBar = F)
    
    Graph
    
  })
  
  observe({
    
    dato <<- D %>%
      filter(Año == as.numeric(input$Anio) & Entidad == input$Estado) %>%
      filter(Total_Anio != 0) %>%
      arrange(Tipo.de.delito)
    
    
    updateSelectInput(session, "Municipio",
                      label = "Seleccione Municipio: ",
                      choices = dato$Municipio,
                      selected = head(dato$Municipio, 1))
  })
  
  output$tm <- highcharter::renderHighchart({
    
    canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")
    legendClickFunction <- JS("function(event) {Shiny.onInputChange('legendClicked', this.name);}")
    
    dato_mpio <<- D %>%
      filter(Año == as.numeric(input$Anio) & Entidad == input$Estado & Municipio == input$Municipio) %>%
      filter(Total_Anio != 0) %>%
      arrange(Tipo.de.delito, Municipio)
    
    delitos <<- unique(dato_mpio$Tipo.de.delito)  
    print(delitos)
    
    tm <- treemap::treemap(dato_mpio,
                           index = c("Tipo.de.delito"),
                           vSize = "Total_Anio",
                           vColor = "Tipo.de.delito",
                           draw = FALSE,
                           palette = paleta,
                           title = paste(dato_mpio$Entidad[1], dato_mpio$Municipio[1]))
    
    hctreemap(tm, allowDrillToNode = FALSE, layoutAlgorithm = "squarified") %>%
      hc_title(text = paste0("Crimen en ", dato_mpio$Municipio[1], ", ", dato_mpio$Entidad[1]))  %>%
      hc_plotOptions(series = list(stacking = FALSE, events = list(click = canvasClickFunction, legendItemClick = legendClickFunction)))
    
  })
  
  makeReactiveBinding("outputText")
  
  observeEvent(input$canvasClicked, {
    outputText <<- paste0("You clicked on series ", input$canvasClicked[1], " and the bar you clicked was from category ", input$canvasClicked[2], ".") 
    print(delitos[as.numeric(input$canvasClicked[2]) + 1])
  })
  
  observeEvent(input$legendClicked, {
    outputText <<- paste0("You clicked into the legend and selected series ", input$legendClicked, ".")
  })
  
  output$text <- renderText({
    outputText      
  })
  
  output$HistDelito <- renderHighchart({
    if(is.null(input$canvasClicked[2])){
      ej_2 <<- D %>%
        filter(Año == as.numeric(input$Anio) & Entidad == input$Estado & Municipio == input$Municipio & Tipo.de.delito == "Robo") %>%
        filter(Total_Anio != 0) 
      
      highchart() %>% 
        hc_chart(type = "column", colorByPoint = TRUE) %>% 
        hc_title(text = paste0("Robo", ", ", dato_mpio$Municipio[1], ", ", dato_mpio$Entidad[1])) %>% 
        hc_colors(paleta[which(Todos_delitos == "Robo")]) %>%
        hc_xAxis(categories = as.character(ej_2$Modalidad)) %>% 
        hc_add_series(data = ej_2$Total_Anio,
                      name = "Ocurrencia", colorByPoint = TRUE)
    } else {
      
      Delito <- delitos[as.numeric(input$canvasClicked[2]) + 1]
      
      ej_2 <<- D %>%
        filter(Año == as.numeric(input$Anio) & Entidad == input$Estado & Municipio == input$Municipio & Tipo.de.delito == Delito) %>%
        filter(Total_Anio != 0) 
      
      highchart() %>% 
        hc_chart(type = "column", colorByPoint = TRUE) %>% 
        hc_title(text = paste0(Delito, ", ", dato_mpio$Municipio[1], ", ", dato_mpio$Entidad[1])) %>% 
        hc_colors(paleta[which(Todos_delitos == Delito)]) %>%
        hc_xAxis(categories = as.character(ej_2$Modalidad)) %>% 
        hc_add_series(data = ej_2$Total_Anio,
                      name = "Ocurrencia", colorByPoint = TRUE)
    }
  })
  
})

