# GLOBAL de la aplicacion Shiny de Crimen

# LIBRERIAS
library(shiny)
library(shinydashboard)
library(dplyr)
library(highcharter)
library(shinycssloaders)
library(readxl)
library(leaflet)
library(plotly)
library(readxl)

# Bases de Datos
D <- read.csv("www/Bases/Delitos_Municipal.csv", encoding = "UTF-8") 
names(D)[1] <- "Año"
D <- D %>%
  mutate(CODGEO = case_when(nchar(Cve..Municipio) == 5 ~ as.character(Cve..Municipio),
                            nchar(Cve..Municipio) == 4 ~ paste0("0", as.character(Cve..Municipio)))) %>%
  mutate(Total_Anio = rowSums(D[,10:21], na.rm = T), 
         Modalidad = paste(Subtipo.de.delito, " ", Modalidad)) %>%
  filter(Total_Anio != 0)

# Informacion Adicional
paleta <- grDevices::rainbow(n = 40)
paleta[33] <- "#996666"
paleta[23] <- "#996680"
paleta[9] <- "#996699"
paleta[24] <- "#806699"
paleta[28] <- "#666699"
paleta[6] <- "#668099"
paleta[18] <- "#669999"
paleta[2] <- "#66998c"
paleta[40] <-"#5c7ad6"
paleta[16] <- "#477585"
paleta[30] <- "#669966"
paleta[31] <- "#739966"
paleta[15] <- "#809966"
paleta[11] <- "#387d94"
paleta[26] <- "#999966"
paleta[5] <- "#998c66"
paleta[38] <- "#998066"
paleta[8] <- "#997366"
paleta[22] <- "#c47de8"
paleta[10] <-"#a1a1f7"
paleta[14] <- "#ac5377"
paleta[3] <- "#669933"
paleta[19] <- "#334d99"
Todos_delitos <- levels(D$Tipo.de.delito)
Todos_estados <- levels(as.factor(D$Entidad))

############
### MAPA ###
###########
edos <- sf::st_read("www/Sin Islas/sin_islas.shp")
# plot(edos)
# leaflet(edos) %>%
#   addPolygons()


## PAAAAAARCHEZOTOTOTOTE ##
edos$CVE_EDO <- c("01", "02", "03", "04", "08", "09", "05", "06", "07", as.character(10:32))

#LLam?mos base de cr?menes por estado
crimen_excel = read_xlsx("www/Bases/Crimen_excel.xlsx")
crimen_csv <- read.csv("www/Bases/Crimen_csv.csv", encoding = "UTF-8") 
crimen_csv <- crimen_csv %>%
  mutate(CVE_ENT = case_when(as.numeric(rownames(crimen_csv)) < 10 ~ paste0("0", rownames(crimen_csv)),
                             as.numeric(rownames(crimen_csv)) >= 10 ~ rownames(crimen_csv)))

#Juntamos ambas bases y quitamos columnas innecesarias
datos<- merge(crimen_excel, crimen_csv, by="CVE_ENT")
datos$NOM_ENT.y <-NULL

#Creamos el mapa
mapa <- merge(edos,
              datos,
              by.x = "CVE_EDO",
              by.y = "CVE_ENT",
              sort = FALSE)

rm(datos, edos, crimen_csv, crimen_excel)
#Escogemos colores
qpal <- colorQuantile("PuRd",mapa$'Paz_2018', n = 9)
#Definiendo colores para la leyenda
my_palette = c(RColorBrewer::brewer.pal(5, "PuRd")[c(1,2,3,4,5)])

# BD de las Donas
Personas <- read.csv("www/Bases/Personas.csv", encoding = "UTF-8")

########################
# COEFICIENTE DE MORAN #
#######################
data <- readstata13::read.dta13("www/mapas_png/Base_fin_pa_graf_2.dta")
M_90=0.3227
M_91=0.2916
M_92=0.2376
M_93=0.2736
M_94=0.2470
M_95=0.2763
M_96=0.2803
M_97=0.3108
M_98=0.2991
M_99=0.2967
M_00=0.2719
M_01=0.2849
M_02=0.2842
M_03=0.2000
M_04=0.2990
M_05=0.2735
M_06=0.2365
M_07=0.2854
M_08=0.0803
M_09=0.0663
M_10=0.0457
M_11=0.0928
M_12=0.1412
M_13=0.2029
M_14=0.2023
M_15=0.1749
M_16=0.1751
M_17=0.1714

M <- c(M_90, M_91, M_92, M_93, M_94, M_95, M_96, M_97,
       M_98, M_99, M_00, M_01, M_02, M_03, M_04, M_05,
       M_06, M_07, M_08, M_09, M_10, M_11, M_12, M_13,
       M_14, M_15, M_16, M_17)

data$id_col <- factor(data$id_col,
                      levels = c(1,2),
                      labels = c("Area metropolitana", "Resto"))
names(data)[1:28] <-  sprintf("STD_%s", as.character(1990:2017))
names(data)[29:56] <- sprintf("LAG_%s", as.character(1990:2017))

# OTRAS GRAFICAS
Macro <- read_xlsx("www/Bases/Variables_macro.xlsx") 
Macro <- Macro %>%
  mutate(CVE_EDO = rownames(Macro))

Macro$CVE_EDO <- c(1, 2, 3, 4, 7, 5, 6, 8, 9, 10:32)

# Datos para Pobreza
Pobreza <- Macro[order(Macro$Pobreza),]
Pobreza[2]<- NULL
Pobreza <- data.frame(Pobreza, stringsAsFactors = FALSE)
Pobreza$x <- factor(Pobreza$x, levels = unique(Pobreza$x)[order(Pobreza$Pobreza, decreasing = TRUE)])
Pobreza <- merge(Pobreza, Macro, sort = F)

# Datos para Rezago
Edu <- Macro[order(Macro$Educacion),]
Edu[3]<- NULL
#Edu <- data.frame(Edu, stringsAsFactors = FALSE)
Edu$x <- factor(Edu$x, levels = unique(Edu$x)[order(Edu$Educacion, decreasing = TRUE)])

