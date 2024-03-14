library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinymanager)
library(shinythemes)
library(bslib)
library(readxl)
library(tidyverse)
library(ggplot2)
library(FactoMineR)
library(DT)
library(plotly)
library(DescTools)
library(formattable)

# Git Hub: https://github.com/andres-roncaglia/Proyecto-WORLDS


# Links con informacion ---------------------

# DATATABLE
# https://rstudio.github.io/DT/shiny.html  # Ejemplos de como usar DT
# https://datatables.net/reference/option/  # Todas las opciones de DT
# https://clarewest.github.io/blog/post/making-tables-shiny/  # Opciones y ejemplos de datatables

# Opciones esteticas
# https://rstudio.github.io/DT/functions.html # otras opciones visuales de DT
# https://www.r-bloggers.com/2023/03/how-to-customise-the-style-of-your-shinydashboard-shiny-app/ # Ayudas esteticas

# OTRO
# https://shiny.posit.co/r/gallery/ # Multiples opciones de shiny apps
# https://shiny.posit.co/r/gallery/advanced-shiny/download-knitr-reports/ # Imprime un reporte
# https://shiny.posit.co/r/gallery/interactive-plots/plot-interaction-advanced/ # Selecciona elementos de un grafico
# https://shiny.posit.co/r/gallery/dynamic-user-interface/dynamic-ui/  # Cambiar de objeto segun la opcion sin un if
# https://riot-api-libraries.readthedocs.io/en/latest/ddragon.html  #data?
# https://shinyapps.dreamrs.fr/shinyWidgets/  # Widgets de shiny
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson4/ # Opciones de reactividad




# Carga de datos ---------

matches <- read_xlsx("F:/Users/iCentro/Desktop/Escritorio/Facu/Analisis exploratorio de datos (Shiny)/Proyecto-WORLDS/matches.xlsx")
players <- read_xlsx("F:/Users/iCentro/Desktop/Escritorio/Facu/Analisis exploratorio de datos (Shiny)/Proyecto-WORLDS/players.xlsx")
champs <- read_xlsx("F:/Users/iCentro/Desktop/Escritorio/Facu/Analisis exploratorio de datos (Shiny)/Proyecto-WORLDS/champs.xlsx")


# matches <- read_xlsx("/cloud/project/matches.xlsx")
# players <- read_xlsx("/cloud/project/players.xlsx")
# champs <- read_xlsx("/cloud/project/champs.xlsx")



champs <- champs %>% mutate(DMG = DMG*1000,
                            photo_name = case_when(Champion == "Kha'Zix" ~ "Khazix",Champion == "Bel'Veth" ~ "Belveth",Champion == "LeBlanc" ~ "Leblanc",Champion == "Kai'Sa" ~ "Kaisa",Champion == "Wukong" ~ "MonkeyKing",Champion == "Renata Glasc" ~ "Renata",T ~ str_replace_all(Champion, "[ ']", "")),
                            Pic = paste0("<img src=\"",
                                         paste0("https://ddragon.leagueoflegends.com/cdn/13.19.1/img/champion/",photo_name,".png"),
                                         "\" height=\"30\" data-toggle=\"tooltip\" data-placement=\"middle\" title=\"",
                                         Champion,
                                         "\"></img>"
                            ))

photo_name <- champs$photo_name
names(photo_name) <- champs$Champion

champs <- champs %>% select(Pic,everything(),-photo_name)

players <- players %>% mutate(DMG = as.numeric(str_replace(substr(DMG, 1 ,stop = str_length(DMG) - 1), ",", ".")[1:length(W)])*1000)



theme_set(theme_bw())



# Sirve para cambiar el color de la app
js <- "Shiny.addCustomMessageHandler('change_skin', function(skin) {
        document.body.className = skin;
       });"


# Interfaz ----------------

ui <- dashboardPage(
  
  skin = "blue",
  
  ## Encabezado -----------
  dashboardHeader(title = "Proyecto LoL"),
  
  ## Barra lateral ----------------
  dashboardSidebar(
    "Menú",
    sidebarMenu(
      menuItem("Datasets", tabName = "data", icon = icon("database")),
      
      menuItem("ACP", icon = icon("puzzle-piece"),
               menuSubItem("Jugadores", tabName = "tab_acp_jugadores"),
               menuSubItem("Campeones", tabName = "tab_acp_campeones")),
      
      menuItem("Histogramas", tabName = "histo", icon = icon("chart-column")),
      
      menuItem("Llaves del torneo", tabName = "torneo", icon = icon("trophy")),
      
      
      menuItem("Leaguepedia", icon = icon("link"), badgeLabel = "Fuente", badgeColor = "blue", href = "https://lol.fandom.com/wiki/2023_Season_World_Championship/Main_Event"),
      
      tags$head(tags$script(js)),
      pickerInput("skin_color", "Tema de la APP", 
                  c("blue", "black", "purple", 
                    "green", "red", "yellow"))
      
      
      
    )
  ),
  
  
  ## Cuerpo ----------
  dashboardBody(
    
    # Esto mueve el ultimo item de la barra lateral hacia abajo
    tags$head(
      tags$style(HTML("
      #sidebarItemExpanded > ul > :last-child {
        position: absolute;
        bottom: 0;
        width: 100%
      }

    "))),
    
    tabItems(
      tabItem(
        
        ### Archivos de datos -------------------
        
        tabName = "data",
        
        fluidRow(column(1,
                        dropdown(
                          tags$h3("Opciones"),
                          circle = F,
                          color = "royal",
                          style = "gradient",
                          icon = icon("gear"),
                          
                          selectInput(inputId = "datos_opciones", label = "Archivo", choices = c("Jugadores", "Campeones", "Partidas"), selected = "Jugadores"),
                          
                          h5("Ver estadisticas por partida"),
                          
                          #Esto sirve para que cuando se use la opcionn de ver las stats por partida en el archivo de partidas salga un cartel de error
                          useSweetAlert(),
                          
                          
                          switchInput(inputId = "por_partida", value = T, onStatus = "success", offStatus = "danger", onLabel = "Sí", offLabel = "No"),
                          
                          pickerInput(inputId = "opciones_heatmap", label = "Heatmap", choices = colnames(select_if(players, is.numeric)), 
                                      options = list(`actions-box` = T, size = 5, title = "Variable")),
                          
                          downloadButton("datos_descarga", "Descarga")
                        ),
        ),
        
        column(10,
               
               DTOutput("tabla_datos"))
        ),
        
        
        #### Tabla informativa del dato seleccionado en la tabla --------------
        uiOutput("individual")
        
      ),
      
      
      
      ### ACP Jugadores -------------------
      tabItem(
        tabName = "tab_acp_jugadores",
        fluidRow(h2("Análisis de componentes principales sobre los campeones"), 
                 
                 #### Columna 1 con boton de opciones ----------------
                 column(1,
                        dropdown(
                          tags$h3("Opciones"),
                          circle = F,
                          color = "royal",
                          style = "gradient",
                          icon = icon("gear"),
                          
                          pickerInput(inputId = "pos_jugadores", label = "Posiciones", choices = c("Top", "Jgl", "Mid", "Adc", "Sup"),
                                      selected = unique(players$POS), options = list(`actions-box` = T, size = 5),
                                      multiple = T),
                          
                          pickerInput(inputId = "equipos_jugadores", label = "Equipos", choices = unique(players$TEAM),
                                      selected = unique(players$TEAM), options = list(`actions-box` = T, size = 5),
                                      multiple = T),
                          
                          selectInput(inputId = "color_jugadores", label = "Color", choices = c("Equipo", "Posición"), selected = "Posición"),
                          
                          sliderInput(inputId = "ejex_jugadores", label = "CP del eje X", min = 1, value = 1,max = ncol(select_if(players, is.numeric)), step = 1),
                          
                          sliderInput(inputId = "ejey_jugadores", label = "CP del eje Y", min = 1, value = 2,max = ncol(select_if(players, is.numeric)), step = 1)
                        )
                 ),
                 
                 
                 #### Columna 2 con grafico de CP --------------
                 column(5, 
                        box(title = "Grafico de los individuos en las CP seleccionadas",
                            plotlyOutput("plot_jugadores"), 
                            width = NULL, 
                            solidHeader = T)
                 ),
                 
                 column(3,
                        h3("Cargas de las CPs:"),
                        solidHeader = T,
                        DTOutput("eig_jugadores")
                 ),
                 
                 column(3,
                        fluidRow(valueBoxOutput("prct_eig_jugadores", width = 11)),
                        fluidRow(valueBoxOutput("prct_eig_jugadores2", width = 11)),
                        fluidRow(valueBoxOutput("prct_eig_jugadores3", width = 11)))
                 
                 
                 
        )
      ),
      
      
      
      ### ACP Campeones -------------------
      tabItem(
        tabName = "tab_acp_campeones",
        fluidRow(h2("Análisis de componentes principales sobre los jugadores"), 
                 
                 #### Columna 1 con boton de opciones ----------------
                 column(1,
                        dropdown(
                          tags$h3("Opciones"),
                          circle = F,
                          color = "royal",
                          style = "gradient",
                          icon = icon("gear"),
                          
                          
                          multiInput(
                            inputId = "highlight_campeones_acp",
                            label = "Campeones:",
                            choices = NULL,
                            choiceNames = lapply(seq_along(champs$Champion),
                                                 function(i) tagList(tags$img(src = champs$Pic[i],
                                                                              width = 20,
                                                                              height = 15), champs$Champion[i])),
                            choiceValues = champs$Champion
                          ),
                          
                          
                          sliderInput(inputId = "ejex_campeones", label = "CP del eje X", min = 1, value = 1,max = ncol(select_if(players, is.numeric)), step = 1),
                          
                          sliderInput(inputId = "ejey_campeones", label = "CP del eje Y", min = 1, value = 2,max = ncol(select_if(players, is.numeric)), step = 1)
                        )
                 ),
                 
                 
                 #### Columna 2 con grafico de CP --------------
                 column(5, 
                        box(title = "Grafico de los individuos en las CP seleccionadas",
                            plotlyOutput("plot_campeones"), 
                            width = NULL, 
                            solidHeader = T)
                 ),
                 
                 column(3,
                        h3("Cargas de las CPs:"),
                        DTOutput("eig_campeones")),
                 
                 column(3,
                        fluidRow(valueBoxOutput("prct_eig_campeones", width = 11)),
                        fluidRow(valueBoxOutput("prct_eig_campeones2", width = 11)),
                        fluidRow(valueBoxOutput("prct_eig_campeones3", width = 11)))
                 
                 
                 
        )
      ),
      
      ## Histograma -------------
      tabItem(
        tabName = "histo",
        
        fluidRow(
          column(1,
                 dropdown(
                   tags$h3("Opciones"),
                   circle = F,
                   color = "royal",
                   style = "gradient",
                   icon = icon("gear"),
                   
                   selectInput(inputId = "histo_data", label = "Datos", choices = c("Jugadores", "Campeones", "Partidas"), selected = "Jugadores"),
                   
                   pickerInput(inputId = "histo_var", label = "Variable", choices = colnames(select_if(players, is.numeric)), 
                               options = list(`actions-box` = T, size = 5))
                   
                   
                 )
          ),
          
          column(5,
                 
                 box(width = NULL,
                     title = "Histograma",
                     solidHeader = T,
                     plotOutput("histograma"))
          )
        )
      ),
      
      ## Torneo -------------
      
      tabItem(
        tabName = "torneo",
        
      )
      
      
      
    )
  )
  
)


# Server ----------

server <- function(input, output, session) {
  
  ## Reactivo tema app ---------
  
  observeEvent(input$skin_color, {
    session$sendCustomMessage("change_skin", paste0("skin-", input$skin_color))
  })
  
  
  ## Reactivo tabla de datos --------------
  
  
  # Se selecciona el dataset a usar dependiendo de las opciones 
  
  #(Es mas sencillo usando la funcion switch):
  # reac_datos_opciones <- reactive({
  #   switch(input$datos_opciones,
  #          "Jugadores" = players,
  #          "Campeones" = champs,
  #          "Partidas" = matches)
  # })
  
  
  reac_datos_opciones <- reactive({
    if (input$por_partida) {
      if (input$datos_opciones == "Jugadores") {players
      } else if (input$datos_opciones == "Campeones") {champs
      } else if (input$datos_opciones == "Partidas") {matches}
    } else {
      if (input$datos_opciones == "Jugadores") {
        players2 <- players
        players2[,c("K","D","A","CS","G","DMG")] <- round(players[,c("K","D","A","CS","G","DMG")]*players$Games,0)
        players2
      } else if (input$datos_opciones == "Campeones") {
        champs2 <- champs
        champs2[,7:16] <- round(champs[,7:16]*champs$Games,2)
        champs2[,c("K","D","A","CS","G", "DMG")] <- round(champs2[,c("K","D","A","CS","G", "DMG")], 0)
        champs2
      } else if (input$datos_opciones == "Partidas") {
        observeEvent({input$por_partida == F}, {
          sendSweetAlert(
            session = session,
            title = "Error...",
            text = "Opcion no disponible para el archivo de partidas",
            type = "error"
          )
        })
        
        matches
      }
    }
  })
  
  
  # Descargar el archivo de datos
  
  output$datos_descarga <- downloadHandler(
    filename = function() {
      paste0(input$datos_opciones, ".csv")
    },
    content = function(file) {
      write.csv(reac_datos_opciones(), file)
    }
  )
  
  
  # Heatmap de variables cuantitativas
  
  observeEvent(input$datos_opciones, {
    updatePickerInput(session = session,
                      inputId = "opciones_heatmap",
                      choices = colnames(select_if(reac_datos_opciones(), is.numeric)))
  })
  
  cortes <- reactive({seq(min(reac_datos_opciones()[,input$opciones_heatmap]), max(reac_datos_opciones()[,input$opciones_heatmap]), max(reac_datos_opciones()[,input$opciones_heatmap])*0.02)}) 
  
  colores <- reactive({
    colorRampPalette(c("#d68b91", "#7597fa"))(length(cortes()) + 1)
  })  
  
  
  
  
  # Tabla a imprimir
  output$tabla_datos <- renderDT({
    
    # Definimos la explicacion de las columnas segun la base seleccionada
    if (input$datos_opciones == "Jugadores") {nombres_columnas <- c("Posición", "Equipo", "Jugador", "Partidas jugadas", "Partidas ganadas", "Partidas perdidas", "Razón de partidas ganadas", "Asesinatos", "Muertes", "Asistencias", "KDA", "Subditos farmeados", "Subditos farmeados", "Oro", "Oro por minuto", "Daño", "Daño por minuto", "Participación de asesinatos", "Aportes de asesinatos a su equipo", "Aporte de oro a su equipo", "Campeones jugados")
    } else if (input$datos_opciones == "Campeones") {nombres_columnas <- c("Foto del Campeón","Campeón", "Partidas jugadas", "Cantidad de jugadores que lo usaron", "Partidas ganadas", "Partidas perdidas", "Razón de partidas ganadas", "Asesinatos", "Muertes", "Asistencias", "KDA", "Subditos farmeados", "Subditos farmeados", "Oro", "Oro por minuto", "Daño", "Daño por minuto", "Participación de asesinatos", "Aportes de asesinatos a su equipo", "Aporte de oro a su equipo")
    } else if (input$datos_opciones == "Partidas") {nombres_columnas <- c("Fecha", "Etapa del torneo", "Equipo del lado azul", "Equipo del lado rojo", "Ganador", "Primer ban del equipo azul", "Segundo ban del equipo azul", "Tercer ban del equipo azul", "Cuarto ban del equipo azul", "Quinto ban del equipo azul", "Primer ban del equipo rojo", "Segundo ban del equipo rojo", "Tercer ban del equipo rojo", "Cuarto ban del equipo rojo", "Quinto ban del equipo rojo", "Campeon Top equipo azul","Campeon Jungla equipo azul","Campeon Mid equipo azul","Campeon ADC equipo azul","Campeon Soporte equipo azul", "Campeon Top equipo rojo","Campeon Jungla equipo rojo","Campeon Mid equipo rojo","Campeon ADC equipo rojo","Campeon Soporte equipo rojo", "Jugador Top equipo azul","Jugador Jungla equipo azul","Jugador Mid equipo azul","Jugador ADC equipo azul","Jugador Soporte equipo azul", "Jugador Top equipo rojo","Jugador Jungla equipo rojo","Jugador Mid equipo rojo","Jugador ADC equipo rojo","Jugador Soporte equipo rojo", "Duración de la partida", "Oro del equipo azul", "Asesinatos del equipo azul", "Torres derribadas por el equipo azul", "Dragones del equipo azul", "Barones del equipo azul", "Heraldos del equipo azul", "Oro del equipo rojo", "Asesinatos del equipo rojo", "Torres derribadas por el equipo rojo", "Dragones del equipo rojo", "Barones del equipo rojo", "Heraldos del equipo rojo")}
    
    tabla_datos <- reac_datos_opciones() %>%
      datatable(selection = "single",escape = F, options = list(pageLength = 10,
                                                                scrollX = T,
                                                                scrollY = "400px",
                                                                paging = F,
                                                                scrollCollapse = T),
                # Con lo siguiente le damos una explicacion a las columnas al pasar el mouse por encima
                class = "row-border hover stripe",
                rownames = FALSE,
                callback = JS(paste0("
                var tips = ['",paste0(nombres_columnas,collapse = "','"),"'],
                header = table.columns().header();
                for (var i = 0; i < tips.length; i++) {
                $(header[i]).attr('title', tips[i]);}")
                )
      ) 
    
    # Si se selecciona alguna columna para el heatmap le aplica el format style
    
    if (input$opciones_heatmap %in% colnames(reac_datos_opciones())) {
      tabla_datos %>% formatStyle(columns = input$opciones_heatmap, 
                                  backgroundColor = styleInterval(cortes(), colores()))
    } else {
      tabla_datos
    }
    
  })
  
  
  
  
  ## Reactivo estadisticas extras ------------------------
  
  observe({
    req(input$tabla_datos_rows_selected)
    fila_selecta <- reac_datos_opciones()[input$tabla_datos_rows_selected,]
    
    ### Objetos para cuadro jugadores --------------------
    
    if (input$datos_opciones == "Jugadores") {
      posicion_jugador <- fila_selecta$POS
      equipo_jugador <- fila_selecta$TEAM
      
      campeones_jugados <- NULL
      diferencia_oro <- NULL
      columna_oro <- NULL
      equipo_contrario <- NULL
      partidas_jugador <- matches[which(matches[,-1] == fila_selecta$PLY, arr.ind = T)[,1],]
      for (i in 1:nrow(partidas_jugador)) {
        fila <- as.numeric(which(matches[,-1] == fila_selecta$PLY, arr.ind = T)[i,1])
        
        columna <- str_replace(colnames(matches)[as.numeric(which(matches[,-1] == fila_selecta$PLY, arr.ind = T)[i,2])+1], "P", "C")
        
        equipo_contrario <- c(equipo_contrario, matches[fila,c("Blue", "Red")][matches[fila,c("Blue", "Red")] != equipo_jugador])
        
        
        columna_oro[1] <- case_when(
          as.numeric(which(matches[fila,c("Blue", "Red")] == equipo_jugador, arr.ind = T)[,2]) == 1 ~ "BG",
          as.numeric(which(matches[fila,c("Blue", "Red")] == equipo_jugador, arr.ind = T)[,2]) == 2 ~ "RG",
          T ~ "ERROR")
        columna_oro[2] <- case_when(
          columna_oro[1] == "RG" ~ "BG",
          columna_oro[1] == "BG" ~ "RG",
          T ~ "ERROR")
        
        diferencia_oro <- c(diferencia_oro,as.numeric(matches[fila, columna_oro[1]] - matches[fila, columna_oro[2]]))
        
        
        campeones_jugados <- c(campeones_jugados, 
                               as.character(matches[fila,columna])
        )
      }
      
      
      
      cant_campeones_jugados <- length(unique(campeones_jugados))
      
      # Se hace este IF para que no tire NA cuando un jugador jugo 1 vez su campeon mas jugado
      if (is.na(Mode(campeones_jugados)[1])) {
        campeon_mas_jugado <- campeones_jugados
        attributes(campeon_mas_jugado) <- list(freq = "1")
      } else {
        campeon_mas_jugado <- Mode(campeones_jugados)
      }
      
      output$foto_camp_mas_jugado_jugador <- renderUI({
        tags$img(src = paste0("https://ddragon.leagueoflegends.com/cdn/13.19.1/img/champion/",photo_name[campeon_mas_jugado[1]],".png"),
                 style = "width:80%")
      })
      
      
      instancia_alcanzada <- case_when(fila_selecta$TEAM == "T1" ~ "Campeón",
                                       T ~ arrange(partidas_jugador, desc(Date))$Match[1])
      diferencia_oro <- c(max(diferencia_oro), equipo_contrario[which.max(diferencia_oro)])
      duracion_partida_jugador <- c(max(partidas_jugador$Len), equipo_contrario[partidas_jugador$Len == max(partidas_jugador$Len)])
      
      
    }
    
    ### Objetos para cuadro campeones --------------------
    
    if (input$datos_opciones == "Campeones") {
      
      output$imagen_campeon_cuadro <- renderUI({
        
        tags$img(src = paste0("https://ddragon.leagueoflegends.com/cdn/img/champion/splash/",as.character(photo_name[fila_selecta$Champion]),"_0.jpg"),
                 style="width:100%")
      })
      
      
      
      partidas_sin_bans <- select(matches, -c("Date","BanB1", "BanB2", "BanB3", "BanB4", "BanB5", "BanR1", "BanR2", "BanR3", "BanR4", "BanR5"))
      posiciones_campeon <- NULL
      jugado_por <- NULL
      
      
      for (i in 1:fila_selecta$Games) {
        
        fila <- as.numeric(which(partidas_sin_bans == fila_selecta$Champion, arr.ind = T)[i,1])
        
        columna <- colnames(partidas_sin_bans)[as.numeric(which(partidas_sin_bans == fila_selecta$Champion, arr.ind = T)[i,2])]
        
        jugado_por <- c(jugado_por, as.character(partidas_sin_bans[fila, str_replace(columna, "C", "P")]))
        
        
        posicion <- case_when(columna == "RTC" | columna == "BTC" ~ "Top",
                              columna == "RJC" | columna == "BJC" ~ "Jgl",
                              columna == "RMC" | columna == "BMC" ~ "Mid",
                              columna == "RAC" | columna == "BAC" ~ "Adc",
                              columna == "RSC" | columna == "BSC" ~ "Sup",)
        
        posiciones_campeon <- c(posiciones_campeon, posicion)
        
      }
      
      
      if (is.na(Mode(jugado_por)[1])) {
        mas_jugado_por <- jugado_por
        attributes(mas_jugado_por) <- list(freq = "1")
      } else {
        mas_jugado_por <- Mode(jugado_por)
      }
      
      output$posiciones_campeon <- renderTable({
        posiciones_campeon <- as.data.frame(table(unlist(posiciones_campeon))) 
        colnames(posiciones_campeon) <- c("Pos", "Frec")
        posiciones_campeon
      }) 
      
    }
    
    
    
    ### Objetos para cuadro partidas --------------------
    
    
    ### Cajas ---------------- 
    
    output$individual <- renderUI({
      
      if (is.null(input$tabla_datos_rows_selected) & (input$datos_opciones == "Jugadores" | input$datos_opciones == "Campeones")) {
        return()
      }
      
      switch (input$datos_opciones,
              
              #### Diseño caja datos extra jugadores -------------------
              
              "Jugadores" = fluidRow(box(
                width = 11,
                column(2, h4("Lugar para imagen")),
                column(10, box(
                  width = NULL, background = "blue",
                  
                  column(6, 
                         fluidRow(h4(paste("Cantidad de campeones jugados:", cant_campeones_jugados))),
                         
                         fluidRow(h4("Campeon mas jugado:")),
                         
                         fluidRow(column(3, uiOutput("foto_camp_mas_jugado_jugador")), 
                                  
                                  column(9, paste(paste(campeon_mas_jugado, collapse = " / "),
                                                  "con", as.numeric(attributes(campeon_mas_jugado)), "partidas" ))
                         ),
                         
                         fluidRow(h4(paste("Posición:", posicion_jugador)))
                  ),
                  
                  
                  column(6, 
                         fluidRow(h4(paste("Equipo:", equipo_jugador))),
                         
                         fluidRow(h4(paste("Partida mas larga:", duracion_partida_jugador[1], "vs", duracion_partida_jugador[2]))),
                         
                         fluidRow(h4(paste("Instancia alcanzada:", instancia_alcanzada))),
                         
                         fluidRow(h4(paste("Diferencia de oro mas amplia de su equipo:", diferencia_oro[1], "vs", diferencia_oro[2])))
                  )
                  
                ))
              )),
              
              #### Diseño caja datos extra campeones -------------------
              
              "Campeones" = fluidRow(box(
                width = 11,
                column(3, uiOutput("imagen_campeon_cuadro")),
                column(9, box(
                  width = NULL, background = "blue",
                  
                  column(6, 
                         fluidRow(h4("Mayor counter:")),
                         
                         fluidRow(h4("Más jugado por:",paste(mas_jugado_por, collapse = " / "), "en", attributes(mas_jugado_por), "partidas")),
                         
                         fluidRow(h4("Posiciones jugadas:"), tableOutput("posiciones_campeon"))
                         
                  ),
                  
                  
                  column(6, 
                         fluidRow(h4("Cantidad de partidas:")),
                         
                         fluidRow(h4("Cantidad de baneos:")),
                         
                         fluidRow(h4("Presencia en el torneo:")),
                         
                         fluidRow(h4("Más acompañado por:"))
                  )
                  
                ))
              )),
              
              #### Diseño caja datos extra partidas -------------------
              
              "Partidas" = fluidRow(box(
                width = 11,
                column(2, h4("Lugar para imagen")),
                column(10, box(
                  width = NULL, background = "blue",
                  
                  column(6, 
                         fluidRow(h4("Campeon mas jugado:")),
                         
                         fluidRow(h4("Equipo campeón:")),
                         
                         fluidRow(h4("Partida màs larga:"))
                  ),
                  
                  
                  column(6, 
                         fluidRow(h4("Partida con mas asesinatos:")),
                         
                         fluidRow(h4("Mayor diferencia de oro al finalizar una partida:")),
                         
                         fluidRow(h4("Dupla más jugada:"))
                  )
                  
                ))
              ))
              
      )
      
    }) 
    
  })
  
  
  
  
  ## Reactivo Jugadores ----------- 
  
  reac_ejex_jugadores <- reactive({input$ejex_jugadores})
  
  reac_ejey_jugadores <- reactive({input$ejey_jugadores})
  
  reac_pos_jugadores <- reactive({input$pos_jugadores})
  
  reac_equipos_jugadores <- reactive({input$equipos_jugadores})
  
  datos_jugadores <- reactive({
    filter(players, POS %in% reac_pos_jugadores(), TEAM %in% reac_equipos_jugadores())
  })
  
  reac_color_jugadores <- reactive({
    case_when(input$color_jugadores == "Posición" ~ datos_jugadores()$POS,
              input$color_jugadores == "Equipo" ~ datos_jugadores()$TEAM)
  })
  
  reac_color_jugadores_txt <- reactive({input$color_jugadores})
  
  acp_jugadores <- reactive({
    PCA(X = select_if(datos_jugadores(), is.numeric) , scale.unit = T, graph = F, ncp = Inf)
  })
  
  ### Grafico acp -------------
  
  output$plot_jugadores <- renderPlotly({
    
    jugadores <- acp_jugadores()$ind$coord %>% 
      bind_cols(datos_jugadores()) %>%
      select(TEAM, PLY, POS,
             x = paste0("Dim.", reac_ejex_jugadores()),
             y = paste0("Dim.", reac_ejey_jugadores())) %>% 
      ggplot() +
      aes(x = x, y = y, label = PLY, color = reac_color_jugadores()) +
      geom_hline(yintercept = 0, linewidth= 0.1) +
      geom_vline(xintercept = 0, linewidth= 0.1) +
      geom_point(alpha = 0.80, size = 3) +
      labs(x = paste0("CP", reac_ejex_jugadores()), y = paste0("CP", reac_ejey_jugadores()), color = reac_color_jugadores_txt()) +
      theme_bw()
    
    ggplotly(jugadores)
  })
  
  
  ### Cargas jugadores --------------
  
  # Creamos la matriz con todos los vectores de cargas de las componentes principales
  cargas <- reactive({cargas <- acp_jugadores()$var$coord}) 
  
  
  # Creamos una distincion de colores para que sea mas rapido ver cuales son las variables mas influyentes en cada CP
  reac_color1_jug <- reactive({case_when(abs(as.numeric(cargas()[, reac_ejex_jugadores()])) > 0.7 ~ 2,
                                         abs(as.numeric(cargas()[, reac_ejex_jugadores()])) > 0.5 ~ 1,
                                         T ~ 0)}) 
  
  reac_color2_jug <- reactive({case_when(abs(as.numeric(cargas()[, reac_ejey_jugadores()])) > 0.7 ~ 2,
                                         abs(as.numeric(cargas()[, reac_ejey_jugadores()])) > 0.5 ~ 1,
                                         T ~ 0)}) 
  
  # Unificamos la matriz de cargas y los vectores de colores, ademas, renombramos las filas y 
  # las columnas para no perder los nombres de las variables y eliminar mas facilmente las columnas de los colores
  reac_cargas_jugadores <- reactive({
    x <- bind_cols(cargas(),reac_color1_jug(),reac_color2_jug())
    rownames(x) <- rownames(cargas())
    colnames(x) <- c(colnames(cargas()), "col1", "col2")
    x
  })
  
  
  # Especificamos cuales son las columnas a esconder para limpiar visualmente la salida
  reac_cps_invisibles <- reactive({
    c(1:ncol(reac_cargas_jugadores()))[-c(reac_ejex_jugadores(),reac_ejey_jugadores())]
  })
  
  
  
  # Creamos el output de la matriz con las cargas de las CPs seleccionadas
  output$eig_jugadores <- renderDT({
    
    # Con datatable le damos el formato de tabla, con list(visible = F, targets, columnas) 
    # escondemos las columnas que no seleccionamos. Con formatRound redondeamos los valores, con formatStyle le 
    # damos color a las celdas segun lo especificado en las columnas que creamos para los colores
    reac_cargas_jugadores() %>%
      datatable(options =  list(pageLength = 9,
                                columnDefs = list(
                                  list(visible=F, targets= reac_cps_invisibles())),
                                searching = F
      )
      ) %>%
      formatRound(columns = 1:ncol(reac_cargas_jugadores()), digits = 3) %>% 
      formatStyle(c(reac_ejex_jugadores(),reac_ejey_jugadores()), c("col1", "col2"),
                  backgroundColor = styleEqual(c(1,2), c("#E5FFCC","#98FB98")))
    
  })
  
  
  
  
  
  ### Autovalores jugadores -----------------
  
  # Creamos cajas de valores para ver aporte de las componentes a la variabilidad total
  # Si la caja se vuelve roja, entonces el porcentaje de la variabilidad total acumulada
  # de la componente seleccionada mas las superiores es mayor al 85% y por lo tanto no es significativa
  
  output$prct_eig_jugadores <- renderValueBox({
    if (acp_jugadores()$eig[reac_ejex_jugadores(),3] <85) {
      color_autovalor <- "olive"
    } else {
      color_autovalor <- "red"
    }
    
    valueBox(
      subtitle = paste0("Variancia explicada por la Componente principal Nº ", reac_ejex_jugadores()),
      value = paste(round(acp_jugadores()$eig[reac_ejex_jugadores(),2], 2), "%"),
      icon = icon("percent"),
      color = color_autovalor
    )
  })
  
  output$prct_eig_jugadores2 <- renderValueBox({
    if (acp_jugadores()$eig[reac_ejey_jugadores(),3] <85) {
      color_autovalor2 <- "olive"
    } else {
      color_autovalor2 <- "red"
    }
    
    valueBox(
      subtitle = paste0("Variancia explicada por la Componente principal Nº ", reac_ejey_jugadores()),
      value = paste(round(acp_jugadores()$eig[reac_ejey_jugadores(),2], 2), "%"),
      icon = icon("percent"),
      color = color_autovalor2
    )
  })
  
  
  output$prct_eig_jugadores3 <- renderValueBox({
    valueBox(
      subtitle = "Variancia explicada por las Componentes Principales",
      value = paste(
        round(acp_jugadores()$eig[reac_ejex_jugadores(),2] + acp_jugadores()$eig[reac_ejey_jugadores(),2], 2),
        "%"),
      icon = icon("percent"),
      color = "light-blue"
    )
  })
  
  
  
  
  
  
  
  ## Reactivo ACP Campeones ----------- 
  
  reac_ejex_campeones <- reactive({input$ejex_campeones})
  
  reac_ejey_campeones <- reactive({input$ejey_campeones})
  
  reac_hlight_campeones <- reactive({input$highlight_campeones_acp})
  
  
  acp_campeones <- PCA(X = select_if(champs, is.numeric) , scale.unit = T, graph = F, ncp = Inf)
  
  
  # acp con campeones resaltados
  
  acp_resaltado <- reactive({
    acp_campeones$ind$coord %>% 
      bind_cols(champs) %>%
      select(Champion,
             x = paste0("Dim.", reac_ejex_campeones()),
             y = paste0("Dim.", reac_ejey_campeones())) %>% 
      filter(Champion %in% reac_hlight_campeones())
  })
  
  
  ### Grafico acp campeones -------------
  
  output$plot_campeones <- renderPlotly({
    
    graf_campeones_acp <- acp_campeones$ind$coord %>% 
      bind_cols(champs) %>%
      select(Champion,
             x = paste0("Dim.", reac_ejex_campeones()),
             y = paste0("Dim.", reac_ejey_campeones())) %>% 
      ggplot() +
      aes(x = x, y = y, label = Champion) +
      geom_hline(yintercept = 0, linewidth= 0.1) +
      geom_vline(xintercept = 0, linewidth= 0.1) +
      geom_point(alpha = 0.80, size = 3, color = "lightblue") +
      geom_point(data = acp_resaltado(),alpha = 0.80, size = 3, color = "blue") +
      labs(x = paste0("CP", reac_ejex_campeones()), y = paste0("CP", reac_ejey_campeones())) +
      theme(legend.position = "none")
    
    ggplotly(graf_campeones_acp)
  })
  
  
  ### Cargas campeones --------------
  
  # Creamos la matriz con todos los vectores de cargas de las componentes principales
  cargas_campeones <- acp_campeones$var$coord
  
  
  # Creamos una distincion de colores para que sea mas rapido ver cuales son las variables mas influyentes en cada CP
  reac_color1_campeones <- reactive({case_when(abs(as.numeric(cargas_campeones[, reac_ejex_campeones()])) > 0.7 ~ 2,
                                               abs(as.numeric(cargas_campeones[, reac_ejex_campeones()])) > 0.5 ~ 1,
                                               T ~ 0)}) 
  
  reac_color2_campeones <- reactive({case_when(abs(as.numeric(cargas_campeones[, reac_ejey_campeones()])) > 0.7 ~ 2,
                                               abs(as.numeric(cargas_campeones[, reac_ejey_campeones()])) > 0.5 ~ 1,
                                               T ~ 0)}) 
  
  # Unificamos la matriz de cargas y los vectores de colores, ademas, renombramos las filas y 
  # las columnas para no perder los nombres de las variables y eliminar mas facilmente las columnas de los colores
  reac_cargas_campeones <- reactive({
    x <- bind_cols(cargas_campeones,reac_color1_campeones(),reac_color2_campeones())
    rownames(x) <- rownames(cargas_campeones)
    colnames(x) <- c(colnames(cargas_campeones), "col1", "col2")
    x
  })
  
  
  # Especificamos cuales son las columnas a esconder para limpiar visualmente la salida
  reac_cps_invisibles_campeones <- reactive({
    c(1:ncol(reac_cargas_campeones()))[-c(reac_ejex_campeones(),reac_ejey_campeones())]
  })
  
  
  
  # Creamos el output de la matriz con las cargas de las CPs seleccionadas
  output$eig_campeones <- renderDT({
    
    # Con datatable le damos el formato de tabla, con list(visible = F, targets, columnas) 
    # escondemos las columnas que no seleccionamos. Con formatRound redondeamos los valores, con formatStyle le 
    # damos color a las celdas segun lo especificado en las columnas que creamos para los colores
    reac_cargas_campeones() %>%
      datatable(options =  list(pageLength = 9,
                                columnDefs = list(
                                  list(visible=F, targets= reac_cps_invisibles_campeones())),
                                searching = F
      )
      ) %>%
      formatRound(columns = 1:ncol(reac_cargas_jugadores()), digits = 3) %>% 
      formatStyle(c(reac_ejex_campeones(),reac_ejey_campeones()), c("col1", "col2"),
                  backgroundColor = styleEqual(c(1,2), c("#E5FFCC","#98FB98")))
    
  })
  
  
  
  ### Autovalores campeones -----------------
  
  # Creamos cajas de valores para ver aporte de las componentes a la variabilidad total
  # Si la caja se vuelve roja, entonces el porcentaje de la variabilidad total acumulada
  # de la componente seleccionada mas las superiores es mayor al 85% y por lo tanto no es significativa
  
  output$prct_eig_campeones <- renderValueBox({
    if (acp_campeones$eig[reac_ejex_campeones(),3] <85) {
      color_autovalor <- "olive"
    } else {
      color_autovalor <- "red"
    }
    
    valueBox(
      subtitle = paste0("Variancia explicada por la Componente principal Nº ", reac_ejex_campeones()),
      value = paste(round(acp_campeones$eig[reac_ejex_campeones(),2], 2), "%"),
      icon = icon("percent"),
      color = color_autovalor
    )
  })
  
  output$prct_eig_campeones2 <- renderValueBox({
    if (acp_campeones$eig[reac_ejey_campeones(),3] <85) {
      color_autovalor2 <- "olive"
    } else {
      color_autovalor2 <- "red"
    }
    
    valueBox(
      subtitle = paste0("Variancia explicada por la Componente principal Nº ", reac_ejey_campeones()),
      value = paste(round(acp_campeones$eig[reac_ejey_campeones(),2], 2), "%"),
      icon = icon("percent"),
      color = color_autovalor2
    )
  })
  
  
  output$prct_eig_campeones3 <- renderValueBox({
    valueBox(
      subtitle = "Variancia explicada por las Componentes Principales",
      value = paste(round(acp_campeones$eig[reac_ejex_campeones(),2] + acp_campeones$eig[reac_ejey_campeones(),2], 2), "%"),
      icon = icon("percent"),
      color = "light-blue"
    )
  })
  
  
  
  
  ## Histograma ----------------
  
  reac_data_histo <- reactive({
    if (input$histo_data == "Jugadores") {players
    } else if (input$histo_data == "Campeones") {champs
    } else if (input$histo_data == "Partidas") {matches}
  })
  
  
  observeEvent(input$histo_data, {
    updatePickerInput(session = session,
                      inputId = "histo_var",
                      choices = colnames(select_if(reac_data_histo(), is.numeric)))
  })
  
  reac_var_histo <- reactive({input$histo_var})
  
  reac_tabla_histo <- reactive({
    tabla_histo <- data.frame(
      x = reac_data_histo()[, reac_var_histo()]
    )
    
    colnames(tabla_histo) <- "x"
    
    tabla_histo
    
  })
  
  
  output$histograma <- renderPlot({
    
    ggplot(reac_tabla_histo()) +
      aes(x = x) +
      geom_histogram(fill = "#bbeaed", color = "#97cff7")+
      scale_x_continuous(breaks = as.integer(seq(min(reac_tabla_histo()$x) - max(reac_tabla_histo()$x)*0.05,max(reac_tabla_histo()$x) + max(reac_tabla_histo()$x)*0.05,by = max(reac_tabla_histo()$x)*0.05))) +
      labs(x = reac_var_histo(), y = "Cantidad")
    
    
  })
  
  
  
}

shinyApp(ui, server)










# IDEAS COSAS A CAMBIAR ---------------------

# Cambiar el formato de la fecha y duracion de las partidas

# Agregar fotos de equipos, jugadores, campeones

# Buscar rol del campeon en el ACP

# Seleccionar un campeon o jugador en la tabla y devolver estadisticas (campeon mas jugado, Mayor cantidad de asesinatos en una partida, Nivel del torneo al que llego, etc)
# Tabla de partidas seleccionar parejas de posiciones y devolver las parejas mas jugadas


# Agregar titulos a los histogramas

# Arreglar que la foto en el picker de acp campeones no aparece

# Tabla con las clasificaciones de los equipos 

# Mover y cambiar titulos



# Agregar opciones de personalizacion estetica









# Cosas descartadas -----------------------

# tabla_histo <- players %>% 
#   group_by(players$"W") %>% 
#   summarise(n = n())
# 
# 
# 
# 
# tabla_histo <-  reactive({
#   tabla_histo <- reac_data_histo() %>% 
#     group_by(reac_data_histo()[, reac_var_histo()]) %>% 
#     summarise(n = n())
#   
#   colnames(tabla_histo) <- c("x","y")
#   
#   tabla_histo
# })


