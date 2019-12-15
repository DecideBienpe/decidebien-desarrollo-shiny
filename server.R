library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
# library(grid)
library(stringr) 

source("R/Functions.R")
load("sets.RData")

function(input, output) {
  
  # Reactive para elegir departamento en primera página
  IdDepa <- shiny::eventReactive(input$depa, {
    getIdDepa(input$depa)
  })
  
  # Reactive para filtrado de tabla basado en inputs
  datos <- reactive({
    # Se asigna el valor de la tabla completa a variable data
    data <- data2_desarrollo %>% filter(Cod == IdDepa())
    
    # Filtrado de listas con ex congresistas
    if (!is.null(input$ex_congreso)) {
      data <- data %>% filter(!(flag_ex1 %in% input$ex_congreso))
    }
    
    # Filtrado de listas según temas de género
    if (!is.null(input$genero)) {
      if (1 %in% input$genero)
        data <- data %>% filter(dif > 0)
      if (2 %in% input$genero)
        data <- data %>% filter(eq1 >= 49)
      if (3 %in% input$genero)
        data <- data %>% filter(pos_f == 1)
    }
    
    # Filtrado de listas con candidatos sentenciados
    if (!is.null(input$sentencias)) {
      if (1 %in% input$sentencias)
        data <- data %>% filter(Sentencia < 1)
      if (2 %in% input$sentencias)
        data <- data %>% filter(Sentencia2 < 1)
    }
    
    # Filtrado de listas con numero 1 designado
    if (!is.null(input$designado)) {
      if (1 %in% input$designado)
        data <- data %>% filter(Designado!=1)
    }
    return(data)
  })
  
  # Primer Tab Panel de primera página: Aplicación de filtros para tabla de partidos filtrada
  output$table <- DT::renderDataTable({
    
    # Seleccionando sólo columna con nombres de partidos que pasan los filtros. Es la tabla que ve el usuario
    datos() %>%
      dplyr::select(Partido) %>%
      arrange(Partido) %>%
      distinct()%>%
      DT::datatable(options = list(pageLength = 20))
  })
  
  # Segundo Tab Panel de primera página: Lista filtrada de candidatos
  output$table3 <- DT::renderDataTable({
    
    # Seleccionando variables relevantes de datos de candidatos en listas filtradas. Es la tabla que ve el usuario
    datos() %>%
      dplyr::select(Partido,Candidato,Número,Sexo,
                    Edad,ConSentencia,Experiencia_Pol,Estudios) %>%
      mutate(str_numero = as.character(`Número`),
             str_numero =  str_pad(str_numero, width = 2, side = "left", pad = "0")) %>% 
      arrange(Partido, str_numero)%>%
      dplyr::select(-str_numero) %>% 
      distinct()%>%
      DT::datatable(options = list(pageLength = 50))
  })
  
  # Título de main panel de primera página: Región
  output$Region <-
    renderText(paste0({
      as.character(Codigos[Codigos$Cod == IdDepa(), 1])
    },", número de escaños (",{as.character(Codigos[Codigos$Cod == IdDepa(), 4])},
    "). Listas que pasan tus filtros:"))
  
  # Texto de ayuda de main panel de primera página
  output$ayuda <-
    renderText({
      "La primera tabla muestra las listas que pasan tus filtros,
      la segunda los candidatos de esas listas que pasan tus filtros.
      De los candidatos mostramos la edad, experiencia política previa (Experiencia_Pol),
      si tienen sentencia declarada en la hoja de vida o no, y el úlltimo grado de estudios alcanzado" })
  
  # Texto en main panel de primera página: fecha de actualización de data
  output$actuali <- renderText({
    "Data actualizada al: 2019-12-03"
  })
  
  # Tabla en segunda página: Resumen de partidos con cantidad de ex congresistas. No está relacionada con inputs
  output$tableResumen<-DT::renderDataTable({
    resumen%>%filter(ExCong>0)%>%
      select(Partido,ExCong)%>%
      rename("NúmeroExCongresistas"="ExCong")
  })
  
  # Gráfico de main panel de segunda página: Resumen según variable escogida
  output$resumen1<-renderPlot({
    
    grafico_resumen(df = resumen, variable = input$variable)
    
  })
  
  
  # Mapa de main panel de primera página
  #=====================
  # Mapa
  #=====================  
  
  output$Mapa <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('./www/',paste(input$depa, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number", input$depa))
    
  }, deleteFile = FALSE)
  #=====================
  # Analisis bivariado
  #=====================
  depa <- eventReactive(input$tpAB.gobutton, {input$tpAB.depa})
  varX <- shiny::eventReactive(input$tpAB.gobutton, {input$tpAB.variableX})
  varY <- shiny::eventReactive(input$tpAB.gobutton, {input$tpAB.variableY})    
  
  output$plotbiv <- renderPlot({
    g <- getbiv(depa = depa(), varX = varX(), varY = varY())
    g
  })
} 
