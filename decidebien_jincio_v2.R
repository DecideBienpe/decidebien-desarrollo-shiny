library(shiny)
library(dplyr)
library(DT)
#library(datatables)

load("sets.RData")

ui <- navbarPage("DecideBien",
                 tabPanel("Filtrar",
  tags$head(includeScript("ganalytics.js")),
  includeCSS("styles.css"),
  # App title ----
  h2("¡Decide bien! Elecciones congresales Perú 2020",class="centrado titulo"),
  #h4("Colaboradores: José Incio-University of Pittsburgh",class="centrado sub-titulo"),
  #h5("Ph.D Candidate-University of Pittsburgh",class="centrado"),
  #h4(a(href="http://www.joseincio.com/", "www.joseincio.com"),class="centrado"),
  #h5(a(href="https://twitter.com/Jlincio", "Twitter"),class="centrado"),
  div(h5("En estas elecciones, ¿te cuesta decidir por qué lista votar? Esta aplicación te puede ayudar.
  Te mostramos la/s listas que cumplen con criterios que son importantes para ti. 
  ¡Únete a los miles de peruanos que se informarán antes de dar su voto este enero!"),
      class="textoIntro"),
  #h5("Para saber más de como se ha codificado y saber como apoyar revisa",
  #   a(href="http://www.joseincio.com/post/decide-bien-elecciones-congresales-2020/", "aquí."),
  #   class="textoIntro"),
  h5("Instrucciones:",class="textoInstrucciones"),
  div(
    HTML("<ul><li>Cuando abres la página todos los filtros están inactivos</li>
       <li>Elige tu departamento</li>
       <li>Activa los filtros que son importantes para ti</li>
       <li>Revisa las listas que pasaron tus filtros</li>
       <li>Revisa los candidatos de las listas que pasaron tus filtros</li>
       </ul>"),class="textoInstrucciones"),
  tags$hr(),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        selectInput(
          "depa",
          label = h3("Elije tu departamento"),
          choices = list(
            "AMAZONAS" = 1,
            "ANCASH" = 2,
            "APURIMAC" = 3,
            "AREQUIPA" = 4,
            "AYACUCHO" = 5,
            "CAJAMARCA" = 6,
            "CALLAO" = 7,
            "CUSCO" = 8,
            "HUANCAVELICA" = 9,
            "HUANUCO" = 10,
            "ICA" = 11,
            "JUNIN" = 12,
            "LA LIBERTAD" = 13,
            "LAMBAYEQUE" = 14,
            "LIMA + RESIDENTES EN EL EXTRANJERO" =
              15,
            "LIMA PROVINCIAS" = 16,
            "LORETO" = 17,
            "MADRE DE DIOS" = 18,
            "MOQUEGUA" = 19,
            "PASCO" = 20,
            "PIURA" = 21,
            "PUNO" = 22,
            "SAN MARTIN" = 23,
            "TACNA" = 24,
            "TUMBES" = 25,
            "UCAYALI" = 26
          ),
          selected = 1
        ),
        class = "resetMargin"
      ),
      tags$hr(),
      h3("¿Qué buscas en una lista?"),
      fluidRow(
        # Este código reemplaza los dos select input de abajo
        checkboxGroupInput(
          "sentencias",
          label = "¿Que los candidatos no tengan sentencias?",
          choiceNames = c(
            "Deseo descartar listas que tengan candidat@s con sentencias penales (Declaradas en Hoja de Vida)",
            "Deseo descartar listas que tengan candidat@s con sentencias alimentarias (Declaradas en Hoja de Vida)"
          ),
          choiceValues = c(1, 2)
        )
      ),
      fluidRow(
        checkboxGroupInput(
          "ex_congreso",
          label = "Deseo no incluir listas con ex-congresistas electos (2016-2019) por:",
          choiceNames = c("Fuerza Popular",
                          "Alianza Popular (APRA/PPC)",
                          "PPK",
                          "Frente Amplio"),
          choiceValues = c(1, 2, 3, 4),
          #inline = TRUE
        ),
        class = "resetMargin"
      ),
      
      fluidRow(
        # Este código reemplaza los tres select input de abajo
        checkboxGroupInput(
          "genero",
          label = "¿Que promuevan la equidad de género?",
          choiceNames = c(
            "Deseo listas con más de lo requerido en la cuota (> 30%)",
            "Deseo listas con paridad (50%)",
            "Deseo listas con una mujer como cabeza de lista"
          ),
          choiceValues = c(1, 2, 3)
        )
      )),
    mainPanel(
      # Output: Table summarizing the values entered ----
      h3(textOutput("Region")),
      h5(textOutput("ayuda"), class = "textoInstrucciones"),
      #tableOutput("table"),
      tabsetPanel(
        id = 'test',
        tabPanel("Listas que cumplen tus filtros", DT::dataTableOutput("table")),
        tabPanel("Candidatos (listas filtradas)", DT::dataTableOutput("table3"))#,
        #tabPanel("Todas las listas", DT::dataTableOutput("table2"))
      ),
      #tableOutput("table"),
      #tags$hr(class="divisorOutput"),
      #h3(textOutput("caption")),
      #tableOutput("candidates"),
      #h3(textOutput("contacto")),
      h3(textOutput("actuali"))
    ))
),
tabPanel("ResumenGeneral",
         p("Aquí resumen de la información por partido")),
tabPanel("Créditos",
         p("Este app está en línea gracias al auspicio de",
           a(href="https://www.transparencia.org.pe/",
             "ASOCIACIÓN CIVIL TRANSPARENCIA"),"
    y a la generosa donación de amig@s. Para ver la lista responsables, aportantes, colaboradores y más información sobre los filtros, revisa",
           a(href="http://www.joseincio.com/post/decide-bien-elecciones-congresales-2020/","aquí."),"Esta plataforma fue iniciada por", 
           a(href="http://www.joseincio.com","José Incio"), "y ahora cuenta con much@s colaboradores
    Cualquier error con la data escribe a: jincio@gmail.com"),
         h4("Donantes"),
         HTML("<ul><li>Angelina Cotler (@CotlerAngelina)</li>
       <li>Javier Tarrillo (@jtarrillov)</li>
       <li>Eliana Carlin (@ElianaCarlin)</li>
       <li>Ricardo Moran (@RicardoMoran)</li>
       <li>Michele Gabriela Fernandez (@@La_micha)</li>
       </ul>"),
         h4("Colaboradores"),
         HTML("<ul><li>Slack1</li>
       <li>Slack1</li>
       <li>Slack1</li>
       <li>RSlack1</li>
       <li>Slack1</li>
       </ul>"),
         p("Repositorio en Github:",
           a(href="https://github.com/jincio/decidebien_desarrollo","aquí."))
         ))
#)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table <- DT::renderDataTable({
    data <- data2_desarrollo%>% filter(Cod == input$depa)
    
    # Este código reemplaza los 4 ifs de abajo
    if (!is.null(input$ex_congreso)) {
      data <- data %>% filter(!(flag_ex1 %in% input$ex_congreso))
    }
    if (!is.null(input$genero)) {
      if (1 %in% input$genero)
        data <- data %>% filter(dif > 0)
      if (2 %in% input$genero)
        data <- data %>% filter(eq1 >= 49)
      if (3 %in% input$genero)
        data <- data %>% filter(pos_f == 1)
    }
    if (!is.null(input$sentencias)) {
      if (1 %in% input$sentencias)
        data <- data %>% filter(Sentencia < 1)
      if (2 %in% input$sentencias)
        data <- data %>% filter(Sentencia2 < 1)
    }
    data %>% 
      dplyr::select(Partido
                    # ,edad,ExpP
      ) %>%
      # rename(#"Ex-Congresistas"="ex","Equidad"="eq1",
      #   #       "Cabeza-Mujer"="pos_f","#Mujeres"="nm1",
      #   #        "(cuota+1)"="dif",
      #   "Edad promedio"="edad",
      #   "% Experiencia política"="ExpP")%>%
      arrange(Partido) %>%distinct()%>% 
      DT::datatable(options = list(pageLength = 20))
  })
  output$table2 <- DT::renderDataTable({
    data <- data2_desarrollo%>% 
      filter(Cod == input$depa) %>%
      dplyr::select(Partido
                    # ,edad,ExpP
      ) %>%
      # rename("Edad promedio"="edad",
      #        "% Experiencia política"="ExpP")%>%
      arrange(Partido) %>%distinct()%>%
      DT::datatable(options = list(pageLength = 20))
  })
  output$table3 <- DT::renderDataTable({
    data <- data2_desarrollo%>% filter(Cod == input$depa)
    
    # Este código reemplaza los 4 ifs de abajo
    if (!is.null(input$ex_congreso)) {
      data <- data %>% filter(!(flag_ex1 %in% input$ex_congreso))
    }
    if (!is.null(input$genero)) {
      if (1 %in% input$genero)
        data <- data %>% filter(dif > 0)
      if (2 %in% input$genero)
        data <- data %>% filter(eq1 >= 49)
      if (3 %in% input$genero)
        data <- data %>% filter(pos_f == 1)
    }
    if (!is.null(input$sentencias)) {
      if (1 %in% input$sentencias)
        data <- data %>% filter(Sentencia < 1)
      if (2 %in% input$sentencias)
        data <- data %>% filter(Sentencia2 < 1)
    }
    data %>% 
      dplyr::select(Partido,Candidato,Número,Sexo,
                    Edad,ConSentencia,Experiencia_Pol,Estudios
                    # ,edad,ExpP
      ) %>%
      # rename(#"Ex-Congresistas"="ex","Equidad"="eq1",
      #   #       "Cabeza-Mujer"="pos_f","#Mujeres"="nm1",
      #   #        "(cuota+1)"="dif",
      #   "Edad promedio"="edad",
      #   "% Experiencia política"="ExpP")%>%
      arrange(Partido, Número)%>%distinct()%>%
      DT::datatable(options = list(pageLength = 50))
  })
  output$Region <-
    renderText(paste0({
      as.character(Codigos[Codigos$Cod == input$depa, 1])
    },", número de escaños (",{as.character(Codigos[Codigos$Cod == input$depa, 4])},
    "). Listas que pasan tus filtros:"))
  output$ayuda <-
    renderText({
      "La primera tabla muestra las listas que pasan tus filtros, 
      la segunda los candidatos de esas listas que pasan tus filtros.
      De los candidatos mostramos la edad, experiencia política previa (Experiencia_Pol),
      si tienen sentencia declarada en la hoja de vida o no, y el último grado de estudios alcanzado" })
  output$actuali <- renderText({
    "Data actualizada al: 2019-12-03"
  })
}

# Run the application
shinyApp(ui = ui, server = server)
