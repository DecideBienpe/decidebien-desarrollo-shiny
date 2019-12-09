library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(grid)
#library(datatables)
source("Functions.R")

load("sets.RData")
conn <- RSQLite::dbConnect(RSQLite::SQLite(), "DecideBien.db")
ui <- navbarPage("DecideBien",
                 tabPanel("Filtrar",
  tags$head(includeScript("ganalytics.js")),
  includeCSS("styles.css"),
  # App title ----
  h2("¡Decide bien! Elecciones congresales Perú 2020",class="centrado titulo"),
  div(h5("En estas elecciones, ¿te cuesta decidir por qué lista votar? Esta aplicación te puede ayudar.
  Te mostramos la/s listas que cumplen con criterios que son importantes para ti. 
  ¡únete a los miles de peruanos que se informarán antes de dar su voto este enero!"),
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
        seldep(strinput = "depa"),
        class = "resetMargin"
      ),
      tags$hr(),
      h3("¿Qué buscas en una lista?"),
      fluidRow(
        # Este código reemplaza los dos select input de abajo
        checkboxGroupInput(
          "sentencias",
          label = "¿Qué los candidatos no tengan sentencias?",
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
          choiceValues = c(1, 2, 3, 4)
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
      ),
      fluidRow(
        checkboxGroupInput(
          "designado",
          label = "¿Que promueva a sus militantes?",
          choiceNames = c(
            "Deseo EXCLUIR listas donde el número 1 no fue electo
            en democracia interna"
          ),
          choiceValues = c(1)
        ) 
      )
      ),
    mainPanel(
      # Output: Table summarizing the values entered ----
      h3(textOutput("Region")),
      h5(textOutput("ayuda"), class = "textoInstrucciones"),
      #tableOutput("table"),
      imageOutput("Mapa",width = "50%",height = "50%" ),
      h5("Fuente: www.wikipedia.org"),
      tabsetPanel(
        id = 'test',
        tabPanel("Listas que cumplen tus filtros", DT::dataTableOutput("table")),
        tabPanel("Candidatos (listas filtradas)", DT::dataTableOutput("table3"))#,
        #tabPanel("Todas las listas", DT::dataTableOutput("table2"))
      ),
      h3(textOutput("actuali"))
    ))
),
tabPanel("ResumenGeneral",
         p("Resumen de la información por partido a nivel nacional"),
         sidebarLayout(
           sidebarPanel(
             selectInput("variable","Variable:",
                         choices=colnames(resumen[,-c(1,2,6)])),
             hr(),
             helpText("Toma en cuenta las listas que NO
                      estan declaradas improcedentes")
           ),
           mainPanel(
             plotOutput("resumen1"),
             tags$hr(),
             DT::dataTableOutput("tableResumen")
           )
         )),
tpAB(resumen=resumen),
tabPanel("Créditos",
         p("Este app está¡ en línea gracias al auspicio de",
           a(href="https://www.transparencia.org.pe/",
             "ASOCIACIóN CIVIL TRANSPARENCIA"),"
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
         h4("Colaboradores/Desarrolladores"),
         HTML("<ul><li>Slack1</li>
       <li>Antonio Cucho (Github: antoniocuga)</li>
       <li>Luis Salas (Github: zattai)</li>
       <li>Malena Maguina (Github: malenamaguina)</li>
       <li>Samuel Calderon (Github:calderonsamuel)</li>
       <li></li>
       </ul>"),
         p("Repositorio en Github:",
           a(href="https://github.com/jincio/decidebien_desarrollo","aquí."))
         ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  IdDepa <- shiny::eventReactive(input$depa, {
    getIdDepa(input$depa)
  })
  output$table <- DT::renderDataTable({
    data <- data2_desarrollo %>% filter(Cod == IdDepa())
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
    if (!is.null(input$designado)) {
    if (1 %in% input$designado)
      data <- data %>% filter(Designado!=1)
    if (2 %in% input$designado)
      data <- data %>% filter(Designado==2)
  }  
    data %>%
      dplyr::select(Partido
                    # ,edad,ExpP
      ) %>%
      arrange(Partido) %>%distinct()%>%
      DT::datatable(options = list(pageLength = 20))
  })
  output$table3 <- DT::renderDataTable({
    data <- data2_desarrollo%>% filter(Cod == IdDepa())

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
    if (!is.null(input$designado)) {
      if (1 %in% input$designado)
        data <- data %>% filter(Designado!=1)
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
      as.character(Codigos[Codigos$Cod == IdDepa(), 1])
    },", número de escaños (",{as.character(Codigos[Codigos$Cod == IdDepa(), 4])},
    "). Listas que pasan tus filtros:"))
  
  output$ayuda <-
    renderText({
      "La primera tabla muestra las listas que pasan tus filtros,
      la segunda los candidatos de esas listas que pasan tus filtros.
      De los candidatos mostramos la edad, experiencia política previa (Experiencia_Pol),
      si tienen sentencia declarada en la hoja de vida o no, y el úlltimo grado de estudios alcanzado" })
  output$actuali <- renderText({
    "Data actualizada al: 2019-12-03"
  })
  output$tableResumen<-DT::renderDataTable({
    resumen%>%filter(ExCong>0)%>%
      select(Partido,ExCong)%>%
      rename("NúmeroExCongresistas"="ExCong")
  })
  output$resumen1<-reactivePlot(function()
    {
      if(input$variable=="Sentenciados"){
        resumen=resumen%>%select(Partido,Sentenciados)%>%
          arrange(Sentenciados)
        p=ggplot(resumen,aes(x=factor(Partido,levels=Partido),y=Sentenciados))+
          geom_bar(stat="identity")+
          labs(title="Candidatos con sentencias declaradas", 
               x="Partido", y = 
                 "Número de candidatos con sentencias")+
          coord_flip()+
          theme_minimal()
      }
      if(input$variable=="Mujeres"){
        resumen=resumen%>%select(Partido,Mujeres)%>%
          arrange(Mujeres)
        p=ggplot(resumen,aes(x=factor(Partido,levels=Partido),y=Mujeres))+
          geom_bar(stat="identity")+
          labs(title="Inclusión de género", 
               x="Partido", y = 
                 "Porcentaje de mujeres en listas")+
          scale_y_continuous(limits = c(0, 100))+
          annotate("text", x = Inf, y = -Inf, label = "www.decidebien.pe",
                   hjust=1.1, vjust=-1.1, col="white", cex=6,
                   fontface = "bold", alpha = 0.8)+
          coord_flip()+
          theme_minimal()
      }
    if(input$variable=="Experiencia_Pol"){
      resumen=resumen%>%select(Partido,Experiencia_Pol)%>%
        arrange(Experiencia_Pol)
      p=ggplot(resumen,aes(x=factor(Partido,levels=Partido),
                           y=Experiencia_Pol))+
        geom_bar(stat="identity")+
        labs(title="Pasado Político", 
             x="Partido", y = 
               "% de candidatos con cargos electos anteriores")+
        scale_y_continuous(limits = c(0, 100))+
        coord_flip()+
        theme_minimal()
    }
    print(p)
    }
  )

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

# Run the application
shinyApp(ui = ui, server = server)