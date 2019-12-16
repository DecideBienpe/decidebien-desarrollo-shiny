library(shiny)
library(dplyr)
library(DT)
# library(ggplot2)
# library(grid)
# library(stringr)

source("./src/Functions.R")
source("./src/ggraficoresumen.R")

dfVariable <- ReadTableVariable()
load("./Data/sets.RData")

resumen.general.variable.choices <- dfVariable$Variable

navbarPage(
  title = "DecideBien",
  
  # Primera página de la barra de navegación
  tabPanel(
    "Filtrar",
    
    tags$head(includeScript("ganalytics.js")),
    includeCSS("styles.css"),
    
    # Título de la página
    h2("¡Decide bien! Elecciones congresales Perú 2020", class =
         "centrado titulo"),
    
    # Presentación
    div(
      class = "textoIntro",
      h5(
        "En estas elecciones, ¿te cuesta decidir por qué lista votar? Esta aplicación te puede ayudar.
  Te mostramos la/s listas que cumplen con criterios que son importantes para ti.
  ¡únete a los miles de peruanos que se informarán antes de dar su voto este enero!"
      )
    ),
    
    # Instrucciones
    h5("Instrucciones:", class = "textoInstrucciones"),
    
    div(
      tags$ul(
        class = "textoInstrucciones",
        tags$li("Cuando abres la página todos los filtros están inactivos"),
        tags$li("Elige tu departamento"),
        tags$li("Activa los filtros que son importantes para ti"),
        tags$li("Revisa las listas que pasaron tus filtros"),
        tags$li("Revisa los candidatos de las listas que pasaron tus filtros")
      )
    ),
    
    tags$hr(),
    
    # Layout de inputs y tablas
    sidebarLayout(
      # sidebar para inputs
      sidebarPanel(
        # Selector de departamento
        fluidRow(seldep(strinput = "depa"),
                 class = "resetMargin"),
        
        tags$hr(),
        
        h3("¿Qué buscas en una lista?"),
        
        # Checkbox de sentencias
        fluidRow(
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
        
        # Checkbox de ex congresistas
        fluidRow(
          checkboxGroupInput(
            "ex_congreso",
            label = "Deseo no incluir listas con ex-congresistas electos (2016-2019) por:",
            choiceNames = c(
              "Fuerza Popular",
              "Alianza Popular (APRA/PPC)",
              "PPK",
              "Frente Amplio"
            ),
            choiceValues = c(1, 2, 3, 4)
          ),
          class = "resetMargin"
        ),
        
        # Checkbox de género
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
        
        # Checkbox de democracia interna
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
      
      # Contenedor de mapa y tablas filtradas
      mainPanel(
        h3(textOutput("Region")),
        h5(textOutput("ayuda"),
           class = "textoInstrucciones"),
        
        # Mapa con departamento resaltado
        imageOutput("Mapa",
                    width = "50%",
                    height = "50%"),
        h5(
          "Fuente: ",
          tags$a(href = "https://es.wikipedia.org/wiki/Departamentos_del_Per%C3%BA#Circunscripciones_actuales", "www.wikipedia.org")
        ),
        
        # Panel de tablas filtrados
        tabsetPanel(
          id = 'test',
          tabPanel("Listas que cumplen tus filtros", DT::dataTableOutput("table")),
          tabPanel(
            "Candidatos (listas filtradas)",
            DT::dataTableOutput("table3")
          )
        ),
        
        # Fecha de actualización
        h3(textOutput("actuali"))
      )
    )
  ),
  
  # Segunda página de la barra de navegación
  tpresumengeneral(resumen.general.variable.choices),
  
  # Tercera página de la barra de navegación: Análisis Bivariado
  tpAB(resumen = resumen),
  
  # Cuarta página de la barra de navegación: Créditos
  tabPanel(
    "Créditos",
    
    # Presentación
    p(
      "Este app está en línea gracias al auspicio de",
      a(href = "https://www.transparencia.org.pe/",
        "ASOCIACIÓN CIVIL TRANSPARENCIA"),
      "
    y a la generosa donación de amig@s. Para ver la lista responsables, aportantes, colaboradores y más información sobre los filtros, revisa",
      a(href = "http://www.joseincio.com/post/decide-bien-elecciones-congresales-2020/", "aquí."),
      "Esta plataforma fue iniciada por",
      a(href = "http://www.joseincio.com", "José Incio"),
      "y ahora cuenta con much@s colaboradores
    Cualquier error con la data escribe a: jincio@gmail.com"
    ),
    
    h4("Donantes"),
    
    tags$ul(
      tags$li("Angelina Cotler (@CotlerAngelina)"),
      tags$li("Javier Tarrillo (@jtarrillov)"),
      tags$li("Eliana Carlin (@ElianaCarlin)"),
      tags$li("Ricardo Moran (@RicardoMoran)"),
      tags$li("Michele Gabriela Fernandez (@La_micha)"),
    ),
    
    h4("Colaboradores/Desarrolladores"),
    
    tags$ul(
      tags$li(
        "Antonio Cucho (Github: ",
        tags$a(href = "https://github.com/antoniocuga", "antoniocuga"),
        ")"
      ),
      tags$li(
        "Luis Salas (Github: ",
        tags$a(href = "https://github.com/zettai", "zettai"),
        ")"
      ),
      tags$li(
        "Malena Maguina (Github: ",
        tags$a(href = "https://github.com/malenamaguina", "malenamaguina"),
        ")"
      ),
      tags$li(
        "Samuel Calderon (Github: ",
        tags$a(href = "https://github.com/calderonsamuel", "calderonsamuel"),
        ")"
      ),
    ),
    
    p(
      "Repositorio en Github:",
      a(href = "https://github.com/jincio/decidebien_desarrollo", "aquí.")
    )
  )
)