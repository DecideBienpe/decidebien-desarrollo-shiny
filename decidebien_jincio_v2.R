library(shiny)
library(dplyr)
load("sets.RData")

ui <- fluidPage(
  #tags$head(includeHTML("incio.html")),#redes sociales
  tags$head(includeScript("ganalytics.js")),
  # App title ----
  h2("¡Decide bien! Elecciones congresales Perú 2020",align="center"),
  h4("Autor: José Incio-Ph.D Candidate University of Pittsburgh",align = "center"),
  #h5("Ph.D Candidate-University of Pittsburgh",align = "center"),
  h5(a(href="http://www.joseincio.com/", "www.joseincio.com"),align = "center"),
  h5(a(href="https://twitter.com/Jlincio", "Twitter"),align = "center"),
  div(h5("En estas elecciones, ¿te cuesta decidir por qué lista votar? Esta aplicación te puede ayudar.
  Te mostramos la/s listas que cumplen con criterios que son importantes para ti. 
  ¡Únete a los miles de peruanos que se informarán antes de dar su voto este enero!"),
      style = "color:#405d27"),
  #h5("Para saber más de como se ha codificado y saber como apoyar revisa",
  #   a(href="http://www.joseincio.com/post/decide-bien-elecciones-congresales-2020/", "aquí."),
  #   style="color:#ff7b25"),
  h5("Instrucciones:",style="color:#ff7b25" ),
  div(
    HTML("<ul><li>Cuando abres la página todos los filtros están inactivos. Esta es la opción, 'Me es indiferente'</li>
       <li>Elige tu departamento</li>
       <li>Activa los filtros que son importantes para ti</li>
       <li>Revisa las listas que pasaron tus filtros</li>
       </ul>"),style="color:#ff7b25" ),
  tags$hr(style="border-color: blue;"),
  p("Este app es posible gracias al auspicio de",
    a(href="https://www.transparencia.org.pe/",
      "ASOCIACIÓN CIVIL TRANSPARENCIA"),"
    y a la generosa donación de amig@s. Para ver la lista de aportantes y más información sobre los filtros, revisa",
    a(href="http://www.joseincio.com/post/decide-bien-elecciones-congresales-2020/","aquí.")),
  fluidRow( selectInput("depa", label = h3("Elije tu departamento"), 
                        choices = list("AMAZONAS"=1,"ANCASH"=2,"APURIMAC"=3,"AREQUIPA"=4,
                                       "AYACUCHO"=5,"CAJAMARCA"=6,"CALLAO"=7,"CUSCO"=8,
                                       "HUANCAVELICA"=9,"HUANUCO"=10,"ICA"=11,
                                       "JUNIN"=12,"LA LIBERTAD"=13,"LAMBAYEQUE"=14,
                                       "LIMA + RESIDENTES EN EL EXTRANJERO"=15,
                                       "LIMA PROVINCIAS"=16,"LORETO"=17,"MADRE DE DIOS"=18,
                                       "MOQUEGUA"=19,"PASCO"=20,"PIURA"=21,
                                       "PUNO"=22,"SAN MARTIN"=23,"TACNA"=24,
                                       "TUMBES"=25,"UCAYALI"=26), 
                        selected = 1)),
  tags$hr(style="border-color: blue;"),
  h3("¿Qué buscas en una lista?"),
  fluidRow(
    h4("¿Que no hayan ex-congresistas (2016-2019)?"),
    column(5,wellPanel(
      selectInput("ex", label = "¿Deseo Ex-congresistas de cualquier partido?", 
                  choices= list("No"=0,"Me es indiferente"=1),
                  selected=1),
      h5("Excluye todo congresista elect@s en el 2016")
    ))),
  p("Puedes filtrar por ex-congresistas según los partidos por los que fueron
    electos. El filtro de arriba (Ex-congresistas de cualquier partido) tiene que estar
    en 'Con uno o más'"),
  fluidRow(
    column(3,wellPanel(
      selectInput("ex_fuji", label = "¿Deseo Ex-congresistas Fujimoristas", 
                  choices = list("No"=0,"Me es indiferente"=1),
                  selected=1)
    )),
    column(3,wellPanel(
      selectInput("ex_apra", label = "¿Deseo Ex-congresistas Apristas/PPC?", 
                  choices= list("No"=0,"Me es indiferente"=1),
                  selected=1),
      h5("Excluye todo congresista elect@s en el 2016 por ALIANZA-POPULAR")
    )),
    column(3,wellPanel(
      selectInput("ex_ppk", label = "¿Deseo Ex-congresistas de PPK?", 
                  choices= list("No"=0,"Me es indiferente"=1),
                  selected=1),
      h5("Excluye todo congresista elect@s en el 2016 por PPK")
    )),
    column(3,wellPanel(
      selectInput("ex_fa", label = "¿Deseo Ex-congresistas de Frente Amplio?", 
                  choices= list("No"=0,"Me es indiferente"=1),
                  selected=1),
      h5("Excluye todo congresista elect@s en el 2016 por Frente Amplio")
    ))
  ),
  fluidRow(
    h4("¿Que promuevan la equidad de género?"),
    column(3,wellPanel(
      selectInput("checkbox4", label = "¿Deseo listas con + mujeres que el mínimo (30%)?",
                  choices= list("Sí"=1,"Me es indiferente"=0),selected = 0),
      h5("Si el partido incluyo al menos una mujer más de lo requerido")
    )),
    column(3,wellPanel(
      selectInput("checkbox5", label = "¿Deseo listas con paridad (50%)?",
                  choices= list("Sí"=1,"Me es indiferente"=0),selected = 0),
      h5("En algunas circunscripciones la cuota es paridad")
    )),
    column(3,wellPanel(
      selectInput("checkbox6", label = "¿Deseo listas con muejeres como cabeza de lista?",
                  choices= list("Sí"=1,"Me es indiferente"=0),selected = 0),
      h5("Si como número 1 va una mujer")
    ))
  ),
  fluidRow(
    h4("¿Que los candidatos no tengan sentencias?"),
    column(4,wellPanel(
      selectInput("checkbox7", label = "¿Deseo descartar listas que tengan candidat@s con sentencias penales?",
                  choices= list("Si"=1,"Me es indiferente"=0),selected = 0),
      h5("Sentencias declaradas en la hoja de vida")
    )),
    column(4,wellPanel(
      selectInput("checkbox8", label = "¿Deseo descartar listas que tengan candidat@s con sentencias alimentarias?",
                  choices= list("Si"=1,"Me es indiferente"=0),selected = 0),
      h5("Sentencias declaradas en la hoja de vida")
    ))
  ),
  mainPanel(
    # Output: Table summarizing the values entered ----
    h3(textOutput("Region")),
    h5(textOutput("ayuda")),
    tableOutput("table"),
    #tags$hr(style="border-color: red;"),
    #h3(textOutput("caption")),
    #tableOutput("candidates"),
    h3(textOutput("contacto")),
    h3(textOutput("actuali"))
  )
)
#)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table <- renderTable({
    data=data2%>%filter(Cod==input$depa)
    if(input$ex==0){
      data=data[data$ex==0,]
    }
    if(input$ex_fuji==0){
      data=data[data$ex_fuji==0,]
    }
    if(input$ex_apra==0){
      data=data[data$ex_apra==0,]
    }
    if(input$ex_ppk==0){
      data=data[data$ex_ppk==0,]
    }
    if(input$ex_fa==0){
      data=data[data$ex_fa==0,]
    }
    if(input$checkbox4==1){
      data=data[data$dif>0,]
    }
    if(input$checkbox5==1){
      data=data[data$eq1>= 49,]
    }
    if(input$checkbox6==1){
      data=data[data$pos_f==1,]
    }
    if(input$checkbox7==1){
      data=data[data$Sentencia<1,]
    }
    if(input$checkbox8==1){
      data=data[data$Sentencia2<1,]
    }
    data%>%dplyr::select(Orgpol,Sentencia,Sentencia2)%>%
      rename(#"Ex-Congresistas"="ex","Equidad"="eq1",
        #       "Cabeza-Mujer"="pos_f","#Mujeres"="nm1",
        #        "(cuota+1)"="dif",
        "Num cand. con sentencia penal en la lista"="Sentencia",
        "Num cand. con senten de alimm/fam en la lista"="Sentencia2")
  })
  output$Region <-renderText(paste({as.character(Codigos[Codigos$Cod==input$depa,1])},
                                   ":Listas que pasan tus filtros"))
  output$ayuda<-renderText({"La tabla muestra el número de candidatos con sentencias declaradas por lista"})
  output$actuali <-renderText({"Data actualizada al: 2019-11-27"})
  output$contacto<-renderText({"Soy responsable de cualquier error y si encuentras alguno avisame a: jincio@gmail.com"})
}

# Run the application 
shinyApp(ui = ui, server = server)
