library(shiny)
library(dplyr)
library(DT)
#library(datatables)

load("sets.RData")

ui <- fluidPage(
  #tags$head(includeHTML("incio.html")),#redes sociales
  tags$head(includeScript("ganalytics.js")),
  # Main App CSS styles
  includeCSS("styles.css"),
  # App title ----
  h2("¡Decide bien! Elecciones congresales Perú 2020",class="centrado titulo"),
  h4("Autor: José Incio-University of Pittsburgh",class="centrado sub-titulo"),
  #h5("Ph.D Candidate-University of Pittsburgh",class="centrado"),
  h5(a(href="http://www.joseincio.com/", "www.joseincio.com"),class="centrado"),
  h5(a(href="https://twitter.com/Jlincio", "Twitter"),class="centrado"),
  div(h5("En estas elecciones, ¿te cuesta decidir por qué lista votar? Esta aplicación te puede ayudar.
  Te mostramos la/s listas que cumplen con criterios que son importantes para ti. 
  ¡Únete a los miles de peruanos que se informarán antes de dar su voto este enero!"),
      class="textoIntro"),
  #h5("Para saber más de como se ha codificado y saber como apoyar revisa",
  #   a(href="http://www.joseincio.com/post/decide-bien-elecciones-congresales-2020/", "aquí."),
  #   class="textoIntro"),
  h5("Instrucciones:",class="textoInstrucciones"),
  div(
    HTML("<ul><li>Cuando abres la página todos los filtros están inactivos ('Me es indiferente')</li>
       <li>Elige tu departamento</li>
       <li>Activa los filtros que son importantes para ti</li>
       <li>Revisa las listas que pasaron tus filtros</li>
       </ul>"),class="textoInstrucciones"),
  tags$hr(),
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
                        selected = 1), class="resetMargin"),
  tags$hr(),
  h3("¿Qué buscas en una lista?"),
  #fluidRow(
  #  h4("¿Que no hayan ex-congresistas (2016-2019)?"),
  #  column(5,wellPanel(
  #    selectInput("ex", label = "¿Deseo Ex-congresistas de cualquier partido?", 
  #                choices= list("No"=0,"Me es indiferente"=1),
  #                selected=1),
  #    h5("No=Excluye listas con congresistas elect@s en el 2016")
  #  )), class="resetMargin"),
  fluidRow(
    h4("Que no incluya listas con ex-congresistas electos (2016-2019) por:"),
    column(2,
           checkboxInput("fuji", label = "Fuerza Popular", value = FALSE)),
    column(2,
           checkboxInput("apra", label = "Alianza Popular", value = FALSE)),
    column(2,
           checkboxInput("ppk", label = "PPK", value = FALSE)),
    column(2,
           checkboxInput("fa", label = "Frente Amplio", value = FALSE))
    ,class="resetMargin"),
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
    h5(textOutput("ayuda"),class="textoInstrucciones"),
    #tableOutput("table"),
    tabsetPanel(
      id = 'test',
      tabPanel("Listas que cumplen tus filtros", DT::dataTableOutput("table")),
      tabPanel("Todas las listas", DT::dataTableOutput("table2"))
    ),
    #tableOutput("table"),
    #tags$hr(class="divisorOutput"),
    #h3(textOutput("caption")),
    #tableOutput("candidates"),
    h3(textOutput("contacto")),
    h3(textOutput("actuali"))
  )
)
#)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table <- DT::renderDataTable({
    data=data2%>%filter(Cod==input$depa)
    #if(input$ex==0){
    #  data=data[data$ex==0,]
    #}
    if(input$fuji==TRUE)
    {
      data=data[data$flag_ex1!=1,]
    }
    if(input$apra==TRUE)
    {
      data=data[data$flag_ex1!=2,]
    }
    if(input$fa==TRUE)
    {
      data=data[data$flag_ex1!=3,]
    }
    if(input$ppk==TRUE)
    {
      data=data[data$flag_ex1!=4,]
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
    data%>%dplyr::select(Orgpol,edad,ExpP)%>%
      rename(#"Ex-Congresistas"="ex","Equidad"="eq1",
        #       "Cabeza-Mujer"="pos_f","#Mujeres"="nm1",
        #        "(cuota+1)"="dif",
        "Edad promedio"="edad",
        "% Experiencia política"="ExpP")%>%
      datatable(options=list(pageLength = 20))
  })
  output$table2<-DT::renderDataTable({
    data=data2%>%filter(Cod==input$depa)%>%
      dplyr::select(Orgpol,edad,ExpP)%>%
      rename("Edad promedio"="edad",
             "% Experiencia política"="ExpP")%>%
      DT::datatable(options=list(pageLength = 20))})
  output$Region <-renderText(paste({as.character(Codigos[Codigos$Cod==input$depa,1])},
                                   ":Listas que pasan tus filtros"))
  output$ayuda<-renderText({"La tabla muestra el promedio de edad de los candidatos por lista (edad) y 
    el porcentaje de candidatos electos alguna vez para algún cargo, incluye todos los cargos (Experiencia Política)"})
  output$actuali <-renderText({"Data actualizada al: 2019-11-27"})
  output$contacto<-renderText({"Soy responsable de cualquier error y 
    si encuentras alguno avisame a: jincio@gmail.com"})
}

# Run the application 
shinyApp(ui = ui, server = server)
