
Create_Tables <- function(){
  Departamento <- data.frame(IdDepartamento = numeric(26),
                              Departamento = character(26), stringsAsFactors = FALSE)
  Departamento$IdDepartamento <- seq(1, 26, by = 1)
  Departamento$Departamento <-  c("AMAZONAS",
                                  "ANCASH",
                                  "APURIMAC",
                                  "AREQUIPA",
                                  "AYACUCHO", 
                                  "CAJAMARCA", 
                                  "CALLAO", 
                                  "CUSCO", 
                                  "HUANCAVELICA", 
                                  "HUANUCO",
                                  "ICA",
                                  "JUNIN",
                                  "LA LIBERTAD",
                                  "LAMBAYEQUE",
                                  "LIMA + EXTERIOR",
                                  "LIMA PROVINCIAS",
                                  "LORETO",
                                  "MADRE DE DIOS",
                                  "MOQUEGUA",
                                  "PASCO",
                                  "PIURA",
                                  "PUNO",
                                  "SAN MARTIN",
                                  "TACNA",
                                  "TUMBES",
                                  "UCAYALI")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), "DecideBien.db")
  RSQLite::dbWriteTable(conn = conn, "Departamento", Departamento)
  RSQLite::dbDisconnect()
}

getIdDepa <- function(strdepa){
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), "DecideBien.db")
    dfDepa <- RSQLite::dbGetQuery(conn, "select * from Departamento")
    RSQLite::dbDisconnect(conn)
    IdDepa <- dfDepa %>% filter(Departamento == strdepa) %>% select(IdDepartamento)
    return(IdDepa$IdDepartamento)
  }

seldep <- function(strinput, incluyeTodos = FALSE){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), "DecideBien.db")
  if (!incluyeTodos) {
    si <- shiny::selectInput(strinput,
                             label = h3("Elije tu departamento"),
                             choices = RSQLite::dbGetQuery(conn, "select Departamento from Departamento"),
                             selected = "AMAZONAS")    
  } else {
    si <- shiny::selectInput(strinput,
                             label = h3("Elije tu departamento"),
                             choices = c('TODOS', RSQLite::dbGetQuery(conn, "select Departamento from Departamento")),
                             selected = "AMAZONAS")
  }
  RSQLite::dbDisconnect(conn)
  return(si)
}

tpAB <- function(resumen){
  df <- read.csv(file = "data2_desarrollo.csv") %>% dplyr::arrange(Region, Orgpol)
  variables <- names(df)[4:length(names(df))]
  tp <- shiny::tabPanel("Analisis Bivariado",
                        shiny::p("Analisis bivariado de la informacion por partido a nivel departamento / Nacional"),
                        shiny::sidebarLayout(
                          shiny::sidebarPanel(
                            seldep(strinput = "tpAB.depa", incluyeTodos = TRUE),
                            shiny::selectInput("tpAB.variableX","Variable X:", choices = variables),
                            shiny::selectInput("tpAB.variableY","Variable Y:", choices = variables),
                            shiny::actionButton(inputId = "tpAB.gobutton", label = "OK" ),
                            shiny::hr(),
                            shiny::helpText("Toma en cuenta las listas que NO estan declaradas improcedentes")
                          ),
                          shiny::mainPanel(
                            shiny::plotOutput("plotbiv")
                          )
                        ))
  return(tp)
}

getbiv <- function(depa, varX, varY){
  df <- read.csv(file = "data2_desarrollo.csv")
  if (!depa == 'TODOS') {
    df <- df %>%
      dplyr::filter(!!rlang::sym("Region") == depa)
  } 
  df <- df %>%
    dplyr::select(c(!!rlang::sym("Orgpol"), !!rlang::sym(varX), !!rlang::sym(varY)))
  
  g <- ggplot2::ggplot(df, aes(x=!!rlang::sym(varX), y=!!rlang::sym(varY), color = df$Orgpol)) 
  g <- g + ggplot2::geom_point()
  g <- g + ggplot2::xlim(0,100)
  return(g)
}
