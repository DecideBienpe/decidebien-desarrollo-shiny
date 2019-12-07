# setwd("c:/users/malen/pucp.pe/Voluntariado/DecideBien/decidebien_desarrollo")
# load("sets.RData")
# install.packages("shiny")
# install.packages("dplyr")
# install.packages("DT")
# install.packages("RSQLite")

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
                                  "LIMA + RESIDENTES EN EL EXTRANJERO",
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
  
#dep_choices <- RSQLite::dbGetQuery(conn, "select Departamento from Departamento")
  
  
  getIdDepa <- function(strdepa){
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), "DecideBien.db")
    dfDepa <- RSQLite::dbGetQuery(conn, "select * from Departamento")
    RSQLite::dbDisconnect(conn)
    IdDepa <- dfDepa %>% filter(Departamento == strdepa) %>% select(IdDepartamento)
    return(IdDepa$IdDepartamento)
  }
  getIdDepa(strdepa)
  
  