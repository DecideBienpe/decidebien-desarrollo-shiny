#' Crea Tabla partido en la bd rsqlite DecideBien.db
#'
#' @examples
#' \dontrun{
#'  Create_Table_Partido()
#' }
#' 
#'@export
Create_Table_Partido <- function() {
  Partido <- read.csv(file = "./Data/raw/Partido.csv")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), "./Data/DecideBien.db")
  RSQLite::dbWriteTable(conn = conn, "Partido", Partido, overwrite = TRUE)
  RSQLite::dbDisconnect(conn = conn)
}

