#' Crea Tabla Variable en la bd rsqlite DecideBien.db
#'
#' @examples
#' \dontrun{
#'  Create_Table_Variable()
#' }
#' 
#'@export
Create_Table_Variable <- function() {
  Variable <- read.csv(file = "./Data/raw/Variable.csv")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), "./Data/DecideBien.db")
  RSQLite::dbWriteTable(conn = conn, "Variable", Variable, overwrite = TRUE)
  RSQLite::dbDisconnect(conn = conn)
}

