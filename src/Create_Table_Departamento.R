#' Crea Tabla departamento en la bd rsqlite DecideBien.db
#'
#' @examples
#' \dontrun{
#'  Create_Tabla_Departamento()
#' }
#' 
#'@export

Create_Tabla_Departamento <- function() {
  Departamento <- data.frame(IdDepartamento = numeric(26),
                             Departamento = character(26), stringsAsFactors = FALSE)
  Departamento$IdDepartamento <- seq(1, 26, by = 1)
  Departamento$Departamento <-  c("AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",  "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", 
                                  "HUANUCO", "ICA", "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA + EXTERIOR",
                                  "LIMA PROVINCIAS", "LORETO", "MADRE DE DIOS", "MOQUEGUA", "PASCO", "PIURA", "PUNO", "SAN MARTIN",
                                  "TACNA", "TUMBES", "UCAYALI")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), "DecideBien.db")
  RSQLite::dbWriteTable(conn = conn, "Departamento", Departamento, overwrite = TRUE)
  RSQLite::dbDisconnect(conn = conn)
}