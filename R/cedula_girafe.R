

# al correr en rstudio la fuente del texto es serif, pero en la web es sans. es un bug del paquete ggiraph
cedula_girafe <- function(.df){
  
  source("R/cedula_texto.R")
  
  rect_filtrado <- tibble(xmin = 0.5, 
                     xmax = 14.5, 
                     ymin= seq(27.2, 2.0, -1.2),
                     ymax = ymin + 1.2,
                     partidos = c("FRENTE AMPLIO",
                                  "FUERZA POPULAR",
                                  "JUNTOS POR EL PERÚ",
                                  "PERÚ PATRIA SEGURA",
                                  "TODOS POR EL PERÚ",
                                  "ALIANZA PARA EL PROGRESO",
                                  "PARTIDO MORADO",
                                  "ACCIÓN POPULAR",
                                  "AVANZA PAÍS",
                                  "RENACIMIENTO UNIDO NACIONAL",
                                  "PODEMOS PERÚ",
                                  "UNIÓN POR EL PERÚ",
                                  "PARTIDO DEMOCRÁTICO SOMOS PERÚ",
                                  "DEMOCRACIA DIRECTA",
                                  "PARTIDO APRISTA PERUANO",
                                  "PERÚ NACIÓN",
                                  "FRENTE POPULAR AGRÍCOLA DEL PERÚ",
                                  "PARTIDO POLÍTICO CONTIGO",
                                  "PARTIDO POLÍTICO NACIONAL PERÚ LIBRE",
                                  "VAMOS PERÚ",
                                  "SOLIDARIDAD NACIONAL",
                                  "PARTIDO POPULAR CRISTIANO")) %>% 
    mutate(partidos = stringi::stri_trans_general(partidos, "Latin-ASCII"))
  
  filtrar <- .df %>%
    dplyr::select(Partido) %>%
    arrange(Partido) %>%
    distinct() 
  
  cedula <- cedula_texto()
  
  if(!is.null(filtrar)){
    filtrado <- dplyr::filter(rect_filtrado, !(partidos %in% filtrar$Partido))
    cedula <- cedula + geom_rect(data = filtrado, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))
  }
  
ggiraph::girafe(code = print(cedula), width_svg = (15/32)*20, height_svg = 20,
                options = list(
                  opts_sizing(rescale = TRUE, width = 0.9))
)
  
}
