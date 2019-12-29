source("R/cedula_texto.R")


cedula_girafe <- function(){
  
ggiraph::girafe(code = print(cedula), width_svg = (15/32)*20, height_svg = 20,
                options = list(
                  opts_sizing(rescale = TRUE, width = 0.9))
)
  
}