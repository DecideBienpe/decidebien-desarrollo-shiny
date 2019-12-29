cedula_onpe <- function(){
  require(ggplot2)
  require(dplyr)
  instrucciones <- tribble(
    ~xmin, ~xmax, ~ymin, ~ymax,
    0.6,  11.8,  28.4,  29.9,
    12.1,  14.4,  28.4,  29.9)
  
  rectangulos <- tibble(xmin = 0.6, xmax = 11.8, ymin= seq(27.3, 2.1, -1.2), ymax = ymin + 1)
  
  preferencial <- tibble(xmin = 10.8, xmax = 11.8, ymin= seq(27.3, 2.1, -1.2), ymax = ymin + 1)
  
  ggplot()+
    # límites del plano y fondo en blanco
    xlim(c(0, 15))+
    ylim(c(0, 32))+
    theme_void() +
    # marco rectangular con borde negro
    geom_rect(aes(xmin = 0, xmax = 15, ymin = 0, ymax = 32), color = "black", alpha = 0) +
    # contenedores debajo del título con instruciones
    geom_rect(data = instrucciones, aes(xmin = xmin, 
                                        xmax = xmax,
                                        ymin = ymin, 
                                        ymax = ymax), 
              alpha = 0.35)+
    # rectángulos para nombres de partidos
    geom_rect(data = rectangulos, aes(xmin = xmin, 
                                      xmax = xmax,
                                      ymin = ymin, 
                                      ymax = ymax), 
              alpha = 0.2) +
    # cuadrados para logo y voto preferencial
    geom_rect(data = preferencial, aes(xmin = xmin, 
                                       xmax = xmax,
                                       ymin = ymin, 
                                       ymax = ymax), 
              color = "black", fill = "white") +
    geom_rect(data = preferencial, aes(xmin = xmin + 1.3, 
                                       xmax = xmax + 1.3,
                                       ymin = ymin, 
                                       ymax = ymax), 
              color = "black", fill = "white") +
    geom_rect(data = preferencial, aes(xmin = xmin + 2.6, 
                                       xmax = xmax + 2.6,
                                       ymin = ymin, 
                                       ymax = ymax), 
              color = "black", fill = "white")
  
}