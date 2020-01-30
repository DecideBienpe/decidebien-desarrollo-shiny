# definiendo df de diseño
df = tribble(
   ~label_partido, ~xmin, ~xmax, ~ymin, ~ymax, ~y_partido,
       "Partido1",   7,   8,   7,   8,        7.5,
       "Partido2",   7,   8,   5,   6,        5.5,
       "Partido3",   7,   8,   3,   4,        3.5,
       "Partido4",   7,   8,   1,   2,        1.5)

df2 <- tibble(label_partido = paste("Partido", 1:22), 
              xmin = 7, 
              xmax = 8, 
              ymin = 2*(22:1)-1, 
              ymax = 2*(22:1), 
              y_partido = ymax-0.5)
lim_y <- 46

grafico_cedula <- function(.df, filtrar = NULL, lim_y = 10){
  
  # diseño base
  plot <- ggplot(.df)+
    # rectangulos de voto preferencial
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "black", fill = "white")+
    geom_rect(aes(xmin = xmin +1, xmax = xmax+1, ymin = ymin, ymax = ymax), color = "black", fill = "white") +
    # delimitando tamaño de  marco contenedor
    xlim(c(0, 10)) +
    ylim(c(0, lim_y)) +
    # nombre de partido
    geom_text(aes(x = 1, y = y_partido, label = label_partido), hjust = 0) +
    # titulo de cedula
    geom_text(aes(x = 5, y = lim_y-0.5, label = "CEDULA")) +
    # marco negro de contenedor, alpha es 0 para lograr transparencia
    geom_rect(aes(xmin = 0, xmax = 10, ymin = 0, ymax = lim_y), color = "black", alpha = 0)
  
    # se filtra la data y se cubren los partidos que no pasan los filtros
    if(!is.null(filtrar)){
      filtrado <- filter(.df, !(label_partido %in% filtrar))
      plot <- plot + geom_rect(data = filtrado, aes(xmin = 0.5, xmax = 9.5, ymin = ymin-0.5, ymax = ymax+0.5))
    }
    # se retorna plot y se elimina el plano cartesiano
    plot +
      theme_void()
}

# sólo cédula
grafico_cedula(df)  

# un partido filtrado
grafico_cedula(df, "Partido2")

# varios filtrados
grafico_cedula(df, c("Partido1", "Partido3"))

grafico_cedula(df2, c("Partido 1", "Partido 3"), lim_y = lim_y)
