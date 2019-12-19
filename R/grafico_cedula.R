# definiendo df de diseño
df = tribble(
   ~label_partido, ~x1, ~x2, ~y1, ~y2, ~y_partido,
       "Partido1",   7,   8,   7,   8,        7.5,
       "Partido2",   7,   8,   5,   6,        5.5,
       "Partido3",   7,   8,   3,   4,        3.5,
       "Partido4",   7,   8,   1,   2,        1.5)

grafico_cedula <- function(.df, filtrar = NULL){
  
  # diseño base
  plot <- ggplot(.df)+
    # rectangulos de voto preferencial
    geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), color = "black", fill = "white")+
    geom_rect(aes(xmin = x1 +1, xmax = x2+1, ymin = y1, ymax = y2), color = "black", fill = "white") +
    # delimitando tamaño de  marco contenedor
    xlim(c(0, 10)) +
    ylim(c(0, 10)) +
    # nombre de partido
    geom_text(aes(x = 1, y = y_partido, label = label_partido), hjust = 0) +
    # titulo de cedula
    geom_text(aes(x = 5, y = 9.5, label = "CEDULA")) +
    # marco negro de contenedor, alpha es 0 para lograr transparencia
    geom_rect(aes(xmin = 0, xmax = 10, ymin = 0, ymax = 10), color = "black", alpha = 0)
  
    # se filtra la data y se cubren los partidos que no pasan los filtros
    if(!is.null(filtrar)){
      filtrado <- filter(.df, !(label_partido %in% filtrar))
      plot <- plot + geom_rect(data = filtrado, aes(xmin = 0.5, xmax = 9.5, ymin = y1-0.5, ymax = y2+0.5))
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
