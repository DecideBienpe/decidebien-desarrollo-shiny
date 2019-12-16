ggraficoresumen <- function(variable){
  dfVariable <- ReadTableVariable()
  resumen <- readRDS("./Data/resumen.RDS") %>%
  select(Partido, !!rlang::sym(variable)) %>%
  arrange(!!rlang::sym(variable))

  strylabel <- dfVariable[dfVariable$Variable == variable,]$ylabel
  strtitle <-  dfVariable[dfVariable$Variable == variable,]$Titulo
  
  p <- ggplot(resumen,aes(x = factor(Partido, levels=Partido), y=!!rlang::sym(variable))) + 
    geom_bar(stat="identity") +
    labs(title=strtitle, x="Partido", y = strylabel) +
    coord_flip() +
    theme_minimal() +
    geom_text(aes(x = Partido, 
                  y = !!rlang::sym(variable) - 0.5, 
                  label = !!rlang::sym(variable), 
                  hjust = 1), 
              color = "#FFFFFF")+
    annotate("text", x = c(5.5, 11, 16.5), y = max(resumen[[variable]])/2, label = "www.decidebien.pe",
             hjust=0.5, vjust=0.5, col="red", cex=6,
             fontface = "bold", alpha = 0.2)  
  return(p)
}

#ggraficoresumen(resumen, "Sentenciados")
