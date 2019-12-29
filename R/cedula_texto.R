source("R/cedula_onpe.R")


cedula_texto <- function(){
  
  partidos <- c("FRENTE AMPLIO",
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
                "PARTIDO POPULAR CRISTIANO")
  
  cedula_onpe()+
    # titulo cedula
    geom_text(aes(x = 7.5, y = 30.5, 
                  label = "ELECCIONES CONGRESALES\nEXTRAORDINARIAS 2020"), fontface = "bold", family = "FreeSans") +
    geom_text(aes(x = 0.8, y = seq(27.8, 2.6, -1.2), label = partidos), hjust = 0)
  
}