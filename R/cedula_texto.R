cedula_texto <- function(){
  
  source("R/cedula_onpe.R")
  
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
    geom_text(aes(x = 7.5, 
                  y = 30.6, 
                  label = "ELECCIONES CONGRESALES\nEXTRAORDINARIAS 2020"), 
              fontface = "bold",
              size = 7,
              lineheight = 0.8) +
    geom_text(aes(x = 0.8, 
                  y = seq(27.8, 2.6, -1.2), 
                  label = partidos),
              fontface = "bold",
              size = 5,
              hjust = 0) +
    geom_text(aes(x = 6.3, 
                  y = 29.6, 
                  label = "ORGANIZACIÓN POLÍTICA"),
              fontface = "bold",
              size = 4.0)+
    geom_text(aes(x = 6.3,
                  y = 28.9,
                  label = "MARQUE CON UNA CRUZ  +   O UN ASPA   X  DENTRO DEL RECUADRO DEL SÍMBOLO\nDE SU PREFERENCIA"),
              size = 3) +
    geom_text(aes(x = 13.25,
                  y = 29.5,
                  label = "VOTO\nPREFERENCIAL"),
              fontface = "bold",
              size = 3.5,
              lineheight = 0.8) +
    geom_text(aes(x = 13.25,
                  y = 28.9,
                  label = "SI DESEA COLOQUE DENTRO\nDE LOS RECUADROS UNO O DOS\nNÚMEROS DE LOS CANDIDATOS\nDE SU PREFERENCIA"),
              size = 1.8,
              lineheight = 0.9)
  
}