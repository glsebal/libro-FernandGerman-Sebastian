library(tidyverse)
url <- "https://raw.githubusercontent.com/rominicky/materiales/main/assets/Provincias.csv"
datos <- read_csv(url)
summary(datos)
datos$Variacion_Personas_Calle <- datos$`Personas en situación de calle (vía pública)` - datos$`Personas en viviendas colectivas (2010)`
datos_grouped <- datos %>%
  group_by(Provincia) %>%
  summarise(Variacion_Promedio_Personas_Calle = mean(Variacion_Personas_Calle))
datos_sorted <- datos_grouped %>%
  arrange(desc(Variacion_Promedio_Personas_Calle))
ggplot(datos_sorted, aes(x = Provincia, y = Variacion_Promedio_Personas_Calle)) +
  geom_bar(stat = "identity") +
  labs(title = "Variación Promedio en el Número de Personas en Situación de Calle (2010-2022)",
       x = "Provincia",
       y = "Variación Promedio") +
  theme_minimal()
