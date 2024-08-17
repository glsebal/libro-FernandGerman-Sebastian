url <- "https://raw.githubusercontent.com/rominicky/materiales/main/assets/Provincias.csv"
destino <- "Provincias.csv"
download.file(url, destino)
provincias <- read.csv(destino)
head(provincias)
names(provincias)
library(ggplot2)

#población provincias
ggplot(data = provincias, aes(x = reorder(Nombre.de.provincia, Variación.de.población..2010.a.2022.), 
                              y = Variación.de.población..2010.a.2022.)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Variación de Población por Provincia (2010-2022)",
       x = "Provincia",
       y = "Variación de Población (%)")


#distribucion viviendas
ggplot(data = provincias, aes(x = reorder(Nombre.de.provincia, Viviendas.particulares..2022.), 
                              y = Viviendas.particulares..2022., fill = "Particulares")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = Viviendas.colectivas..2022., fill = "Colectivas"), stat = "identity") +
  coord_flip() +
  labs(title = "Distribución de Viviendas por Provincia (2022)",
       x = "Provincia",
       y = "Número de Viviendas") +
  scale_fill_manual(values = c("Particulares" = "blue", "Colectivas" = "red"),
                    name = "Tipo de Vivienda")


#superficie y poblacion
ggplot(data = provincias, aes(x = Superficie.en.km2, y = Población..2022.)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relación entre Superficie y Población por Provincia (2022)",
       x = "Superficie (km2)",
       y = "Población Total")
names(provincias)

#comparación población entre provincias
provincias_filtradas <- subset(provincias, Código.de.provincia %in% c(6, 2, 10, 34))
library(ggplot2)

ggplot(data = provincias_filtradas, aes(x = factor(Código.de.provincia), y = Población..2022., fill = Nombre.de.provincia)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparación de Población en 2022 entre Provincias",
       x = "Código de Provincia",
       y = "Población en 2022") +
  scale_fill_discrete(name = "Provincia")


#comparación situación de calle
provincias_filtradas$Proporción.calle <- provincias_filtradas$Personas.en.situación.de.calle..vía.pública. / provincias_filtradas$Población..2022. * 100

ggplot(data = provincias_filtradas, aes(x = factor(Código.de.provincia), y = Proporción.calle, fill = Nombre.de.provincia)) +
  geom_bar(stat = "identity") +
  labs(title = "Proporción de Personas en Situación de Calle en 2022",
       x = "Código de Provincia",
       y = "Proporción (%)") +
  scale_fill_discrete(name = "Provincia")

#variación población provincias
library(tidyverse)
variacion_poblacional <- provincias %>%
  select(Provincia, Variación.de.población..2010.a.2022.)
ggplot(variacion_poblacional, aes(x = reorder(Provincia, Variación.de.población..2010.a.2022.), y = Variación.de.población..2010.a.2022.)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variación de la Población por Provincia (2010-2022)", x = "Provincia", y = "Variación de Población") +
  theme_minimal()

#viviendas particulares
viviendas_2022 <- provincias %>%
  select(Provincia, Viviendas.particulares..2022., Viviendas.colectivas..2022.)
ggplot(viviendas_2022, aes(x = reorder(Provincia, Viviendas.particulares..2022.), y = Viviendas.particulares..2022.)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  labs(title = "Distribución de Viviendas Particulares por Provincia (2022)", x = "Provincia", y = "Viviendas Particulares") +
  theme_minimal()

#población por provincias
library(tidyverse)
dataset_filtrado <- provincias %>%
  filter(Código.de.provincia %in% c(2, 6, 10, 34))
variacion_poblacional <- dataset_filtrado %>%
  select(Provincia, Variación.de.población..2010.a.2022.)
ggplot(variacion_poblacional, aes(x = reorder(Provincia, Variación.de.población..2010.a.2022.), y = Variación.de.población..2010.a.2022.)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variación de la Población por Provincia (2010-2022)", x = "Provincia", y = "Variación de Población") +
  theme_minimal()


#población gráfico torta
library(ggplot2)
library(dplyr)
library(ggplot2)
filtered_data <- provincias %>%
  filter(Código.de.provincia %in% c(2, 6, 10, 34))
ggplot(filtered_data, aes(x = "", y = Población.total, fill = factor(Nombre.de.provincia))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(fill = "Provincia", y = "Población Total", title = "Distribución de la Población Total por Provincia") +
  theme_void()




