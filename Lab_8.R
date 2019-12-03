### Lab_8####
## 2 de Diciembre 2019

## Abrir las mallas y librerías que se utilizarán #### 
costo_vs_residuos <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")

waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

library(tidyverse)
library(gganimate)
library(RColorBrewer)
library(viridisLite)
library(haven)

## Eliminar los valores de NA de las mallas ####
Coast_Waste <- na.omit(costo_vs_residuos)
mis_gdp <- na.omit(mismanaged_vs_gdp)
waste_gdp <- na.omit(waste_vs_gdp)

## Renombraré las columnas de las mallas para facilitar la manipulación de datos ####

colnames(Coast_Waste)[4] <- "Residuos"
colnames(Coast_Waste)[5] <- "Poblacion_costera"
colnames(Coast_Waste)[6] <- "Poblacion_total"

mis_gdp <- mis_gdp %>% select(-Year)
colnames(mis_gdp)[3] <- "Mis_PC"
colnames(mis_gdp)[4] <- "Pob"
colnames(mis_gdp)[5] <- "GDP"

colnames(waste_gdp)[4] <- "Residuos_P"
colnames(waste_gdp)[5] <- "GDP"
colnames(waste_gdp)[6] <- "Poblacion_total"


## Ordenar los datos para generar la primera gráfica con los 10 principales países generadores de residuos ####
## 1°_ Ordenar de manera descendete para tener los primeros diez ####

Principales_Generadores <- Coast_Waste %>% arrange(desc(Residuos)) %>% print()

## Agrupar solo los principales 10 generadores en una malla ####
Principales_Generadores <- slice(Principales_Generadores, 1:10) %>% print()

## Gráfico 1 ####
## Con la misma malla de los Principales Generadores, generaré otro gráfico ####
## Horizontal ####

l <- ggplot(Principales_Generadores, aes(x=Entity, y=Residuos)) +
  geom_segment( aes(x=Entity, xend=Entity, y=0, yend=Residuos), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  labs(title = "Principales países con mal manejo de Residuos en 2010", x = "Ciudades", y = "Residuos Plásticos") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) 

## Mostrar gráfico 1 ####
l

## Gráfico 2 ####

## Eliminar la columna "Year"

library(gapminder)
library(plotly)
library(viridisLite)
library(hrbrthemes)

## Ordenar las columnas ##
mis_gdp <- mis_gdp %>% select(Entity, Code, Mis_PC, Pob, GDP)

p <- mis_gdp %>%
  mutate(gdpPercap=round(GDP,2)) %>%
  mutate(pop=round(Pob/1,0)) %>%
  mutate(Mis_PC=round(Mis_PC,2)) %>%
  arrange(desc(pop)) %>%
  mutate(Entity = factor(Entity, Entity)) %>%
  mutate(text = paste("País: ", Entity, "\nPoblación: ", pop, "\nCantidad de residuos kg_dia: ", Mis_PC, "\nPIB per capita: ", gdpPercap, sep="")) %>%
  ggplot( aes(x=gdpPercap, y=Mis_PC, size = pop, color = Entity, text=text)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  labs(title = "Residuos plásticos per cápita [Kg/día] en 2010 a nivel mundial" , x = "Producto interno bruto", y = "Residuos plásticos") +
theme_ipsum(base_family = "Calibri") +
  theme(legend.position="none", 
        axis.title = element_blank())

pp <- ggplotly(p, tooltip="text")

## Mostrar gráfico #### 
pp

## Gráfico 3 #### 

r <- ggplot(waste_gdp, aes(x=Poblacion_total, y=Residuos_P)) + 
  geom_point(aes(col=Entity, size=Residuos_P)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(1000, 900000)) + 
  ylim(c(0, 1)) + 
  labs(subtitle="Residuos (kg/día) Vs Población", 
       y = "Residuos (kg/día)", 
       x = "Población", 
       title = "Residuos plásticos por día a nivel mundial") +
  theme_minimal() +
  theme(legend.position="bottom", 
        axis.title = element_blank())

## Mostrar gráfico 3 ####
r

## Gráfico 4 #### 

library(gganimate)

Los_20_mas <- waste_gdp %>% arrange(desc(Residuos_P)) %>% print()
Los_20_mas <- slice(Los_20_mas, 1:20) %>% print()

 b <- ggplot(Los_20_mas, aes(Poblacion_total, GDP, size = Residuos_P, color = Entity)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
   labs(title = 'Residuos plásticoss: {frame_time}', x = 'PIB', y = 'Población') +
   transition_time(GDP) +
   ease_aes('linear')

 ## Mostrar gráfico 4 ####
b
