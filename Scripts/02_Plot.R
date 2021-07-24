# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-20/readme.md
# Get the Data
netflix <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')
library(tidyverse)
library(gganimate)

# https://www.dafont.com/es/bebas.font?text=netflix&psize=l
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
#font<- "Bebas" #Fuente que voy a utlizar titulo (Netflix)


# Filtro por peli
# Además en la columna "duration" le quito "min" del final y transformo a número
movies <- netflix %>%
  filter(type == "Movie") %>%
  mutate(minutes = as.numeric(str_remove(duration, " min")))

# Quiero comparar el tiempo en comedias, dramas y scifi. Para ello, 
# los selecciono por separado luego los junto.
# busco filas en las que aparezca "comedies", "dramas" o "scifi"

comedies <- movies %>%
  filter(str_detect(listed_in, 'Comedies')) %>%
  mutate(group = "Comedies")

dramas <- movies %>%
  filter(str_detect(listed_in, 'Dramas')) %>%
  mutate(group = "Dramas")

scifi <- movies %>%
  filter(str_detect(listed_in, 'Sci-Fi & Fantasy')) %>%
  mutate(group = "Sci-fi")

df <- rbind(comedies, dramas, scifi) #junto los 3 df      

head(df) #visualizo el conjunto

# Hago un primer gráfico estático
df %>%
  #  filter(release_year %in% c(2010:2020))%>%
  ggplot(aes(x = group, y = minutes)) +
  geom_boxplot()  

#Examino los años para tomar decisión sobre que años quiero visualizar 
table(df$release_year)


#Añado el filtro y algo de color
df %>%
  filter(release_year %in% c(2010:2020)) %>%
  ggplot(aes(x = group, y = minutes)) +
  geom_boxplot(fill = "#e50914", color = "#DEDEDE")  

# A parte de los elementos estéticos  en el titulo del eje "x" pongo un texto 
#cambiante según el valor del año
p3 <- df %>%
  filter(release_year %in% c(2010:2020)) %>%
  ggplot(aes(x = group, y = minutes)) +
  geom_boxplot(fill = "#e50914", color = "#DEDEDE")  +
  labs(title = "NETFLIX", 
       subtitle = "Duración películas según año", 
       caption = "@AnguloBrunet \n @RLadiesPuebla", 
       x = '{closest_state}', #Novedad! Esto hace que el título sea dinámico
       y = "") +
  theme(axis.text = element_text(colour = "#DEDEDE", family = "Bebas"), 
        plot.title = element_text(color = "#e50914", size = 50,
                                  hjust = .5, face = "bold", family = "Bebas"),
        plot.subtitle = element_text(color = "#DEDEDE", 
                                     hjust = .5, face = "italic"),
        plot.caption = element_text(color = "#DEDEDE",
                                    face = "italic"),
        axis.title = element_text(family = "Bebas",
                                 colour = "white", size = 30), 
        panel.background = element_rect(fill = '#141414'),
        plot.background = element_rect(fill = "#141414"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "bottom", #leyenda debajo
        legend.background = element_rect(fill = '#141414'), #fondo leyenda
        legend.text = element_text(colour = "#DEDEDE", family = "Bebas")) +
  transition_states(
    release_year,
    transition_length = 2,
    state_length = 1
  ) 

animate(p3,   
        height = 6, width = 11, units = "in", res = 150,
        rewind = FALSE)

anim_save("02_peliculas_box.gif", animation = last_animation())
