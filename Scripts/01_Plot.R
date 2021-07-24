# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-20/readme.md
# Get the Data
netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')
library(tidyverse)
library(gganimate)

# https://www.dafont.com/es/bebas.font?text=netflix&psize=l
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
#font <- "Bebas" #Fuente que voy a utlizar titulo (Netflix)

# Filtramos por serie
series <- netflix_titles %>%
    filter(type == "TV Show")

# Examinamos los datos para tomar decisiones.
series %>%
    group_by(release_year) %>%
    count() %>%
    ggplot(aes(x = release_year, y = n)) +
    geom_point()

# Teniendo en cuenta la distribución anterior filtro entre 1980
#y 2020 (en 2021 hay poquitos casos)
# Además cambiamos puntos por barras (columnas)
series %>%
    filter(release_year >= 1980) %>%
    filter(release_year <= 2020) %>%
    group_by(release_year) %>%
    count() %>%
    ggplot(aes(x = release_year, y = n)) +
    geom_col()

# Añadimos una transición simple y "guardamos"
md_1 <- series %>%
    filter(release_year >= 1980) %>%
    filter(release_year <= 2020) %>%
    group_by(release_year) %>%
    count() %>%
    ggplot(aes(x = release_year, y = n)) +
    geom_col() +
    transition_states(release_year) +
    shadow_mark()

#Renderizamos el plot con las características que queremos
animate(md_1,   #nframes ha de ser múltiple del n de puntos
        height = 3, width = 5, units = "in", res = 150, #en cm a veces no me funciona
        rewind = FALSE)

#Modificamos el plot para que quede más bonito
p1 <- series %>%
    filter(release_year >= 1980) %>%
    filter(release_year <= 2020) %>%
    group_by(release_year) %>%
    count() %>%
    ggplot(aes(x = release_year, y = n, fill = n)) + # añado color de las barras según n
    geom_col() +
    scale_fill_gradient(low = "#DEDEDE", high = "#e50914") + # color de las barras
    labs(title = "NETFLIX",
         subtitle = "Número de series según año de lanzamiento desde 1980",
         caption = "@AnguloBrunet \n @RLadiesPuebla") +
    theme(axis.text = element_text(colour = "#DEDEDE"), #cambio el
          plot.title = element_text(color = "#e50914",  #texto titulo
                                    size = 50, hjust = .5,
                                    face = "bold", family = "Bebas"),
          plot.subtitle = element_text(color = "#DEDEDE", #texto subtitulo
                                       hjust = .5, face = "italic"),
          plot.caption = element_text(color = "#DEDEDE", #texto pie
                                      face = "italic"),
          panel.background = element_rect(fill = '#141414'), #fondo
          plot.background = element_rect(fill = "#141414"), #fondo
          panel.border = element_blank(), #panel
          panel.grid.major = element_blank(), #lineas del fondo
          panel.grid.minor = element_blank(),  #lineas del fondo
          legend.position = "none") +  #quito la leyenda
    transition_states(release_year,wrap = FALSE) +
    shadow_mark() # cuando pasa un año se queda la barra

#Renderizamos
animate(p1,
        height = 6, width = 11, units = "in", res = 150,
        rewind = FALSE)

anim_save("01_lanzamiento.gif", animation = last_animation())
