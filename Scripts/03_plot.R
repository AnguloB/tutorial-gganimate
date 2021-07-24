# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-20/readme.md
# Get the Data
netflix <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')
library(tidyverse)
library(gganimate)

# https://www.dafont.com/es/bebas.font?text=netflix&psize=l
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
#font<- "Bebas" #Fuente que voy a utlizar titulo (Netflix)




library(lubridate)
df <- netflix %>%
    filter(type == "Movie") %>%
    group_by(date_added) %>%
    count() %>%
    mutate(fecha = mdy(date_added)) %>% # transformo la fecha del formato qe hay
    mutate(dia = day(fecha)) %>% # extraigo dia
    mutate(mes = month(fecha)) %>% #extraigo mes
    mutate(anyo = year(fecha)) %>% #extragio año
    mutate(mes = case_when(mes == 1 ~ "Enero",  #traduzco mes
                           mes == 2 ~ "Febrero", 
                           mes == 3 ~ "Marzo", 
                           mes == 4 ~ "Abril", 
                           mes == 5 ~ "Mayo", 
                           mes == 6 ~ "Junio", 
                           mes == 7 ~ "Julio", 
                           mes == 8 ~ "Agosto", 
                           mes == 9 ~ "Septiembre", 
                           mes == 10 ~ "Octubre", 
                           mes == 11 ~ "Noviembre",
                           mes == 12 ~ "Diciembre" )) %>%
    mutate(mes = factor(mes, levels = c("Diciembre", "Noviembre", 
                                        "Octubre", "Septiembre", 
                                        "Agosto", "Julio", 
                                        "Junio", "Mayo", 
                                        "Abril", "Marzo", 
                                        "Febrero", "Enero"))) 
p2 <- df %>%
    mutate(dayoftheweek = weekdays(fecha)) %>%
    filter(anyo %in% c(2015:2020)) %>%
    ggplot(aes(x = factor(dia), y = factor(mes), fill = n)) +
    geom_tile() +
    scale_fill_gradient(low = "#43465e", high = "#e50914") + 
    labs(title = "NETFLIX", 
         subtitle = "Fecha en que la película fue añadida", 
         caption = "@AnguloBrunet \n @RLadiesPuebla", 
         x = '{closest_state}',
         y = "") +
    theme(axis.text = element_text(colour = "#DEDEDE", family = "Bebas"), 
          plot.title = element_text(color = "#e50914", size = 50, hjust = .5, face = "bold", family = "Bebas"),
          plot.subtitle = element_text(color = "#DEDEDE", hjust = .5, face = "italic"),
          plot.caption = element_text(color = "#DEDEDE", face = "italic"),
          axis.title = element_text(family = "Bebas", colour = "white", size = 30),
          panel.background = element_rect(fill = '#141414'),
          plot.background = element_rect(fill = "#141414"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.position = "bottom", 
          legend.background = element_rect(fill = '#141414'), 
          legend.text = element_text(colour = "#DEDEDE", family = "Bebas")) +
    transition_states(anyo,
                      wrap = FALSE, 
                      transition_length = 1,
                      state_length = 4) + #añadimos tiempo a cada diapositiva 
    #para poder visualizar mejor
    shadow_mark() 



animate(p2,   
        height = 6, width = 11, units = "in", res = 150,
        rewind = FALSE)

anim_save("03_peliculas.gif", animation = last_animation())


df1 <- netflix %>%
    filter(type == "Movie") %>%
    mutate(fecha = mdy(date_added)) %>% #transformo la fecha del formato qe hay
    mutate(mes = month(fecha)) %>% #extraigo mes
    mutate(anyo = year(fecha)) %>% #extragio año
    mutate(mes = case_when(mes == 1 ~ "Enero",  #traduzco mes
                           mes == 2 ~ "Febrero", 
                           mes == 3 ~ "Marzo", 
                           mes == 4 ~ "Abril", 
                           mes == 5 ~ "Mayo", 
                           mes == 6 ~ "Junio", 
                           mes == 7 ~ "Julio", 
                           mes == 8 ~ "Agosto", 
                           mes == 9 ~ "Septiembre", 
                           mes == 10 ~ "Octubre", 
                           mes == 11 ~ "Noviembre",
                           mes == 12 ~ "Diciembre" )) %>%
    mutate(mes = factor(mes, levels = c("Diciembre", "Noviembre", 
                                        "Octubre", "Septiembre", 
                                        "Agosto", "Julio", 
                                        "Junio", "Mayo", 
                                        "Abril", "Marzo", 
                                        "Febrero", "Enero"))) %>%
    mutate(dayoftheweek= weekdays(fecha))%>%
    mutate(dayoftheweek = factor(dayoftheweek, 
                                 levels = c("Monday", "Tuesday", "Wednesday",  "Thursday", 
                                            "Friday", "Saturday", "Sunday"))) %>%
    group_by(anyo,mes,  dayoftheweek) %>%
    count()


p3 <-  df1 %>%
    filter(anyo %in% c(2015:2020)) %>%
    ggplot(aes(x = dayoftheweek, y = factor(mes), fill = n)) +
    geom_tile() +
    scale_fill_gradient(low = "#43465e", high = "#e50914") + 
    labs(title = "NETFLIX", 
         subtitle = "Día de la semana y año cuando la película fue añadida", 
         caption = "@AnguloBrunet \n @RLadiesPuebla", 
         x = '{closest_state}',
         y = "") +
    theme(axis.text = element_text(colour = "#DEDEDE", family = "Bebas"), 
          plot.title = element_text(color = "#e50914", size = 50, hjust = .5, face = "bold", family = "Bebas"),
          plot.subtitle = element_text(color = "#DEDEDE", hjust = .5, face = "italic"),
          plot.caption = element_text(color = "#DEDEDE", face = "italic"),
          axis.title = element_text(family = "Bebas", colour = "white", size = 30),
          panel.background = element_rect(fill = '#141414'),
          plot.background = element_rect(fill = "#141414"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.position = "bottom", 
          legend.background = element_rect(fill = '#141414'), 
          legend.text = element_text(colour = "#DEDEDE", family = "Bebas")) +
    transition_states(anyo,
                      wrap = FALSE, 
                      transition_length = 1,
                      state_length = 4) + #añadimos tiempo a cada diapositiva 
    #para poder visualizar mejor
    shadow_mark() 



animate(p3,   
        height = 6, width = 11, units = "in", res = 150,
        rewind = FALSE)

anim_save("03_peliculas_b.gif", animation = last_animation())
