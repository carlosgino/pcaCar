#https://drsimonj.svbtle.com/running-a-model-on-separate-groups

library(tidyverse)
library(broom)
mtcars %>% 
  nest(-am) %>% 
  mutate(am = factor(am, levels = c(0, 1), labels = c("automatic", "manual")),
         fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, augment)) %>% 
  unnest(results) %>% 
  ggplot(aes(x = mpg, y = .fitted)) +
  geom_abline(intercept = 0, slope = 1, alpha = .2) +  # Line of perfect fit
  geom_point() +
  facet_grid(am ~ .) +
  labs(x = "Miles Per Gallon", y = "Predicted Value") +
  theme_bw()

#Algunas cosas para hacer / tener en cuenta antes de comenzar ...

#"los tibbles son una versión moderna de los marcos de datos ".
mtcars
mtcars %>% nest(-cyl)

d <- mtcars %>% nest(-cyl)
d$data[d$cyl == 4] #datos de cilindros = 4

mtcars %>% nest(-cyl, -am)

#Donde vea ..., usando un solo punto ( .) representará cada tibble anidado

mtcars %>% 
  nest(-cyl) %>% 
  mutate(fit = map(data, ~ t.test(.$mpg)))

#mutate(fit = ...)es una función dplyr que agregará una nueva columna a nuestro tibble llamada fit.
#map(data, ...)es una función purrr que itera a través de cada celda de la datacolumna 
#(que tiene nuestros tibbles anidados).
# ~ t.test(.$mpg)está ejecutando el t.test para cada celda. Debido a que esto ocurre dentro map(), 
#debemos comenzar con ~, y usar .siempre que queramos hacer referencia al tibble anidado que se está iterando.
#¿Qué hay <S3: htest>en la fitcolumna? Es el t.test()modelo ajustado para cada tibble anidado. 
#Al igual que nos asomamos a una sola datacelda, echemos un vistazo a una sola fitcelda - para autos con 4 cilindros:


d <- mtcars %>% 
  nest(-cyl) %>% 
  mutate(fit = map(data, ~ t.test(.$mpg)))
d$fit[d$cyl == 4]


#extrae el p.values de cada t.test en una nueva columna llamada p:

mtcars %>% 
  nest(-cyl) %>% 
  mutate(fit = map(data, ~ t.test(.$mpg)),
         p   = map_dbl(fit, "p.value"))

#map_dbl()se utiliza porque queremos devolver un número (un "doble")
#en lugar de una lista de objetos (que es lo que map() hace)


mtcars %>% 
  nest(-cyl) %>% 
  mutate(fit = map(data, ~ t.test(.$mpg)),
         results = map(fit, glance))


