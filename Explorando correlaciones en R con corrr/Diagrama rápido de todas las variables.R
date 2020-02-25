#https://drsimonj.svbtle.com/quick-plot-of-all-variables

library(purrr)
library(tidyr)
library(ggplot2)

mtcars %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#convertir a factores
d <- mtcars
d$vs <- factor(d$vs)
d$am <- factor(d$am)

library(purrr)
d %>% keep(is.numeric) %>% head()

library(tidyr)
d %>%
  keep(is.numeric) %>% 
  gather() %>%
  head()

library(ggplot2)
d %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()                         # as density




#https://drsimonj.svbtle.com/quick-plot-of-all-variables



#Grafica algunas variables contra muchas otras con tidyr y ggplot2

library(tidyr)
library(ggplot2)

mtcars %>%
  gather(-mpg, -hp, -cyl, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = mpg, color = hp, shape = factor(cyl))) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

#Este gráfico muestra un panel de gráfico de dispersión separado para cada una
#de las muchas variables en contra mpg; Todos los puntos están coloreados por hp,
# y las formas se refieren a cyl.


library(tidyr)
mtcars %>% gather() %>% head()

#Esto funciona bien si solo queremos trazar cada variable por sí misma 
#(por ejemplo, para obtener información univariada).


#Hacemos analisis bivariado para empezar...
mtcars %>% 
  gather(-mpg, key = "var", value = "value") %>%
  head()

mtcars %>%
  gather(-mpg, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = mpg)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

#Ahora tenemos un gráfico de dispersión de cada variable en contra mpg. 



#Extraer más de una variable

mtcars %>%
  gather(-mpg, -hp, key = "var", value = "value") %>% 
  head()


mtcars %>%
  gather(-mpg, -hp, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = mpg, color = hp)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()


#Volvámonos locos y cambiemos la forma del punto por cyl:

mtcars %>%
  gather(-mpg, -hp, -cyl, key = "var", value = "value") %>% 
  head()

mtcars %>%
  gather(-mpg, -hp, -cyl, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = mpg, color = hp, shape = factor(cyl))) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()


#Por ejemplo, agreguemos líneas de loess de ggplot2 con stat_smooth():

mtcars %>%
  gather(-mpg, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = mpg)) +
  geom_point() +
  stat_smooth() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()



#https://drsimonj.svbtle.com/proportionsfrequencies-with-mean-and-booleans

x <- letters[1:10]
x == "b"  # return a boolean vector which is TRUE whenver x is "b"
#>  [1] FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

x <- 1:10
x > 5  # TRUE whenever x is greater than 5
#>  [1] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE


x > 5 & x %% 2 == 0  # TRUE when x > 5 AND divisible by 2
#>  [1] FALSE FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE FALSE  TRUE

x <- c(1, 2, NA, 4)
is.na(x) # TRUE when x is a missing value
#> [1] FALSE FALSE  TRUE FALSE


x <- 1:10
mean(x > 5)  # proportion of values in x greater than 5
#> [1] 0.5

x <- 1:10
as.numeric(x > 5)

x <- 1:10
x

test <- x > 5
test

as.numeric(test)

sum(test)

length(test)

sum(test) / length(test)

mean(test)


d <- mtcars
head(d)

# Proportion of rows (cars) with cyl == 6 (6 cylinders)
mean(d$cyl == 6)

# Proportions of rows (cars) with hp > 250 (horsepower over 200)
mean(d$hp > 250)

# Proportion of cars with 8-cylinders and that get more than 15 Miles/(US) gallon
mean(d$cyl == 8 & d$hp > 15)





