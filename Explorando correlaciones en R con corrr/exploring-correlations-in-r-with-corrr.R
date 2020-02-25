#https://drsimonj.svbtle.com/exploring-correlations-in-r-with-corrr

d <- mtcars
d$hp[3] <- NA
head(d)

#Podríamos estar motivados por la multicolinealidad :

fit_1 <- lm(mpg ~ hp,    data = d)
fit_2 <- lm(mpg ~ hp + disp, data = d)

summary(fit_1)
summary(fit_2)

rs <- cor(d)
rs

#Si el uso es "todo", las NA se propagarán conceptualmente, es decir, el valor resultante 
#será NA siempre que una de sus observaciones contribuyentes sea NA.

#Tienes que manejar los valores perdidos con use:
  
rs <- cor(d, use = "pairwise.complete.obs")
rs

#¿Podemos centrarnos en el subconjunto con dplyr? No

dplyr::select(rs, mpg, hp, disp)
#Error in UseMethod("select_") : 
#no applicable method for 'select_' applied to an object of class "c('matrix', 'double', 'numeric')"

#Riiiiiight! Es una matriz y dplyr es para marcos de datos.
class(rs)
#[1] "matrix"

#Entonces podemos usar corchetes con matrices? O no…

vars <- c("mpg", "hp", "disp")
rs[rownames(rs) %in% vars]

#Mm, los corchetes pueden asumir diferentes funciones con matrices. 
#Sin una coma, se trata como un vector. 
#Con una coma, podemos especificar por separado las dimensiones.

vars <- c("mpg", "hp", "disp")
rs[rownames(rs) %in% vars, colnames(rs) %in% vars]

#Jajaja Alta correlación entre variables de entrada (multicolinealidad).

vars <- c("mpg", "hp", "disp")
rs[rownames(rs) %in% vars, colnames(rs) %in% vars]

factanal(na.omit(d), factors = 2)

factanal(na.omit(d), factors = 5)


#Intentemos encontrar todas las variables con una correlación mayor a 0.90. 

col_has_over_90 <- apply(rs, 2, function(x) any(x > .9))
rs[, col_has_over_90]

#Excluir diagonal:

diag(rs) <- NA
col_has_over_90 <- apply(rs, 2, function(x) any(x > .9, na.rm = TRUE))
rs[, col_has_over_90]


#Explorando datos con el tidyverse.
#explorar con tidyverse es muy facil

library(tidyverse)
d %>% 
  select(mpg:drat) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~key, scales = "free")


library(corrr)
d %>% 
  correlate() %>% 
  focus(mpg:drat, mirror = TRUE) %>% 
  network_plot()


rs <- correlate(d)
rs
class(rs)
#lo mismo que anterior con mas argumentos
correlate(d, method = "spearman", diagonal = 1)

rs %>% 
  select(mpg:drat) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~key)


#¿Qué tal ese desafío para encontrar cols con una correlación mayor que .9?
  
any_over_90 <- function(x) any(x > .9, na.rm = TRUE)
rs %>% select_if(any_over_90)

rs %>% 
  focus(mpg, disp, hp)

rs %>% 
  focus(-mpg, -disp, -hp)

rs %>% 
  focus(mpg, disp, hp, mirror = TRUE)

rs %>% 
  focus(matches("^d"))

rs %>% 
  focus_if(any_over_90, mirror = TRUE)

#Uno de mis usos favoritos es establecer focus()correlaciones 
#de una variable con todas las demás y trazar los resultados.

rs %>% 
  focus(mpg)

rs %>%
  focus(mpg) %>%
  mutate(rowname = reorder(rowname, mpg)) %>%
  ggplot(aes(rowname, mpg)) +
  geom_col() + coord_flip()

#También puede rearrange()todo el marco de datos basado en algoritmos de agrupamiento:
rs %>% rearrange()

#O shave()el triángulo superior / inferior a los valores perdidos
rs %>% shave()

#O stretch()en un formato más ordenado.

rs %>%  stretch()

rs %>%
  shave() %>% 
  stretch(na.rm = FALSE) %>% 
  ggplot(aes(r)) +
  geom_histogram()


rs %>%
  focus(mpg:drat, mirror = TRUE) %>% 
  rearrange() %>% 
  shave(upper = FALSE) %>% 
  select(-hp) %>% 
  filter(rowname != "drat")

rs %>% fashion()

rs %>%
  focus(mpg:drat, mirror = TRUE) %>% 
  rearrange() %>% 
  shave(upper = FALSE) %>% 
  select(-hp) %>% 
  filter(rowname != "drat") %>% 
  fashion()

rs %>% rplot()


rs %>%
  rearrange(method = "MDS", absolute = FALSE) %>%
  shave() %>% 
  rplot(shape = 15, colors = c("red", "green"))


#O hacer un network_plot()

rs %>% network_plot(min_cor = .6)


con <- DBI::dbConnect(RSQLite::SQLite(), path = ":dbname:")
db_mtcars <- copy_to(con, mtcars)
class(db_mtcars)

db_mtcars %>% correlate(use = "complete.obs")


sc <- sparklyr::spark_connect(master = "local")
#> * Using Spark: 2.1.0
mtcars_tbl <- copy_to(sc, mtcars)
correlate(mtcars_tbl, use = "complete.obs")