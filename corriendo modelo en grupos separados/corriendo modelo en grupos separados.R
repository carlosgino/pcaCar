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


#Si extrae información como esta, lo siguiente que probablemente desee hacer es unnest()la siguiente:

mtcars %>% 
  nest(-cyl) %>% 
  mutate(fit = map(data, ~ t.test(.$mpg)),
         results = map(fit, glance)) %>% 
  unnest(results)


#graficando los intervalos de confianza de la t student

mtcars %>% 
  nest(-cyl) %>% 
  mutate(fit = map(data, ~ t.test(.$mpg)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  ggplot(aes(x = factor(cyl), y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .2) +
  labs(x = "Cylinders (cyl)", y = "Miles Per Gallon (mpg)")


#graficando una regresion lineal x cada cilindro

mtcars %>% 
  nest(-cyl) %>% 
  mutate(fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = (.))))



# Todo lo que hemos hecho es intercambiar t.test()por lm(), utilizando nuestras variables y datos en los 
#lugares apropiados. Vamos glance()al modelo:

mtcars %>% 
  nest(-cyl) %>% 
  mutate(fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results)



#Vayamos a trazar los valores de R cuadrado para ver cuánta variación se tiene en cuenta en cada modelo:

mtcars %>% 
  nest(-cyl) %>% 
  mutate(fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  ggplot(aes(x = factor(cyl), y = r.squared)) +
  geom_bar(stat = "identity") +
  labs(x = "Cylinders", y = expression(R^{2}))

#A mí me parece que el modelo tiene peor desempeño para los autos con 8 cilindros
# que para los autos con 4 o 6 cilindros.


# A diferencia glance(), augment()extrae información que coincide con cada fila de los datos originales

mtcars %>% 
nest(-cyl) %>% 
  mutate(fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, augment))




#Nuestra resultscolumna nuevamente contiene marcos de datos. Veamos

mtcars %>% 
  nest(-cyl) %>% 
  mutate(fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, augment)) %>% 
  unnest(results)


#A continuación se muestra una gráfica de estos valores predichos contra los valores reales

mtcars %>% 
  nest(-cyl) %>% 
  mutate(fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, augment)) %>% 
  unnest(results) %>% 
  ggplot(aes(x = mpg, y = .fitted)) +
  geom_abline(intercept = 0, slope = 1, alpha = .2) +  # Line of perfect fit
  geom_point() +
  facet_grid(cyl ~ .) +
  theme_bw()





#usando randomForest

library(randomForest)
library(nycflights13)

# Convenience function to get importance information from a randomForest fit
# into a data frame
imp_df <- function(rf_fit) {
  imp <- randomForest::importance(rf_fit)
  vars <- rownames(imp)
  imp %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(var = vars)
}

set.seed(123)
flights %>% 
  # Selecting data to work with
  na.omit() %>% 
  select(carrier, arr_delay, year, month, day, dep_time) %>% 
  filter(carrier %in% c("9E", "AS", "VX")) %>% 
  # Nesting data and fitting model
  nest(-carrier) %>% 
  mutate(fit = map(data, ~ randomForest(arr_delay ~ ., data = .,
                                        importance = TRUE,
                                        ntree = 100)),
         importance = map(fit, imp_df)) %>% 
  # Unnesting and plotting
  unnest(importance) %>% 
  ggplot(aes(x = `%IncMSE`, y = var, color = `%IncMSE`)) +
  geom_segment(aes(xend = min(`%IncMSE`), yend = var), alpha = .2) +
  geom_point(size = 3) +
  facet_grid(. ~ carrier) +
  guides(color = "none") +
  theme_bw()



#######################################################################################################


knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "figs/",
  fig.height = 3,
  fig.width = 4,
  fig.align = "center"
)


library(tidyverse)
library(rpart)
library(randomForest)

### THIS IS UNPOLISHED

## BETTER TOCONVERT TO POST ABOUT REGRESSION MODELS ONLY

invoke_models <- function(.data, model_tribble){
  model_tribble %>%
    mutate(fits = invoke_map(f, params, data = .data),
           .fitted = map(fits, predict))
}

models <- tribble(
  ~mod,                ~f,             ~params,
  "Linear Regression", "lm",           list(formula = mpg ~ .),
  "Regression Tree",   "rpart",        list(formula = mpg ~ ., method = "anova"),
  "Random Forest",     "randomForest", list(formula = mpg ~ .)
)

mtcars %>%
  nest(-cyl) %>%
  mutate(cyl = paste(cyl, "Cyl"),
         fits = map(data, ~invoke_models(., models)),
         mpg = map(data, ~ .$mpg)) %>% 
  select(-data) %>% 
  unnest(fits, .drop = F) %>% 
  unnest(mpg, .fitted) %>% 
  ggplot(aes(y = .fitted, x = mpg, xend = mpg, yend = mpg)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_segment(alpha = .3) +
  geom_point() +
  theme_bw() +
  facet_grid(cyl ~ mod)



# Fit a regression model predicting Miles Per Gallon (mpg)
# from the mtcars data set
fit <- lm(mpg ~ hp, data = mtcars)

# Extract actual data and model results.
d <- augment(fit)

# Create a plot of the actual mpg data against the predicted (.fitted) results
ggplot(d, aes(y = .fitted, x = mpg, xend = mpg, yend = mpg)) +
  # Add a dotted line of perfect fit. Points on or near this line are well
  # predicted by the model.
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  # Add paths that join the points to the line of perfect fit
  geom_segment(alpha = .3) +
  # Add the points themselves
  geom_point() +
  # Add a plain theme
  theme_bw()



#Tibbles and Tribbles
#We'll be using tibbles (a modern data frame) and hand-creating some with tribble() like in the following example:

tribble(
~f,   ~params,
"lm", "mpg ~ hp",
"lm", "mpg ~ hp + wt",
"lm", "mpg ~ hp + wt + disp"
)


#Steps 1 and 2: nesting data and fitting model

mtcars %>% 
  nest(-cyl) %>% 
  mutate(fits = map(data, ~ lm(mpg ~ hp, data = .)))


#Step 3: extracting fitted values
#Next we want to extract (augment()) the predicted (.fitted) values by adding the following to mutate().

mtcars %>% 
  nest(-cyl) %>% 
  mutate(fits = map(data, ~ lm(mpg ~ hp, data = .)),
         predicted = map(fits, augment))



#Step 4: unnest() the predicted values.
#Let's unnest() it as follows:

mtcars %>% 
nest(-cyl) %>% 
mutate(fits = map(data, ~ lm(mpg ~ hp, data = .)),
predicted = map(fits, augment)) %>% 
unnest(predicted)


#Step 5: plotting results
#Finally, we plot the results similarly to the ggplot2 example shown earlier, 
#but with the addition of facet_grid() to split the results into separate panels:
  
  mtcars %>% 
  nest(-cyl) %>% 
  mutate(fits = map(data, ~ lm(mpg ~ hp, data = .)),
         predicted = map(fits, augment)) %>% 
  unnest(predicted) %>% 
  ggplot(aes(y = .fitted, x = mpg, xend = mpg, yend = mpg)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_segment(alpha = .3) +
  geom_point() +
  theme_bw() +
  facet_grid(cyl ~ .)

#Remember, this plot does not show the results of one regression model separately for each group.
#This plot shows the results of three separate regression models, one for each group in cyl.


  
  
#Un grupo varios modelos

#  1 Defina una lista de nombres de modelos, llamadas a funciones y parámetros con tribble ().
#  2 Guarde los ajustes de estos modelos en los datos como una nueva columna (a través de mutate () y invoke_map ()).
#  3 Extraiga los valores predichos (con augment () si es posible).
#  4 Unest () los valores predichos.
#  5 Grafique los resultados 
#  Tenga en cuenta que los pasos 3 a 5 son los mismos que antes.
  
  
  
#Paso 1: Definir tibble de modelos. Definiremos los modelos de la siguiente manera:
    
d <- tribble (
      ~ mod,        ~ f, ~ params,
    "1 predictor",   lm, mpg ~ hp,
    "2 predictores", lm, mpg ~ hp + disp,
    "3 predictores", lm, mpg ~ hp + disp + wt
)

#Step 2: Fit the models

d %>% mutate(fits = invoke_map(f, params, data = mtcars))


#Steps 3 - 5: Extract, unnest, and plot fitted values

d %>%
  mutate( fits = invoke_map(f, params, data = mtcars),
         predicted = map(fits, augment)) %>% 
  unnest(predicted) %>% 
  ggplot(aes(x = mpg, y = .fitted)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_segment(aes(xend = mpg, yend = mpg), alpha = .3) +
  geom_point() +
  theme_bw() +
  facet_grid(. ~ mod)


#Many groups, many models

# Function for invoking many models in a tribble()
# on a data frame
invoke_models <- function(.data, model_tribble){
  model_tribble %>%
    mutate(fits = invoke_map(f, params, data = .data))
}



models <- tribble(
  ~mod,           ~f,  ~params,
  "1 predictor",  lm,  mpg ~ hp,
  "2 predictors", lm,  mpg ~ hp + disp,
  "3 predictors", lm,  mpg ~ hp + disp + wt
)



mtcars %>% 
  nest(-cyl)



mtcars %>%
  nest(-cyl) %>%
  mutate(fits = map(data, ~invoke_models(., models)))


mtcars %>%
  nest(-cyl) %>%
  mutate(fits = map(data, ~invoke_models(., models))) %>% 
  unnest(fits)


mtcars %>%
  nest(-cyl) %>%
  mutate(fits = map(data, ~invoke_models(., models))) %>% 
  unnest(fits) %>% 
  mutate(predicted = map(fits, augment)) %>% 
  unnest(predicted) %>% 
  ggplot(aes(y = .fitted, x = mpg, xend = mpg, yend = mpg)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_segment(alpha = .3) +
  geom_point() +
  theme_bw() +
  facet_grid(cyl ~ mod)


d <- tribble(
  ~mod,                ~f,             ~params,
  "Linear Regression", "lm",           mpg ~ hp + disp + wt,
  "Regression Tree",   "rpart",        mpg ~ hp + disp + wt,
  "Random Forest",     "randomForest", mpg ~ hp + disp + wt
)


d %>% mutate(fits = invoke_map(f, params, data = mtcars))


d %>%
   mutate(fits = invoke_map(f, params, data = mtcars),
          .fitted = map(fits, predict))


 results <- models %>%
              mutate(fits = invoke_map(f, params, data = d)) %>% 
              unite(model, f, params, sep = ": ")
 
 # Plot results
 results %>%
   mutate(predicted = map(fits, augment)) %>% 
   unnest(predicted) %>% 
   ggplot(aes(x = mpg, y = .fitted)) +
   geom_abline(intercept = 0, slope = 1, linetype = 2) +
     geom_segment(aes(xend = mpg, yend = mpg), alpha = .3) +
     geom_point() +
     facet_grid(. ~ model) +
     theme_bw()


