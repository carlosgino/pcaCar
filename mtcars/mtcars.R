library(dplyr)
library(rsparkling)

mtcars_tbl <- copy_to(sc, mtcars, overwrite = TRUE)
mtcars_tbl
mtcars_hf <- as_h2o_frame(sc, mtcars_tbl)
mtcars_hf

library(h2o)

y <- "mpg"
x <- setdiff(names(mtcars_hf), y)

# Split the mtcars H2O Frame into train & test sets
splits <- h2o.splitFrame(mtcars_hf, ratios = 0.7, seed = 1)


#ajuste del modelo
fit <- h2o.gbm(x = x, 
               y = y, 
               training_frame = splits[[1]],
               min_rows = 1,
               seed = 1)
print(fit)


#Performance
perf <- h2o.performance(fit, newdata = splits[[2]])
print(perf)



#Prediccion
pred_hf <- h2o.predict(fit, newdata = splits[[2]])
head(pred_hf)

pred_sdf <- as_spark_dataframe(sc, pred_hf)
head(pred_sdf)



library(sparklyr)
spark_install(version = "2.1.0")


















install.packages("sparklyr")

library(sparklyr)
spark_install(version = "2.1.0")

options(rsparkling.sparklingwater.version = "2.1.0") # Using Sparkling Water 2.0.1
library(rsparkling) 









sc <- spark_connect(master = "local", version = "2.1.0")









# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("methods","statmod","stats","graphics","RCurl","jsonlite","tools","utils")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install, and initialize the H2O package for R. 
# In this case we are using rel-tverberg 2 (3.10.4.8).
install.packages("h2o", type = "source", repos = "http://h2o-release.s3.amazonaws.com/h2o/rel-tverberg/2/R")

install.packages("h2o", "3.10.4.8")



