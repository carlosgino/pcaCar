#https://drsimonj.svbtle.com/how-to-create-correlation-network-plots-with-corrr-and-ggraph

library(tidyverse)
library(corrr)
library(igraph)
library(ggraph)

#################################################################################
#Enfoque basico 
# Create a tidy data frame of correlations
tidy_cors <- d %>% 
  correlate() %>% 
  stretch()

# Convert correlations stronger than some value
# to an undirected graph object
graph_cors <- tidy_cors %>% 
  filter(abs(r) > `VALUE_BETWEEN_0_AND_1`) %>% 
  graph_from_data_frame(directed = FALSE)

# Plot
ggraph(graph_cors) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph()

###############################################################################

#Ejemplos

tidy_cors <- mtcars %>% 
  correlate() %>% 
  stretch()

tidy_cors


graph_cors <- tidy_cors %>%
  filter(abs(r) > .3) %>%
  graph_from_data_frame(directed = FALSE)

graph_cors


ggraph(graph_cors) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name))


#mejorando el grafico agregando color 
ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
  geom_node_point(color = "white", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  labs(title = "Correlations between car variables")



## Ejemplo 2

library(fivethirtyeight)
drinks
library(countrycode)

# Get relevant data for Australia and countries in Europe and the Americas
d <- drinks %>% 
  mutate(continent = countrycode(country, "country.name", "continent")) %>% 
  filter(continent %in% c("Europe", "Americas") | country == "Australia") %>% 
  select(country, contains("servings"))


# Scale data to examine relative amounts, rather than absolute volume, of
# beer, wine and spirits drunk
scaled_data <- d %>% mutate_if(is.numeric, scale)

# Tidy the data
tidy_data <- scaled_data %>% 
  gather(type, litres, -country) %>% 
  drop_na() %>% 
  group_by(country) %>% 
  filter(sd(litres) > 0) %>% 
  ungroup()

# Widen into suitable format for correlations
wide_data <- tidy_data %>% 
  spread(country, litres) %>% 
  select(-type)

wide_data

tidy_cors <- wide_data %>% 
  correlate() %>% 
  stretch()

tidy_cors


graph_cors <- tidy_cors %>%
  filter(abs(r) > .9) %>%
  graph_from_data_frame(directed = FALSE)

graph_cors


ggraph(graph_cors) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name))


#mejorando el grafico agregando color 
ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
  geom_node_point(color = "white", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  labs(title = "Correlations between drinks variables")
