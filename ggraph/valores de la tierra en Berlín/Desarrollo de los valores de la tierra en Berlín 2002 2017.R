#https://rawgit.com/patperu/brw/master/index.html#setup
#patperu


#1 Introducción 

#Los datos provienen del agente de FIS del 
# Departamento de Desarrollo Urbano y Vivienda del Senado.
#Los atributos faltan para los años 2009 y 2010.




#2 Configuración

library("gdalUtils")
library("sp")
library("ggplot2")
suppressPackageStartupMessages(library("rgdal"))
suppressPackageStartupMessages(library("dplyr"))
library("leaflet")
library("viridis")
library("scales")

knitr::opts_chunk$set(echo = TRUE)



read_brw <- function(year) {
  
  dsn  <- paste0("WFS:http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/re_brw", year)
  dest <- paste0("./data/brw", year, ".shp")
  fis  <- paste0("fis:re_brw", year)
  
  ogr2ogr(s_srs = "EPSG:25833", 
          t_srs = "WGS84", 
          f = "ESRI Shapefile", 
          dsn, 
          dest, 
          fis,
          overwrite = TRUE)
  
}

read_shp <- function(x) {
  
  a1 <- paste0("./data/brw", x, ".shp")
  a2 <- paste0("brw", x)
  z <- readOGR(a1, a2, stringsAsFactors = FALSE)
  z@data
}




#3 Descargue los registros de WFS

Map(read_brw, 2002:2017)


#4 Importar los datos de atributos

dat <- Map(read_shp, 2002:2017) %>%
  bind_rows() %>%
  readr::type_convert() %>%
  mutate(year = as.numeric(format(STICHTAG, "%Y"))) %>%
  arrange(spatial_na, year)

glimpse(dat)



#5 Número de valores por área

table(dat$BEZIRK, dat$year) %>% knitr::kable()



#6 Número de valores por tipo de uso

table(dat$NUTZUNG, dat$year) %>% knitr::kable()

z <- dat %>% 
  group_by(BEZIRK, NUTZUNG, GFZ, year) %>% 
  summarise(count = n(),
            q25 = quantile(BRW, 0.25, na.rm = TRUE),
            q75 = quantile(BRW, 0.75, na.rm = TRUE),
            q95 = quantile(BRW, 0.95, na.rm = TRUE),
            iqr = IQR(BRW, na.rm = TRUE),
            mean = mean(BRW, na.rm = TRUE),
            med = median(BRW, na.rm = TRUE))  %>%
  filter(!is.na(BEZIRK)) %>%
  ungroup()



#7 en usoW - Wohngebiet

p0 <- ggplot(filter(z, year >= 2011, NUTZUNG == "W - Wohngebiet"),
             aes(year, mean, colour = "BEZIRK", group = "BEZIRK"))
p0 <- p0 + geom_line()
p0 <- p0 + facet_grid(as.factor(GFZ) ~ BEZIRK, scales = "free")
p0 <- p0 + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 8),
                 axis.text.y  = element_text(size = 8), 
                 legend.position="none")
p0 <- p0 + labs(x = "",
                y = "Durchschnitt in  €",
                title = "Bodenrichtwerte in der Nutzung 'W - Wohngebiet' (2011-2017)",
                subtitle = "",
                caption = "Quelle: FIS-Broker / Gutachterausschuss für Grundstückswerte in Berlin")
p0



#8 en uso M2 - Mischgebiet


p1 <- ggplot(filter(z, year >= 2011, NUTZUNG == "M2 - Mischgebiet"),
             aes(year, mean, colour = "BEZIRK", group = "BEZIRK"))
p1 <- p1 + geom_line()
p1 <- p1 + facet_grid(as.factor(GFZ) ~ BEZIRK, scales = "free")
p1 <- p1 + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 8),
                 axis.text.y  = element_text(size = 8),
                 legend.position="none")
p1 <- p1 + labs(x = "",
                y = "Durchschnitt in €",
                title = "Bodenrichtwerte in der Nutzung 'M2 - Mischgebiet' (2011-2017)",
                subtitle = "",
                caption = "Quelle: FIS-Broker / Gutachterausschuss für Grundstückswerte in Berlin")
p1




#9 Mapa de los valores de la tierra en 2017 (BRW <= 8,000 €)


brw <- readOGR("./data/brw2017.shp", "brw2017", 
               stringsAsFactors = FALSE, 
               encoding = "UTF-8")

brw <- brw[brw$BRW <= 8000, ]

pal <- colorNumeric(
  palette = viridis_pal()(10),
  domain = brw@data$BRW
)

popup <- paste0("<b>", brw@data$spatial_al, " - ",
                brw@data$NUTZUNG , " - ",
                brw@data$GFZ, " - ", 
                brw@data$BRW, " Euro/qm")

leafMap <- leaflet(height = "800px", width = "800px") %>%
  setView(lng = 13.383, lat = 52.516, zoom = 11) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = brw,
              stroke = TRUE,
              dashArray = 1,
              weight = 1.5,
              color = "white",
              smoothFactor = 0.20,
              fillOpacity = 0.60,
              fillColor = ~pal(brw@data$BRW),
              popup = popup,
              group = "Bodenrichtwerte") %>%
  addLegend("bottomright",
            pal = pal,
            values = brw@data$BRW,
            title = "Euro/qm",
            labFormat = labelFormat(suffix = " "),
            opacity = 1)

leafMap





#10 Distribución de BRW según GFZ


p1 <- filter(dat, 
             NUTZUNG == "W - Wohngebiet", 
             GFZ %in% c(0.4, 1, 1.2, 2.5) )

p1 <- p1[which(p1$BRW < mean(p1$BRW) + (2.5 * sd(p1$BRW))), ]

brw_violin <- ggplot(p1, aes(x=factor(year), y=BRW, fill=factor(year))) + 
  geom_violin(color = "grey50") +
  stat_summary(fun.y=mean, geom="point", size=2, colour="white") +
  stat_summary(fun.y=median, geom="point", size=2, colour="red") + 
  facet_wrap( ~ GFZ, ncol = 2, scales = "free") + 
  theme(legend.position="none") +
  scale_y_continuous(labels = comma) +
  labs(x="Jahr (2009+2010 fehlen!)",
       y="Bodenrichtwert(€)",
       title="Verteilung der Bodenrichtwerte (W-Wohngebiet) nach ausgewählter GFZ",
       subtitle="Nominal prices (2006 - 2017); BRW means visualized as points, median in red",
       caption="Quelle: FIS-Broker / Gutachterausschuss für Grundstückswerte in Berlin")

brw_violin


devtools::session_info()



