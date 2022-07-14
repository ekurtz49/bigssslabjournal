---
  title: "Journal 1"
#bibliography: references.bib
author: "Emily Kurtz"
output: 
  html_document:
  css: tweaks.css
toc:  true
toc_float: true
number_sections: false

---
  
  
  
  ```{r, globalsettings, echo=FALSE, warning=FALSE, results='hide'}
library(knitr)

knitr::opts_chunk$set(echo = TRUE)
opts_chunk$set(tidy.opts=list(width.cutoff=100),tidy=TRUE, warning = FALSE, message = FALSE,comment = "#>", cache=TRUE, class.source=c("test"), class.output=c("test2"))
options(width = 100)
rgl::setupKnitr()



colorize <- function(x, color) {sprintf("<span style='color: %s;'>%s</span>", color, x) }

```

```{r klippy, echo=FALSE, include=TRUE}
klippy::klippy(position = c('top', 'right'))
#klippy::klippy(color = 'darkred')
#klippy::klippy(tooltip_message = 'Click to copy', tooltip_success = 'Done')
```

Last compiled on `r format(Sys.time(), '%B, %Y')`

<br>
  
  ----
  
  White index code.


----




if (!dir.exists("./data/rawGIS/")) dir.create("./data/rawGIS/")
# Loading 100m-by-100m raster data:
rast <- sf::st_read(dsn = "./data/rawGIS/cbs_vk100_2021_v1.gpkg")


# Next we load the shapefile of the administrative neighbourhoods ('buurt') and districts ('wijk'):
neighbShape <- sf::st_read(dsn = "./data/rawGIS/WijkBuurtkaart_2021_v1", layer = "buurt_2021_v1")
districtShape <- sf::st_read(dsn = "./data/rawGIS/WijkBuurtkaart_2021_v1", layer = "wijk_2021_v1")
# ... And then the zipcode shapes:
postcode4Shape <- sf::st_read(dsn = "./data/rawGIS/CBS-PC4-2020-v1", layer = "CBS_pc4_2020_v1")
postcode5Shape <- sf::st_read(dsn = "./data/rawGIS/CBS-PC5-2020-v1", layer = "CBS_pc5_2020_v1")
postcode6Shape <- sf::st_read(dsn = "./data/rawGIS/CBS-PC6-2020-v1", layer = "CBS_pc6_2020_v1")
