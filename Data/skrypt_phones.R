library("curl")
library("httr")
library("dplyr")
library("lubridate")
library("tidyr")
library("ggplot2")

# read measurements of phones
dane <- read.csv("measurements.csv", stringsAsFactors = FALSE)
head(dane, 2)

daneBemowo <- filter(dane, 
                     zoneid %in% c(1, 553, 751),
                     datetime > "2017-09-14 00:00:00")

szerokie <- spread(dane[,1:3], zoneid, intensity)
head(szerokie)

szerokieA <- szerokie[substr(szerokie[,1],12,13) %in% c("09",10:19),]
cor(szerokieA[,-1])

daneBemowo$zoneid <- factor(daneBemowo$zoneid, labels=c("Arkadia", "Dolinka Służewiecka", "Mordor"))

ggplot(daneBemowo, aes(ymd_hms(datetime), intensity*10000, fill=factor(zoneid))) +
  geom_col() + facet_grid(zoneid~., scales = "free_y") +
  theme_light()

czasy <- unique(dane$datetime)

library(sf)
daneGodzina <- filter(dane, datetime == "2017-09-04 12:00:00")

library(ggplot2)
library(animation)

saveGIF({
  for (czas in czasy[1:24]) {
    daneGodzina <- filter(dane, datetime == czas)
    
    tmp <- gsub(daneGodzina[,4], pattern = "[^0-9 \\.,]", replacement = "")
    tmp2 <- strsplit(tmp, split=",")
    tmp3 <- lapply(tmp2, strsplit, split = " ")
    tmp4 <- lapply(tmp3, function(x) {
      tt <- sapply(x, function(y) {
        as.numeric(y)
      })
      tt <- cbind(tt, tt[,1])
      st_polygon(list(t(tt)))
    })
    
    geometry <- do.call(st_sfc, tmp4)
    
    sales <- 100/(st_area(geometry) + 5*10^-6)
    
    ss <- st_sf(intensity = pmin(daneGodzina$intensity *
                                 10000 * sales/2649104,
                                 2000), geometry )
    
    pl <- ggplot(ss) + 
      geom_sf(aes(fill = intensity, color=intensity)) + 
      theme_minimal() +
      scale_fill_gradient(limits = c(0, 2000), low = "#ffffff", high = "#990000") +
      scale_color_gradient(limits = c(0, 2000), low = "#ffffff", high = "#990000") +
      ggtitle(paste("Warsaw ", czas)) +
      theme(legend.position = "none") +
      coord_sf(xlim=c(20.85,21.15), ylim = c(52.05,52.35))
    
    print(pl)
  }
}, clean = FALSE, interval=0.2)


```

