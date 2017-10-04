idx = (one_day$my_lat > 51.0) & (one_day$my_lat < 53.0) & (one_day$my_lon > 19.0) & (one_day$my_lon < 23.0)
clear = one_day[idx,]
#clear = clear[,c("my_lat", "my_lon", "delay", "line", "brigade")]
one_day_tmp = sample_n(clear, 10000)

library(dplyr)
library(parallel)
asd = apply(one_day_tmp, 1, list)
a = Sys.time()
tmp = mclapply(asd, function(x) {x = x[[1]]; find_zone(x["my_lon"], x["my_lat"])}, mc.cores=4 )
Sys.time() - a
