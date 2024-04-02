

library(lubridate)
library(tidyr)
library(ggplot2)

source("modelfx.R")

library(sf)
shapefile = read_sf(dsn = "VA_shapefile_wpop", layer = "VA_shapefile_wpop")

movement_data = read.csv("VA_mobility.csv")
pat_locator = data.frame(ID = shapefile$STCOFIPS, name = shapefile$NAME, inf = 0, pop = shapefile$pop)
pat_locator[which(pat_locator$name == "Montgomery"),]$inf = 10

pat_locator$transmission = 2/6
pat_locator$recovery = 1/6
HPop = InitiatePop(pat_locator$ID,pat_locator$inf,pat_locator$pop,pat_locator$transmission, pat_locator$recovery)

input_dates = 1:100
HPop_update = runSim(HPop,movement_data, input_dates)

output_data = pivot_longer(HPop_update$all_spread, cols = -c(day))
ggplot() + geom_line(output_data,mapping = aes(x=day,y=value,colour = name,group=name),size = 1) +
  scale_y_continuous(name = "# Infectious",trans="log10")+theme_bw(base_size = 20) + 
  theme(legend.position = "none")


output_data_all = aggregate(output_data$value, by=list(output_data$day), FUN = sum)

names(output_data_all) = c("day","inf")
ggplot() + geom_line(output_data_all,mapping = aes(x=day,y=inf),size = 1) +
  scale_y_continuous(name = "# Infectious")+theme_bw(base_size = 20)+theme(legend.position = "top")


day_50_data = subset(output_data,day == 50)
shapefile_with_inf = merge(shapefile,day_50_data,by.x="STCOFIPS",by.y="name")

library(ggplot2)
ggplot() + geom_sf(shapefile, mapping = aes(),fill = "light grey") + 
  geom_sf(subset(shapefile_with_inf,value > 0), mapping = aes(fill = value)) + 
  scale_fill_distiller(palette = "YlOrRd" , direction = 1)
