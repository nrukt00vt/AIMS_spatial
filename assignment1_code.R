

library(lubridate)
library(tidyr)
library(ggplot2)

source("modelfx.R")
movement_data = read.table("testmove.csv",sep=",",header=T)

patNames = unique(movement_data$to_pat)[order(unique(movement_data$to_pat))]


initialInf_vec = c(0,0,0,0,10)
totalPop_vec = c(100,100,100,100,100)
beta_vec = rep(.5,5)
gamma_vec = rep(.25,5)

HPop = InitiatePop(patNames, initialInf_vec,totalPop_vec,beta_vec, gamma_vec)
input_dates = 1:100
HPop_update = runSim(HPop,movement_data, input_dates)
output_data = pivot_longer(HPop_update$all_spread, cols = -c(day))

ggplot() + geom_line(output_data,mapping = aes(x=day,y=value,colour = name,group=name),size = 1) +
  scale_y_continuous(name = "# Infectious")+theme_bw(base_size = 20)+theme(legend.position = "top")


output_data_all = aggregate(output_data$value, by=list(output_data$day), FUN = sum)

names(output_data_all) = c("day","inf")
ggplot() + geom_line(output_data_all,mapping = aes(x=day,y=inf),size = 1) +
  scale_y_continuous(name = "# Infectious")+theme_bw(base_size = 20)+theme(legend.position = "top")


movement_data$movers = 0

HPop_update_without_movement = runSim(HPop,movement_data, input_dates)
output_data_wo_movement = pivot_longer(HPop_update_without_movement$all_spread, cols = -c(day))

ggplot() + geom_line(output_data_wo_movement,mapping = aes(x=day,y=value,colour = name,group=name),size = 1) +
  scale_y_continuous(name = "# Infectious")+theme_bw(base_size = 20)+theme(legend.position = "top")


output_data_all_wo_movement = aggregate(output_data_wo_movement$value, by=list(output_data_wo_movement$day), FUN = sum)

names(output_data_all_wo_movement) = c("day","inf")
ggplot() + geom_line(output_data_all_wo_movement,mapping = aes(x=day,y=inf),size = 1) +
  scale_y_continuous(name = "# Infectious")+theme_bw(base_size = 20)+theme(legend.position = "top")
