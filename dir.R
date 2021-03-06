
#loading packages
library(dplyr)
library(plyr)
library(ggplot2)
library(tidyr)
library(arules)
library(arulesViz)


#set directory
setwd("~/Dropbox/Data Science/Instacart")

files <- c("aisles","departments","order_products__prior","order_products__train","orders","products")

for (i in 1:length(files)){
  assign(files[i], read.csv(paste(files[i],".csv",sep = "")))
}


#joining tables
join_tables <- function(data){
  orders_x <- merge(data,orders, by = "order_id", all.x = TRUE) %>%
    merge(., products, by = "product_id",all.x = TRUE) %>%
    merge(., aisles, by = "aisle_id", all.x = TRUE) %>%
    merge(., departments, by="department_id", all.x= TRUE)
    
    #get priors, training or test subset and drop unecessary columns
    clean <- c("department_id","aisle_id","eval_set")
    orders_final <- orders_x[,!names(orders_x) %in% clean]
    
    return(orders_final) 
}



