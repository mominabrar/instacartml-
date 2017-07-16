
source("dir.R")

#creating a sample of orders andd priors

sample_orders <- function(perc, data){
  all_order_id <- unique(orders_train$order_id)
  sample_orders <- sample(all_order_id, 0.3 * length(all_order_id))
  orders_train <- orders_train[orders_train$order_id %in% sample_orders,]
}


#first five products in cart
cart_products <- function(n,data){
  number <- seq(1,n,1)
  ordered_firstn <- data[data$add_to_cart_order %in% number,]
  sort_data <- ordered_firstn[order(ordered_firstn$order_id,ordered_firstn$add_to_cart_order),]
  return(sort_data)
}
#aggregate data for each user
text2vec <- function(n, data, by, with){
  text_data <- cart_products(n,data)
  text_data$product_name <- gsub(",","",text_data[,with])
  product_user <- aggregate(as.formula(paste(with,by, sep = " ~ ")), data = text_data, paste, collapse = ",")
  text <- as.character(product_user[,with])
  text <- strsplit(text, ",")
  maxLen <- max(sapply(text, length))
  text <- t(sapply(text, function(x) c(x, rep(NA, maxLen - length(x)))))
  text <- as.data.frame(text)
  return(text)
}











