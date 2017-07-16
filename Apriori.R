
source("dir.R")
library(doAzureParallel)

# 1. Generate your credential and cluster configuration files.  
generateClusterConfig("cluster.json")
generateCredentialsConfig("credentials.json")

# 2. Fill out your credential config and cluster config files.
# Enter your Azure Batch Account & Azure Storage keys/account-info into your credential config ("credentials.json") and configure your cluster in your cluster config ("cluster.json")

# 3. Set your credentials - you need to give the R session your credentials to interact with Azure
setCredentials("credentials.json")

# 4. Register the pool. This will create a new pool if your pool hasn't already been provisioned.
cluster <- makeCluster("cluster.json")

# 5. Register the pool as your parallel backend
registerDoAzureParallel(cluster)

# 6. Check that your parallel backend has been registered
getDoParWorkers()



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











