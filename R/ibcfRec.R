
ibcfRec = function(userID){
  data <- read.table('u.data', header=TRUE, sep="\t", row.names=NULL)
  usefulData <- data.frame(user = data$uid, item = data$itemId, rating = data$rating)
  ratingMatrix = as(usefulData, 'realRatingMatrix')
  userVector = ratingMatrix[userID,]
  if(!file.exists('recModel.rds')){
    recModel <- Recommender(ratingMatrix[1:500], method='IBCF')
    saveRDS(recModel, 'recModel.rds')
  }
  recModel<-readRDS('recModel.rds')
  recommendation <- predict(recModel, userVector, n=10)
  as(recommendation, "list")  
}