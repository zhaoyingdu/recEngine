require('RPostgreSQL')
require('recommenderlab')

connectDB <- function(){
  username<-'postgres'
  password<-'zhaoyingdu'
  driver<-dbDriver('PostgreSQL')
  connection<-dbConnect(drv = driver, dbname='temp',
      host='127.0.0.1', port = 5109,
      user=username, password = password
    )
  #print(dbGetInfo(connection))
}




ibcfRec = function(userID){
  # todo: 
  connection = connectDB()
  dbGetInfo(connection)
  data = dbReadTable(connection, 
    'uirtable')
  
  usefulData <- data.frame(user = data$uid, item = data$itemid, rating = data$rating)
  ratingMatrix = as(data, 'realRatingMatrix')
  userVector = ratingMatrix[userID,]
  if(!file.exists('./recModel.rds')){
    recModel <- Recommender(ratingMatrix[1:500], method='IBCF')
    saveRDS(recModel, './recModel.rds')
  }
  recModel<-readRDS('./recModel.rds')
  recommendation <- predict(recModel, userVector, n=10)
  as(recommendation, "list")  
}

