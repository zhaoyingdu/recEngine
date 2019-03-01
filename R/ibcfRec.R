require('RPostgreSQL')
require('recommenderlab')

connectDB <- function(ip){
  username<-'postgres'
  password<-'zhaoyingdu'
  driver<-dbDriver('PostgreSQL')
  connection<-dbConnect(drv = driver, dbname='temp',
      host=ip, port = 5109,
      user=username, password = password
    )
}

ibcfRec = function(userID,ip){
  # todo: 
  connection = connectDB(ip)
  print(dbGetInfo(connection))
  data = dbReadTable(connection, 
    'uirtable')
  
  usefulData <- data.frame(user = data$uid, item = data$itemid, rating = data$rating)
  ratingMatrix = as(data, 'realRatingMatrix')
  userVector = ratingMatrix[userID,]
  if(!file.exists('/files/recModel.rds')){
    recModel <- Recommender(ratingMatrix[1:500], method='IBCF')
    saveRDS(recModel, 'recModel.rds')
  }else{
    recModel<-readRDS('/files/recModel.rds')
  }
  recommendation <- predict(recModel, userVector, n=10)
  as(recommendation, "list")  
}

