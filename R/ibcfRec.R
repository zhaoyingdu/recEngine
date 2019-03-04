  # todo: use returned list to fetch actual book data and return
  # todo: implement method to revoke content in binarydata regularly, i.e. update model and matrix


require('RPostgres')
require('recommenderlab')
require('blob')
require('jsonlite')

connectDB <- function(){
  dbConnect(RPostgres::Postgres(),dbname = 'temp', 
    host = 'localhost', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
    port = 5109, # or any other port specified by your DBA
    user = 'postgres',
    password = 'zhaoyingdu')
}


# store freshly computed RatingMatrix and Recommender model to database
# developer can decide how long to keep this data,
# as too long many provide outdated recommendations, 
# while too often may be an over kill, 
# because calculating these takes very long time.
storeIbcfModel = function(con){
  data = dbReadTable(con,'transaction')
  usefulData <- data.frame(user = data$uid, item = data$isbn, rating = data$quantity)
  ratingMatrix <- as(usefulData, 'realRatingMatrix')  # save this value
  recModel <- Recommender(ratingMatrix[1:500], method='IBCF')
  res = dbSendStatement(con, 'insert into model values($1::bytea)')
  dbBind(res, params=list(as.blob(serialize(recModel,con=NULL))))
  dbClearResult(res)
}


# get ratingMatrix and recommender model from database
# this function should only be called when there is surely 
# such data in db
# return a vector: [[1]] matrix, [[2]]model


getModel=function(connection){ 
  res=dbSendQuery(connection, 'SELECT * from model')
  data=dbFetch(res,n=1)
  dbClearResult(res)

  model = unserialize(data[1,1][[1]])


}


# get a fresh user vector
# idea behind is that we can keep recommender model longer and older
# while one particular user might change more often and gives
# more influencial factor to his own result,
# therefore, we are encouraged to use fresh user vector,
# as opposed to fetch it from the rating matrix stored in the binarydata table
getUserVector = function(con, userID){
  data = dbReadTable(con, 'transaction')
  usefulData <- data.frame(user = data$uid, item = data$isbn, rating = data$quantity)
  ratingMatrix <- as(usefulData, 'realRatingMatrix')
  #print(ratingMatrix)
  userVector = ratingMatrix[userID,]
}



# gives a top 10 recommended list
# pure logical function
# params: userID - to retrive user vector to be analyzed on
# matrix-rating matrix to be used
# model - rating model to be used

getRecList = function(userVector,model){
  #userVector = matrix[userID,]
  recommendation <- predict(model, userVector, n=10)
  namedList<- as(recommendation, "list") 
  namedList[[1]]
}

dataExist = function(con){
  res = dbGetQuery(con, 'select count(*) from model')
  if(res == 1){
    TRUE
  }else{
    FALSE
  }
}

ibcfRec = function(userID){
  con = connectDB()
  if(!dataExist(con)){
    storeIbcfModel(con)
  }
  userVector = getUserVector(con,userID)
  #matrixModel = fetchMatrixAndModel(con)
  model = getModel(con)
  getRecList(userVector,model) #matrixModel[[1]], matrixModel[[2]])
}

