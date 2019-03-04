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
  ratingMatrix <- binarize(as(usefulData, 'realRatingMatrix'), minRating=1)  # save this value  
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
  usefulData <- data.frame(user = data$uid, colnames = data$isbn, rating = data$quantity)
  ratingMatrix <- binarize(as(usefulData, 'realRatingMatrix'),minRating=1)
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

userRec = function(userID){
  con = connectDB()
  if(!dataExist(con)){
    storeIbcfModel(con)
  }
  userVector = getUserVector(con,userID)
  model = getModel(con)
  getRecList(userVector,model)
}

popularRec = function(bookId){
  con = connectDB()
  if(!dataExist(con)){
    storeIbcfModel(con)
  }
  

  res = dbSendQuery(con, 'select distinct isbn from transaction ORDER BY isbn')
  colnames = dbFetch(res, n=-1)[,1]
  dbClearResult(res)
  testSubMatrix = matrix(rep(0,length(colnames)), nrow = 1, byrow = TRUE, dimnames = list('dummy', colnames))  
  testSubMatrix[1,bookId] = 1
  testRatingMatrix = as(testSubMatrix, 'binaryRatingMatrix')


  model = getModel(con)
  getRecList(testRatingMatrix, model) 
}


print('user dummy\n')
print(userRec('dummy'))
print('user a\n')
print(userRec('a'))
print('user 501\n')
print(userRec('501'))


if(FALSE){
print('1\n')
print(popularRec('1'))
print('2\n')
print(popularRec('2'))
print('3\n')
print(popularRec('3'))
}
print('500\n')
print(popularRec('500'))
print('600\n')
print(popularRec('600'))
print('700\n')
print(popularRec('700'))
#TODO: COVERT 0S' TO NA to get it work


