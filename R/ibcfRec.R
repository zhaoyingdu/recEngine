  # todo: use returned list to fetch actual book data and return
  # todo: implement method to revoke content in binarydata regularly, i.e. update model and matrix


require('RPostgres')
require('recommenderlab')
require('blob')
require('jsonlite')

connectDB <- function(){
  dbConnect(RPostgres::Postgres(),dbname = 'postgres', 
    host = 'pvpcraft.ca', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
    port = 4385, # or any other port specified by your DBA
    user = 'postgres',
    password = 'NeverSendThePassInSlack')
}


# store freshly computed RatingMatrix and Recommender model to database
# developer can decide how long to keep this data,
# as too long many provide outdated recommendations, 
# while too often may be an over kill, 
# because calculating these takes very long time.
storeIbcfModel = function(con){
  data = dbReadTable(con,'TRANSACTION')
  #select ones' part of the book from transaction table, select zeros' part of the books from the difference btw book table and isbn in transaction table
  usefulData <- data.frame(uid = data$UID, isbn = data$ISBN, quantity = data$QUANTITY)
  
  if(FALSE){
  str(usefulData)
  #
  res = dbSendQuery(con,'select "ISBN" from "BOOK" EXCEPT SELECT "ISBN" FROM "TRANSACTION" ORDER BY "ISBN"')
  untrackedISBN = dbFetch(res,-1)
  dbClearResult(res)
  res = dbSendQuery(con,'select "UID" from "USER" EXCEPT SELECT "UID" FROM "TRANSACTION" ORDER BY "UID"') # purpose is for constructing
  # rating matrix.. could do it for user Rec specific calls, but the logic is harder to track
  untrackedUID = dbFetch(res,-1)
  dbClearResult(res)
  print(untrackedISBN$ISBN)
  
  if(!(ISBNlen==0 && UIDlen==0)){
    if(ISBNlen==0){
      untrackedISBN = c(data$ISBN[1])
    }
    if(UIDlen==0){
      untrackedUser = c(data$UID[1])
    }

    if(ISBNlen<= UIDlen){
      untrackedISBN = rep(untrackedISBN$ISBN, length.out = UIDlen)
    }else{
      untrackedUID = rep(untrackedUID$UID, length.out = ISBNlen)
    }

    fillerData = data.frame(
      uid = untrackedUID,
      isbn = untrackedISBN,
      quantity = rep(0,length.out=length(untrackedUID))
    )
    usefulData = rbind(usefulData, fillerData)
  }
  
  if(FALSE){
  ISBNlen = length(untrackedISBN)
  if(ISBNlen!=0){
    fillerData = data.frame(
      uid = rep('dummy',length.out = ISBNlen),
      isbn = untrackedISBN$ISBN,
      quantity = rep(0,length.out=ISBNlen)
    )
    str(fillerData)
    usefulData = rbind(usefulData, fillerData)
  }
  #UIDlen = length(untrackedUID)
  }
  }
  
  ratingMatrix <- binarize(as(usefulData, 'realRatingMatrix'), minRating=1)  # save this value  
  recModel <- Recommender(ratingMatrix[1:500], method='IBCF')
  res = dbSendStatement(con, 'insert into "RECMODEL" values($1::bytea)')
  dbBind(res, params=list(as.blob(serialize(recModel,con=NULL))))
  dbClearResult(res)
}

# get ratingMatrix and recommender model from database
# this function should only be called when there is surely 
# such data in db
# return a vector: [[1]] matrix, [[2]]model
getModel=function(connection){ 
  res=dbSendQuery(connection, 'SELECT * from "RECMODEL"')
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

  data = dbReadTable(con, 'TRANSACTION')
  usefulData <- data.frame(user = data$UID, colnames = data$ISBN, rating = data$QUANTITY)
  if(userID %in% data$UID){
    if(FALSE){
      res = dbSendQuery(con,'select "ISBN" from "BOOK" EXCEPT SELECT "ISBN" FROM "TRANSACTION" ORDER BY "ISBN"')
      untrackedISBN = dbFetch(res,-1)
      dbClearResult(res)
      ISBNlen = length(untrackedISBN)
      if(ISBNlen!=0){
        fillerData = data.frame(
          uid = rep('dummy',length.out = ISBNlen),
          isbn = untrackedISBN$ISBN,
          quantity = rep(0,length.out=ISBNlen)
        )
        str(fillerData)
        usefulData = rbind(usefulData, fillerData)
      }
    }
    ratingMatrix <- binarize(as(usefulData, 'realRatingMatrix'),minRating=1)
    #print(ratingMatrix)
    userVector = ratingMatrix[userID,]
    print(userVector)
  }else{
    'null'
  }
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
  res = dbGetQuery(con, 'select count(*) from "RECMODEL"')
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

  #userHasHistory = dbGetQuery(con, "select count(*) from 'TRANSACTION' where 'UID' = $1")
  #if()
  data = dbReadTable(con, 'TRANSACTION')
  usefulData <- data.frame(user = data$UID, colnames = data$ISBN, rating = data$QUANTITY)
  if(userID %in% data$UID){
    ratingMatrix <- binarize(as(usefulData, 'realRatingMatrix'),minRating=1)
    #print(ratingMatrix)
    userVector = ratingMatrix[userID,]
    model = getModel(con)
    getRecList(userVector,model)
  }else{
    'null'
  }
}

popularRec = function(bookId){
  con = connectDB()
  if(!dataExist(con)){
    storeIbcfModel(con)
  }
  

  res = dbSendQuery(con, 'select distinct "ISBN" from "TRANSACTION" ORDER BY "ISBN"')
  colnames = dbFetch(res, n=-1)[,1]
  if(bookId %in% colnames){

  dbClearResult(res)
  testSubMatrix = matrix(rep(0,length(colnames)), nrow = 1, byrow = TRUE, dimnames = list('dummy', colnames))  
  testSubMatrix[1,bookId] = 1

  #print(testSubMatrix)
  testRatingMatrix = as(testSubMatrix, 'binaryRatingMatrix')


  model = getModel(con)
  getRecList(testRatingMatrix, model) 
  }else{
    'null'
  }
}
