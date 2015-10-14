# Boosting decision stump algorithm for regression probelem

## created decision stump using lstat and rm only two attributes 

#install required packages
#install.packages("MASS")


start <- Sys.time()	# timing: start

#load MASS package
#library(MASS)
#attach(Boston)  # load Boston data set into memory


Data<-read.csv("E:/Course_Material/Project-Ensemble_methods/datasets/Used_datasets/airfoil_self_noise.csv",sep="\t",header=F,col.names = c("Frequency","Angleofattack","Chordlength","Free-stream","Suction","class"))
#Data<-read.csv("E:/Course_Material/Project-Ensemble_methods/datasets/Used_datasets/slump_test.csv",sep=",",header=T)
#Data<-read.csv("E:/Course_Material/Project-Ensemble_methods/datasets/Used_datasets/parkinsons_updrs.csv",sep=",",header=T)

head(Data)
attach(Data)
set.seed(0)
# divide the data in training and test data set

training<-sample(nrow(Data),nrow(Data)/2)

train <- Data[training, ]

training.data<-Data[training, ]

test <-Data[-training,]

names(test)
# initialize learning rate and number of trees

lr= 0.01  # learning rate  
B=200 # number of boosted trees


## initialize matrix to store feature and prediction rule

prediction<-matrix(nrow=B,ncol=4)

dimnames(prediction) = list(c(),c("feature", "threshold","lessthan","greaterthan"))

# assign residual
r<-0

################ Calcualte the ranges for each column ##################

col<-subset(train, select = -class) 
names(col)

threshold<-0

rangess<-list()

for(s in 1:ncol(col))
{
  thres<-0
  #min<-range(col[s])[1]
  #max<-range(col[s])[2]
  
  for ( j in 1:nrow(col[s]))
  {
    
    ##### checking the condition not to exceeds the values
    
    if(j!=nrow(col[s]))
      thres[j]<-unifeature[j]+(unifeature[j+1]-unifeature[j])/2
  }
  
  #thres<-seq(from=min,to=max,by=(min+max)/2)
  rangess[[s]]<-thres
} 

## asign residuals

r<-train$class

######STEP 2 : Generate treess ##########################################

Beststump<-matrix(nrow=ncol(train)-1,ncol=3)


#Beststump<-matrix(nrow=ncol(train)-1,ncol=3)


dimnames(Beststump) = list(c(),c("feature","RSS", "threshold"))

### Main loop run for number of trees##############

for(b in 1:B)   # getnerate 1000 decision stumps
{
  #col<-subset(train, select = c(lstat,rm))
  for (n in 1:length(col))
  { 
    ######## create a matrix to store the RSS and its threshold value
    
    RM<-matrix(nrow=length(rangess[[n]]),ncol=2)
    
    unifeature<-col[,n]
    
    for(j in 1:length(rangess[[n]]))
    {
      
      lstat1<-0
      lstat2<-0
      yhats<-0
      yhatg<-0
      yhat1<-0
      yhat2<-0
      z<-0
      
      dc<-rangess[[n]][j]
      
      lstat1<-subset(r,unifeature<dc)  # split data less than decision stump
      yhats<-mean(lstat1)
      
      #cat("yhats",yhats,"\n")
      lstat2<-subset(r,unifeature>=dc) # split data greater or equal to decsion stump
      
      yhatg<-mean(lstat2)
      
      # cat("yhatg",yhatg,"\n")
      
      
      yhat1<-sum((lstat1-yhats)^2)
      
      yhat2<- sum((lstat2-yhatg)^2)
      
      z<-yhat1+yhat2
      
      RM[j,1]<-z
      RM[j,2]<-dc
      
    }  ##### all thresholds loop ends here
    
    minRSS<-RM[which.min(RM[,1]),] ### threshold having minimum RSS
    
    RSS<-minRSS[1]
    threshold<-minRSS[2]  ## best threshold for particular feature
    
    #cat(RSS,threshold,"\n")
    
    ## Store the n=feature,RSS and decision stump
    
    Beststump[n,1]<-names(col)[n]
    Beststump[n,2]<-RSS
    Beststump[n,3]<-threshold
    
  } ## number of feature   
  
  
  ############ Store the prediction rule i.e. (feature,threshold) for future use
  
  BestRSS<-Beststump[which.min(Beststump[,2]),]
  
  stump<-BestRSS[3]
  attri<-BestRSS[1]
  #train.RSS<-BestRSS[2]
  
  prediction[b,1]<- attri
  prediction[b,2]<- stump
  #prediction[b,5]<- train.RSS
  
  att<- train[,attri]
  
  #Calculate the mean of partition A i.e.observation less than stump value i.e.
  ## partition value
  
  ###### predict left region
  
  partA<-subset(r,att<stump)
  
  yhatsmall<-mean(partA)
  
  prediction[b,3]<-yhatsmall
  
  ###### predict right region
  
  partB<-subset(r,att>=stump)
  yhatgreater<-mean(partB)
  
  prediction[b,4]<-yhatgreater
  
  # Update the residuals
  
  for(i in 1:nrow(train))
  {
    if(att[i]<stump)
      r[i]=r[i]-lr*yhatsmall
    else
      r[i]=r[i]-lr*yhatgreater
    #if(att[i]>=stump)
  }
  
} ### end number of trees loop




##################### Test the boosted decision stump rule on test data#############

#Add extra column in test data to store the predicted values
pred.test<-0
#test<-cbind(test,testpredict=0)
#test<-test[-15]
names(test)

# loop over the test data to predict the values

for( t in 1:nrow(test))
{
  testfhat<-0
  
  # Apply all prediction rule on each test object
  
  for(p in 1:nrow(prediction))
  {
    # Take the first prediction rule from prediction matrix
    fe<-prediction[p,1]
    thr<-prediction[p,2]
    # Choose the value from test data for particular column 
    testobject<-test[t,colnames(test)==fe]
    ###### predict left region
    
    if(testobject<thr)
    {
      pred<-prediction[p,3]
    }
    
    if(testobject>=thr)
    {
      pred<-prediction[p,4]
    }
    
    #cat("prediction",pred,"\n")
    
    testfhat<-testfhat+as.numeric(pred)
    #cat(testfhat)
  }
  
  #cat(testfhat,"\n")
  pred.test[t]<-lr*testfhat
  
}




##################################################################################

################### Compute test MSE 

MSE<-sum((test$class-pred.test))

testMSE<-MSE/nrow(test)

cat("test MSE is = ",testMSE)

end <- Sys.time()

cat("Finished. Time elapsed:",difftime(end,start,units="secs"),"\n")


#####################################################################
tMSE[plt]<-testMSE

plt<-plt+1




############## Plot the error airfoil_self_noise.csv

tMSE<-c(28.99805)
Ntresss<-c(200,400,600,800,1000)


############## Plot the error airfoil_self_noise

tMSE<-c(32.84,44.67371,38.19104,37.79359,37.70212)
Ntresss<-c(200,400,600,800,1000)

###Boston
tMSE<-c(16.33321,20.76752,22.14981,22.40630,22.22296)
Ntresss<-c(200,400,600,800,1000)

plot(Ntresss,tMSE,xlab="rounds of Boosting",ylab="test MSE",main="error function",type="l",col="dark red")

