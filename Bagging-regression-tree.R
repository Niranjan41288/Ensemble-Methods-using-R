### implement bagging on decision tree

#install required packages 
install.packages("tree")
install.packages("MASS") # Boston data set

#load required packages in R memory
library(tree)
library(MASS)


#attach required data set
# Change the data set accordingly

#Data<-Boston
#Data<-read.csv("E:/Course_Material/Project-Ensemble_methods/datasets/Used_datasets/airfoil_self_noise.csv",sep="\t",header=F,col.names = c("Frequency","Angleofattack","Chordlength","Free-stream","Suction","response"))
#Data<-read.csv("E:/Course_Material/Project-Ensemble_methods/datasets/Used_datasets/slump_test.csv",sep=",",header=T)
Data<-read.csv("E:/Course_Material/Project-Ensemble_methods/datasets/Used_datasets/parkinsons_updrs.csv",sep=",",header=T)


# Construct bag-free model for comparison

start <- Sys.time()	# start timer

head(Data)#Check the dataset
names(Data)# Check the column names
attach(Data) # attach dataset

############## Set the response variable################

response<-'total_UPDRS'

# set seed
set.seed(1)

# divide the data in training and test data set

training<-sample(nrow(Data),nrow(Data)/2)

#test<- -training

train <- Data[training, ]

test <-Data[-training,]

#attach the training and test data

attach(train)
attach(test)


## apply the tree function i.e. build a tree model using training data

bagfree_tree_model<-tree(train[,response]~.,data=train)

# look at summary
summary(bagfree_tree_model)

#plot the tree
plot(bagfree_tree_model)
text(bagfree_tree_model,pretty=0)

## check how the model is doing using the testing dataset

bagfree_tree_pred<-predict(bagfree_tree_model,test)

#plot(bagfree_tree_pred,test[,response])

# Calcualte the Mean squared error

MSE<-mean((bagfree_tree_pred-test[,response])^2)  #25.04559

cat("The mean sqared error of bag_free_model is ",MSE)



#############bag model for regression tress###################

B=1000 # number of trees

# Generate k learners

length_divisor<-2

rm(predictions) # remove predictions matrix if exist any

predictions<-cbind(1:nrow(test)) # create predictions matrix to store bagged trees columnwise

#apply bagging

for (b in 1:B)
{
  # Take a bootsrapes sample from the training data
  
  sampledata<-sample(nrow(train),size=floor((nrow(train)/length_divisor)))
  
  
  ### Fit a decision tree on bootstraped sample
  
  tree_model<-tree(train[sampledata,][,response]~.,train[sampledata,]) # fhat
  
  ## check how the model is doing using the testing dataset
  # Make the prediction
  tree_pred<-data.frame(predict(object=tree_model,test)) # fhat(x)
  
  #Store the predictions for each bootstrapped sample
  predictions<-cbind(predictions,tree_pred)
  
}


predictions <- predictions[,-1] # Remove extra column added 


#mean sqaured error for bagged model
yhat<-rowMeans(predictions) ## take mean of rowsi.e.avrage of fhat(x)

TestMSE<-mean((yhat-test[,response])^2) 

cat("test MSE of bagged tree model",TestMSE)

end <- Sys.time()	# timing: end
cat("Finished. Time elapsed:",difftime(end,start,units="secs"),"\n")



### Plot of MSE as a function of number of bagging rounds on Boston datset######
Nbaggedtresss<-c(200,400,600,800,1000)
baggesTESTMSE<-c(16.99202,16.98702,16.89970,16.95077,17.00949)
plot(Nbaggedtresss,baggesTESTMSE,xlab="number of bagged trees",ylab="test MSE",main="error function",type="b",col="dark red")
