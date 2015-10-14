################# Boosting using Adaboost algorithm for 
#################classification problems


Data<-read.csv("path/tic-tac-toe.csv",header=F,col.names = c("a1","a2","a3","a4","a5","a6","a7","a8","a9","class"))

head(Data)
names(Data)

#Convert the chararcter class into class of +1 and -1

Data$class<-2*as.integer(Data$class=='positive')-1

### Divide the data set into testing and training 

# divide the data in training and test data set

training<-sample(nrow(Data),nrow(Data)/2)

train <- Data[training, ]

test <-Data[-training,]

head(train)
head(test)

attach(train)

attach(test)


# Before applying Adaboost on classifier let's implement logistic classifier 
# and check what is the performance

# Predict model using different values of thresholds

threshold<-seq(from=0.1,to=0.9,by=0.1)# c(0.5, 0.6,0.7,0.8,0.9
specificity<-0
sensitivity<-0
ErrorRate1<-0

TotalPositive<-nrow(subset(test,test$class==1))
TotalNegative<-nrow(subset(test,test$class==-1))

for(t in 1:length(threshold))
{

  glm.fit<-glm(train$class~a1+a2+a3+a4+a5+a6+a7+a8+a9,family=gaussian,(data=train))

# Predict the rule

glm.probs<-predict(glm.fit,type="response")

glm.pred=rep(1,nrow(test))
glm.pred[glm.probs>threshold[t]]=-1

confusion<-table(glm.pred,test$class)

ErrorRate1[t]<-mean(sign(glm.pred)!=test$class)*100 # predicted percentage

sensitivity[t]<-confusion[1,1]/TotalPositive # Insert true positive rate
specificity[t]<-confusion[1,1]/TotalNegative  # Insert flase positive rate

}


#Plot the ROC curve to see the model performance 


plot(specificity,sensitivity,type="l",col="3",main="ROC Curve")


# Apply AdaBoost using Logistic classifier

## weights for each observation


Adaspecificity<-0
Adasensitivity<-0
AdaErrorRate<-0

#m<-nrow(AdultData)

m<-nrow(train)

attach(train)

rm(weight)

weight <- rep(1,m)/m # Initailize weights

cat("Initial weights",weight,"\n")


M=500 # number of classifier

#Initialize the matrix of alphas and error rates

alpha<-rep(0,M)
E<-rep(0,M)
finalclassifier<-0
error<-0


for(thr in 1:length(threshold))
{
 for(t in 1:M)
 {
     e<-0
      # Train weak learner using distgribution DT
      # fit logistic classifier to the training data uisng weights
      
  glm.fit<-glm(class~a1+a2+a3+a4+a5+a6+a7+a8+a9,family=gaussian,weights=(weight*m),(data=train))
      
     # Predict the rule
      
      glm.probs<-predict(glm.fit,type="response")

      glm.pred=rep(1,nrow(train))
      glm.pred[glm.probs>threshold[thr]]=-1
      
    # Calculate the minimized weighted error for misclassified observations
      
       for(j in 1:nrow(train))
        {
         
          if(train$class[j]!=glm.pred[j])
           {
            e=e+weight[j]
           }
        }
  
  ## divide the minimized error by sum of weights to get normalized error
      error[t]<-e/sum(weights)

        E[t]<-error[t]
   
#Calcualte the alpha only if error is less than 0.5
#if(E[t]<=0.49)
 # {
  #  print("error rate is greater than 0.5 skip the iteration")
   # next
  #}
         
#else
#{
    if(E[t]!=0)
    {
    alpha[t]=0.5*log2((1-E[t])/E[t])
    print("alpha")
    }

### update the weights 
  
   if(alpha[t]!=0)
   {
     for(i in 1:nrow(train))
      {
        if(train$class[i]!=glm.pred[i])
          {
            weight[i]=weight[i]*exp(alpha[t]) 
          }
        else
          {
            weight[i]=weight[i]*exp(-alpha[t]) 
          }
       }
  
    }
#}## main else ends here
     
        

  finalclassifier<-finalclassifier+(alpha[t]* predict(glm.fit,test))


 } ### end for loop for number of classifiers

  # Calculate the error rate
  
  AdaErrorRate[thr]<-mean(sign(finalclassifier)!=test$class)*100


}## Threshold loop ends here


cat("test error rate is", Ada.ErrorRate*100)
     
AdaErrorRate
ErrorRate1




