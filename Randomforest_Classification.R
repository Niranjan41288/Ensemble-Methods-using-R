#################Implementation of Random Forest#####################

# get the data from local directory

#Dataset<-read.csv("E:/Course_Material/Project-Ensemble_methods/datasets/diabetic_data.csv")
Dataset<-read.csv("E:/Course_Material/Project-Ensemble_methods/datasets/Used_datasets/tic-tac-toe.csv",header=F,col.names = c("a1","a2","a3","a4","a5","a6","a7","a8","a9","label"))
#Dataset<-read.csv("E:/Course_Material/Project-Ensemble_methods/datasets/Other_Datasets/weather1.csv")
#Dataset<-read.csv("E:/Course_Material/Project-Ensemble_methods/datasets/wilt/training.csv")
#Dataset<-read.csv("E:/Course_Material/Project-Ensemble_methods/datasets/Used_datasets/soybean-small.csv",header=F,col.names = c("a1","a2","a3","a4","a5","a6","a7","a8","a9","label"))

#Dataset<-read.csv("E:/Course_Material/Project-Ensemble_methods/datasets/Used_datasets/soybean-small.csv",header=F,col.names = c("b","c","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17","a18","a19","a20","a22","a23","a24","a25","a26","a27","a28","a29","a30","a31","a32","a33","a34","label"),sep=",")

# Initialize tree
trees=400

attach(Dataset) #Load data set in R memory
head(Dataset$label)
# divide the data in training and test data set

training<-sample(nrow(Dataset),nrow(Dataset)/2)

train <- Dataset[training, ]

test <-Dataset[-training,]

data<-train #assign train data to variable data

response<-'label' #assign response varaible


#################### Random Forest algorithm  ####################


##########Define all function used in the random forest geneartion########


############### Function to check purity for each split subset##############

PurityChecK<-function(Data)
{
  for( r in 1:length(R))
  {
    pos<-subset(Data,Data[,response]==R[r])
    
    if(nrow(pos)==0)
      break
  }
  
  if(nrow(pos)==0)
    Purity<-'TRUE'
  
  if(nrow(pos)!=0)
    Purity<-'FALSE'
  
  
  return(Purity)
}



## Define the functions to split depending upon the class of node


################# split function to split factor values################

SplitFactor<-function(Data,node)
{
  
  # E1<-split(Data,Data[,node])
  mylist<-list()
  
  L<-levels(Data[,node])
  
  
  for(i in 1:length(L))
  {
    
    mylist[i]<-list(subset(Data,Data[,node]==L[i]))
    
  }
  
  return(mylist)  # return the output to calling function
  
}


#################function to split logical values################

SplitLogical<-function(Data,node)
{
  #L<-unique(weather[,child2])
  mylistlogical<-list()
  L<-unique(Data[,node])
  
  for(i in 1:length(L))
    mylistlogical[i]<-list(subset(Data,Data[,node]==L[i]))
  
  return(mylistlogical) # return to the calling fucntion
}


#################function to split Integer/numerical values################   

SplitInteger<-function(Data,node,split)
{
  S<-0
  
  S[1]<-list(subset(Data,Data[,node]<split))
  
  # Greater than S2
  
  S[2]<-list(subset(Data,Data[,node]>=split))
  
  
  return(S) # return to the calling function
  
}


### Define the function for selection of the best attribute at each split

AttributeSelection<-function(weather)
{
  attach(weather)
  
  ### Store number of observation and number of attributes 
  
  N=nrow(weather) # NUmber of observation in given data set
  M=ncol(weather) # Number of attributes in given data set
  
  
  ##Set number of attributes to select randomly it should be m<<M
  ## It should be constat at each split
  
  m=3 
  
  
  # Generalize response variable
  
  response<-'label'
  
  # Exclude output label from random feature selection
  
  we<-weather
  
  we[,response]<-NULL    
  
  
  cat("Select",m, "attribute from the given data set \n")
  
  features<-sample(we,m)
  
  #names(features)
  
  #class(features)
  
  cat("Choose best attribute among selected attribute to split\n")
  
  
  # Use entropy for factor or logical column
  # and threshold for numerical column
  
  cat("calculate before entropy\n")
  
  R<-levels(weather[,response])
  
  E=0
  
  for( r in 1:length(R))
  {
    pos<-subset(weather,weather[,response]==R[r])
    posincome<-nrow(pos) 
    
    if(posincome!=0)
      E<- E-((posincome/N)*log2(posincome/N))
  }
  
  
  # Calculate information gain for each attribute
  
  # Intialize matrix to store the attribute and its entropy
  
  EN=matrix(nrow=length(features),ncol=2)
  
  # Intialize matrix to store the attribute and its information gain
  
  InfoGain=matrix(nrow=length(features),ncol=3)
  
  
  # calculate entropy for each attribute
  
  
  for (f in 1:length(features)) # loop over each feature
  { 
    
    attri<-names(features)[f]  # Take the first feature
    
    # Check the class of feature
    
    #Find the levels of feature
    
    if(class(weather[,attri])=="logical")
    {
      cat(attri, "is a logical attribute","\n")
      
      L<-unique(weather[,attri])
    }
    
    else
    {
      #cat(" not logical attribute",attri,"\n")
      L<-levels(weather[,attri])
    }
    
    # L<-levels(weather$Outlook)  # take the levels of particular feature
    
    
    if(class(weather[,attri])=="factor" | class(weather[,attri])=="logical")
    {
      
      cat(attri,"is a facctor \n")
      
      EA=0   # Intialize expected entropy  
      
      for (l in 1:length(L))
      {
        # Loop over each levels 
        # Calculate entropy for each distinct value of attribute
        
        T=0
        E1=0
        
        # Find the total for each level of attribute and his response
        for(r in 1:length(R))
        {
          p<-subset(weather,weather[,attri]==L[l] & weather[,response]==R[r])
          T<-T+nrow(p)
          
        }
        
        # Calculate the entropy for each level of attribute
        for(j in 1:length(R))
        {
          p1<-subset(weather,weather[,attri] == L[l] & weather[,response]==R[j])
          
          if(nrow(p1)!=0)
          {
            E1<-E1-((nrow(p1)/T)*log2(nrow(p1)/T)) 
          }
        }
        
        
        #Expected New Entropy 
        
        EA<-EA+((T/N)*E1)  ## EA is a entropy of one attribute
        
        
      }# Loop of level for one attribute
      
      
      EN[f,1]<-names(features)[f] 
      EN[f,2]<-EA
      
      # Calculate the information gain and store in Information Gain matrix
      
      InfoFactor<-E-EA
      
      ## Store the information Gain of factor attribite in Information Gain matrix
      InfoGain[f,1]<-names(features)[f] 
      InfoGain[f,2]<-InfoFactor
      
      
    }  # if loop ends here   
    
    else
    {
      cat(attri,"is a integer or numerical \n")
      
      # take different threshold value and choose the threshold 
      # which having less MSE
      
      #sort data according to attribute X 
      
      attribute<-weather[,attri]  
      
      feature<-na.omit(attribute) # handling missing values
      
      #srtfeature<-sort(feature,decreasing=FALSE,na.last = NA)
      
      unifeature<-unique(feature)
      
      ################## for that particular feature select threshold value having 
      ###################small training RSS##############################################
      
      ######## create a matrix to store the RSS and its threshold value
      
      InfoGainCont<-matrix(nrow=length(unifeature),ncol=3)
      
      for ( k in 1:length(unifeature))
      {
        ##### checking the condition not to exceeds the values
        
        if(k!=length(unifeature))
        {
          ## taking range of values from feature column for selection of 
          ### best threshold value using below formula
          dc<-unifeature[k]+(unifeature[k+1]-unifeature[k])/2
        }
        
        lstat1<-subset(weather,unifeature<dc)  # split data less than decision stump
        
        EC1=0
        
        # Calcualte the entropy for less than threshold value
        
        for(j in 1:nrow(lstat1))
        {
          p1<-subset(weather,weather[,response]==R[j])
          
          if(nrow(p1)!=0)
          {
            EC1<-EC1-((nrow(p1)/nrow(lstat1))*log2(nrow(p1)/nrow(lstat1))) 
          }
        }
        
        
        # lstat greater than
        
        EC2=0
        
        lstat2<-subset(weather,unifeature>=dc) # split data greater or equal to decsion stump
        
        
        # Calcualte the entropy for greater than threshold value
        
        for(j in 1:nrow(lstat2))
        {
          p1<-subset(weather,weather[,response]==R[j])
          
          if(nrow(p1)!=0)
          {
            EC2<-EC2-((nrow(p1)/nrow(lstat2))*log2(nrow(p1)/nrow(lstat2))) 
          }
        }
        
        
        #Expected New Entropy 
        
        ECA<-((nrow(lstat1)/N)*EC1) + ((nrow(lstat2)/N)*EC2) 
        
        
        # Calculate the information gain
        
        IFC<-E-ECA
        
        # Store the feature name,information gain and its threshold value
        
        InfoGainCont[k,1]<-names(features)[f] 
        InfoGainCont[k,2]<-dc
        InfoGainCont[k,3]<-IFC
        
      } # loop for Calculation of information gain for each possible values 
      #of column ends here
      
      # Store the feature and its information gain in InfoGain matrix
      
      MAXIFCNT<-InfoGainCont[which.max(InfoGainCont[,3]),]
      
      BESTFEATURECNT<-MAXIFCNT[1]  
      InfoCont<-MAXIFCNT[3]
      thresh<-MAXIFCNT[2]
      
      InfoGain[f,1]<-BESTFEATURECNT
      InfoGain[f,2]<-InfoCont
      InfoGain[f,3]<-thresh
      
    } # else ends here 
    
    
    
    
  } # For loop over feature ends here
  
  cat("returining inforamtion gain matrix to function \n")
  
  return(InfoGain)
  
  
} 


########## all function definations ends here#####################

### Initialize list to store the tree

RandomForest<-list()

#Intialize number of tress

#trees=100

############# Generation of random decision trees #################

for(t in 1:trees)
{
  
  cat("build",t,"tree")
  
  #intialize matrix to store the tree model 
  Treemodel<-matrix(nrow=1000,ncol=9)
  
  dimnames(Treemodel) = list(c(),c("Branch no","Mainnode","childnode","data type","split value","split", "no.of samples","Yhat","terminal node?"))
  
  id<-0
  
  # Take a ramdom sample of data from data set
  dataset<-list()
  Roots<-c()
  splits<-0
  N<-nrow(data)
  
  d<-sample(nrow(data),size=N,replace=TRUE)
  weather<-data[d,] 
  
  # Building tree using this data set
  
  # Call function for attribute selection
  
  y<-AttributeSelection(weather)  # get a root node
  
  # Now we have information gain for all features
  
  #Take the feature having maximum information gain
  
  MAX<-y[which.max(y[,2]),]
  
  root<-MAX[1]       
  threshold<-MAX[3]
  
  
  # root node to select
  Roots[1]<-root
  
  if((class(weather[,root])=='integer') | (class(weather[,root])=='numeric'))
    splits[1]<-threshold
  
  if((class(weather[,root])=='integer') | (class(weather[,root])=='numeric'))
    cat("Split the data on",root,"node using threshold value of",threshold,"\n")
  
  if((class(weather[,root])=='logical') |(class(weather[,root])=='factor'))
    cat("Split the data on",root,"node\n")
  
  
  ### Now split the data set on root node 
  
  # Check the class of root node and then divide it accordignly
  
  ###################### class factor########################
  
  while(length(Roots)!=0)
  {
    root<-Roots[1]
    
    
    if(length(dataset)!=0)
      weather<-as.data.frame(dataset[1])
    
    if((class(weather[,root])=='integer') | (class(weather[,root])=='numeric'))
      threshold<-splits[1]
    
    
    if(class(weather[,root])=='factor')
    {   
      cat("Class of ",root,"is factor so call function SplitFactor\n")
      
      # call function
      
      Data<-weather
      node<-root
      
      if(length(dataset)!=0)
        dataset[[1]]<-NULL
      
      Roots=Roots[-1]  # Remove the node from root vector
      
      S1<-SplitFactor(Data,node)
      
      
      for(i in 1:length(S1))
      {
        weather<-do.call(rbind.data.frame, S1[i])
        
        # check for purity
        
        Data<-weather
        Pure<-PurityChecK(Data)  # Calling purity function
        
        ### if it is impure set then split again
        
        if(Pure=='FALSE')
        {
          
          weather<-S1[[i]]
          
          y1<-AttributeSelection(weather)# get a next node to split
          
          MAX<-y1[which.max(y1[,2]),]
          
          child1<-MAX[1]    
          
          if((class(weather[,root])=='integer') | (class(weather[,root])=='numeric'))
          {
            split<-MAX[3]
            splits[length(splits)+1]<-split
          }
          
          Roots[length(Roots)+1]<-child1
          dataset[length(dataset)+1]<-list(weather)
          
          Treemodel[id+1,1]<-id+1
          Treemodel[id+1,2]<-root
          Treemodel[id+1,4]<-"factor"
          Treemodel[id+1,7]<-nrow(S1[[i]])
          
          le<-names(which.max(table(S1[[i]][,root])))
          
          Treemodel[id+1,3]<-le
          Treemodel[id+1,9]<-"N"
          id<-id+1
        }
        
        else
        {
          
          Treemodel[id+1,1]<-id+1
          Treemodel[id+1,2]<-root
          Treemodel[id+1,4]<-"factor"
          Treemodel[id+1,7]<-nrow(S1[[i]])
          
          le<-names(which.max(table(S1[[i]][,root])))
          
          output<-names(which.max(table(S1[[i]][,response])))
          
          Treemodel[id+1,3]<-le
          Treemodel[id+1,8]<-output
          Treemodel[id+1,9]<-"Y"
          id<-id+1
        }
        
      }
      
    } # if ends
    
    
    
    ################### Class Logical########################
    
    if(class(weather[,root])=='logical')
    {
      
      cat("Class of ",root,"is logical so call function SplitLogical\n")
      
      
      Data<-weather
      
      node<-root
      
      if(length(dataset)!=0)
        dataset[[1]]<-NULL
      
      Roots=Roots[-1]   
      
      S1<-SplitLogical(Data,node)
      
      for(i in 1:length(S1))
      {
        # Check purity
        
        Data<-S1[[i]]
        
        Pure<-PurityChecK(Data)
        
        if(Pure=='FALSE')
        {
          
          weather<-S1[[i]]
          
          #weather[,root]<-NULL
          
          y1<-AttributeSelection(weather)
          
          #Roots=Roots[which(Roots!=root)]
          MAX<-y1[which.max(y1[,2]),]
          
          child1<-MAX[1]    # left hand node 
          
          if(class(weather[,root])=='integer')
          {
            split1<-MAX[3]
            splits[length(splits)+1]<-split1
          }
          
          
          splits[length(splits)+1]<-split
          Roots[length(Roots)+1]<-child1
          dataset[length(dataset)+1]<-list(weather)
          
          Treemodel[id+1,1]<-id+1
          Treemodel[id+1,2]<-root
          Treemodel[id+1,4]<-"logical"
          Treemodel[id+1,7]<-nrow(S1[[i]])
          
          le<-names(which.max(table(S1[[i]][,root])))
          
          Treemodel[id+1,3]<-le
          Treemodel[id+1,9]<-"N"
          
          id<-id+1
          
        }
        
        else
        {
          Treemodel[id+1,1]<-id+1
          Treemodel[id+1,2]<-root
          Treemodel[id+1,4]<-"logical"
          Treemodel[id+1,7]<-nrow(S1[[i]])
          
          le<-names(which.max(table(S1[[i]][,root])))
          
          output<-names(which.max(table(S1[[i]][,response])))
          
          Treemodel[id+1,3]<-le
          Treemodel[id+1,8]<-output
          Treemodel[id+1,9]<-"Y"
          
          id<-id+1
          
        }
        
      }
    }#logical if ends here
    
    ###########################integer########################
    
    if((class(weather[,root])=='integer') | (class(weather[,root])=='numeric'))
    {
      cat("Class of ",root,"is numerical or integer \n")
      
      # less than split value
      
      # Call SplitInteger function
      
      Data<-weather
      node<-root
      split<-threshold
      
      splits<-splits[-1]
      
      if(length(dataset)!=0)
        dataset[[1]]<-NULL
      
      Roots=Roots[-1]
      
      IntegerData<-SplitInteger(Data,node,split)
      
      
      # Check purity
      
      for(i in 1:2)
      {
        
        Data<-IntegerData[[i]]
        
        Pure<-PurityChecK(Data)
        
        if(Pure=='FALSE')
        {
          
          weather<-IntegerData[[i]]
          
          y1<-AttributeSelection(weather)
          
          MAX<-y1[which.max(y1[,2]),]
          
          child1<-MAX[1]    # left hand node 
          
          if((class(weather[,root])=='integer') | (class(weather[,root])=='numeric'))
          {
            split1<-MAX[3]
            splits[length(splits)+1]<-split1
          }
          
          Roots[length(Roots)+1]<-child1
          dataset[length(dataset)+1]<-list(weather)
          
        }
        else
        {
          ## assign max 
          Treemodel[id+1,1]<-id+1
          Treemodel[id+1,2]<-root
          Treemodel[id+1,4]<-"continuous"
          Treemodel[id+1,5]<-split
          
          if(i==1)
            Treemodel[id+1,6]<-"<"
          if(i==2)
            Treemodel[id+1,6]<-">="  
          
          Treemodel[id+1,7]<-nrow(IntegerData[[i]])
          Treemodel[id+1,8]<-names(which.max(table(IntegerData[[i]][,response])))
          Treemodel[id+1,9]<-"Y"
          id<-id+1
        }
      }
      
      
    } ## integer if loop ends here
    
    
    
    
  }  ## while loop ends   
  
  # Remove the rows having NA values across each column
  
  Treemodel<-Treemodel[apply(Treemodel,1,function(x)any(!is.na(x))),]      
  
  RandomForest[[t]]<-Treemodel
  
} # loop for number of trees   # Divide the data set in left and right child nodes say S1 and S2


#### Testing Random forest algorithm on test data


output1<-0
for(i in 1:nrow(test))
{
  for( r in 1:length(RandomForest))
  {
    tree1<-RandomForest[[r]]
    
    #i<-0
    
    for( t in 1:nrow(tree1))
    {
      #cat(t,"\n")
      class<-tree1[t,4]
      
      if(class=="factor" | class=="logical")
      { 
        #cat(class,"\n")
        attr<-tree1[t,2]
        child<-tree1[t,3]
        testobject<-test[i,colnames(test)==attr]
        
        if(tree1[t,9]=="Y")
        { 
          output1[i]<-tree1[t,8]
          # i<-i+1
          #next
        }
        
        if(tree1[t,9]=="N")
          next
        
      }## Factor loop ends
      
      #################### if class integer########################
      
      if(class=="continuous")
      { 
        #print("integerr")
        
        terminalnode<-tree1[t,9]
        attr<-tree1[t,2]
        thresh<-tree1[t,5]
        splitt<-tree1[t,6]
        
        testobject<-test[i,colnames(test)==attr]
        
        ######################## Less than #####################
        
        if(tree1[t,7]!=0)
        {
          if(splitt=="<") 
          {
            if(testobject<thresh)
            {
              if(terminalnode=="Y") 
              {
                #print("terminal node")
                output1[i]<-tree1[t,8]
                #i<-i+1
                #break
              }
              
              if(terminalnode=="N") 
              {  
                # print("not a terminal node")
                next
              }
            }
          }
          
          ############################################
          if(splitt==">=") 
          {
            if(testobject>=thresh)
            {
              if(terminalnode=="Y") 
              {
                # print("terminal node")
                output1[i]<-tree1[t,8]
                # i<-i+1
                #break
              }
              
              if(terminalnode=="N") 
              {  
                # print("not a terminal node")
                next
              }
            }
          }    
          
        } # no. of samples
        
      }## if Integer class ends
      
    }### tree loop ends
    
    
  }## Random forest ends
  
}## Test test ends


# Misclassification error rate is 
cat("misclassificaion error rate is",mean(test$label!=output1)*100)



##Plot the misclassification error rate as a function of number of trees
errorrate<-c(36.53445,35.90814,35.49061,35.69937,34.89432)
no.treess<-c(100,200,300,400,500)

plot(no.treess,errorrate,type="l",col="2",xlab="number of trees",ylab="misclassification error rate",main="Random Forest")
             
      
