
CVgeneric <-
  function (classifier, trainingfeatures, traininglabels,  lossfunc, K, seed=123) {
    
    #`trainingfeatures` is a data frame with one feature values in each column
    features<-names(trainingfeatures) #a vector of names of training features
    model<- as.formula(paste("label~", paste(features, collapse='+')))
    
    data<-cbind(trainingfeatures, label=traininglabels)
    n <- nrow(trainingfeatures)
    set.seed(seed)
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:K and sample n of them  
    
    # K fold cross-validated error
    CV=NULL
    
    for (i in 1:K) { 
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      fit=classifier(model,  data=data[train.index,])
      #actual test set y
      actual.labels <- data[test.index, "label"]
      #predicted test set y
      predicted.labels=predict(fit, data[test.index,])$class
      
      #actual - predicted on test data
      error= lossfunc(actual.labels,predicted.labels)
      accuracy = 1-error
      #error rates 
      CV=c(CV,accuracy)
    }
    #Output
    return(CV)
  }
