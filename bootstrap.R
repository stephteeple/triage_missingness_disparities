
func_02_bootstrap <- function(i, train, test, pointest) {
    
    train <- train_orig
    test <- test_orig
    
    # If generating point estimates only, do not bootstrap
    if (pointest == TRUE) {
        
        print(paste0("Calculating point estimate: no bootstrapping needed"))
        print(paste0("Unique visits in train = ", length(unique(train$visitid))))
        print(paste0("Unique visits in test = ", length(unique(test$visitid))))
        results = list(train = train, test = test)
        
    }
    
    else if (pointest == FALSE) {

    print(paste0("Bootstrapping training data for iteration ", i))
    
    # Checks 
    print(paste0("Before boostrapping: unique visits in train = ", length(unique(train$visitid))))
    print(paste0("Unique visits in test = ", length(unique(test$visitid))))
    
    # Bootstrap training data
    set.seed(i) # so you actually get a different resampled dataset each time!
    trows <- sample(nrow(train), replace = TRUE) 
    train <- train[trows,]
    
    # Checks 
    print(paste0("After boostrapping: unique visits in train = ", length(unique(train$visitid))))
    print(paste0("Unique visits in test = ", length(unique(test$visitid))))

    results = list(train = train, test = test)
    return(results)
    
    }

}
