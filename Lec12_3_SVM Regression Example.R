
    # Required libraries
    library(e1071)
    library(caret)
    
    # Load data and partition
    boston = MASS::Boston
    set.seed(123)
    indexes = createDataPartition(boston$medv, p = .8, list = F)
    train = boston[indexes, ]
    test = boston[-indexes, ]
    
    # SVM regression
    model_reg = svm(medv~., data=train)
    print(model_reg)
    pred = predict(model_reg, test)
    x=1:length(test$medv)
    plot(x, test$medv, pch=18, col="red")
    lines(x, pred, lwd="1", col="blue")
    
    # accuracy check
    mae = MAE(test$medv, pred)
    rmse = RMSE(test$medv, pred)
    r2 = R2(test$medv, pred, form = "traditional")
    
    cat(" MAE:", mae, "\n", "RMSE:", rmse, "\n", "R-squared:", r2)
    
    install.packages("forecast")
    library(forecast)
    lm1 <- lm(medv~., data=train)
    accuracy(lm1)

    # paldang Chl-a 
    paldang <- read.csv("Paldang.csv")
    indexes = createDataPartition(paldang$Chla, p = .67, list = F)
    train = paldang[indexes, ]
    test = paldang[-indexes, ]
    
    # SVM regression
    model_reg = svm(Chla~., data=train)
    print(model_reg)
    pred = predict(model_reg, test)
    x=1:length(test$Chla)
    plot(x, test$Chla, pch=18, col="red")
    lines(x, pred, lwd="1", col="blue")
    
    # accuracy check
    mae = MAE(test$Chla, pred)
    rmse = RMSE(test$Chla, pred)
    r2 = R2(test$Chla, pred, form = "traditional")
    
    cat(" MAE:", mae, "\n", "RMSE:", rmse, "\n", "R-squared:", r2)
    