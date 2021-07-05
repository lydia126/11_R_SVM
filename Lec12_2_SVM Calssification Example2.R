
    # Data preparation
    
    set.seed(10111)
    x = matrix(rnorm(40), 20, 2)
    y = rep(c(-1, 1), c(10, 10))
    x[y == 1,] = x[y == 1,] + 1
    plot(x, col = y + 3, pch = 19)
    
    # Linear SVM Classifier
    library(e1071)
    dat = data.frame(x, y = as.factor(y))
    svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
    print(svmfit)
    
    plot(svmfit, dat)
    
    # make your own plot
    make.grid = function(x, n = 75) {
      grange = apply(x, 2, range)
      x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
      x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
      expand.grid(X1 = x1, X2 = x2)
    }
    xgrid = make.grid(x)
    xgrid[1:10,]
    ygrid = predict(svmfit, xgrid)
    plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
    points(x, col = y + 3, pch = 19)
    points(x[svmfit$index,], pch = 5, cex = 2)
    # Extract linear coefficients
    beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
    beta0 = svmfit$rho
    plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
    points(x, col = y + 3, pch = 19)
    points(x[svmfit$index,], pch = 5, cex = 2)
    abline(beta0 / beta[2], -beta[1] / beta[2])
    abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
    abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)
    
    # Non-Linear SVM Classifier
    load(file = "ESL.mixture.rda")
    attach(ESL.mixture)
    plot(x, col = y + 1)
    dat = data.frame(y = factor(y), x)
    #fit an SVM with radial kernel and cost as C=5
    fit = svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "radial", cost = 5)
    # plot
    xgrid = expand.grid(X1 = px1, X2 = px2)
    ygrid = predict(fit, xgrid)
    plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
    points(x, col = y + 1, pch = 19)
    func = predict(fit, xgrid, decision.values = TRUE)
    func = attributes(func)$decision
    xgrid = expand.grid(X1 = px1, X2 = px2)
    ygrid = predict(fit, xgrid)
    plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
    points(x, col = y + 1, pch = 19)
    contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
    contour(px1, px2, matrix(func, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)
    
    