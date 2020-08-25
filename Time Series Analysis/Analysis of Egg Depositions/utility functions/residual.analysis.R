residual.analysis <- function(model, std = TRUE,start = 2, class = c("ARIMA","GARCH","ARMA-GARCH")[1]){
  # If you have an output from arima() function use class = "ARIMA"
  # If you have an output from garch() function use class = "GARCH". 
  # Please note that you should use tseries package to be able to run this function for GARCH models.
  # If you have an output from ugarchfit() function use class = "ARMA-GARCH"
  library(TSA)
  library(FitAR)
  if (class == "ARIMA"){
    if (std == TRUE){
      res.model = rstandard(model)
    }else{
      res.model = residuals(model)
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "ARMA-GARCH"){
      res.model = model@fit$residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Figure 14. Time series plot of standardised residuals")
  abline(h=0)
  hist(res.model,main="Figure 15. Histogram of standardised residuals")
  acf(res.model,main="Figure 16. ACF of standardised residuals")
  pacf(res.model,main="Figure 17. PACF of standardised residuals")
  qqnorm(res.model,main="Figure 18. QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  print(shapiro.test(res.model))
  k=0
  LBQPlot(res.model, lag.max = length(model$residuals) - 1, StartLag = k + 1, k = 0, SquaredQ = FALSE)
  title("Figure 19.                                          .")
}