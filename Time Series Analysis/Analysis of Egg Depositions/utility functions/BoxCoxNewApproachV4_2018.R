library(FSAdata) # For Example 1
library(TSA) # For Example 2
library(AID) # For the boxcoxnc() function
library(nortest)

BoxCoxSearch = function(y, lambda=seq(-3,3,0.01), 
                        m= c("sf", "sw","ad" ,"cvm", "pt", "lt", "jb"), plotit = T, verbose = T){
  N = length(m)
  BC.y = array(NA,N)
  BC.lam = array(NA,N)
  for (i in 1:N){
    if (m[i] == "sf"){
      wrt = "Shapiro-Francia Test"
    } else if (m[i] == "sw"){
      wrt = "Shapiro-Wilk  Test"
    } else if (m[i] == "ad"){
      wrt = "Anderson-Darling Test"
    } else if (m[i] == "cvm"){
      wrt = "Cramer-von Mises Test"
    } else if (m[i] == "pt"){
      wrt = "Pearson Chi-square Test"
    } else if (m[i] == "lt"){
      wrt = "Lilliefors Test"
    } else if (m[i] == "jb"){
      wrt = "Jarque-Bera Test"
    } 
    
    print(paste0("------------- ",wrt," -------------"))
    out = tryCatch({boxcoxnc(y, method = m[i], lam = lambda, lambda2 = NULL, plot = plotit, alpha = 0.05, verbose = verbose)
                   BC.lam[i] = as.numeric(out$lambda.hat)}, 
                   error = function(e) print("No results for this test!"))
    
  }
  return(list(lambda = BC.lam,p.value = BC.y))
}

# --- Example 1 ---
data("ChinookKR")
y1 = ChinookKR$spawners
a = BoxCoxSearch(y1, plotit=T, verbose = T)


# --- Example 2 ---

data(gold)
y =  gold
lam=seq(-4,4,0.01) # Restrict the lambda values
#m = c("sf", "sw","ad" ,"cvm", "pt", "sf", "lt", "jb", "ac")
m = c("sw", "pt", "sf", "jb") # Choose the tests to apply
a = BoxCoxSearch(y, lam=lam, m= m)
