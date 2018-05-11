# Read data ---------------------------------------------------------------

setwd("~/Desktop/抽樣調查")
p_sys <- read.csv("products.csv",header = T) #population 


# Function ----------------------------------------------------------------

fun_sys<-function(n,data){

  # System Sampling ---------------------------------------------------------

  N = as.numeric(nrow(data))
  k = as.integer(N/n)  
  r = sample(k,1)    
  s = 0 
  #s= the row number which would be sampled
  for(i in 1:n){
    s[i]=r+(i-1)*k
  }
  
  data.frame(data[s,]) #match the row number
}

# Run Function ------------------------------------------------------------

s_sys<- fun_sys(n=2000,data=p_sys) #sample 

