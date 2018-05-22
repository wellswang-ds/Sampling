# Read data ---------------------------------------------------------------

library(data.table)
product <- fread('https://raw.githubusercontent.com/wellsytheman/Sampling/master/Sampling_with_4_different_methods/products.csv')
p_sys <- product


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

