
# Read data ---------------------------------------------------------------

library(data.table)
product <- fread('https://raw.githubusercontent.com/wellsytheman/Sampling/master/Sampling_with_4_different_methods/products.csv')
p_srs <- product

# Function ----------------------------------------------------------------

fun_srs <-function(n,data){
  
  # SRS ---------------------------------------------------------------------
  
  set.seed(979)
  N <- as.numeric(nrow(data) )# N in SRS = 母體中的元素總數（以資料列數計算）
  SRS <- sample(N,size=n,replace = FALSE) 
  
  
  # Get data ----------------------------------------------------------------
  
  data.frame(data[SRS,])# match the row number 
  
}

# Run function ------------------------------------------------------------

s_srs<-fun_srs(n=1068,data=p_srs) #sample s

