# Read data ---------------------------------------------------------------

setwd("~/Desktop/抽樣調查")
p_srs <- read.csv("products.csv",header = T)


# Function ----------------------------------------------------------------

fun_srsn<-function(z=1.96,d,a,data){
  # z= 信心水準下的Z值(預設confidence 95% 下的Z值為1.96) , d= 抽樣誤差
  # a= 感興趣的變數（欲推估之變數）
  
  # Ratio estimation --------------------------------------------------------
  
    p<-sum(a!=0)/length(a)  #估計母體比例
    N<-length(a) #母體總數
    n <-ceiling(((z/d)^2*p*(1-p))/(1+1/N*((z/d)^2*p*(1-p)-1)))
    paste("樣本數=", n)
    
}

# Run Function ------------------------------------------------------------

fun_srsn(z= 1.96, d= 0.015, a= p_srs$WebActivity, data= p_srs) 
# sample size n is determined


