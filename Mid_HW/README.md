
# Sampling with 4 different kinds of method 
  
## 1. Simple Random Sampling

### Read data 

```r
setwd("~/Documents/FJU大四/Sampling/Mid_HW")
p_srs <- read.csv("products.csv",header = T) # population 
```

### Function

```r
fun_srs <-function(n,data){
  
  # SRS ---------------------------------------------------------------------
  
  set.seed(979)
  N <- as.numeric(nrow(data) )# N in SRS = total units in the poplation
  SRS <- sample(N,size=n,replace = FALSE) 
  
  
  # Get data ----------------------------------------------------------------
  
  data.frame(data[SRS,])# match the row number 
  
}
```


### Run Function

```r
s_srs<-fun_srs(n=1068,data=p_srs) #sample s
```
   
  
## 2. SRS with Ratio Estimation 
### Read data

```r
setwd("~/Documents/FJU大四/Sampling/Mid_HW")
p_srs <- read.csv("products.csv",header = T)
```

### Function

```r
fun_srsn<-function(z=1.96,d,a,data){
    
  # Ratio Estimation-------------------------------------------
    p<-sum(a!=0)/length(a)  #估計母體比例
    N<-length(a) #母體總數
    n <-ceiling(((z/d)^2*p*(1-p))/(1+1/N*((z/d)^2*p*(1-p)-1)))
    paste("樣本數=", n)
    
}
```
  z= 信心水準下的Z值(預設confidence 95% 下的Z值為1.96)  
  d= 抽樣誤差  
  a= 感興趣的變數（欲推估之變數）

### Run Function


```r
  fun_srsn(z= 1.96, d= 0.015, a= p_srs$WebActivity, data= p_srs) 
```

```
## [1] "樣本數= 3344"
```
  sample size n is determined   
  

## 3. Systematic Sampling  
### Read data

```r
setwd("~/Documents/FJU大四/Sampling/Mid_HW")
p_sys <- read.csv("products.csv",header = T) #population 
```


### Function 

```r
fun_sys<-function(n,data){

  # Systematic Sampling 
  
  N = as.numeric(nrow(data))
  k = as.integer(N/n)  
  r = sample(k,1)    
  s = 0 
  
  for(i in 1:n){
    s[i]=r+(i-1)*k
  }
  
  data.frame(data[s,]) 
}
```
s = the row number which would be sampled  
After the s is sampled, match the row number to the data frame  

### Run Function 

```r
s_sys<- fun_sys(n=2000,data=p_sys) #sample  
```



## 4. Repeated Systematic Sampling and Confidence Interval 
### Read data

```r
setwd("~/Documents/FJU大四/Sampling/Mid_HW")
p_sys <- read.csv("products.csv",header = T)
```

### Function
n = sample number  
n_rsys = repeat how many times  
v = the name of the variable you want to estimate  
data = the data to be sampled

N = total units in population  
gn = number of units in each repeated systematic sampling  
gap = the gap in each repeated systematic sampling  
stp = the starting point of each 10 repeat systematic sampling  
sam = an empty matrix to fill all 10 rsys sample rows   


```r
fun_rsys<-function(n,n_rsys,v,data){
  
  # Repeated Systematic Sampling 

    N<- as.numeric(nrow(data)) 
    gn<- n/n_rsys 
    gap<- floor(N/gn) 
    stp<- sample(1:gap,n_rsys,replace = FALSE) 
    sam <- matrix(rep(0,gn),nrow=gn,ncol=10) 


  # Fill ‘sam' with 10 repeated sample row number

    for(i in 1:n_rsys){
      sam[,i] <- seq(stp[i],stp[i]+(gn-1)*gap,by=gap)
    }

  # Get Sample Data
    
    s_rsys<- data.frame(data[sam,])

  # Use the sample data to estimate 95% CI

    m<-mean(s_rsys[,v])
    s<-(var(s_rsys[,v]))^(1/2)
    print(paste('95% CI=','(',m-1.96*s,',',m+1.96*s,')'))
    
    s_rsys 
}
```
The last s_rsys in the code is to get the sample data outside the function

### Run function 

```r
s_rsys<- fun_rsys(n=3000,n_rsys=10,v='Age',data=p_sys)
```

```
## [1] "95% CI= ( 26.2959617897657 , 70.1440382102343 )"
```
sample data get!




