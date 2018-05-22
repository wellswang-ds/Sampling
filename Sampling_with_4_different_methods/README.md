
# Sampling with 4 different kinds of method 
Here we demonstrate 4 different kinds of sampling method with the given data `product.csv`

<details><summary>Table of Contents</summary><p>

* [0. Data](#0-data)
* [1. Simple Random Sampling](#1-simple-random-sampling)
* [2. SRS with Ratio Estimation](#2-srs-with-ratio-estimation)
* [3. Systematic Sampling](#3-systematic-sampling)
* [4. Repeated Systematic Sampling and Confidence Interval](#4-repeated-systematic-sampling-and-confidence-interval)

</p></details><p></p>

## 0. Data
### Showing the first 10 rows of the data

```r
library(data.table)
product <- fread('https://raw.githubusercontent.com/wellsytheman/Sampling/master/Sampling_with_4_different_methods/products.csv')
library(knitr)
kable(product[1:10,], format = 'markdown')
```



|row ID                      |MaritalStatus |Gender | EstimatedYearlyIncome| NumberOfContracts| Age| Target| Available401K| CustomerValueSegment| ChurnScore| CallActivity| SentimentRating|Products      | WebActivity| Iteration|
|:---------------------------|:-------------|:------|---------------------:|-----------------:|---:|------:|-------------:|--------------------:|----------:|------------:|---------------:|:-------------|-----------:|---------:|
|R25_Row25_Row25_Row25#0     |M             |M      |                 10000|                 2|  64|      1|             1|                    1|        0.1|            2|               2|CO Investment |           1|         0|
|R30_Row30_Row30_Row30#0     |M             |F      |                 10000|                 2|  63|      1|             1|                    1|        0.1|            2|               2|CO Investment |           1|         0|
|R35_Row35_Row35_Row35#0     |M             |F      |                 10000|                 2|  62|      1|             1|                    1|        0.1|            2|               2|CO Investment |           1|         0|
|R37_Row37_Row37_Row37#0     |S             |F      |                 40000|                 2|  32|      1|             0|                    2|        0.5|            2|               0|CO Investment |           0|         0|
|R38_Row38_Row38_Row38#0     |M             |F      |                 10000|                 2|  62|      1|             1|                    1|        0.5|            2|               2|CO Investment |           1|         0|
|R44_Row44_Row44_Row44#0     |M             |M      |                 20000|                 2|  61|      1|             1|                    1|        0.1|            2|               2|CO Investment |           1|         0|
|R49_Row49_Row49_Row49#0     |S             |F      |                 40000|                 2|  30|      0|             1|                    2|        0.5|            2|               0|CO Investment |           0|         0|
|R84_Row84_Row84_Row84#0     |S             |M      |                 80000|                 2|  53|      1|             0|                    2|        0.1|            3|               2|CO Investment |           0|         0|
|R133_Row133_Row133_Row133#0 |S             |F      |                 30000|                 2|  30|      0|             0|                    2|        0.0|            2|               0|CO Investment |           0|         0|
|R135_Row135_Row135_Row135#0 |S             |M      |                 30000|                 2|  30|      1|             0|                    2|        0.5|            2|               0|CO Investment |           0|         0|

  
## 1. Simple Random Sampling

### Read data 

```r
p_srs <- product 
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
s_srs<-fun_srs(n=1068,data=product) #sample s
```
   
  
## 2. SRS with Ratio Estimation 
### Read data

```r
p_srs <- product
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
  fun_srsn(z= 1.96, d= 0.015, a= product$WebActivity, data= product) 
```

```
## [1] "樣本數= 3344"
```
  sample size n is determined   
  

## 3. Systematic Sampling  
### Read data

```r
p_sys <- product
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
s_sys<- fun_sys(n=2000,data=product) #sample  
```



## 4. Repeated Systematic Sampling and Confidence Interval 
### Read data

```r
p_sys <- product
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
fun_rsys<-function(n,n_rsys,v, data){
  
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
    
    sam = as.numeric(sam)
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
s_rsys<- fun_rsys(n=3000,n_rsys=10,v='Age', data=product)
```

```
## [1] "95% CI= ( 26.2959617897657 , 70.1440382102343 )"
```

### sample data get! (Showing the first 10 rows)

```r
kable(s_rsys[1:10,], format = 'markdown')
```



|row.ID                          |MaritalStatus |Gender | EstimatedYearlyIncome| NumberOfContracts| Age| Target| Available401K| CustomerValueSegment| ChurnScore| CallActivity| SentimentRating|Products      | WebActivity| Iteration|
|:-------------------------------|:-------------|:------|---------------------:|-----------------:|---:|------:|-------------:|--------------------:|----------:|------------:|---------------:|:-------------|-----------:|---------:|
|R307_Row307_Row307_Row307#0     |M             |M      |                 80000|                 2|  55|      1|             1|                    2|        0.5|            3|               2|CO Investment |           0|         0|
|R1521_Row1521_Row1521_Row1521#0 |S             |F      |                 10000|                 2|  35|      1|             0|                    3|        0.0|            1|               0|CO Investment |           0|         0|
|R2592_Row2592_Row2592_Row2592#0 |M             |F      |                 70000|                 2|  58|      1|             1|                    3|        0.2|            3|               5|CO Investment |           2|         0|
|R3231_Row3231_Row3231_Row3231#0 |M             |F      |                120000|                 4|  38|      0|             1|                    1|        1.0|            4|               0|CO Investment |           5|         0|
|R3999_Row3999_Row3999_Row3999#0 |S             |F      |                 10000|                 2|  37|      0|             1|                    3|        0.1|            1|               0|CO Investment |           0|         0|
|R5259_Row5259_Row5259_Row5259#0 |S             |F      |                 10000|                 2|  44|      1|             1|                    3|        0.0|            1|               4|CO Investment |           4|         0|
|R6105_Row6105_Row6105_Row6105#0 |S             |F      |                 10000|                 2|  38|      0|             1|                    3|        0.1|            2|               0|CO Investment |           0|         0|
|R6809_Row6809_Row6809_Row6809#0 |S             |M      |                 10000|                 2|  47|      0|             1|                    3|        0.0|            1|               3|CO Investment |           2|         0|
|R7629_Row7629_Row7629_Row7629#0 |S             |M      |                 20000|                 2|  39|      0|             0|                    3|        0.1|            1|               0|CO Investment |           0|         0|
|R8334_Row8334_Row8334_Row8334#0 |S             |F      |                 10000|                 2|  46|      0|             1|                    3|        0.0|            3|               3|CO Investment |           2|         0|




