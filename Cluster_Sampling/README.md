

# Cluster Sampling
## 8.3 Estimation of a population mean and total
### 建立課本例題的資料


```r
cluster<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
number_of_residents_mi<-c(8,12,4,5,6,6,7,5,8,3,2,6,5,10,9,3,6,5,5,4,6,8,7,3,8)
total_income_per_cluster_yi<-c(96000,121000,42000,65000,52000,40000,75000,65000,45000,
                               50000,85000,43000,54000,49000,53000,50000,32000,22000,
                               45000,37000,51000,30000,39000,47000,41000)
percapita_income<-data.frame(cluster,number_of_residents_mi,total_income_per_cluster_yi)
library(knitr)
kable(percapita_income,format = 'markdown')
```



| cluster| number_of_residents_mi| total_income_per_cluster_yi|
|-------:|----------------------:|---------------------------:|
|       1|                      8|                       96000|
|       2|                     12|                      121000|
|       3|                      4|                       42000|
|       4|                      5|                       65000|
|       5|                      6|                       52000|
|       6|                      6|                       40000|
|       7|                      7|                       75000|
|       8|                      5|                       65000|
|       9|                      8|                       45000|
|      10|                      3|                       50000|
|      11|                      2|                       85000|
|      12|                      6|                       43000|
|      13|                      5|                       54000|
|      14|                     10|                       49000|
|      15|                      9|                       53000|
|      16|                      3|                       50000|
|      17|                      6|                       32000|
|      18|                      5|                       22000|
|      19|                      5|                       45000|
|      20|                      4|                       37000|
|      21|                      6|                       51000|
|      22|                      8|                       30000|
|      23|                      7|                       39000|
|      24|                      3|                       47000|
|      25|                      8|                       41000|
### 輸入例題中各變數的值

```r
y<-percapita_income$total_income_per_cluster_yi #y = observations of target variable
m<-percapita_income$number_of_residents_mi # the  number of elements in cluster
N<-415
M<-2500
```
### 估計母體平均數及其變異數、誤差上下界

```r
cluster.mu<-function(y=0,m=0,N=0){
  n=length(m)
  mean={sum(y)/sum(m)}
  var=(1-(n/N))*(sd(y-((sum(y)/sum(m))*m)))^2/(n*(mean(m))^2)
  confi_interval={
    m_interval<-paste(mean-var^(1/2)*2,mean+var^(1/2)*2,sep = ",")
    m_confidence<-paste("(",m_interval,")",sep = "")
  }
  cbind(mean,var,confi_interval)
}
```
#### 課本例題

```r
cluster.mu(y,m,N)
```

```
##      mean               var               
## [1,] "8801.32450331126" "653785.194437304"
##      confi_interval                       
## [1,] "(7184.18481307135,10418.4641935512)"
```
### 估計母體總數及其變異數、誤差上下界####

```r
cluster.total<-function(y=0,m=0,N=0,M=NA){
  if(is.na(M)==F){
    n=length(m)
    total=M*(sum(y)/sum(m))
    var=M^2*((1-(n/N))*(sd(y-((sum(y)/sum(m))*m)))^2/(n*(mean(m))^2))
    confi_interval={
      ptwM_interval<-paste(total-var^(1/2)*2,total+var^(1/2)*2,sep = ",")
      ptwM_confidence<-paste("(",ptwM_interval,")",sep = "")
    }
    cbind(total,var,confi_interval)
  }
  else{
    n=length(m)
    total=N/n*sum(y)
    var=N^2*(1-(n/N))*(sd(y)^2/n)
    confi_interval={
      ptwN_interval<-paste(total-var^(1/2)*2,total+var^(1/2)*2,sep = ",")
      ptwN_confidence<-paste("(",ptwN_interval,")",sep = "")
    }
    cbind(total,var,confi_interval)
  }
}
```
#### 課本例題

```r
cluster.total(y,m,N,M)
```

```
##      total              var               
## [1,] "22003311.2582781" "4086157465233.15"
##      confi_interval                       
## [1,] "(17960462.0326784,26046160.4838779)"
```

```r
cluster.total(y,m,N,M=NA)
```

```
##      total      var             confi_interval                       
## [1,] "22061400" "3072279860000" "(18555815.9630669,25566984.0369331)"
```
## 8.5 Selecting the Sample Size for Estimating Population Means and Totals
### 估計具估計誤差B的mu的樣本大小

```r
mu_n<-function(y,m,M_mean=mean(m),N,B=2*var_ybar^(1/2),var_ybar){
  sr<-sd(y-((sum(y)/sum(m))*m))
  D<-(B^2*M_mean^2)/4
  n<-(N*sr^2)/(N*D+sr^2)
  n
}
```
#### 課本例題

```r
mu_n(y,m,M_mean=mean(m),N,500)
```

```
## [1] 166.5787
```
### 估計具估計誤差B的tau的樣本大小

```r
tau_n<-function(y,m,N,B=2*var_ybar^(1/2),var_ybar=0){
  if(is.na(M)==F){
    sd<-sd(y-((sum(y)/sum(m))*m))
  }else{
    sd<-sd(y)
  }
  D<-B^2/(4*N^2)
  n<-(N*sd^2)/(N*D+sd^2)
  n
}
```
#### 課本例題
##### 未知M，用M*ytbar

```r
B<-1000000
M<-NA
tau_n(y,m,N,B)
```

```
## [1] 182.8665
```
##### 已知M，用M*ybar

```r
B<-1000000
M<-2500
tau_n(y,m,N,B)
```

```
## [1] 212.8836
```
## 8.6 Estimation of a Population Proportion

```r
a <-c(4,7,1,3,3,4,4,2,3,2,1,3,2,5,4,1,4,2,3,1,3,3,4,0,3)  #有興趣元素

eastimate_p <- function(a,m,n,N=415){   # a: 有興趣元素，m: 元素(居民) n:抽出幾群 N: 母群集數
  p.hat <- sum(a)/sum(m)    #計算P.hat
  sp.2 <- sum((a-(p.hat*m))^2)/(n-1)   #計算Sp平方
  v.p.hat <- ((N-n)/(N*n*mean(m)^2))*sp.2  #計算V(p)
  BB <- 1.96*sqrt(v.p.hat)  #計算兩倍標準差
  AA <- cbind(p.hat,BB)  # 合併點估計與兩倍標準差
  return(AA)
}

eastimate_p(a,number_of_residents_mi,25)
```

```
##          p.hat         BB
## [1,] 0.4768212 0.04582662
```
## 8.7 Selecting the Sample Size for Estimating Proportions

```r
find_p <- function(a,m,n,B,N=415){  # a: 有興趣元素 m: 元素(居民) n:抽出幾群
  p.hat <- sum(a)/sum(m)    #計算P.hat
  sp.2 <- sum((a-(p.hat*m))^2)/(n-1)   #計算Sp平方
  D <- B^2*mean(m)^2/1.96^2  #計算D
  n <-N*sp.2/(N*D+sp.2) #計算n需要取幾群
  nn <- ceiling(n)  #無條件進位
  return(nn)
}

find_p(a,number_of_residents_mi,25,0.04)
```

```
## [1] 33
```
## 8.8 Cluster Sampling Combined with Stratification
#### 資料一

```r
cluster<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
number_of_residents_mi<-c(8,12,4,5,6,6,7,5,8,3,2,6,5,10,9,3,6,5,5,4,6,8,7,3,8)
total_income_per_cluster_yi<-c(96000,121000,42000,65000,52000,40000,75000,65000,45000,
                               50000,85000,43000,54000,49000,53000,50000,32000,22000,
                               45000,37000,51000,30000,39000,47000,41000)
percapita_income1<-data.frame(cluster,number_of_residents_mi,total_income_per_cluster_yi)
kable(percapita_income1,format = 'markdown')
```



| cluster| number_of_residents_mi| total_income_per_cluster_yi|
|-------:|----------------------:|---------------------------:|
|       1|                      8|                       96000|
|       2|                     12|                      121000|
|       3|                      4|                       42000|
|       4|                      5|                       65000|
|       5|                      6|                       52000|
|       6|                      6|                       40000|
|       7|                      7|                       75000|
|       8|                      5|                       65000|
|       9|                      8|                       45000|
|      10|                      3|                       50000|
|      11|                      2|                       85000|
|      12|                      6|                       43000|
|      13|                      5|                       54000|
|      14|                     10|                       49000|
|      15|                      9|                       53000|
|      16|                      3|                       50000|
|      17|                      6|                       32000|
|      18|                      5|                       22000|
|      19|                      5|                       45000|
|      20|                      4|                       37000|
|      21|                      6|                       51000|
|      22|                      8|                       30000|
|      23|                      7|                       39000|
|      24|                      3|                       47000|
|      25|                      8|                       41000|

```r
y1<-percapita_income1$total_income_per_cluster_yi   #資料一y值
m1<-percapita_income1$number_of_residents_mi        #資料一m值
```
#### 資料二

```r
cluster<-c(1,2,3,4,5,6,7,8,9,10)
number_of_residents_mi<-c(2,5,7,4,3,8,6,10,3,1)
total_income_per_cluster_yi<-c(18000,52000,68000,36000,45000,96000,64000,115000,41000,12000)
percapita_income2<-data.frame(cluster,number_of_residents_mi,total_income_per_cluster_yi)
kable(percapita_income2,format = 'markdown')
```



| cluster| number_of_residents_mi| total_income_per_cluster_yi|
|-------:|----------------------:|---------------------------:|
|       1|                      2|                       18000|
|       2|                      5|                       52000|
|       3|                      7|                       68000|
|       4|                      4|                       36000|
|       5|                      3|                       45000|
|       6|                      8|                       96000|
|       7|                      6|                       64000|
|       8|                     10|                      115000|
|       9|                      3|                       41000|
|      10|                      1|                       12000|

```r
y2<-percapita_income2$total_income_per_cluster_yi   #資料二y值
m2<-percapita_income2$number_of_residents_mi        #資料二m值
```
#### 輸入由題目給定的各別母群體大小

```r
N1=415
N2=168
```
#### 計算個別母群體所抽樣的群體數

```r
n1=nrow(percapita_income1)
n2=nrow(percapita_income2)
```
#### 計算群體抽樣與分層結合後的平均數、變異數及95%信賴水準下的信賴區間

```r
cluster_with_strata<-function(N1,N2,y1,y2,n1,n2,m1,m2){
  M<-N1*mean(m1)+N2*mean(m2)
  y1bar<-mean(y1)
  y2bar<-mean(y2)
  m1bar<-mean(m1)
  m2bar<-mean(m2)
  ycbar<-((N1*mean(y1)+N2*mean(y2))/(N1*mean(m1)+N2*mean(m2)))
  sc1<-sd(y1-ycbar*m1)
  sc2<-sd(y2-ycbar*m2)
  var<-(1/M^2)*((N1^2*(1-n1/N1)*sc1^2/n1)+N2^2*(1-n2/N2)*sc2^2/n2)
  confi_interval={
    interval<-paste(ycbar-var^(1/2)*2,ycbar+var^(1/2)*2,sep = ",")
    paste("(",interval,")",sep = "")
  }
  cbind(ycbar,var,confi_interval)
}
```
#### 帶入課本例題

```r
cluster_with_strata(N1,N2,y1,y2,n1,n2,m1,m2)
```

```
##      ycbar              var               
## [1,] "9385.24836326506" "412613.319895129"
##      confi_interval                       
## [1,] "(8100.54867867706,10669.9480478531)"
```

## 8.9 Cluster Sampling with Probability Proportional to Size
### Textbook Example 1 pg.275
#### Step 1

```r
Division <- c(1,2,3,4,5,6,7,8)
number_of_employees<-c(1200,450,2100,860,2840,1910,390,3200)
```
輸入資料  


#### Step 2

```r
Cumulative_range<-cumsum(number_of_employees)
Days_of_sick_leave<-data.frame(Division, number_of_employees,Cumulative_range)
kable(Days_of_sick_leave,format = 'markdown')
```



| Division| number_of_employees| Cumulative_range|
|--------:|-------------------:|----------------:|
|        1|                1200|             1200|
|        2|                 450|             1650|
|        3|                2100|             3750|
|        4|                 860|             4610|
|        5|                2840|             7450|
|        6|                1910|             9360|
|        7|                 390|             9750|
|        8|                3200|            12950|

```r
pps <-Days_of_sick_leave$number_of_employees   
```
計算出每個群的範圍用來作PPS抽樣,並給定稍後要丟入function內的變數


### Textbook Example 2 p.276

After investigating the clusters chosen by pps, we can now start to estimate the population parameter.

#### Step 3

```r
n=3
y1=4320
y2=4160
y3=5790
m1=2100
m2=1910
m3=3200

yi = c(y1,y2,y3)
mi = c(m1,m2,m3)
```
輸入抽出後做調查的n 個集群大小mi，與所調查出的觀察值yi

#### Step 4

```r
yibar = yi/mi
pps_mean = round(mean(yibar),2)
pps_var = round(1/(n*(n-1))*sum((yibar-pps_mean)^2),4)
confi_interval={
    interval<-paste(pps_mean-pps_var^(1/2)*2,pps_mean+pps_var^(1/2)*2,sep = ",")
    paste("(",interval,")",sep = "")
  }
```

估計母體參數與信賴區間  

### Results
 

Table: Estimation of population mean, variance, and confidence interval 

pps_mean   pps_var   confi_interval                  
---------  --------  --------------------------------
2.01       0.0118    (1.792744390176,2.227255609824) 
