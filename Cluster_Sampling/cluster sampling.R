#8.3 Estimation of a population mean and total
###建立課本例題的資料####

cluster<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
number_of_residents_mi<-c(8,12,4,5,6,6,7,5,8,3,2,6,5,10,9,3,6,5,5,4,6,8,7,3,8)
total_income_per_cluster_yi<-c(96000,121000,42000,65000,52000,40000,75000,65000,45000,
                               50000,85000,43000,54000,49000,53000,50000,32000,22000,
                               45000,37000,51000,30000,39000,47000,41000)
percapita_income<-data.frame(cluster,number_of_residents_mi,total_income_per_cluster_yi)

###輸入例題中各變數的值####
y<-percapita_income$total_income_per_cluster_yi #y = observations of target variable
m<-percapita_income$number_of_residents_mi # the  number of elements in cluster
N<-415
M<-2500

###估計母體平均數及其變異數、誤差上下界####
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
####課本例題
cluster.mu(y,m,N)

###估計母體總數及其變異數、誤差上下界####
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
####課本例題
cluster.total(y,m,N,M)
cluster.total(y,m,N,M=NA)

#8.5 Selecting the Sample Size for Estimating Population Means and Totals
###估計具估計誤差B的mu的樣本大小####
mu_n<-function(y,m,M_mean=mean(m),N,B=2*var_ybar^(1/2),var_ybar){
  sr<-sd(y-((sum(y)/sum(m))*m))
  D<-(B^2*M_mean^2)/4
  n<-(N*sr^2)/(N*D+sr^2)
  n
  }
####課本例題
mu_n(y,m,M_mean=mean(m),N,500)

###估計具估計誤差B的tau的樣本大小####
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
####課本例題
#####未知M，用M*ytbar
B<-1000000
M<-NA
tau_n(y,m,N,B)
#####已知M，用M*ybar
B<-1000000
M<-2500
tau_n(y,m,N,B)

#8.6 Estimation of a Population Proportion
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

#8.7 Selecting the Sample Size for Estimating Proportions

find_p <- function(a,m,n,B,N=415){  # a: 有興趣元素 m: 元素(居民) n:抽出幾群
  p.hat <- sum(a)/sum(m)    #計算P.hat
  sp.2 <- sum((a-(p.hat*m))^2)/(n-1)   #計算Sp平方
  D <- B^2*mean(m)^2/1.96^2  #計算D
  n <-N*sp.2/(N*D+sp.2) #計算n需要取幾群
  nn <- ceiling(n)  #無條件進位
  return(nn)
}

find_p(a,number_of_residents_mi,25,0.04)

#8.8 Cluster Sampling Combined with Stratification
####資料一
cluster<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
number_of_residents_mi<-c(8,12,4,5,6,6,7,5,8,3,2,6,5,10,9,3,6,5,5,4,6,8,7,3,8)
total_income_per_cluster_yi<-c(96000,121000,42000,65000,52000,40000,75000,65000,45000,
                               50000,85000,43000,54000,49000,53000,50000,32000,22000,
                               45000,37000,51000,30000,39000,47000,41000)
percapita_income1<-data.frame(cluster,number_of_residents_mi,total_income_per_cluster_yi)

y1<-percapita_income1$total_income_per_cluster_yi   #資料一y值
m1<-percapita_income1$number_of_residents_mi        #資料一m值

####資料二
cluster<-c(1,2,3,4,5,6,7,8,9,10)
number_of_residents_mi<-c(2,5,7,4,3,8,6,10,3,1)
total_income_per_cluster_yi<-c(18000,52000,68000,36000,45000,96000,64000,115000,41000,12000)
percapita_income2<-data.frame(cluster,number_of_residents_mi,total_income_per_cluster_yi)

y2<-percapita_income2$total_income_per_cluster_yi   #資料二y值
m2<-percapita_income2$number_of_residents_mi        #資料二m值

####輸入由題目給定的各別母群體大小
N1=415
N2=168

####計算個別母群體所抽樣的群體數
n1=nrow(percapita_income1)
n2=nrow(percapita_income2)

####計算群體抽樣與分層結合後的平均數、變異數及95%信賴水準下的信賴區間
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

####帶入課本例題
cluster_with_strata(N1,N2,y1,y2,n1,n2,m1,m2)

