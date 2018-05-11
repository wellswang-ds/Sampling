library(data.table)
setwd('d:/')                                #設定工作資料夾
data = fread('Fir.csv')                     #讀取csv檔

n=30

mean(data$division)                         #算出樣本平均變異數Rmhead

var(data$division)                          #算出樣本平均變異數Sm^2
a=var(data$division)

d = 0.15                                    #給定d值
c = 1.96                                    #給定1-alpha=0.95時z_alpha/2之值


n1 = (c/d)^2*a
print(n1)

if(n1 > n ){
  cat("not enough")
}else if(n1 < n){
  cat("enough")
}                                           #判斷樣本數是某足夠
