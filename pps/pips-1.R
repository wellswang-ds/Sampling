library(data.table)
setwd('d:/')                                #設定工作資料夾
data = fread('Sec.csv')                     #讀取csv檔

N = 90
n = 18

mean(data$division)                         #算出樣本平均變異數Rmhead

var(data$division)                          #算出樣本平均變異數Sm^2
a=var(data$division)

d = 0.35                                    #給定d值
alpha = 0.1                                 #給定alpha=0.1

n1 = (a/(alpha*d^2))/(1+(1/N)*(a/(alpha*d^2))) #Pips下試查之樣本數
print(n1)

if(n1 < n ){
  cat("enough")
}else if(n1 > n){
  cat("not enough")
}
