library(data.table)
setwd('d:/')                                #設定工作資料夾
data = fread('Thr.csv')                     #讀取csv檔

N = 90                                      #母體總數
Ty = 41.02                                  #y的總和
n = 15                                      #抽取的樣本數

mean(data$y)                                #算出y的平均數
mean(data$x)                                #算出x的平均數

var(data$x)                                 #算出樣本平均變異數Sm^2

Rhead = mean(data$x)/mean(data$y)           #算出樣本比率Rhead
print(Rhead)

a = var(data$x)
ybar = mean(data$y)
xbar = mean(data$x) 

e = 0.9                                     #給定1-alpha=0.9

f = (N * xbar)/Ty
print(f)


q = Rhead * (Rhead-((N * xbar)/Ty)) + ((N/n)-1)*(a/(ybar*Ty))
print(q)

if(q < 0 ){
  cat("dhead=0")
}else if(q > 0){
  cat("calculate;")
}                                           #判斷dhead^2是否小於0


g = q/(1-e)                                 #算出根號內之值

dhead = sqrt(g)                             #算出d的估計值
print(dhead)