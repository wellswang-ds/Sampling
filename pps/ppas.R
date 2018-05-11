library(data.table)
setwd('d:/')                                     #設定工作資料夾
data = fread('data-1.csv')                       #讀取csv檔

colnames(data)[7] = 'yi_Ty'                      #讀取data中第四欄之資料
#----------------------------------------------



a = c(data$yi_Ty)                                #把yi/Ty指派進a                  
b = sample(c(1:90),prob=a,size=1)                #由編號1~90號中先抽出一個pps樣本
print(b)

c = c(1:90)
c_1 = c[-b]                                      #扣除剛剛抽出的pps樣本

d = sample(c(c_1),size=17)                       #抽取剩下的17個樣本
print(d)
