
library(data.table)
setwd('d:/')                                #設定工作資料夾
data = fread('data-1.csv')                  #讀取csv檔

colnames(data)[4] = 'accumulation100yi'     #讀取data中第四欄之資料
#----------------------------------------------


bound = data$accumulation100yi              #指派bound為accumulation100yi
bound = c(1,bound)                          #生成函數
n = length(bound)-1                         #定義函數來檢查向量的長度
class = paste('u' , c(1:n),sep='')


a = function(x,bound,class){  
     bo = 1
     i = 1
  while(bo){
     if(x>=bound[i] && x<bound[i+1]){
      value = class[i]
      bo = 0
    }
    i=i+1
  }
  return( value )
}

text = sapply(c(1:n),
              function(i){
                if(i>=2) value = paste(bound[i]+1,'~',bound[i+1],sep = ' ')
                else value = paste(bound[i],'~',bound[i+1],sep = ' ')
                return(value)
                })                            #

bound_class = cbind(class,text)



#sapply將公式帶入到輸入檔案的每個元素上回傳一個vector
#&& 用於單一值的邏輯判斷
#sep是參數用來指定字符的分隔符號
#--------------------------------------
# 多個判斷



x = sample(c(1:max(bound)),30)                #隨機抽取30個pps樣本

value = sapply(c(1:length(x)),
               function(i) 
                 a(x[i],bound,class))         #找出抽取之Ui

print(bound_class)                            #印出分類範圍
print(x)                                      #印出抽取之樣本
print(value)                                  #印出抽取之Ui
