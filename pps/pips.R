
library(data.table)
setwd('d:/')                                #設定工作資料夾
data = fread('data-1.csv')                  #讀取csv檔

colnames(data)[6] = 'increase900yi'         #讀取data中第六欄之資料
#----------------------------------------------


bound = data$increase900yi                  #指派bound為increase900yi
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
              })

bound_class = cbind(class,text)



#sapply將公式帶入到輸入檔案的每個元素上回傳一個vector
#&& 用於單一值的邏輯判斷
#sep是參數用來指定字符的分隔符號
#--------------------------------------
# 多個判斷



c = max(data$Plantingareayi)                #找到yi最大值
d = sum(data$Plantingareayi)                #計算yi總和
print(d/c)                                  #輸出Ty/MAXy之值


e = max(data$increase900yi)                 #找到累積yi總和最大值
k = e/18                                    #累積yi總和最大值除樣本數n=18
print(k)                                    #輸出抽樣間隔

first = sample(c(1:k),1)                    #隨機取1個樣本

x = seq(first,by=k,length.out=18)           #抽取剩下的17個樣本

value = sapply(c(1:length(x)),
               function(i) 
                 a(x[i],bound,class))       #去找x所屬之Ui 

#print(bound_class)
print(x)                                    #輸出pips抽樣結果
print(value)                                #輸出被抽選之Ui
