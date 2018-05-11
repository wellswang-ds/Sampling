setwd("C:/Users/steven/Desktop/黃彥嘉")     #寫設定工作資料夾；setwd是set working directory的簡寫。
DB <- read.csv("DB.csv",header=TRUE)     #讀入資料檔；header 是指資料是否有包含欄位名稱。

cor(DB$area,DB$output)     #看兩變數間的相關程度

plot(DB)     #散布圖


######前置作業######

#大樣本資料
N <- nrow(DB)     #N為母體總數
n_big <- 5850     #n_big為要抽的大樣本數
sample1 <- sample(1:N,n_big)     #sample1為母體總數中抽出的大樣本之編號
big_data <- DB[sample1,]     #big_data為大樣本  
Y_L <- mean(big_data[[1]])     #大樣本中栽培面積的平均

#小樣本資料
n_big <- nrow(big_data)     #n_big為大樣本總數
n_little <- 90     #n_little為要抽的小樣本數
sample2 <- sample(1:n_big,n_little)     #sample2為大樣本總數中抽出的小樣本之編號
little_data <- big_data[sample2,]     #little_data為小樣本
X_bar <- mean(little_data[[2]])     #X_bar為小樣本中產量的平均
y_bar <- mean(little_data[[1]])     #X_bar為小樣本中栽培面積的平均


S_square <- 19.1476     #因為S_square和SR_square的公式一直試不出來，所以先用課本例子的
SR_square <- 0.0523


#經費
totalC <- 100000     #調查總經費100000元
big_C <- 15     #查詢大樣本中每戶的栽培面積，一戶15元
little_c <- 120     #每戶實地蒐集費用120元


#變異數及準確度
Alpha <- 0.05     #假設Alpha=0.05
d <- 0.3     #假設d=0.3
Var <- ((((1/n_big)*(S_square-SR_square))-((1/N)*(S_square)))/Alpha)^(1/2)      #d不能取低於var，課本第348頁


#判斷d數值大小合不合適
if(d < Var){
  c("Error:在此參數設定下，敏感度d必須超過",Var)
}else{
  
  
  #判斷預設樣本參數抽取成本總和超過總金額
  if(n_big*big_C+n_little*little_c > totalC){
    c("Error:預設樣本參數抽取成本總和超過總金額")
  }else
  {
    
    
    ######1.在調查總金費為固定值下，如何配置大樣本與小樣本的樣本數，使變異數為最低，即準確度最高######
    #中文課本第342~346頁公式9.2
    n_big_f1 <- ceiling(totalC/(big_C+little_c*(((big_C/little_c)*(SR_square/(S_square-SR_square)))^(1/2))))     #ceiling為四捨五入的指令
    n_little_f1 <- ceiling(n_big_f1*(((big_C/little_c)*(SR_square/(S_square-SR_square)))^(1/2)))
    
    if(n_big_f1 > n_big){     #n_big_f1為配置大樣本的樣本數
      n_big_f1 <- n_big_f1
      n_big_f1_s <- n_big_f1-n_big     #n_big_f1_s為補抽的大樣本數
    }else{
      n_big_f1 <- n_big
      n_big_f1_s <- c("不需要")
    }
    
    if(n_little_f1 > n_little){     #n_little_f1為配置小樣本的樣本數
      n_little_f1 <- n_little_f1
      n_little_f1_s <-  n_little_f1-n_little          #n_little_f1_s為補抽的小樣本數
    }else{  
      n_little_f1 <- n_little
      n_little_f1_s <- c("不需要")
    }
    
   VAR1 <- (((1/n_big_f1)-(1/N))*S_square)+(((1/n_little_f1)-(1/n_big_f1))*SR_square)
    totalC_f1 <- n_big_f1*big_C+n_little_f1*little_c     
    
    
    ######2.固定準確度，即固定變異數之值，如何配置大樣本與小樣本的樣本數以使調查總金費為最低######
    #中文課本第347~349頁公式9.4
    S_square <- 19.1476     
    SR_square <- 0.0523
    
    n_big_f2 <- ceiling(((S_square-SR_square)+(SR_square/(((big_C/little_c)*(SR_square/(S_square-SR_square)))^(1/2))))/(Var+(S_square/N)))
    n_little_f2 <- ceiling(n_big_f2*(((big_C/little_c)*(SR_square/(S_square-SR_square)))^(1/2)))
    
    if(n_big_f2 > n_big){
      n_big_f2 <- n_big_f2
      n_big_f2_s <- n_big_f2-n_big
    }else{
      n_big_f2 <- n_big
      n_big_f2_s <- c("不需要")
    }
    
    if(n_little_f2 > n_little){
      n_little_f2 <- n_little_f2
      n_little_f2_s <-  n_little_f2-n_little
    }else{
      n_little_f2 <- n_little
      n_little_f2_s <- c("不需要")
    }
    
    totalC_f2 <- n_big_f2*big_C+n_little_f2*little_c
    
    
    ######3.給定大樣本數及準確度要求之下，決定最低需有的小樣本數#######
    #中文課本第350~351頁公式9.6
    
    n_little_f3 <- ceiling(SR_square/(Alpha*(d^2)-(((1/n_big)*(S_square-SR_square))-((1/N)*S_square))))     
    
    if(n_little_f3 > n_little){
      n_little_f3 <- n_little_f3
      n_little_f3_s <-  n_little_f3-n_little
    }else{
      n_little_f3 <- n_little
      n_little_f3_s <- c("不需要")
    }
    
    
    ####報表#####
    report <- as.data.frame(rbind(cbind(c("方式1"),c("在調查總金費為固定值下，如何配置大樣本與小樣本的樣本數，使變異數為最低，即準確度最高"),n_big_f1_s,n_little_f1_s,round(VAR1,digits = 3),totalC_f1)
                                  ,cbind(c("方式2"),c("固定準確度，即固定變異數之值，如何配置大樣本與小樣本的樣本數以使調查總金費為最低"),n_big_f2_s,n_little_f2_s,c("X"),totalC_f2)
                                  ,cbind(c("方式3"),c("給定大樣本數及準確度要求之下，決定最低需有的小樣本數"),c("已給定"),n_little_f3_s,c("X"),c("X"))))
    colnames(report) <- c("情況意向","方式敘述","大樣本補抽","小樣本補抽","變異數","總金費")
    
    View(report)
    
    
  }
}


#data.frame為資料框架
#colnames為設定欄位名稱


######課本第352頁公式9.8######
S_square <- 19.1476     
SR_square <- 0.0523
N <- 20000
n_big <- 5850
n_little <- 90

##1.給定1-Alpha，d的估計值##

Alpha <- 0.1

d_e <- ((((1/n_big)*(S_square-SR_square))+((1/n_little)*SR_square)-((1/N)*S_square))/Alpha)^(1/2)

#課本答案寫d=0.1304是錯的


##2.給定d，1-Alpha的估計值##

d <- 0.2

Alpha <- ((((1/n_big)*(S_square-SR_square))+((1/n_little)*SR_square)-((1/N)*S_square))/(d^2))

E <- 1-Alpha

#課本答案寫1-Alpha=0.9575是錯的