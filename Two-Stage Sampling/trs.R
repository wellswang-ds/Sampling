#read.csv 讀取csv檔案
#將資料存取成Rdata命名為data_all
data_all <- read.csv("~/Desktop/輔大課程/106抽樣/二階段/data.csv"
                     ,header=T
                     ,fileEncoding = "BIG5")
str(data_all)
#將第一欄區域別的名稱改為英文area方便作業
colnames(data_all)[1]="area"

#area作為縣市分群，用unique查看共有幾個縣市，總共20個，M=20
allcity<-unique(data_all$area)
allcity

library(sampling) 
first_sample=NULL
##從M個子母體中簡單隨機抽出m=5個
#cluster集群抽樣，srswor取出不放回、srswr取出放回
set.seed(3)
first_sample<-cluster(data_all
                      ,clustername=c("area")
                      ,size=5
                      ,method=c("srswor"))
unique(first_sample$area)

#母體及抽樣數比較#母體及抽樣數比較
#抽出之M及樣本數
table_first_sample<-as.data.frame(table(first_sample$area)
                                  [which(table(first_sample$area)!=0)])


#樣本比例配置
sample_size=120
table_first_sample$p<-table_first_sample$Freq/sum(table_first_sample$Freq)
table_first_sample$sample_size<-round(sample_size*table_first_sample$p)
table_first_sample#第一階段抽樣狀況

sum(table_first_sample$p)#驗證比例估計之比例加總是否符合機率公理假設
sum(table_first_sample$sample_size)#驗證總抽樣樣本數是否為設定之120筆

#分層抽樣strata() stratanames分層的依據,size各層要被抽出的樣本數
second_sample<-strata(first_sample, stratanames=c("area")
                      , size=table_first_sample$sample_size
                      ,method=c("srswor"))

#抽出縣市以及樣本數
table_second_sample<-as.data.frame(table(second_sample$area)
                                   [which(table(second_sample$area)!=F)])

table_first_sample
table_second_sample

output<-getdata(data_all,second_sample)#取出抽樣資料
write.csv(output,"~/Desktop/輔大課程/106抽樣/二階段/output.csv"
          ,row.names = FALSE
          ,fileEncoding = "BIG5")


