# Read data ---------------------------------------------------------------

library(data.table)
product <- fread('https://raw.githubusercontent.com/wellsytheman/Sampling/master/Sampling_with_4_different_methods/products.csv')
p_sys <- product

# Function ----------------------------------------------------------------

fun_rsys<-function(n,n_rsys,v,data){
    #n = sample number
    #n_rsys = repeat how many times
    #v = the name of the variable you want to estimate
    #data = the data to be sampled
  
  # Repeated Systematic Sampling --------------------------------------------
  
    N<- as.numeric(nrow(data)) #total units in population
    gn<- n/n_rsys #number of units in each repeated systematic sampling
    gap<- floor(N/gn) #the gap in each repeated systematic sampling
    stp<- sample(1:gap,n_rsys,replace = FALSE) #the starting point of each 10 repeat systematic sampling
    sam <- matrix(rep(0,gn),nrow=gn,ncol=10) #empty matrix to fill all 10 rsys sample rows 

  # Fill ‘sam' with 10 repeated sample row number --------------------------
    
    for(i in 1:n_rsys){
      sam[,i] <- seq(stp[i],stp[i]+(gn-1)*gap,by=gap)
    }

  # Get Sample Data ---------------------------------------------------------
    
    sam <- as.numeric(sam)
    s_rsys<- data.frame(data[sam,])

  # Use the sample data to estimate 95% CI ----------------------------------
    
    m<-mean(s_rsys[,v])
    s<-(var(s_rsys[,v]))^(1/2)
    print(paste('95% CI=','(',m-1.96*s,',',m+1.96*s,')'))
    
    s_rsys #get the sample data outside the function
    
}


# Run function ------------------------------------------------------------

s_rsys<- fun_rsys(n=3000,n_rsys=10,v='Age',data=p_sys) #get the sample data




