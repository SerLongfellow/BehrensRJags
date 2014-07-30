likelihood.dir <- './likelihoods'

range <- c(5,6,8,10,11,13,15,16,18,19,20,seq(23,28),30,31,seq(33,36),39,40,seq(42,45))

data.mat <- NULL

for(subject.number in range){
  
  for(type in c('social','house')){
    
    if(subject.number < 10){
      data.file <- paste(likelihood.dir,'/subj_00',subject.number,'_',type,'_likelihoods.csv',sep='')
    }
    else{
      data.file <- paste(likelihood.dir,'/subj_0',subject.number,'_',type,'_likelihoods.csv',sep='')
    }
    
    data.csv <- read.csv(data.file, header=FALSE)
    data.tmp <- as.matrix(data.csv)
    
    data.mat <- rbind(data.mat,data.tmp)
    
  } 
}

social.mat <- data.mat[seq(1,nrow(data.mat),2),]
house.mat <- data.mat[seq(2,nrow(data.mat),2),]

#Social statistical analysis
social.vts.aic <- social.mat[,2]
social.vf.aic <- social.mat[,4]
tt.social.result <- t.test(social.vts.aic,social.vf.aic,paired=TRUE)
print(tt.social.result)

#House statistical analysis
house.vts.aic <- house.mat[,2]
house.vf.aic <- house.mat[,4]
tt.house.result <- t.test(house.vts.aic,house.vf.aic,paired=TRUE)
print(tt.house.result)

