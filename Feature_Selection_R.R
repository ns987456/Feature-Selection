
#install.packages('readxl')
library(readxl)
data_w= read_excel('C:/Users/Downloads/data_w.xlsx') 
head(data_w)

#install.packages('dplyr')
library(dplyr)    

##preparing data for analysis
#split data to two table (class1 & class2)
data_class1=filter(data_w, class == "1")
data_class1=data_class1 %>% select(-obj, -class)

data_class2=filter(data_w, class == "2")
data_class2=data_class2 %>% select(-obj, -class)

# demonstration of data 
data_class1[, 4]
dim(data_class1)
dim(data_class1)[2]

## normality test

# defining two vector for saving result 
ks_class1=c()
ks_class2=c()

# for loop (Kolmgorov-Smirnov)
for (i in 1:dim(data_class1)[2]){
  ks_class1[i]=ks.test(data_class1[, i], "pnorm")[2]
  ks_class2[i]=ks.test(data_class2[, i], "pnorm")[2]
}
warnings()
# example of the presence of ties in variable(we have error due to repeated value in 33 variables)
ks.test(data_class1[, 5], "pnorm")

#we change our desision and using the anderson normality test
#install.packages("nortest")
library(nortest)
# testing one variable
ad.test(as.matrix(data_class1[,5]))[2]

# for loop 
for (i in 1:dim(data_class1)[2]){
  ks_class1[i]=ad.test(as.matrix(data_class1[,i]))[2]
  ks_class2[i]=ad.test(as.matrix(data_class2[,i]))[2]
}

##############################################################################################################
##############################################################################################################
hist(as.matrix(data_class1[,5]), col = 'red', main = 'Distribution')
ad_class1=c()
ad_class2=c()

for (i in 1:dim(data_class1)[2]){
  if(ad.test(as.matrix(data_class1[,i]))[2]>0.05){ ad_class1[i]='normal' } else{ ad_class1[i]='nonnormal'} 
  if(ad.test(as.matrix(data_class2[,i]))[2]>0.05){ ad_class2[i]='normal' } else{ ad_class2[i]='nonnormal'} 
}

ad_class1[i]


##############################################################################################################
##############################################################################################################
featuers={}
for (i in 1:dim(data_class1)[2]){
  if (ad_class1[i]==ad_class2[i]) {
    featuers[[i]]= t.test(data_class1[,i], data_class2[,i])[3]
    #assign(paste0("F", i), featuers[[i]])
    names(featuers) <- paste("f", seq_along(featuers), sep = "");featuers
    
  } else {featuers[[i]]=wilcox.test(as.matrix(data_class1[,1]), as.matrix(data_class2[,1]),  exact=FALSE)[3]
  #assign(paste0("F", i), featuers[[i]])
  names(featuers) <- paste("f", seq_along(featuers), sep = "");featuers}
}

featuers[2]
class(featuers)
names(featuers)

############################################################################################################
############################################################################################################
#I assume p-value<0.0l is meaningful you can change it
mean.features=c()

for (i in 1:dim(data_class1)[2]){
  if (featuers[[i]]<0.05) {
    mean.features[i]= names(featuers[i])
    }
}

mean.features
mean.features <- mean.features[!is.na(mean.features)]
mean.features
length(mean.features)


colnames(data_w)

data_final=data_w %>%
  select(as.vector(mean.features))

data_final
length(data_final)
data_w[,c(1,2) ]
data_final=cbind(data_final, data_w[,c(1,2) ])
data_final

#install.packages('openxlsx')
library(openxlsx)

# for writing a data.frame or list of data.frames to an xlsx file
write.xlsx(data_final, 'C:/Users/OneDrive/Desktop/final_features.xlsx')
