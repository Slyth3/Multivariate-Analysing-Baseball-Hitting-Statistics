library(ggplot2)


# set working directory and outpaths 
setwd("G:/My Drive/Data Science/University/Texas Tech/Semester 2/Multivariate/Project/")
filepath = "./cleandata.csv"


# Read data
baseball = read.csv(filepath)

rownames(baseball) <- baseball$Name

head(baseball)

###################################### PROCESS data  
#subset interesting columns representative of various talents 
df <- baseball[,c(1,2,5,7,8,9,10,19,29)]

#initial assessment of data correlation, hr and running speed have almost no correlation, slightly negative, very interesting insight.
round(cor(df[3:ncol(df)]),4)
corr.matrix <- round(cor(df[,3:ncol(df)]),4)
#correlation plot-shows moderate, large, and small correlations
library(corrplot)
corrplot(corr.matrix, method = "color")


#scale 

df.s <- scale(df[3:ncol(df)])



####################################### PCA ################################ 

#perform pca
baseball.pca <- princomp(df.s)

plot(baseball.pca$scores)
labels(baseball.pca$scores, labels=df$Name)

summary(baseball.pca, loadings = TRUE)





################################ OTHER PLOTS ################################
# scaled df 
df_s <- scale(df[3:ncol(df)])
pca<- princomp(df_s)
plot(pca$scores[,1:2])


#GGPLOT

library(ggplot2)

#team
ggplot() +
  geom_point(aes(x = pca$scores[,1], y = pca$scores[,2], col = df$Team))

#WOBA
ggplot() +
  geom_point(aes(x = pca$scores[,1], y = pca$scores[,2], col = df$wOBA))

#hits
ggplot() +
  geom_point(aes(x = pca$scores[,1], y = pca$scores[,2], col = df$hits))



     