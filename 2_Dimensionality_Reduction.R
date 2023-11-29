library(ggplot2)


# set working directory and outpaths 
setwd("G:/My Drive/Data Science/University/Texas Tech/Semester 2/Multivariate/Project/")
filepath = "./cleandata.csv"


# Read data
baseball = read.csv(filepath)

rownames(baseball) <- baseball$Name

head(baseball)

###################################### PROCESS data  ###############################
#subset interesting columns representative of various talents 
df <- baseball[,c(1,2,5,7,8,9,10,19,29)]

#scale 
df.s <- scale(df[3:ncol(df)])



####################################### PCA ################################ 

#perform pca
baseball.pca <- princomp(df.s,scores = TRUE)

# Summary 
summary(baseball.pca, loadings = TRUE)

" we can see that the first to components represents 83% of the data variance

component 1: aligns more to scores and hits than compared to component 2
Component 2: looks more at running speed and stolen bases "

#  plot PCA scores by names 
ggplot() +
  geom_text(aes(x = baseball.pca$scores[,1], 
                y = baseball.pca$scores[,2], 
                label = df$Name,
                col = df$Team), 
            nudge_x = 0.5, nudge_y = 0.5, check_overlap = TRUE)

"example: Sam Haggerty should have high running speed but low run.score "

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



     