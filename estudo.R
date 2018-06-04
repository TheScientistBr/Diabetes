library("sas7bdat")
library("plyr")
library("gpuR")
library("gdata")

nrGPU <- detectGPUs()
myGPU <- gpuInfo(context_idx = nrGPU+1L)
setContext(id = grep("GeForce",myGPU)+nrGPU)

df <- read.csv(file = "data/df.csv",stringsAsFactors = FALSE)
ndfs <- read.csv(file = "data/ndfs.csv",stringsAsFactors = FALSE)

colnames(df) <- c("X","id","proc","espp","mot","fe","dt","st")
colnames(ndfs) <- c("X","id","proc","espp","mot","fe","dt","st")

head(df)
tail(ndfs)

gdf <- rbind(df,ndfs)
gdf <- gdf[,-1]
gdf$espp[gdf$espp<0] <- 0
gdf$mot[gdf$mot<0] <- 0

#-- dataset prepared
# start to modify by frequency

n <- NULL

# for a gpu use with matrix

gdfx <- gpuR::gpuMatrix(as.matrix(gdf),nrow = 87204,ncol = 7,type = "double")
bgdfx <- block(gdfx,1L,87204L,1L,7L)
for(id_i in as.character(bgdfx[,1])) {
  cli <- grep(pattern = id_i,bgdfx[])
  p <- count(df = bgdfx[cli,2])
  colnames(p) <- c("proc","freq")
  p <- cbind2(id=rep(bgdfx[cli[1],1],dim(p)[1]),p,st=rep(bgdfx[cli[1],7],dim(p)[1]))
  n <- rbind2(n,p)
}


write.csv(n,"n.csv")

head(x)
tail(x)
fit <- lm(st+id ~ . -X, data = gdf)
summary(fit)
confint(fit,level = 0.95)
plot(fitted(fit),residuals(fit))
predicted.intervals <- predict(fit,gdf,interval = 'confidence',level=0.95)
plot(predicted.intervals)

gdf <- scale(gdf)
ndfs <- scale(ndfs)
plot(gdf[,2:7])

head(gdf)
tail(gdf)

##########################
# linear regression
fit <- lm(st  ~ ., data = gdf)
summary(fit)

###########################
## Cluterization
tree <- hclustvar(gdf)
stab <- stability(tree, graph = FALSE,B = 10)
nrCluster <- which.is.max(stab$meanCR)
plot(stab)
plot(tree)

# kmeans
pca <- princomp(gdf[,2:6], cor=T)
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1]
pc.comp2 <- -1*pc.comp[,2]
newComp <- cbind(pc.comp1, pc.comp2)
cl <- kmeans(newComp, 2)
plot(pc.comp1, pc.comp2,col=cl$cluster+2,xlab = "Componente 1", ylab = "Componente 2",
     main = "KMeans & PCA")
points(cl$centers, pch=16)
cl$size

library("partykit")
ct <- ctree(st +id ~ . -st, data=gdf)
plot(ct)


######################
# Random forest
library("MASS")
library("randomForest")
set.seed(1)
train=sample(1:nrow(gdf),dim(gdf)[1]*.7)
test=sample(gdf[-train,])

rf <- randomForest(st + id ~ . -dt, data = gdf , subset = train)
rf

###############
#Baes 
library(e1071)
nb <- naiveBayes(st ~ ., data=gdf)
pred <- predict(nb,newdata = test)
table(pred,test)


###########################
# knn
library("class")
set.seed(1)
sTrain <- sample(1:nrow(gdf),dim(gdf)[1]*.7)
train <- gdf[sTrain,]
test <- sample(gdf[-sTrain,])

knn.test <- knn(train=train, test=test, cl=sTrain, k=2)
md2 <- train(gdf$st ~ ., data=gdf, method = "knn")
plot(md, cex = .8, col = "dodgerblue", main = "k = 2")

lines(lstat_grid$lstat, pred_001$pred, col = "darkorange", lwd = 0.25)
table(md,test$st)

library(mlbench)
library(kknn)
library("caret")
set.seed(1)
kknn.train = train.kknn(st ~., data=gdf, kmax=5, distance=2,
                        kernel=c("rectangular", "triangular", "epanechnikov"))
plot(kknn.train)

grid1 = expand.grid(.k=seq(2,20, by=1))
control = trainControl(method="cv")
set.seed(123)
knn.train = train(st ~., data=gdf, method="knn", trControl=control, tuneGrid=grid1)
knn.train

knn.test = knn(train[,'st'], test[,-9], train[,9], k=13)
table(knn.test, test$diabetes)


#######
# Summarise dataset
id1 <- as.character(gdf$id[2])
id2 <- as.character(gdf$id[87200])

proc <- factor(gdf$proc)
espp <- factor(gdf$espp)
mot <- factor(gdf$mot)
fe <- factor(gdf$fe)

gdf1 <- gdf[as.character(gdf$id)==id1,]
gdf2 <- gdf[as.character(gdf$id)==id2,]

summary((espp))
head(espp)
