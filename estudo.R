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

gdfx <- gpuR::vclMatrix(as.matrix(gdf),nrow = 87204,ncol = 7,type = "double")
bgdfx <- block(gdfx,1L,87204L,1L,7L)
for(id_i in unique(as.character(bgdfx[,1]))) {
  cli <- which(gdfx[,1]==id_i)
  p <- aggregate(x = bgdfx[cli,3], by = list(bgdfx[cli,2]), count)
  p <- count(df = bgdfx[cli,2])
  colnames(p) <- c("proc","freq")
  p <- cbind(id=rep(bgdfx[cli[1],1],dim(p)[1]),p,st=rep(bgdfx[cli[1],7],dim(p)[1]))
  n <- rbind(n,p)
}
write.csv(n,"n.csv")


plotCli <- function(id_i) {
  list_cli <- unique(as.character(gdf[,1]))
  cli <- which(gdfx[,1]==list_cli[id_i])
  p <- count(df = bgdfx[cli,2])
  colnames(p) <- c("proc","freq")
  p <- cbind(id=rep(bgdfx[cli[1],1],dim(p)[1]),p,st=rep(bgdfx[cli[1],7],dim(p)[1]))
  n <- rbind(n,p)
  stCli <- which(gdf[,"id"]==list_cli[id_i])
  st <- gdf[stCli[1],"st"]
  st <- ifelse(st == 1L,st <- "Diabético",st <- "Normal")
  plot(n$proc,n$freq,main = st)
}


par(mfrow=c(4,2)) 
plotCli(1)
plotCli(2)
plotCli(3)
plotCli(4)
plotCli(500)
plotCli(700)
plotCli(800)
plotCli(900)

id_0 <- as.character(gdf[87000,1])
id_1 <- as.character(gdf[1,1])

n <-read.csv("n.csv")
n <- gpuR::vclMatrix(as.matrix(gdf),nrow = 87204,ncol = 7,type = "double")
cli <- which(n[,1]==id_1)
nb1 <- as.data.frame(n[cli,])
cli <- which(n[,1]==id_0)
nb0 <- as.data.frame(n[cli,])

plot(nb1$proc,nb1$freq, col = "blue")
par(new=T)
plot(nb0$proc,nb0$freq, col = "red")

nClear <- n[,n$st>1]
