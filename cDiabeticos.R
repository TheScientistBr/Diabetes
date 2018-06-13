library("zipf")
loadConfig()



myClass <- config.get("myClass")
dirData <- config.get("dirData")
loadPackage("data.table")
classes <- read.csv(paste0(dirData,"/class.txt"), stringsAsFactors = FALSE, header = TRUE)
classes$class <- as.character(classes$class)
classes <- classes[,-1]
book_words <- read.table(file = "n.csv",stringsAsFactors = FALSE,sep = ",",header = T)

book_words <- book_words[,-1]
names(book_words) <- c("file","word","tf_idf","class")
write.csv(book_words,paste0(dirData,"/Book_words.csv"))


loadConfig()
myClass <- config.get("myClass")
dirData <- config.get("dirData")
loadPackage("data.table")
classes <- read.csv(paste0(dirData,"/class.txt"),
                    stringsAsFactors = FALSE, header = TRUE)
classes$class <- as.character(classes$class)
book_words <- read.table(file = paste0(dirData,"/Book_Words.csv"),header = T,sep = ",",
                         stringsAsFactors = FALSE)
for(classe in classes$class) {
  print(paste("processing Class ", classe))
  subClass <- subset(book_words, class == classe)
  files <- as.character(unique(sort(subClass$file)))
  nfiles <- as.integer(length(files)*kfold)
  files <- files[1:nfiles]
  subClass <- subset(book_words, file %in% files)
  centroid <- as.character(unique(sort(subClass$word)))
  centroid <- as.data.table(centroid)
  colnames(centroid) <- c("word")
  setkey(centroid,word)
  centroid$mean <- 0
  lixo <- sapply(centroid$word, function(myword) {
    centroid[myword,"mean"] <-
      mean(subClass[which(subClass$word == myword),]$tf_idf)
  })
  centroid <- as.data.table(lixo,keep.rownames = TRUE)
  colnames(centroid) <- c("word","mean")
  write.csv(centroid,paste0(dirData,"/centroid.",classe),
            fileEncoding = "UTF-8")
  remove(centroid,subClass,nfiles,files,lixo)
}

classes <-data.frame(class=as.character(c("0","1")),descricao=c("normal","diabetico"),stringsAsFactors = FALSE)
write.csv(classes,"data/class.txt",sep = ";",quote = T)
