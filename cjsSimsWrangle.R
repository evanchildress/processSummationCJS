outDir<-"~/output"
files<-list.files(outDir)

for(f in files){
  assign(f,cbind(readRDS(file.path(outDir,f)),
                 parameter=c("pBeta","phiBeta1","phiBeta2","phiBeta3","phiBeta4","sdPhi"),
                 simNum=strsplit(substr(f,4,20),"[.]")[[1]][1])
  )
}

results<-do.call(rbind,mget(files))
rownames(results)<-NULL
saveRDS(results,"~/cjsSimResults.rds")