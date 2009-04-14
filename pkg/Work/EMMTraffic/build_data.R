EMMTraffic <- read.table("MaggiDataset.txt")

colnames(EMMTraffic) <- paste("Loc",1:7,sep="_")
save(EMMTraffic, file="EMMTraffic.rda")

