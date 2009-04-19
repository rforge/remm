if(FALSE) {
Ouse <- read.csv("ouse.csv")

colnames(Ouse) <- c("Crakehill", "Skelton", "Skipbridge")
Ouse <- as.matrix(Ouse)

Ouse_86 <- Ouse[1:479,]
rownames(Ouse_86) <- NULL
Ouse_94 <- Ouse[480:(480+671),]
rownames(Ouse_94) <- NULL
Ouse_95 <- Ouse[(480+672):nrow(Ouse),]
rownames(Ouse_95) <- NULL

save(Ouse_86, Ouse_94, Ouse_95, file="Ouse.rda")

}

Derwent <-  read.csv("derwent.csv")
colnames(Derwent) <- c("Long Bridge", "Matlock Bath", "Chat Sworth",
"What Stand Well", "Wye@Ashford", "Amber@Wind Field Park")

save(Derwent, file="Derwent.rda")


if(FALSE) {
def.par <- par(no.readonly = TRUE)
layout(1:ncol(Derwent))
for(i in 1:ncol(Derwent)) plot(Derwent[,i], type="l", main=colnames(Derwent[i]), ylab="Gauged Flows")
par(def.par)
}


