library("rEMM")

data("EMMsim")
emm <- EMM(measure="euclidean", threshold=0.1)
system.time(build(emm, EMMsim_train))

data(Derwent)
Derwent_scaled <- scale(Derwent)
emm <- EMM(measure="euclidean", threshold=3)
system.time(build(emm, Derwent_scaled))


data("16S")
emm <- EMM("Kullback", threshold=0.1)
system.time(build(emm, Mollicutes16S+1))

