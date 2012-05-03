library("rEMM")

data("EMMsim")
emm <- EMM(measure="euclidean", threshold=0.1)
print("EMMsim")
print(system.time(build(emm, EMMsim_train)))

data("Derwent")
Derwent_scaled <- scale(Derwent)
emm <- EMM(measure="euclidean", threshold=3)
print("Derwent")
print(system.time(build(emm, Derwent_scaled)))


data("16S")
emm <- EMM("Kullback", threshold=0.1)
print("16S")
print(system.time(build(emm, Mollicutes16S+1)))

### reclustering

data <- synthetic_stream(k=100, d=5, n_test=0)
emm <- EMM(threshold=.01)
 build(emm, data$train, verb=TRUE)


