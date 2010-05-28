source("../R/AllClasses.R")
source("../R/SimpleMC.R")

x <- new("SimpleMC")
smc_countMatrix(x)
smc_size(x)
smc_states(x)
str(x)

x <- SimpleMC(5)
smc_countMatrix(x)
smc_size(x)
smc_states(x)
str(x)


x <- smc_addState(x, "a")
x <- smc_addState(x, "b")
smc_countMatrix(x)
smc_size(x)
smc_states(x)

x <- smc_addState(x, c("c", "d"))
smc_countMatrix(x)
smc_size(x)


x <- smc_addState(x, c("c"))
smc_countMatrix(x)
smc_size(x)


x <- smc_addTransition(x, "a", "b")
x <- smc_addTransition(x, "a", "b")
smc_countMatrix(x)
x <- smc_addTransition(x, c("c", "c"), c("a","e"))
smc_countMatrix(x)

x <- smc_addTransition(x, c("c", "c"), c("a","b"))
smc_countMatrix(x)


x <- smc_addState(x, c("f", "h"))
smc_countMatrix(x)
smc_size(x)
smc_states(x)
str(x)





x <- smc_removeState(x, c("b", "f"))
smc_countMatrix(x)
smc_size(x)
smc_states(x)


x <- smc_addState(x, "h")
smc_countMatrix(x)


x <- smc_addTransition(x, c("a", "c"), c("a","c"), w=c(3,8))
smc_countMatrix(x)
x <- smc_removeSelfTransition(x)
smc_countMatrix(x)


x <- smc_addTransition(x, c("d","d","a","c", "c"), c("d","a","h","c","d"))
smc_countMatrix(x)
x_m <- smc_mergeStates(x, c("a", "d", "h"))
smc_countMatrix(x_m)


smc_containsState(x_m, c("a", "b", "c"))

