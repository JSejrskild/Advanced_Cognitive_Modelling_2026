
print(getwd())
setwd("../1_matching_pennies")
print(getwd())
print(list.files("."))
# import internal
source("src/agents.R")

x <- RandomAgent_f(100)


