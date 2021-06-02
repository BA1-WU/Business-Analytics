# The following code is taken from the nineth chapter of the online script, which provides more detailed explanations:
# 

#-------------------------------------------------------------------#
#---------------------Install missing packages----------------------#
#-------------------------------------------------------------------#

# At the top of each script this code snippet will make sure that all required packages are installed
## ------------------------------------------------------------------------
req_packages <- c("lpsolve")
req_packages <- req_packages[!req_packages %in% installed.packages()]
lapply(req_packages, install.packages)


#-------------------------------------------------------------------#
#------------------------Linear Programming-------------------------#
#-------------------------------------------------------------------#


### Introduction ####
# objective
objective<-c(3,5)

# constraints
constraint.lhs <- matrix(c(3, 0, 0, 2, 3, 2), nrow = 3, byrow = TRUE)
constraint.direction <- c("<=","<=","<=")
constraint.rhs <- c(4,12,18)

# optimization
lp(direction="max", objective, constraint.lhs, constraint.direction, constraint.rhs)

# display final values
lp(direction="max", objective, constraint.lhs, constraint.direction, 
   constraint.rhs)$solution

# display shadow prices and decision variables
lp("max", objective, constraint.lhs, constraint.direction, 
   constraint.rhs, compute.sens=TRUE)$duals



### Transportation Problem ####

# cost matrix
costs<-matrix(c(10,2,20,11,12,7,9,20,4,14,16,18), nrow = 3, byrow = TRUE)

colnames(costs) <- c("Customer 1", "Customer 2", "Customer 3", "Customer 4")
rownames(costs) <- c("Supplier 1", "Supplier 2", "Supplier 3")

# constraints
row.signs <- rep("<=", 3)
row.rhs <- c(15, 25, 10)

col.signs <- rep(">=", 4)
col.rhs <- c(5, 15, 15, 15)

# optimization
lp.transport(costs, "min", row.signs, row.rhs, col.signs, col.rhs)
lp.transport(costs, "min", row.signs, row.rhs, col.signs, col.rhs)$solution