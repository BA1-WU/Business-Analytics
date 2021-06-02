# The following code is taken from the eighth chapter of the online script, which provides more detailed explanations:
# 

#-------------------------------------------------------------------#
#---------------------Install missing packages----------------------#
#-------------------------------------------------------------------#

# At the top of each script this code snippet will make sure that all required packages are installed
## ------------------------------------------------------------------------
req_packages <- c("EnvStats")
req_packages <- req_packages[!req_packages %in% installed.packages()]
lapply(req_packages, install.packages)


#-------------------------------------------------------------------#
#----------------------------Simulation-----------------------------#
#-------------------------------------------------------------------#


#1000 simulation runs
n <- 1000

# INPUT ####
# Deterministic input parameters
revenuepm <- 130 #revenue per month and unit
varcostpm <- 40  #variable costs per month and unit
marketshare1 <- 0.08

# Probabilistic input parameters
marketsize1 <- round(rnorm(n,2000000,400000),0)
RDcost <- runif(n, 600000000, 800000000)
clinicaltrialcost <- rnorm(n,150000000,30000000)
marketgrowthfactor2 <- rtri(n, 0.02, 0.06, 0.03)   #year 2
marketsharegrowthrate2 <- rtri(n, 0.15, 0.25, 0.2)
marketgrowthfactor3 <- rtri(n, 0.02, 0.06, 0.03)   #year 3
marketsharegrowthrate3 <- rtri(n, 0.15, 0.25, 0.2)
marketgrowthfactor4 <- rtri(n, 0.02, 0.06, 0.03)   #year 4
marketsharegrowthrate4 <- rtri(n, 0.15, 0.25, 0.2)
marketgrowthfactor5 <- rtri(n, 0.02, 0.06, 0.03)   #year 5
marketsharegrowthrate5 <- rtri(n, 0.15, 0.25, 0.2)

# CALCULATIONS ####
# Total Project Costs
projectcost <- RDcost + clinicaltrialcost
# Market Size per Year
marketsize2 <- marketsize1*(1+marketgrowthfactor2)
marketsize3 <- marketsize2*(1+marketgrowthfactor3)
marketsize4 <- marketsize3*(1+marketgrowthfactor4)
marketsize5 <- marketsize4*(1+marketgrowthfactor5)
# Market Share per Year
marketshare2 <- marketshare1*(1+marketsharegrowthrate2)
marketshare3 <- marketshare2*(1+marketsharegrowthrate3)
marketshare4 <- marketshare3*(1+marketsharegrowthrate4)
marketshare5 <- marketshare4*(1+marketsharegrowthrate5)
# Sales
sales1 <- marketsize1 * marketshare1
sales2 <- marketsize2 * marketshare2
sales3 <- marketsize3 * marketshare3
sales4 <- marketsize4 * marketshare4
sales5 <- marketsize5 * marketshare5
# Annual Revenue
revenue1 <- sales1 * revenuepm*12
revenue2 <- sales2 * revenuepm*12
revenue3 <- sales3 * revenuepm*12
revenue4 <- sales4 * revenuepm*12
revenue5 <- sales5 * revenuepm*12
# Annual Cost
cost1 <- sales1 * varcostpm*12
cost2 <- sales2 * varcostpm*12
cost3 <- sales3 * varcostpm*12
cost4 <- sales4 * varcostpm*12
cost5 <- sales5 * varcostpm*12
# Annual Profit
profit1 <- revenue1 - cost1
profit2 <- revenue2 - cost2
profit3 <- revenue3 - cost3
profit4 <- revenue4 - cost4
profit5 <- revenue5 - cost5

# OUTPUT ####
# Cumulative Net Profit
cumnetprofit1 <- profit1 - projectcost
cumnetprofit2 <- cumnetprofit1 + profit2
cumnetprofit3 <- cumnetprofit2 + profit3
cumnetprofit4 <- cumnetprofit3 + profit4
cumnetprofit5 <- cumnetprofit4 + profit5


# TASK 1.1 ####
#a. Mean Cumulative Net Profit after the fifth year
mean(cumnetprofit5)
hist(cumnetprofit5, xlab = "Cumulative Net Profit", 
     main = "Cumulative Net Profit after the fifth year", xaxt = "n")
axis(side=1, at=seq(min(cumnetprofit5), max(cumnetprofit5), by=500000000), las=1, 
     labels=paste(round(seq(min(cumnetprofit5), max(cumnetprofit5), by=500000000)/100000000,1), "million", sep=" "))


#b. Chances that the Cumulative Net Profit after the third year is positive
mean(cumnetprofit3)
hist(cumnetprofit3, xlab = "Cumulative Net Profit", 
     main = "Cumulative Net Profit after the third year", xaxt = "n")
axis(side=1, at=seq(min(cumnetprofit3), max(cumnetprofit3), by=500000000), las=1, 
     labels=paste(round(seq(min(cumnetprofit3), max(cumnetprofit3), by=500000000)/100000000,1), "million", sep=" "))
abline(v = 0, col = "darkred", lwd = 2)
pnorm(0, mean(cumnetprofit3), sd(cumnetprofit3), lower.tail = FALSE)


#c. 95%-confidence interval of the Cumulative Net Profit after the fifth year
z <- round(qnorm(0.975),2)
(upper.bound <- mean(cumnetprofit5) + z * (sd(cumnetprofit5)/sqrt(n)))
(lower.bound <- mean(cumnetprofit5) - z * (sd(cumnetprofit5)/sqrt(n)))


# TASK 1.2 ####
#d. sample size so that the mean cumnetprofit5 lies within a 95% confidence interval
z <- round(qnorm(0.975),2)
# the mean should lie within $10,000,000 in 95% of the cases 
A <- 10000000/2
n <- round((z^2 * sd(cumnetprofit5)^2) / A^2, 0)
n


#e. new mean cumulative net profit after the fifth year
# run calculations from top again, with new n!


# TASK 2 ####
#demand for first month
poss.demand <- c(10000, 12000, 13000, 14000, 15000, 16000, 17000, 18000)
probabilitiy <- c(0.05,0.1,0.15,0.17,0.18,0.15,0.13,0.07)

#a
#simulate demand 100,000 times
demand <- sample(poss.demand, n, TRUE, probabilitiy)

#b
#plot simulated demand to check distribution, add a normally distributed curve with the same parameters
hist(demand,6, freq = FALSE, ylim = c(0,0.0002))
curve(dnorm(x,mean(demand),sd(demand)), col="darkblue", lwd=2, add=TRUE, yaxt="n")

mean(demand)

#c
Newsvendor <- function(demand, order){
  d <- 5   #disposal cost
  p <- 100 #penalty cost
  revenue <- rep(NA, n)
  shortage <- rep(NA, n)
  leftover <- rep(NA, n)
  sold <- rep(NA, n)
  cost <- rep(NA, n)
  
  for (t in 1:n) {
    if (order > demand[t]){
      sold[t] <- demand[t]
      revenue[t] <- sold[t] * revenuepm
      cost[t] <- sold[t] * varcostpm
      shortage[t] <- 0
      leftover[t] <- order - sold[t]
    }else{
      sold[t] <- order
      revenue[t] <- sold[t] * revenuepm
      cost[t] <- sold[t] * varcostpm
      shortage[t] <- demand[t] - sold[t]
      leftover[t] <- 0
    }
  }  
  
  # SAVE RESULTS OF EACH RUN
  Simulation.data <- data.frame(
    Demand = demand,
    Sold = sold,
    Revenue = revenue,
    VarCost = cost,
    Shortage = shortage,
    Leftover = leftover,
    Disposal.Cost = leftover * d,
    Penalty.Cost = shortage * p
  )
  
  Simulation.data$Profit <- Simulation.data$Revenue - Simulation.data$Disposal.Cost - Simulation.data$Penalty.Cost - Simulation.data$VarCost 
  return(mean(Simulation.data$Profit))
}

#order either 15,000, 18,000, or 11,000 doses
order1 <- 15000
order2 <- 18000
order3 <- 11000

print(Newsvendor(demand, order1))  #Profit when ordering 15,000 doses
print(Newsvendor(demand, order2))  #Profit when ordering 18,000 doses
print(Newsvendor(demand, order3))  #Profit when ordering 11,000 doses

