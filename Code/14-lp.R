# The following code is taken from the fourth chapter of the online script, which provides more detailed explanations:
# 

#-------------------------------------------------------------------#
#---------------------Install missing packages----------------------#
#-------------------------------------------------------------------#

# At the top of each script this code snippet will make sure that all required packages are installed
## ------------------------------------------------------------------------
req_packages <- c("Hmisc", "ggplot2", "car", "psych", "GPArotation","hornpa")
req_packages <- req_packages[!req_packages %in% installed.packages()]
lapply(req_packages, install.packages)


#-------------------------------------------------------------------#
#-------------------Linear Programming--------------------#
#-------------------------------------------------------------------#

