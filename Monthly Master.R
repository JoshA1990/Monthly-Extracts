

#                                  MONTHLY RUNS


###################################Install and load the packages###################################
#(note only need to install packages once, but need to reload library each time)
#Working with strings package
#install.packages ("stringr")
library(stringr)

#data wrangling/ analysis package
library(dplyr)

#Viewing data package
library(kableExtra) 

#data wrangling/ analysis package
library(tidyverse)

library(lubridate)

#Dates package
library(eeptools)

#visualisation
library(ggplot2)

#Set the working director for the Nationality and Org Code files.
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data")

#Read in the first two files
nationality <- read_csv("Nationality groupings.csv")
NHS_orgs <- read_csv("Org Codes NHS Digital.csv")


#Set the working directory for the ESR files
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/Joiners_leavers_FTE/Data from ESR/ESR")

#Import in all the functions
source("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Nurse code/nurse_functions.R")
source("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Midwife code/midwife_functions.R")
source("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Nursing Associate code/nursing_associate_functions.R")


#Read in the monthly 
Raw_Data_y1 <- read_csv("Staff in Post - Workforce 202310 extracted Dec 23.csv")
Raw_Data_y2 <- read_csv("Staff in Post - Workforce 202311 extracted Jan 24.csv")

#Call monthly set of nurse runs
nurse_stock()
nurse_stock_headcount()
nurse_nationality()
nurse_nationality_headcount()
nurse_trust()

#Call monthly set of midwife runs
midwife_stock()
midwife_stock_headcount()
midwife_nationality()
midwife_nationality_headcount()
midwife_trust()

#Call monthly set of nursing associate runs
nursing_associate_stock()
nursing_associate_stock_headcount()
nursing_associate_nationality()

#Read in the yearly 
Raw_Data_y1 <- read_csv("Staff in Post - Workforce 202211 extracted Jan 23.csv")
Raw_Data_y2 <- read_csv("Staff in Post - Workforce 202311 extracted Jan 24.csv")

#Call yearly set of nurse runs
nurse_nationality()
nurse_trust()

#Call yearly set of midwife runs
midwife_nationality()

#Call yearly set of nursing associate runs
nursing_associate_nationality()


#Read in cumulative
Raw_Data_y1 <- read_csv("Staff in Post - Workforce 202303 extracted May 23.csv")
Raw_Data_y2 <- read_csv("Staff in Post - Workforce 202311 extracted Jan 24.csv")

#Call cumulative set of nurse runs
nurse_nationality_cumulative()
nurse_trust()

#Call cumulative set of midwife runs
midwife_nationality()

