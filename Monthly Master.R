

#                                  MONTHLY RUNS


###################################Install and load the packages###################################
#(note only need to install packages once, but need to reload library each time)
#data wrangling/ analysis package
library(tidyverse)


#Set the working director for the Nationality and Org Code files.
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data")

#Read in the first two files
nationality <- read_csv("Nationality groupings.csv")
NHS_orgs <- read_csv("Org Codes NHS Digital.csv")


#Set the working directory for the ESR files
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/All")

#Import in all the functions
source("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Monthly-Extracts/nurse_monthly_functions.R")
source("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Monthly-Extracts/midwife_monthly_functions.R")
source("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Monthly-Extracts/nurse_associate_monthly_functions.R")


#Read in the monthly 
Raw_Data_y1 <- read_csv("Staff in Post - Workforce 202403 extracted May 24.csv")
Raw_Data_y2 <- read_csv("Staff in Post - Workforce 202404 extracted Jun 24.csv")

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
Raw_Data_y1 <- read_csv("Staff in Post - Workforce 202304 extracted Jun 23.csv")
Raw_Data_y2 <- read_csv("Staff in Post - Workforce 202404 extracted Jun 24.csv")

#Call yearly set of nurse runs
nurse_nationality()
nurse_trust()

#Call yearly set of midwife runs
midwife_nationality()

#Call yearly set of nursing associate runs
nursing_associate_nationality()


#Read in cumulative
Raw_Data_y1 <- read_csv("Staff in Post - Workforce 202403 extracted May 24.csv")
Raw_Data_y2 <- read_csv("Staff in Post - Workforce 202404 extracted Jun 24.csv")

#Call cumulative set of nurse runs
nurse_nationality_cumulative()
nurse_trust()

#Call cumulative set of midwife runs
midwife_nationality()

