
# Code to append extracts onto very large csv files
# Specifically used for dashboard Trust files

#Import tidyverse for any other wrangling

library(tidyverse)

# Set and change working directory for the base file to be imported
getwd()
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/Dashboard/Data for dashboard/Backup")

# Read base files in

#Put name of file in between the " " - the code will do the rest
###  DO NOT CHANGE THESE -------------------------------------------------------
trust_cumulative <- "joiner_leaver_cumulative_trust_master"

trust_annual <- "joiner_leaver_annual_trust_master"

# Code to use string and make the read_csv

cumulative_base <- read_csv(paste0(trust_cumulative,".csv"))
  
annual_base <- read_csv(paste0(trust_annual,".csv"))

#-------------------------------------------------------------------------------
# Import new extracts in that we will be joining on

# Cumulative first, only change the file name

setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/Nurses/Trust Cumulative")

cumulative_file <- "Nurse 20240311 joiners_leavers_trust 2023-MAR to 2024-JAN "

cumulative_add <- read_csv(paste0(cumulative_file,".csv"))


#Repeat for annual

setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/Nurses/Trust annual")

annual_file <- "Nurse 20240311 joiners_leavers_trust 2023-JAN to 2024-JAN "

annual_add <- read_csv(paste0(annual_file,".csv"))

################################################################################

# Bind the new extracts to the old

cumulative_appended <- cumulative_base %>% rbind(cumulative_add)
annual_appended <- annual_base %>% rbind(annual_add)


################################################################################

# Write new files to folder

write_csv(cumulative_appended,"C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/Dashboard/Data for dashboard/Backup/joiner_leaver_cumulative_trust_master.csv")
write_csv(annual_appended,"C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/Dashboard/Data for dashboard/Backup/joiner_leaver_annual_trust_master.csv")


unique(annual_appended$Date_from)
unique(cumulative_appended$Date_from)
