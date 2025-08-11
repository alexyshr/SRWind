# library(stars)
# library(sf)
# library(lubridate)
# library(xts)
# library(ncdf4)
# library(lubridate)
library(tidyverse)
# library(xlsx)
# library(stringr)
# library(ggplot2)
# library(viridis)
# library(ggthemes)
# library(tibble)
library(openxlsx)

output_path <- "../../data_srwind/out/"


# Colombia
# 1- Combined Hurricane and Non Hurricane File
#
# 1 - POT PP
#
#h_nh_c_file <- paste0(output_path, "/", "combined_h_nh_col_pot_pp.xlsx")
#
#
# 2 - POT - GPA
#h_nh_c_file <- paste0(output_path, "/", "combined_h_nh_col_pot_gpa.xlsx")
#h_nh_c_file <- paste0(output_path, "/", "combined_h_nh_col_pot_gp.xlsx")

#

# 3 - POT - LN3
#h_nh_c_file <- paste0(output_path, "/", "combined_h_nh_col_pot_ln3.xlsx")
#

# 4 - POT - GUM
h_nh_c_file <- paste0(output_path, "/", "combined_h_nh_col_pot_gum.xlsx")
#

sheet1 = "Sheathing_c_z1_e"
sheet2 = "Sheathing_c_z2_e"
sheet3 = "Sheathing_c_z3_e"

sheet4 = "Shingles_c_z1_e"
sheet5 = "Shingles_c_z2_e"
sheet6 = "Shingles_c_z3_e"



# 5 - POT - WEI
#h_nh_c_file <- paste0(output_path, "/", "combined_h_nh_col_pot_wei.xlsx")
#

# 6 - YM - GEV
#h_nh_c_file <- paste0(output_path, "/", "combined_h_nh_col_ym_gev.xlsx")
#


h_nh_c = openxlsx::read.xlsx(xlsxFile=h_nh_c_file, sheet = "combined_h_nh")

h_nh_c$station = as.integer(h_nh_c$station)
h_nh_c
colnames(h_nh_c)




#This function need to be applied to st_apply
mycuenta = 0
# Load function mcs_wind
# # Monte Carlo Simulations + LHS (latin hypercube sampling) for
#
#    WIND PRESURES (low rise enclosed building)
source("./code/function_lib.r")


mri_fixed_vel = c(10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000, "C700", "C1700", "C3000")


#
# 1- MCS_sheathing
#

#
n_samples=1000000
#
#
#

print("******** MCS_sheathing")
print(" > Sheathing Zone 1 - Enclosed")

pf_beta_sheathing = apply(h_nh_c, 1, mcs_wind,
                          wind_band_threshold = 14,
                          wind_band_location=15,
                          wind_band_scale=16,
                          wind_band_shape=16, #Not needed in POT PP
                          wind_band_events_per_year = 17, #Check this parameter for calculations
                          wind_band_station = 21,
                          number_of_samples=n_samples, 
                          number_of_rvs=7,
                          names_of_rvs = c("Kh", "Kd", "GCp", "GCpi", "R", "VNH", "VH"), #exposure, wind direction, external pressure, 
                          #internal pressure, capacity, velocity non hurricanes, velocity hurricanes
                          type_pdf_rvs = c("stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qlnorm", "lmomco::quagum", "interpolation_hurricane"), #"stats::qnorm"),
                          rvs_location= c(0.71, 1.01, -0.855, 0.150, 6.2), #LAS VALUE OF RESISTANCE need to be in kN/m2
                          rvs_cov=c(0.19, 0.093, 0.12, 0.33, 0.12),
                          names_of_deterministics = c("Kzt", "Ke"), #topographic, altitude (air density)
                          values_of_deterministics = c(1, 1),
                          bands_c = c(46:56), 
                          mri_c = c(10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000),
                          bands_h = c(33:44),
                          mri_h = c(10, 25, 50, 100, 250, 500, 700, 1000, 1700, 2500, 5000, 10000),
                          mri_fixed_vel = mri_fixed_vel,
                          bands_fixed_vel = c(46:59),
                          plot=c(NA)
                          )


# pf <- t(pf) %>%
#   as_tibble() %>%
#   setNames(paste0("pf_", mri))
#the_names = c("m_R", "m_V", "m_S", paste0("m_", mri), "cov_R", "cov_V", "cov_S", paste0("cov_", mri), "pf", paste0("pf_", mri), "b", paste0("b_", mri))

pf_beta_sheathing <- t(pf_beta_sheathing) %>% as.data.frame()
#colnames(pf_beta_sheathing) = the_names
# beta <- t(beta) %>%
#   as.data.frame(col.names=paste0("b_", mri))

# 
# 
#h_nh_c = cbind(h_nh_c, pf_beta)

#MCS_sheathing = cbind(h_nh_c[, c(7,21)], pf_beta_sheathing)
MCS_sheathing = cbind(h_nh_c, pf_beta_sheathing)
# Save the results MCS in same excel file, different sheet


# check for file existance
if (file.exists(h_nh_c_file)) {
  wb <- openxlsx::loadWorkbook(h_nh_c_file)
  
  #check sheet existance name(wb) gives name of sheet in wb
  y <- FALSE
  for (x in names(wb)){
    if (x == sheet1) {
      y <- TRUE
    }
  }
  if (y != TRUE) {
    openxlsx::addWorksheet(wb, sheet1)
  }
  
}else {
  print(paste0("Excel file ", h_nh_c_file, " must exist!"))
}

openxlsx::writeData(wb, sheet = sheet1, MCS_sheathing, colNames = T)
openxlsx::saveWorkbook(wb, h_nh_c_file, overwrite = T)
rm(wb)

print(" > Sheathing Zone 2 - Enclosed")

pf_beta_sheathing = apply(h_nh_c, 1, mcs_wind,
                          wind_band_threshold = 14,
                          wind_band_location=15,
                          wind_band_scale=16,
                          wind_band_shape=16, #Not needed in POT PP
                          wind_band_events_per_year = 17, #Check this parameter for calculations
                          wind_band_station = 21,
                          number_of_samples=n_samples, 
                          number_of_rvs=7,
                          names_of_rvs = c("Kh", "Kd", "GCp", "GCpi", "R", "VNH", "VH"), #exposure, wind direction, external pressure, 
                          #internal pressure, capacity, velocity non hurricanes, velocity hurricanes
                          type_pdf_rvs = c("stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qlnorm", "lmomco::quagum", "interpolation_hurricane"), #"stats::qnorm"),
                          rvs_location= c(0.71, 1.01, -1.615, 0.150, 6.2), #LAS VALUE OF RESISTANCE need to be in kN/m2
                          rvs_cov=c(0.19, 0.093, 0.12, 0.33, 0.12),
                          names_of_deterministics = c("Kzt", "Ke"), #topographic, altitude (air density)
                          values_of_deterministics = c(1, 1),
                          bands_c = c(46:56), 
                          mri_c = c(10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000),
                          bands_h = c(33:44),
                          mri_h = c(10, 25, 50, 100, 250, 500, 700, 1000, 1700, 2500, 5000, 10000),
                          mri_fixed_vel = mri_fixed_vel,
                          bands_fixed_vel = c(46:59),
                          plot=c(NA)
                          )


# pf <- t(pf) %>%
#   as_tibble() %>%
#   setNames(paste0("pf_", mri))
#the_names = c("m_R", "m_V", "m_S", paste0("m_", mri), "cov_R", "cov_V", "cov_S", paste0("cov_", mri), "pf", paste0("pf_", mri), "b", paste0("b_", mri))

pf_beta_sheathing <- t(pf_beta_sheathing) %>% as.data.frame()
#colnames(pf_beta_sheathing) = the_names
# beta <- t(beta) %>%
#   as.data.frame(col.names=paste0("b_", mri))

# 
# 
#h_nh_c = cbind(h_nh_c, pf_beta)

#MCS_sheathing = cbind(h_nh_c[, c(7,21)], pf_beta_sheathing)
MCS_sheathing = cbind(h_nh_c, pf_beta_sheathing)
# Save the results MCS in same excel file, different sheet


# check for file existance
if (file.exists(h_nh_c_file)) {
  wb <- openxlsx::loadWorkbook(h_nh_c_file)
  
  #check sheet existance name(wb) gives name of sheet in wb
  y <- FALSE
  for (x in names(wb)){
    if (x == sheet2) {
      y <- TRUE
    }
  }
  if (y != TRUE) {
    openxlsx::addWorksheet(wb, sheet2)
  }
  
}else {
  print(paste0("Excel file ", h_nh_c_file, " must exist!"))
}

openxlsx::writeData(wb, sheet = sheet2, MCS_sheathing, colNames = T)
openxlsx::saveWorkbook(wb, h_nh_c_file, overwrite = T)
rm(wb)

print(" > Sheathing Zone 3 - Enclosed")

pf_beta_sheathing = apply(h_nh_c, 1, mcs_wind,
                          wind_band_threshold = 14,
                          wind_band_location=15,
                          wind_band_scale=16,
                          wind_band_shape=16, #Not needed in POT PP
                          wind_band_events_per_year = 17, #Check this parameter for calculations
                          wind_band_station = 21,
                          number_of_samples=n_samples, 
                          number_of_rvs=7,
                          names_of_rvs = c("Kh", "Kd", "GCp", "GCpi", "R", "VNH", "VH"), #exposure, wind direction, external pressure, 
                          #internal pressure, capacity, velocity non hurricanes, velocity hurricanes
                          type_pdf_rvs = c("stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qlnorm", "lmomco::quagum", "interpolation_hurricane"), #"stats::qnorm"),
                          rvs_location= c(0.71, 1.01, -2.470, 0.150, 6.2), #LAS VALUE OF RESISTANCE need to be in kN/m2
                          rvs_cov=c(0.19, 0.093, 0.12, 0.33, 0.12),
                          names_of_deterministics = c("Kzt", "Ke"), #topographic, altitude (air density)
                          values_of_deterministics = c(1, 1),
                          bands_c = c(46:56), 
                          mri_c = c(10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000),
                          bands_h = c(33:44),
                          mri_h = c(10, 25, 50, 100, 250, 500, 700, 1000, 1700, 2500, 5000, 10000),
                          mri_fixed_vel = mri_fixed_vel,
                          bands_fixed_vel = c(46:59),
                          plot=c(NA)
                          )


# pf <- t(pf) %>%
#   as_tibble() %>%
#   setNames(paste0("pf_", mri))
#the_names = c("m_R", "m_V", "m_S", paste0("m_", mri), "cov_R", "cov_V", "cov_S", paste0("cov_", mri), "pf", paste0("pf_", mri), "b", paste0("b_", mri))

pf_beta_sheathing <- t(pf_beta_sheathing) %>% as.data.frame()
#colnames(pf_beta_sheathing) = the_names
# beta <- t(beta) %>%
#   as.data.frame(col.names=paste0("b_", mri))

# 
# 
#h_nh_c = cbind(h_nh_c, pf_beta)

#MCS_sheathing = cbind(h_nh_c[, c(7,21)], pf_beta_sheathing)
MCS_sheathing = cbind(h_nh_c, pf_beta_sheathing)
# Save the results MCS in same excel file, different sheet


# check for file existance
if (file.exists(h_nh_c_file)) {
  wb <- openxlsx::loadWorkbook(h_nh_c_file)
  
  #check sheet existance name(wb) gives name of sheet in wb
  y <- FALSE
  for (x in names(wb)){
    if (x == sheet3) {
      y <- TRUE
    }
  }
  if (y != TRUE) {
    openxlsx::addWorksheet(wb, sheet3)
  }
  
}else {
  print(paste0("Excel file ", h_nh_c_file, " must exist!"))
}

openxlsx::writeData(wb, sheet = sheet3, MCS_sheathing, colNames = T)
openxlsx::saveWorkbook(wb, h_nh_c_file, overwrite = T)
rm(wb)



#
# Sheathing Breached
#
# 
# print(" > Sheathing Zone 1 - Breached")
# 
# 
# mycuenta = 200
# pf_beta_sheathing = apply(h_nh_c, 1, mcs_wind,
#                           wind_band_threshold = 14,
#                           wind_band_location=15,
#                           wind_band_scale=16,
#                           pf_type_pdf ="normal", 
#                           number_of_samples=n_samples, 
#                           number_of_rvs=6,
#                           type_of_imput="combined", # "hurricanes", "non_hurricanes", "combined"
#                           names_of_rvs = c("Kh", "Kd", "GCp", "GCpi", "R", "V"), #exposure, wind direction, external pressure, internal pressure, capacity
#                           type_pdf_rvs = c("stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qlnorm", "evd::qgpd"),
#                           rvs_location= c(0.71, 1.01, -0.855, 0.460, 6.2), #LAS VALUE OF RESISTANCE need to be in kN/m2
#                           rvs_cov=c(0.19, 0.093, 0.12, 0.33, 0.12),
#                           names_of_deterministics = c("Kzt", "Ke"), #topographic, altitude (air density)
#                           values_of_deterministics = c(1, 1),
#                           bands_rl = c(46:59), 
#                           mri = mri,
#                           seed=1111)
# 
# # pf <- t(pf) %>%
# #   as_tibble() %>%
# #   setNames(paste0("pf_", mri))
# #the_names = c("m_R", "m_V", "m_S", paste0("m_", mri), "cov_R", "cov_V", "cov_S", paste0("cov_", mri), "pf", paste0("pf_", mri), "b", paste0("b_", mri))
# 
# pf_beta_sheathing <- t(pf_beta_sheathing) %>% as.data.frame()
# #colnames(pf_beta_sheathing) = the_names
# # beta <- t(beta) %>%
# #   as.data.frame(col.names=paste0("b_", mri))
# 
# # 
# # 
# #h_nh_c = cbind(h_nh_c, pf_beta)
# 
# #MCS_sheathing = cbind(h_nh_c[, c(7,21)], pf_beta_sheathing)
# MCS_sheathing = cbind(h_nh_c, pf_beta_sheathing)
# # Save the results MCS in same excel file, different sheet
# 
# 
# # check for file existance
# if (file.exists(h_nh_c_file)) {
#   wb <- openxlsx::loadWorkbook(h_nh_c_file)
#   
#   #check sheet existance name(wb) gives name of sheet in wb
#   y <- FALSE
#   for (x in names(wb)){
#     if (x == "Sheathing_z1_b") {
#       y <- TRUE
#     }
#   }
#   if (y != TRUE) {
#     openxlsx::addWorksheet(wb, "Sheathing_z1_b")
#   }
#   
# }else {
#   print(paste0("Excel file ", h_nh_c_file, " must exist!"))
# }
# 
# openxlsx::writeData(wb, sheet = "Sheathing_z1_b", MCS_sheathing, colNames = T)
# openxlsx::saveWorkbook(wb, h_nh_c_file, overwrite = T)
# 
# 
# print(" > Sheathing Zone 2 - Breached")
# 
# mycuenta = 250
# pf_beta_sheathing = apply(h_nh_c, 1, mcs_wind,
#                           wind_band_threshold = 14,
#                           wind_band_location=15,
#                           wind_band_scale=16,
#                           pf_type_pdf ="normal", 
#                           number_of_samples=n_samples, 
#                           number_of_rvs=6,
#                           type_of_imput="combined", # "hurricanes", "non_hurricanes", "combined"
#                           names_of_rvs = c("Kh", "Kd", "GCp", "GCpi", "R", "V"), #exposure, wind direction, external pressure, internal pressure, capacity
#                           type_pdf_rvs = c("stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qlnorm", "evd::qgpd"),
#                           rvs_location= c(0.71, 1.01, -1.615, 0.460, 6.2), #LAS VALUE OF RESISTANCE need to be in kN/m2
#                           rvs_cov=c(0.19, 0.093, 0.12, 0.33, 0.12),
#                           names_of_deterministics = c("Kzt", "Ke"), #topographic, altitude (air density)
#                           values_of_deterministics = c(1, 1),
#                           bands_rl = c(46:59), 
#                           mri = mri,
#                           seed=1111)
# 
# # pf <- t(pf) %>%
# #   as_tibble() %>%
# #   setNames(paste0("pf_", mri))
# #the_names = c("m_R", "m_V", "m_S", paste0("m_", mri), "cov_R", "cov_V", "cov_S", paste0("cov_", mri), "pf", paste0("pf_", mri), "b", paste0("b_", mri))
# 
# pf_beta_sheathing <- t(pf_beta_sheathing) %>% as.data.frame()
# #colnames(pf_beta_sheathing) = the_names
# # beta <- t(beta) %>%
# #   as.data.frame(col.names=paste0("b_", mri))
# 
# # 
# # 
# #h_nh_c = cbind(h_nh_c, pf_beta)
# 
# #MCS_sheathing = cbind(h_nh_c[, c(7,21)], pf_beta_sheathing)
# MCS_sheathing = cbind(h_nh_c, pf_beta_sheathing)
# # Save the results MCS in same excel file, different sheet
# 
# 
# # check for file existance
# if (file.exists(h_nh_c_file)) {
#   wb <- openxlsx::loadWorkbook(h_nh_c_file)
#   
#   #check sheet existance name(wb) gives name of sheet in wb
#   y <- FALSE
#   for (x in names(wb)){
#     if (x == "Sheathing_z2_b") {
#       y <- TRUE
#     }
#   }
#   if (y != TRUE) {
#     openxlsx::addWorksheet(wb, "Sheathing_z2_b")
#   }
#   
# }else {
#   print(paste0("Excel file ", h_nh_c_file, " must exist!"))
# }
# 
# openxlsx::writeData(wb, sheet = "Sheathing_z2_b", MCS_sheathing, colNames = T)
# openxlsx::saveWorkbook(wb, h_nh_c_file, overwrite = T)
# 
# print(" > Sheathing Zone 3 - Breached")
# 
# 
# mycuenta = 300
# 
# pf_beta_sheathing = apply(h_nh_c, 1, mcs_wind,
#                           wind_band_threshold = 14,
#                           wind_band_location=15,
#                           wind_band_scale=16,
#                           pf_type_pdf ="normal", 
#                           number_of_samples=n_samples, 
#                           number_of_rvs=6,
#                           type_of_imput="combined", # "hurricanes", "non_hurricanes", "combined"
#                           names_of_rvs = c("Kh", "Kd", "GCp", "GCpi", "R", "V"), #exposure, wind direction, external pressure, internal pressure, capacity
#                           type_pdf_rvs = c("stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qlnorm", "evd::qgpd"),
#                           rvs_location= c(0.71, 1.01, -2.470, 0.460, 6.2), #LAS VALUE OF RESISTANCE need to be in kN/m2
#                           rvs_cov=      c(0.19, 0.093, 0.12, 0.33, 0.12),
#                           names_of_deterministics = c("Kzt", "Ke"), #topographic, altitude (air density)
#                           values_of_deterministics = c(1, 1),
#                           bands_rl = c(46:59), 
#                           mri = mri,
#                           seed=1111)
# 
# # pf <- t(pf) %>%
# #   as_tibble() %>%
# #   setNames(paste0("pf_", mri))
# #the_names = c("m_R", "m_V", "m_S", paste0("m_", mri), "cov_R", "cov_V", "cov_S", paste0("cov_", mri), "pf", paste0("pf_", mri), "b", paste0("b_", mri))
# 
# pf_beta_sheathing <- t(pf_beta_sheathing) %>% as.data.frame()
# #colnames(pf_beta_sheathing) = the_names
# # beta <- t(beta) %>%
# #   as.data.frame(col.names=paste0("b_", mri))
# 
# # 
# # 
# #h_nh_c = cbind(h_nh_c, pf_beta)
# 
# #MCS_sheathing = cbind(h_nh_c[, c(7,21)], pf_beta_sheathing)
# MCS_sheathing = cbind(h_nh_c, pf_beta_sheathing)
# # Save the results MCS in same excel file, different sheet
# 
# 
# # check for file existance
# if (file.exists(h_nh_c_file)) {
#   wb <- openxlsx::loadWorkbook(h_nh_c_file)
#   
#   #check sheet existance name(wb) gives name of sheet in wb
#   y <- FALSE
#   for (x in names(wb)){
#     if (x == "Sheathing_z3_b") {
#       y <- TRUE
#     }
#   }
#   if (y != TRUE) {
#     openxlsx::addWorksheet(wb, "Sheathing_z3_b")
#   }
#   
# }else {
#   print(paste0("Excel file ", h_nh_c_file, " must exist!"))
# }
# 
# openxlsx::writeData(wb, sheet = "Sheathing_z3_b", MCS_sheathing, colNames = T)
# openxlsx::saveWorkbook(wb, h_nh_c_file, overwrite = T)
# 





#
# 2- MCS_shingles
#
#
n_samples=1000000
#
#
#
print("******** MCS_shingles")
print(" > Shingles Zone 1 - Enclosed")

pf_beta_shingles = apply(h_nh_c, 1, mcs_wind,
                          wind_band_threshold = 14,
                          wind_band_location=15,
                          wind_band_scale=16,
                          wind_band_shape=16, #Not needed in POT PP
                          wind_band_events_per_year = 17, #Check this parameter for calculations
                          wind_band_station = 21,
                          number_of_samples=n_samples, 
                          number_of_rvs=7,
                          names_of_rvs = c("Kh", "Kd", "GCp", "GCpi", "R", "VNH", "VH"), #exposure, wind direction, external pressure, 
                          #internal pressure, capacity, velocity non hurricanes, velocity hurricanes
                          type_pdf_rvs = c("stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "lmomco::quagum", "interpolation_hurricane"), #"stats::qnorm"),
                          rvs_location= c(0.71, 1.01, -0.855, 0.150, 3.35), #LAS VALUE OF RESISTANCE need to be in kN/m2
                          rvs_cov=c(0.19, 0.093, 0.12, 0.33, 0.19),
                          names_of_deterministics = c("Kzt", "Ke"), #topographic, altitude (air density)
                          values_of_deterministics = c(1, 1),
                          bands_c = c(46:56), 
                          mri_c = c(10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000),
                          bands_h = c(33:44),
                          mri_h = c(10, 25, 50, 100, 250, 500, 700, 1000, 1700, 2500, 5000, 10000),
                          mri_fixed_vel = mri_fixed_vel,
                          bands_fixed_vel = c(46:59),
                          plot=c(NA)
                          )


# pf <- t(pf) %>%
#   as_tibble() %>%
#   setNames(paste0("pf_", mri))
#the_names = c(paste0("pf_", mri), "pf", paste0("b_", mri), "b") 
pf_beta_shingles <- t(pf_beta_shingles) %>% as.data.frame()
#colnames(pf_beta_shingles) = the_names
# beta <- t(beta) %>%
#   as.data.frame(col.names=paste0("b_", mri))

# 
# 
#h_nh_c = cbind(h_nh_c, pf_beta)

#MCS_shingles = cbind(h_nh_c[, c(7,21)], pf_beta_shingles)
MCS_shingles = cbind(h_nh_c, pf_beta_shingles)

# Save the results MCS in same excel file, different sheet

# check for file existance
if (file.exists(h_nh_c_file)) {
  wb <- openxlsx::loadWorkbook(h_nh_c_file)
  
  #check sheet existance name(wb) gives name of sheet in wb
  y <- FALSE
  for (x in names(wb)){
    if (x == sheet4) {
      y <- TRUE
    }
  }
  if (y != TRUE) {
    openxlsx::addWorksheet(wb, sheet4)
  }
  
}else {
  print(paste0("Excel file ", h_nh_c_file, " must exist!"))
}


openxlsx::writeData(wb, sheet = sheet4, MCS_shingles, colNames = T)
openxlsx::saveWorkbook(wb, h_nh_c_file, overwrite = T)
rm(wb)


print(" > Shingles Zone 2 - Enclosed")

pf_beta_shingles = apply(h_nh_c, 1, mcs_wind,
                         wind_band_threshold = 14,
                         wind_band_location=15,
                         wind_band_scale=16,
                         wind_band_shape=16, #Not needed in POT PP
                         wind_band_events_per_year = 17, #Check this parameter for calculations
                         wind_band_station = 21,
                         number_of_samples=n_samples, 
                         number_of_rvs=7,
                         names_of_rvs = c("Kh", "Kd", "GCp", "GCpi", "R", "VNH", "VH"), #exposure, wind direction, external pressure, 
                         #internal pressure, capacity, velocity non hurricanes, velocity hurricanes
                         type_pdf_rvs = c("stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "lmomco::quagum", "interpolation_hurricane"), #"stats::qnorm"),
                         rvs_location= c(0.71, 1.01, -1.615, 0.150, 3.35), #LAS VALUE OF RESISTANCE need to be in kN/m2
                         rvs_cov=c(0.19, 0.093, 0.12, 0.33, 0.19),
                         names_of_deterministics = c("Kzt", "Ke"), #topographic, altitude (air density)
                         values_of_deterministics = c(1, 1),
                         bands_c = c(46:56), 
                         mri_c = c(10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000),
                         bands_h = c(33:44),
                         mri_h = c(10, 25, 50, 100, 250, 500, 700, 1000, 1700, 2500, 5000, 10000),
                         mri_fixed_vel = mri_fixed_vel,
                         bands_fixed_vel = c(46:59),
                         plot=c(NA)
                        )


# pf <- t(pf) %>%
#   as_tibble() %>%
#   setNames(paste0("pf_", mri))
#the_names = c(paste0("pf_", mri), "pf", paste0("b_", mri), "b") 
pf_beta_shingles <- t(pf_beta_shingles) %>% as.data.frame()
#colnames(pf_beta_shingles) = the_names
# beta <- t(beta) %>%
#   as.data.frame(col.names=paste0("b_", mri))

# 
# 
#h_nh_c = cbind(h_nh_c, pf_beta)

#MCS_shingles = cbind(h_nh_c[, c(7,21)], pf_beta_shingles)
MCS_shingles = cbind(h_nh_c, pf_beta_shingles)

# Save the results MCS in same excel file, different sheet

# check for file existance
if (file.exists(h_nh_c_file)) {
  wb <- openxlsx::loadWorkbook(h_nh_c_file)
  
  #check sheet existance name(wb) gives name of sheet in wb
  y <- FALSE
  for (x in names(wb)){
    if (x == sheet5) {
      y <- TRUE
    }
  }
  if (y != TRUE) {
    openxlsx::addWorksheet(wb, sheet5)
  }
  
}else {
  print(paste0("Excel file ", h_nh_c_file, " must exist!"))
}


openxlsx::writeData(wb, sheet = sheet5, MCS_shingles, colNames = T)
openxlsx::saveWorkbook(wb, h_nh_c_file, overwrite = T)
rm(wb)

print(" > Shingles Zone 3 - Enclosed")

pf_beta_shingles = apply(h_nh_c, 1, mcs_wind,
                         wind_band_threshold = 14,
                         wind_band_location=15,
                         wind_band_scale=16,
                         wind_band_shape=16, #Not needed in POT PP
                         wind_band_events_per_year = 17, #Check this parameter for calculations
                         wind_band_station = 21,
                         number_of_samples=n_samples, 
                         number_of_rvs=7,
                         names_of_rvs = c("Kh", "Kd", "GCp", "GCpi", "R", "VNH", "VH"), #exposure, wind direction, external pressure, 
                         #internal pressure, capacity, velocity non hurricanes, velocity hurricanes
                         type_pdf_rvs = c("stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "lmomco::quagum", "interpolation_hurricane"), #"stats::qnorm"),
                         rvs_location= c(0.71, 1.01, -2.470, 0.150, 3.35), #LAS VALUE OF RESISTANCE need to be in kN/m2
                         rvs_cov=c(0.19, 0.093, 0.12, 0.33, 0.19),
                         names_of_deterministics = c("Kzt", "Ke"), #topographic, altitude (air density)
                         values_of_deterministics = c(1, 1),
                         bands_c = c(46:56), 
                         mri_c = c(10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000),
                         bands_h = c(33:44),
                         mri_h = c(10, 25, 50, 100, 250, 500, 700, 1000, 1700, 2500, 5000, 10000),
                         mri_fixed_vel = mri_fixed_vel,
                         bands_fixed_vel = c(46:59),
                         plot=c(NA)
                        )

# pf <- t(pf) %>%
#   as_tibble() %>%
#   setNames(paste0("pf_", mri))
#the_names = c(paste0("pf_", mri), "pf", paste0("b_", mri), "b") 
pf_beta_shingles <- t(pf_beta_shingles) %>% as.data.frame()
#colnames(pf_beta_shingles) = the_names
# beta <- t(beta) %>%
#   as.data.frame(col.names=paste0("b_", mri))

# 
# 
#h_nh_c = cbind(h_nh_c, pf_beta)

#MCS_shingles = cbind(h_nh_c[, c(7,21)], pf_beta_shingles)
MCS_shingles = cbind(h_nh_c, pf_beta_shingles)

# Save the results MCS in same excel file, different sheet

# check for file existance
if (file.exists(h_nh_c_file)) {
  wb <- openxlsx::loadWorkbook(h_nh_c_file)
  
  #check sheet existance name(wb) gives name of sheet in wb
  y <- FALSE
  for (x in names(wb)){
    if (x == sheet6) {
      y <- TRUE
    }
  }
  if (y != TRUE) {
    openxlsx::addWorksheet(wb, sheet6)
  }
  
}else {
  print(paste0("Excel file ", h_nh_c_file, " must exist!"))
}


openxlsx::writeData(wb, sheet = sheet6, MCS_shingles, colNames = T)
openxlsx::saveWorkbook(wb, h_nh_c_file, overwrite = T)
rm(wb)

#
# Shingles - Breached
#


# print(" > Shingles Zone 1 - Breached")
# 
# mycuenta = 1150
# pf_beta_shingles = apply(h_nh_c, 1, mcs_wind,
#                          wind_band_threshold = 14,
#                          wind_band_location=15,
#                          wind_band_scale=16,
#                          pf_type_pdf ="normal", 
#                          number_of_samples=n_samples, 
#                          number_of_rvs=6,
#                          type_of_imput="combined", # "hurricanes", "non_hurricanes", "combined"
#                          names_of_rvs = c("Kh", "Kd", "GCp", "GCpi", "R", "V"), #exposure, wind direction, external pressure, internal pressure, capacity
#                          type_pdf_rvs = c("stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "evd::qgpd"),
#                          rvs_location= c(0.71, 1.01, -0.855, 0.460, 3.35), # VALUE OF RESISTANCE need to be in kN/m2
#                          rvs_cov=c(0.19, 0.093, 0.12, 0.33, 0.19),
#                          names_of_deterministics = c("Kzt", "Ke"), #topographic, altitude (air density)
#                          values_of_deterministics = c(1, 1),
#                          bands_rl = c(46:59), 
#                          mri = mri,
#                          seed=1111)
# 
# # pf <- t(pf) %>%
# #   as_tibble() %>%
# #   setNames(paste0("pf_", mri))
# #the_names = c(paste0("pf_", mri), "pf", paste0("b_", mri), "b") 
# pf_beta_shingles <- t(pf_beta_shingles) %>% as.data.frame()
# #colnames(pf_beta_shingles) = the_names
# # beta <- t(beta) %>%
# #   as.data.frame(col.names=paste0("b_", mri))
# 
# # 
# # 
# #h_nh_c = cbind(h_nh_c, pf_beta)
# 
# #MCS_shingles = cbind(h_nh_c[, c(7,21)], pf_beta_shingles)
# MCS_shingles = cbind(h_nh_c, pf_beta_shingles)
# 
# # Save the results MCS in same excel file, different sheet
# 
# # check for file existance
# if (file.exists(h_nh_c_file)) {
#   wb <- openxlsx::loadWorkbook(h_nh_c_file)
#   
#   #check sheet existance name(wb) gives name of sheet in wb
#   y <- FALSE
#   for (x in names(wb)){
#     if (x == "Shingles_z1_b") {
#       y <- TRUE
#     }
#   }
#   if (y != TRUE) {
#     openxlsx::addWorksheet(wb, "Shingles_z1_b")
#   }
#   
# }else {
#   print(paste0("Excel file ", h_nh_c_file, " must exist!"))
# }
# 
# 
# openxlsx::writeData(wb, sheet = "Shingles_z1_b", MCS_shingles, colNames = T)
# openxlsx::saveWorkbook(wb, h_nh_c_file, overwrite = T)
# 
# 
# 
# print(" > Shingles Zone 2 - Breached")
# 
# mycuenta = 1200
# pf_beta_shingles = apply(h_nh_c, 1, mcs_wind,
#                          wind_band_threshold = 14,
#                          wind_band_location=15,
#                          wind_band_scale=16,
#                          pf_type_pdf ="normal", 
#                          number_of_samples=n_samples, 
#                          number_of_rvs=6,
#                          type_of_imput="combined", # "hurricanes", "non_hurricanes", "combined"
#                          names_of_rvs = c("Kh", "Kd", "GCp", "GCpi", "R", "V"), #exposure, wind direction, external pressure, internal pressure, capacity
#                          type_pdf_rvs = c("stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "evd::qgpd"),
#                          rvs_location= c(0.71, 1.01, -1.615, 0.460, 3.35), # VALUE OF RESISTANCE need to be in kN/m2
#                          rvs_cov=c(0.19, 0.093, 0.12, 0.33, 0.19),
#                          names_of_deterministics = c("Kzt", "Ke"), #topographic, altitude (air density)
#                          values_of_deterministics = c(1, 1),
#                          bands_rl = c(46:59), 
#                          mri = mri,
#                          seed=1111)
# 
# # pf <- t(pf) %>%
# #   as_tibble() %>%
# #   setNames(paste0("pf_", mri))
# #the_names = c(paste0("pf_", mri), "pf", paste0("b_", mri), "b") 
# pf_beta_shingles <- t(pf_beta_shingles) %>% as.data.frame()
# #colnames(pf_beta_shingles) = the_names
# # beta <- t(beta) %>%
# #   as.data.frame(col.names=paste0("b_", mri))
# 
# # 
# # 
# #h_nh_c = cbind(h_nh_c, pf_beta)
# 
# #MCS_shingles = cbind(h_nh_c[, c(7,21)], pf_beta_shingles)
# MCS_shingles = cbind(h_nh_c, pf_beta_shingles)
# 
# # Save the results MCS in same excel file, different sheet
# 
# # check for file existance
# if (file.exists(h_nh_c_file)) {
#   wb <- openxlsx::loadWorkbook(h_nh_c_file)
#   
#   #check sheet existance name(wb) gives name of sheet in wb
#   y <- FALSE
#   for (x in names(wb)){
#     if (x == "Shingles_z2_b") {
#       y <- TRUE
#     }
#   }
#   if (y != TRUE) {
#     openxlsx::addWorksheet(wb, "Shingles_z2_b")
#   }
#   
# }else {
#   print(paste0("Excel file ", h_nh_c_file, " must exist!"))
# }
# 
# 
# openxlsx::writeData(wb, sheet = "Shingles_z2_b", MCS_shingles, colNames = T)
# openxlsx::saveWorkbook(wb, h_nh_c_file, overwrite = T)


# print(" > Shingles Zone 3 - Breached")
# 
# mycuenta = 1250
# pf_beta_shingles = apply(h_nh_c, 1, mcs_wind,
#                          wind_band_threshold = 14,
#                          wind_band_location=15,
#                          wind_band_scale=16,
#                          pf_type_pdf ="normal", 
#                          number_of_samples=n_samples, 
#                          number_of_rvs=6,
#                          type_of_imput="combined", # "hurricanes", "non_hurricanes", "combined"
#                          names_of_rvs = c("Kh", "Kd", "GCp", "GCpi", "R", "V"), #exposure, wind direction, external pressure, internal pressure, capacity
#                          type_pdf_rvs = c("stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "evd::qgpd"),
#                          rvs_location= c(0.71, 1.01, -2.470, 0.460, 3.35), # VALUE OF RESISTANCE need to be in kN/m2
#                          rvs_cov=c(0.19, 0.093, 0.12, 0.33, 0.19),
#                          names_of_deterministics = c("Kzt", "Ke"), #topographic, altitude (air density)
#                          values_of_deterministics = c(1, 1),
#                          bands_rl = c(46:59), 
#                          mri = mri,
#                          seed=1111)
# 
# # pf <- t(pf) %>%
# #   as_tibble() %>%
# #   setNames(paste0("pf_", mri))
# #the_names = c(paste0("pf_", mri), "pf", paste0("b_", mri), "b") 
# pf_beta_shingles <- t(pf_beta_shingles) %>% as.data.frame()
# #colnames(pf_beta_shingles) = the_names
# # beta <- t(beta) %>%
# #   as.data.frame(col.names=paste0("b_", mri))
# 
# # 
# # 
# #h_nh_c = cbind(h_nh_c, pf_beta)
# 
# #MCS_shingles = cbind(h_nh_c[, c(7,21)], pf_beta_shingles)
# MCS_shingles = cbind(h_nh_c, pf_beta_shingles)
# 
# # Save the results MCS in same excel file, different sheet
# 
# # check for file existance
# if (file.exists(h_nh_c_file)) {
#   wb <- openxlsx::loadWorkbook(h_nh_c_file)
#   
#   #check sheet existance name(wb) gives name of sheet in wb
#   y <- FALSE
#   for (x in names(wb)){
#     if (x == "Shingles_z3_b") {
#       y <- TRUE
#     }
#   }
#   if (y != TRUE) {
#     openxlsx::addWorksheet(wb, "Shingles_z3_b")
#   }
#   
# }else {
#   print(paste0("Excel file ", h_nh_c_file, " must exist!"))
# }
# 
# 
# openxlsx::writeData(wb, sheet = "Shingles_z3_b", MCS_shingles, colNames = T)
# openxlsx::saveWorkbook(wb, h_nh_c_file, overwrite = T)

#fn_out1 <- openxlsx::createWorkbook()
#openxlsx::addWorksheet(fn_out1, "combined_h_nh")
# writeData(fn_out1, sheet = "combined_h_nh", x = h_nh_c)
# saveWorkbook(fn_out1, exel_file, overwrite = TRUE)
# 
# m = 6.2
# s = 0.12*6.2
# location <- log(m^2 / sqrt(s^2 + m^2))
# shape <- sqrt(log(1 + (s^2 / m^2)))
# print(paste("location:", location))
# print(paste("shape:", shape))
# draws3 <- rlnorm(n=1000000, location, shape)
# mean(draws3)
# sd(draws3)
# 
# qlnorm(0.5, meanlog=location, sdlog=shape)
# #par("mar")
# #par(mar=c(1,1,1,1))
# curve(dlnorm(x, meanlog=location, sdlog=shape), from=0, to=20)#

# scale = 3.273322
# location = 58
# shape = 0.00001
# 
# evd::qgpd(0.269, loc=58, scale=3.273322, shape=0.00001)
