##################### Read 19 data sets of serial-recall tests compiled by Farrell, Hurlstone, & Lewandowsky (2013, Memory & Cognition) for analysis of fill-in vs. in-fill errors #######

# each line is one trial; successive columns represent successive outputs, coding the input position of the item recalled 
# in that output; -1 = omission, -9 = extralist intrusion

rm(list=ls())

# experiment names
Ename = c('Farrell_Lewandowsky_2003_E1', 
  'Farrell_Lewandowsky_2003_E3', 
  'Farrell_Lewandowsky_2004_E1',
  'Nimmo_Lewandowsky_2006_E1',
  'Nimmo_Lewandowsky_2006_E2_aud',
  'Nimmo_Lewandowsky_2006_E2_vis',
  'Lewandowsky_Brown_Wright_Nimmo_2006_E1_quiet',
  'Lewandowsky_Brown_Wright_Nimmo_2006_E1_suppr',     
  'Lewandowsky_Geiger_Oberauer_2008_E1',
  'Lewandowsky_Geiger_Oberauer_2008_E2',
  'Lewandowsky_Geiger_Oberauer_2008_E3',
  'Lewandowsky_Geiger_Oberauer_2008_E4',
  'Lewandowsky_Farrell_2008_E2',
  'Farrell_2008_E1',
  'Farrell_2008_E2',
  'Lewandowsky_Geiger_Morrell_Oberauer_2010_E1',
  'Lewandowsky_Geiger_Morrell_Oberauer_2010_E2',
  'Lewandowsky_Geiger_Morrell_Oberauer_2010_E3',
  'Farrell_Lewandowsky_inpress_E1',
  'Farrell_Lewandowsky_inpress_E2',
  'Farrell_Lewandowsky_inpress_E3')


Data <- list()
for (experiment in 1:length(Ename)) {
  filename <- paste0(Ename[experiment], '.dat')
  data <- read.table(filename, header=F)
  names(data)[1] <- "id"
  for (serpos in 1:(dim(data)[2]-1)) names(data)[serpos+1] <- paste0("serpos", serpos)
  Data[[experiment]] <- data
}



