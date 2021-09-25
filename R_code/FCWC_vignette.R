### vignette
source('~/Documents/R/Github/FCWC-data-processing/R_code/aquatroll_processing.R')

setwd('~/Documents/R/Github/FCWC-data-processing/data/2019-10-15')
files <- list.files()

for(i in 1:length(files)){
  input <- files[i]
  file_sum <- summary_aquatroll(input)
  out <- data_extract_aquatroll(input)
  output <- interp_aquatroll(out,spar=.6)
}
