# FCWC-data-processing

The code in this repository are used to process data collected by an [In-Situ Aqua TROLL 600 Multiparameter sonde](https://in-situ.com/us/aqua-troll-600-multiparameter-sonde). The data are collected by non-profit called Florida Commercial Watermen's Conservation ([FCWC](https://floridawatermen.org)) and is a group of commercial fishermen located in southwest Florida and then transmitted to scientists at NOAA's Southeast Fisheries Science Center ([SEFSC](https://www.fisheries.noaa.gov/about/southeast-fisheries-science-center)), NOAA's Atlantic Oceanographic and Meteorological Laboratory ([AOML](https://www.aoml.noaa.gov/)), and University of Miami's Cooperative Institute for Marine and Atmospheric Studies ([CIMAS](https://cimas.rsmas.miami.edu/)). The data undergo quality assurance and quality control (QA/QC) and then visualized products are sent back to the collaborating fishermen. The data products are a work in progress with the goal of being responsive to the needs of the fishermen and to produce informative data products that can be used for planning purposes.

The original files are directly uploaded to GoogleDrive by the [VuSitu App](https://in-situ.com/us/vusitu-app) as an htm. The R functions in this repository are designed to perform a few different tasks: 

* **aquatroll_fields** a helper function that just lists the standard parameters reported in aquatroll htm files
* **summary_aquatroll** takes an .htm or .csv and returns a summary data.frame of the data including if parameters were missing from the input file or if there was sensor drop out, both of which has been an issue.
* **data_extract_aquatroll** takes an .htm or .csv and extract all the raw data and put it into a data.frame
* **interp_aquatroll** takes the data.frame output of **data_extract_aquatroll** and smooths and interpolates selected parameters for downstream visualization
* **bottom_finder** is useful for plotting profiles to find bottom for each profile
* **breaks** makes handy breaks for color palettes
* **running** calculates a moving linear regression slope and moving average

The current code does not perform explicit quality control on the data; however, the **summary_aquatroll** function will return missing parameters from a list of expected parameters and a list of parameters that dropped out. Drop out is when the parameter has some measurements but not for the whole time the unit was recording. This can happen when there is a bad connection or other sensor failure. If this happens, it is recommended that In-Situ technical support be contacted for further help. Instead of explicit QA/QC of the raw data, the data are instead box-car averaged into 1 meter bins. The resulting figure output is in development and needs to be beta tested with the end users (i.e., the watermen who are collecting the data).


---

# Details

The overall objective of the code is produce a visual data product that can be distributed to various stakeholders, including the fishermen that collect the data.

![alt text](https://github.com/imaginaryfish/FCWC-data-processing/blob/master/figures/FCWC_2019-Oct-12.png "Example data product")

---

To do 

* make vignette on how to use functions with example data using markdown
* make shiny app to display data https://shiny.rstudio.com/
* update .md using: https://r4ds.had.co.nz/r-markdown-formats.html or https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet or https://rmarkdown.rstudio.com/ examples

