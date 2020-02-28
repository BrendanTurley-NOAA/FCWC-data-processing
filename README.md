# FCWC-data-processing
Simple code to process data coming from an Insitu Aqua TROLL 600.

The original files are exported as a html. I am using Insitu's html to csv parser because I am most used to csv format. The eventual goal is to code a parser in R and have the processing occur in a single script, or at least with minimal number different platforms/languages. The current code does not perform any quality control, the code of which is current in development. Instead of QA/QC, the data are instead box-car averaged into 1 meter bins.

The resulting figure output is in development and needs to be beta tested with the end users (i.e., the watermen who are collecting the data).
