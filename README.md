# Loneliness, mental health, and the Work-from-home revoution

This package replicates the main results in the paper by the same name (Ben Cowan and Joe Spearing).

## Description

The code uploads the Understanding Society data set, categorizes occupations as teleworkable or non-teleworkable using the Dingel and Neimann (2020) definition, runs the differences-in-differences estimators from the paper, and then outputs the main results.
All data are included except the Understanding Society data (see below). 
The file "occupations_by_remote_workability" produces our final classification of occupations by whether they are teleworkable (taking the Dingel and Neimann occupations where we can and manually classifying the rest).

## Getting Started

### Packages

The required packages are listed at the beginning of each script with the code that says "library(*packagename*)". They will need to be installed using "install.packges(@packagename*)".

### Data

* All data except the Understanding Society data are in the repository
* The Understanding Society data are provided under a special license. To replicate the results you will need to apply for the data from the UK data service: https://ukdataservice.ac.uk/

### Executing program

Run the code in the following order: 
* data_select_merge.R
* estimate_event_studies_and_dd_v4.R
* get_results_tables_v2.R
