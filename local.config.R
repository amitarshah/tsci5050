#' ---
#' title: "Generic R Project Configuration File"
#' author: "Alex F. Bokov, Ph.D."
#' date: "10/18/2018"
#' ---
#' 

#### inputdata ####
#' 
#' The inputdata variable determines which data files will get read into your
#' project. The values are the file locations and the names are the variables
#' to which they will be assigned after they are read into R
#' 
#' In the `config.R` file there should only be simulations of your actual data
#' or datasets that you are _certain_ you have permission to redistribute 
#' publicly.
#' 
#' If there is also a `local.config.R` file, that one will override `config.R`
#' and that one can contain paths to actual data, presumably on each 
#' collaborator's local computer.
inputdata <- c(
  dat01 = 'C:/Users/shahar/Documents/R projects/data/example_data_pbc.csv'
);
#### footer ####
c()


