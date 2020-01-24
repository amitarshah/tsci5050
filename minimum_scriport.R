#' # Minimum Scriport
#' 
#' Add the names of packages (enclosed in quotes) you need to this vector
.projpackages <- c('pander');
#' If you want to reuse calculations done by other scripts, add them to `.deps`
#' below after `'dictionary.R'`.
.deps <- c( 'dictionary.R' ); 
#+ load_deps, echo=FALSE, message=FALSE, warning=FALSE,results='hide'
# Do not edit the next line
.junk<-capture.output(source('./scripts/global.R',chdir=TRUE,echo=FALSE));
#' Edit the next line only if you copy or rename this file (make the new name the
#' argument to `current_scriptname()`)
.currentscript <- current_scriptname('minimum_scriport.R');
#' ### Start
#' 
#' In the above lines were the minimum set of commands needed for a script to 
#' have access to all the features of [Ripcord](overview.html). Add any commands 
#' you like below. All the datasets specified in your 
#' [`local.config.R`](local.config.R) file will be available here and if 
#' [`local.config.R`](local.config.R) does not exist, then all the datasets 
#' specified in your [`config.R`](config.R) file.
#' 
#' Put any R commands you want below. The following datasets are available:
#' `r pander(names(inputdata))`.
#
# Your code here!
#
#' ### Save results
#' 
#' Now the results are saved and available for use by other scriports if you
#' place `r sprintf("\x60'%s'\x60",.currentscript)` among the values in their 
#' `.deps` variables.
save(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
#' ### Finish
#+ echo=FALSE, results='hide'
c()
