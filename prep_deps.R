#' ---
#' title: "Load All Scriports"
#' ---
#' 
.projpackages <- c()
.deps <- c( 'example_analysis.R','simdata.R'); 
#+ load_deps, echo=FALSE, messages=FALSE, warnings=FALSE
.junk<-capture.output(source('./scripts/global.R',chdir=TRUE,echo=FALSE));
#.currentscript <- parent.frame(2)$ofile;
.currentscript <- current_scriptname('prep_deps.R');



#' Normally this is where calculations should go, but this script is invoked for
#' its side effects: triggering all the other scripts.




save(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
#+ echo=FALSE
c()
