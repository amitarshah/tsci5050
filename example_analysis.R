#' ---
#' title: "Example Analysis"
#' css: "production.css"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: false
#' ---
#' 
#' ### Settings 
#' 
#' In the below two lines are the minimum script-level settings you need. 
#' The `.projpackages` object has the names of the packages you need installed 
#' if necessary and then loaded for this scriport. The `.deps` object contains
#' other scriports on which this one _directly_ depends (you don't need to worry
#' about the indirect ones-- each scriport manages its own dependencies for 
#' packages and scriports). The recommended value to start with is the one shown 
#' here. You can add more if the need arises later. For more information, please
#' see the [overview](overview.html) scriport.
.projpackages <- c('GGally','tableone','pander','dplyr','ggplot2');
.deps <- c( 'dictionary.R' ); 
#+ load_deps, echo=FALSE, message=FALSE, warning=FALSE,results='hide'
# do not edit the next two lines
.junk<-capture.output(source('./scripts/global.R',chdir=TRUE,echo=FALSE));
#' Set some formatting options for this document
panderOptions('table.alignment.default','right');
panderOptions('table.split.table',Inf);
panderOptions('p.wrap','');
panderOptions('p.copula',', and ');

.currentscript <- current_scriptname('example_analysis.R');
if(!exists('dat01')) dat01 <- get(names(inputdata)[1]);
#' 
#' 
#' ### Choosing predictor and response variables
#' 
#' The values below are variables arbitrarily selected for demonstration
#' purposes. To do your actual analysis, replace `binary_outcome` and 
#' `numeric_outcome` below with vectors containing one or more column names you 
#' want to serve as the outcome variables. If you have no binary outcomes, set
#' the value to an empty vector, `c()`. Likewise if you have no numeric 
#' 
#' 
#' this is the data dictionary for this data:
pander (attr(dat01, "tblinfo"))

binary_outcome <- c("hepato", "spiders", "ascites");

numeric_outcome <- c()
#' 
#' Insuring that the binary outcomes get treated as discrete values
#' 
for(ii in c(binary_outcome,'stage')){
  dat01[[ii]] <- factor(dat01[[ii]],exclude=NULL);
}
#' Again, replace `predictorvars` below with vectors containing one or more 
#' column names you actually want to serve as the predictor variables

predictorvars <- c("age", "albumin", "platelet", "protime", "stage")

predictorvars;
#' If you are satisfied with your choice of `binary_outcome`, `numeric_outcome`,
#' and `predictorvars` you don't need to change the following line. It simply
#' combines the above.
mainvars <- c(predictorvars,binary_outcome);
mainvars;
#' ### Plot the data
#' 
#' #### Explore pairwise relationships 
#' 
#' A plot of all pairwise relationships between the variables of interest.
#+ ggpairs_plot, message=FALSE, warning=FALSE, cache=TRUE

# select just the columns in 'mainvars' (otherwise the plot will take forever)
# and turn the ones tagged as 'ordinal' into factors
dat01[,mainvars] %>% mutate_at(.,v(c_ordinal,.),factor) %>% 
  # now make a scatterplot matrix, using the first 'binary_outcome' to assign 
  # color
  ggpairs(.,aes_string(color=binary_outcome[1]));
#' 
#' Note: if your data does not have a binary outcome, an artificial value is
#' randomly generated for demonstration purposes.
#' 
#' #### Exploring group differences. 
#' 
#' The `r sprintf("\x60%s\x60",binary_outcome[1])`
#' variable is again used to stratify the groups to create a cohort table that
#' can, among other things, help identify potential confounders and unexpected
#' explanatory variables.
#' 
mutate(dat01, trt=factor(trt, exclude = "",levels = c(1,2,NA)
                         ,labels = c('No','Yes','Unknown'))) %>% 
  CreateTableOne(vars = mainvars, strata = "trt"
                 ,data=., includeNA=TRUE, test=FALSE) %>%
  print(printToggle=FALSE) %>% pander(caption='Cohort Characterization');
#' 
#' ### Specify the statistical models
#' 
#' #### Binary outcome
#' 
#' Formulas for the binary outcome models
#+ binfrm, results='asis'
bin_formulas <- c();
for(yy in binary_outcome){
  browser();
  for(xx in predictorvars){
    bin_formulas <- c(bin_formulas,sprintf('%s ~ %s',yy,xx))
    }
  };

pander(cbind(bin_formulas),justify='l');
#' Logistic regression models fitted to the above formulas.
bin_models <- sapply(bin_formulas,function(xx){
  glm(xx,dat01,family='binomial') %>% update(.~.)},simplify=FALSE);
#' The results of the univariate model fits
#+ panderbin,results='asis'
for(xx in bin_models) {cat('***','\n'); cat(pander(xx)); cat('***','\n');};
#' 
#' 
#' ### Save results
#' 
#' Now the results are saved and available for use by other scriports if you
#' place `r sprintf("\x60'%s'\x60",.currentscript)` among the values in their 
#' `.deps` variables.
save(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
#+ echo=FALSE, results='hide'
c()
