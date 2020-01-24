##### disclaimer ####
message('
This script will run scripts remotely off the internet and copy files from 
github into your current directory, ', getwd(),'. If you have any files with 
matching names, they will be be moved to a backup directory so they won\'t get
overwritten. Before continuing, please make sure that the data for your project
is available on this computer and you know where it is. YOU WILL BE ASKED FOR 
THE LOCATIONS OF YOUR DATA FILES. DEPLOYMENT WILL NOT BE ABLE TO PROCEED WITHOUT 
AT LEAST ONE DATA FILE. This script will probably also install or update 
R-packages on your computer, and it may take a while. If that\'s a problem, this
is the time to cancel the deployment.
        
You are running this at your own risk and with no warranty whatsoever.');
.menu01 <- if(!interactive()) 2 else -1;
if(file.exists('.auto.menu01.R')) .menu01 <- source('.auto.menu01.R')$value;
if(.menu01 ==  -1){
  .menu01 <- menu(c('Go ahead, I am ready and I know where my data is.'
                    ,'Stop this script without making any changes to my computer.'
                    ))};


if(.menu01 != 1) stop('

No problem, better safe than sorry!

Please read the comments in this script to understand what it does, and make 
sure you have backups of all the files in the directory where you run this 
script (or that you run it in an empty directory). Then, feel free to come back
and try this at a later time.');

#### deploy directory ####
if(length(intersect(c('desktop','documents','downloads','rgui.exe','r.exe'
                      ,'r-portable.exe'),tolower(list.files(all=TRUE))))||
   normalizePath('~') == normalizePath(getwd()) ||
   tolower(basename(getwd())) %in% c('desktop','documents','downloads','tmp')){
  .newdir <- paste0('project.',format(Sys.Date(),'%Y%m%d'));
  .savepaths <- file.path('~',c('Documents','documents','Desktop','desktop'));
  if(!is.na(.newdirpath<-match(TRUE
                               ,dir.exists(normalizePath(.savepaths
                                                         ,winslash = '/'
                                                         ,mustWork = FALSE))))){
    .newdir <- file.path(.savepaths[.newdirpath],.newdir)};
  message(sprintf(
    "You are currently in the '%s' directory. You may have a hard time finding your 
project here later. We recommend allowing this script to create a new directory, 
'%s', and deploy there.",getwd(),.newdir));
  .menu00 <- if(!interactive()) 1 else -1;
  if(file.exists('.auto.menu00.R')).menu00 <- source('.auto.menu00.R')$value;
  sprintf('')
  while(!.menu00 %in% 1:2){
    .menu00 <- menu(c(sprintf("Yes, go ahead and deploy to '%s'",.newdir)
                      ,sprintf("No, I really do want to deploy here, in '%s'"
                               ,getwd()))
                    ,title='Where should we deploy this script?')};
  if(.menu00 == 1){dir.create(.newdir); setwd(.newdir)};
}


gitbootstrap <- function(gitrepos=list(#trailR=list(repo='bokov/trailR'
                                       #           ,ref='integration')
                                       tidbits=list(repo='bokov/tidbits'
                                                     ,ref='integration')
                                       ,rio=list(repo='bokov/rio'
                                                 ,ref='master'))
                         ,instreqs=c()){
  if(!require('devtools')){
    install.packages('devtools',dependencies=TRUE
                     ,repos=getOption('repos','https://cran.rstudio.com'))};
  for(ii in names(gitrepos)){
    do.call(devtools::install_github,gitrepos[[ii]]);
    library(ii,character.only = TRUE)};
  if(exists('instrequire')) instrequire(instreqs);
}

clean_slate <- function(...){gitbootstrap();tidbits:::clean_slate(...)};

#### init ####
.templatepath <- 'http://github.com/bokov/ripcord/archive/enh_wincompat.zip';
.scriptspath <- 'http://github.com/bokov/ut-template/archive/enh_wincompat.zip';
.oldoptions <- options();
options(browser='false'); # to make usethis::use_zip calm down a little bit
.tempenv00 <- new.env();
if(file.exists('autoresponse.R')){
  source('autoresponse.R',local = .tempenv00);
  for(ii in ls(.tempenv())) stack(ii);
};

#' TODO: parse own URL
#' 
#' Install needed libraries
#### file copy ####
message('Installing needed packages and their dependencies.'
        ,'This may take a while, please be patient.');
gitbootstrap(instreqs = c('crayon','usethis','rmarkdown'));

#' 
#' Copy down the latest version of the specified branch
.ztemp0 <- usethis::use_zip(.templatepath,'.',cleanup = TRUE);
#' Merge into current directory, with backups
mergedirs(.ztemp0);
#' Rename .Rproj file
if(file.exists('ripcord.Rproj')){
  localrproj <- paste0(basename(getwd()),'.Rproj');
  if(file.exists(localrproj)) file.rename(localrproj,paste0('old.',localrproj));
  try(file.rename('ripcord.Rproj',localrproj),silent=TRUE);
}
#' Get info about the scripts submodule
.scriptsinfo <- getkeyval(
  filesections('.gitmodules',sectionrxp = '\\[.*submodule'
               ,targetrxp = '\\[.*submodule .*scripts.*\\]')[[1]]
  ,c('path','url','branch'));
#' Download the appropriate version of the submodule
.ztemp1 <- usethis::use_zip(
  with(.scriptsinfo,paste0(url,'/archive/',c(branch,'master')[1],'.zip'))
  ,'.',cleanup = TRUE);
#' Remove the existing scripts directory
unlink(.scriptsinfo$path,recursive = TRUE,force = TRUE);
#' Rename the newly downloaded one to scripts
file.rename(.ztemp1,.scriptsinfo$path);
if(file.exists(.localfns <- file.path(.scriptsinfo$path,'functions.R'))){
  source(.localfns)};
options(browser=.oldoptions$browser); # restore the browser option

#### confmainloop ####
if(file.exists('.auto.confch.R')) stack(source('.auto.confch.R')$value
                                        ,'confchfile');
readline(ui_todo("
You will be asked to select the main data file you intend to use with this 
project. Press any key to continue."));
.inputdata <- smartsetnames(smartfilechoose('data/example_data_pbc.csv'
                                       ,pop('confchfile')));
# TODO: the same within loop, and grow with each cycle
# TODO: print .inputdata as part of extramessage
.confmain <- -1;
while(.confmain<4 || length(.inputdata)==0){
  .confmain <- smartmenu(c( 'Select an additional file.'
                           ,'Change the name of a variable.'
                           ,'Unselect one of the files.'
                           ,'Save selections and continue.'
                           ),batchmode = 4,autoresponse = pop('confchfile')
                         ,title = 'What do you wish to do?'
                         ,extramessage = {
                           ui_line(
'\n\nThese are the files you have chosen and the variable names to which they will
 be assigned after getting imported into R:\n');
                           for(ii in names(.inputdata)){
                             ui_line(
                               '{ui_field(ii)}\t{ui_path(.inputdata[ii])}')}});
  switch(.confmain
         # add a file
         ,.inputdata <- smartsetnames(c(.inputdata
                                        ,smartfilechoose('data/example_data_pbc.csv'
                                                         ,ignorecancel = FALSE
                                                         ,auto=pop('confchfile')
                                                         )))
         ,{ # rename a variable
           .torename <- smartmenu(.inputdata
                                  ,title='Which variable do you wish to rename?'
                                  ,extramessage = {
                                    ui_line(
"After you enter in the name you want it will get modified for uniqueness, 
compatibility with R and ease of typing. Names starting with {ui_code('dat')} 
will be renumbered. Type {ui_code(0)} to cancel and go back to previous menu.")}
                                  ,ignorezero = FALSE,batchmode=0
                                  ,auto=pop('confchfile'));
           if(.torename==0) next;
           message(ui_line(
             "\nIf you want a default name to be auto-assigned, type the 'enter' key"));
           names(.inputdata)[.torename]<-smartreadline(
             sprintf("Type in a new name for '%s': "
                     ,names(.inputdata)[.torename])
             ,auto=pop('confchfile'));
           .inputdata <- smartsetnames(.inputdata);
         }
         ,{ # remove a file
           .toremove <- smartmenu(.inputdata
                                  ,title='Which file do you wish to un-select?'
                                  ,extramessage = {ui_line(
'The file you unselect will not be touched, just its data will not be imported 
into this project. Type {ui_code(0)} to not select any files and return to the 
previous menu.')}
                                  ,ignorezero = FALSE, batchmode = 0
                                  ,auto=pop('confchfile'));
           if(.toremove==0) next;
           .inputdata <- .inputdata[-.toremove];
           if(length(.inputdata)==0){
             message(ui_todo('There must be at least one data file.'));
             .inputdata <- smartsetnames(smartfilechoose('data/example_data_pbc.csv'
                                                         ,pop('confchfile')));
           }
           });
}

#' Close off the logfile
if(file.exists('.logfile')) cat('\n)',file='.logfile',append=TRUE);
#### create config file ####
source('scripts/snippets.R');
.newconfig <- filesections('config.R');
.newconfig$inputdata <- paste0(.snippets$config_inputdata
                               ,'c(\n  ',paste(sprintf("%s = '%s'"
                                                       ,names(.inputdata)
                                                       ,.inputdata)
                                               ,collapse='\n ,'),'\n);');
writeLines(unlist(.newconfig),'local.config.R');
.userconfigdone <- FALSE;
#' Update hooks.
if(file.exists('scripts/quickstart_patch.R')) source('scripts/quickstart_patch.R');

#### load scriports ####
#tidbits:::load_deps(c('simdata.R','dictionary.R'));
load_deps2('overview.R');
ui_done('Deployment successful, report builds are done.');
if(file.exists('overview.html')) try(browseURL('overview.html'));

#' DONE: actually create an updated `local.config.R` from this data
#' DONE: update the scripts to handle a vector-valued `inputdata`
#' MOOT: decide what to do when inputdata doesn't exist in downloaded config.R
#' MOOT: decide what to do when inputdata exists but some or all files don't exist
#' MOOT: decide what to do when inputdata exists and files do all exist
#' TODO: recommend ~~chocolatey and~~ git install for Windows users
#' TODO: recommend git install for MacOS and Linux users if missing

c()
