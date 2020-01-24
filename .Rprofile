
options(tsci.systemwrapper = function(cmd='',...,VERBOSE=getOption('sysverbose',T)
                          ,CHECKFILES=c('files')){ # nodeps
  args <- list(...); sysargs <- list();
  # separate out the args intended for system
  for(ii in intersect(names(args),names(formals(system)))){
    sysargs[[ii]] <- args[[ii]]; args[[ii]] <- NULL;};
  # check to make sure all arguments listed in checkfiles contain only files
  # that exist
  for(ii in intersect(CHECKFILES,names(args))){
    if(!all(.exist <- file.exists(args[[ii]]))){
      stop('The following files cannot be found:\n'
           ,paste(args[[ii]][!.exist],collapse=', '))}};
  for(xx in args) cmd <- paste(cmd,paste(xx,collapse=' '));
  if(VERBOSE) message('Executing the following command:\n',cmd);
  return(do.call(system,c(command=cmd,sysargs)));
});

options(tsci.gitsub = function(stopfile='.developer'){if(!file.exists(stopfile)){
  unlink(getOption('tsci.systemwrapper')("git submodule --quiet foreach 'echo $path'"
                       ,intern=TRUE,VERBOSE=FALSE)
         ,recursive = TRUE,force = TRUE);
  getOption('tsci.systemwrapper')('git submodule update --init --recursive --remote')} else {
    message('Developer mode-- ignoring.'); return(0);
  }});

clean_slate <- function(command="",removepatt='^\\.RData$|.*\\.[Rr]\\.rdata$|.*\\.html$|.*_cache$'
                        ,all=TRUE,cleanglobal=TRUE
                        ,envir=parent.frame()){
  if(!interactive()) warning('This function is intended to run in an '
                             ,'interactive session to restart that\n  '
                             ,'session on a clean slate. If you are calling it '
                             ,'non-interactively  (from a\n  script or '
                             ,'function), don\'t expect any code that you put '
                             ,'after it to work!');
  # remove cached files
  unlink(list.files(pattern=removepatt,all.files=TRUE,full.names = TRUE),recursive=TRUE,force=TRUE);
  # clear out calling environment
  rm(list=ls(all.names=all,envir = envir),envir = envir);
  # also global environment if specified
  if(cleanglobal) rm(list=ls(all.names=all,envir=.GlobalEnv),envir = .GlobalEnv);
  # if rstudioapi available, use it to restart the session
  if(requireNamespace('rstudioapi') && rstudioapi::isAvailable()){
    rstudioapi::restartSession(command)};
}
