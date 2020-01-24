# config
deps <- 'overview.R';
expected_out <- c('data','dictionary','example_analysis'
                    ,'prep_deps','simdata');
cleanup_files <- '*\\.html$|*\\.R.rdata$|_files$|_cache$';

# cleanup
unlink(list.files(pattern=cleanup_files),recursive = TRUE, force = TRUE);

# load scripts
if(file.exists(scripts<-normalizePath('scripts/functions.R',winslash='/'))){
  source(scripts);
  message('Project functions loaded.')
  } else stop(scripts,' not found!');

# verify that required files exist and run the render
if(!file.exists('config.R')) stop('config.R not found!');
if(!all(deps_found<-file.exists(deps))) {
  stop('scripts missing:\n',paste(names(deps)[!deps_found],collapse=', '))
  } else {
    load_deps2(deps,debug = TRUE);
    message('load_deps2() has run');
}

# verify that RData files were created
if(!all(rdata_found<-file.exists(paste0(expected_out,'.R.rdata')))){
  stop('RData missing:\n',paste(names(rdata_found)[!rdata_found],collapse=', '));
} else message('All RData created');

# verify that HTML files were created
if(!all(html_found<-file.exists(paste0(expected_out,'.html')))){
  stop('html missing:\n',paste(names(html_found)[!html_found],collapse=', '));
} else message('All HTML created');

c()