################### SOURCE WHATEVER FILES ######################
#' remotes::install_github('PEDSnet/argos')

library(argos)
library(srcr)
library(stringr)

library(RPresto) #' 1.4.8
library(DBI) #' 1.3.0
library(dbplyr) #' 2.5.1 or 2.5.2
library(RPostgres)
library(assertthat)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(tibble) #' includes the view function 
library(squba)
library(readr)
library(usethis)

source(file.path(getwd(), 'setup', 'trino_patch.R'))
source(file.path(getwd(), 'code', 'funs.R'))
source(Sys.getenv('PEDSNET_TRINO_HTTR')) #' source the HTTR information

idd_session<-argos$new('idd_session')$init_session(db_src = srcr(Sys.getenv('PEDSNET_TRINO_V61')),
                                                   cdm_schema = 'pedsnet_dcc_v61',
                                                   vocabulary_schema = 'v61_vocabulary',
                                                   results_schema = 'idd_christakis_v61',
                                                   cdm = 'pedsnet',
                                                   table_case = 'lower',
                                                   results_name_tag = '',
                                                   db_trace = TRUE,
                                                   retain_intermediates = TRUE)

set_argos_default(idd_session)
