packHandler <- function(){
  ## call/install packages
  ### package list
  packages <- c(
    'dplyr',
    'tidyr',
    'ggplot2',
    'openxlsx',
    'lubridate',
    'janitor',
    'stringi',
    'gamlss',
    'mgcv',
    'mgcv.helper',
    'EnvStats',
    'htmltools',
    'copula',
    'RCurl',
    'tibble',
    'mcp'
  )
  
  ### install or load packages
  if (!require(install.load)) {
    install.packages('install.load')
  }
  
  install.load::install_load(packages)
}