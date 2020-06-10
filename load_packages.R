use_package <- function(p){
  if (!is.element(p, installed.packages()[,1])){
    message('package ',p,' not found. Installing..')
    if(p == "HDeconometrics") {
      devtools::install_github("gabrielrvsc/HDeconometrics")
    } else {
      install.packages(p, dep = TRUE)}  
  }
  message('Loading package ',p)
  require_worked <- try(require(p, character.only = TRUE))
  if(!require_worked) {
    install.packages(p, dep = TRUE)
  }
}
use_package("devtools")
use_package("tidyr")
use_package("tidyverse")
use_package("lubridate")
use_package("writexl")
use_package("readxl")
use_package("httr")
use_package("plotly")
use_package("magrittr")
use_package("rlist")

