# usethis::use_test()
init_h2o <- function(){
  h2o.init() # Starting h2o session
  h2o.no_progress() # Disable progress bar in console
}