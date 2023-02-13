#' Function to run the Data Loofah app
#' @export


runDataLoofah <- function(){
  appDir <- system.file("myapp", package = "DataLoofah")
  if(appDir == ""){
    stop("Could not find myapp. Try re-installing `DataLoofah`.", call. = FALSE)
  }

  shiny::runApp(appDir)
}
