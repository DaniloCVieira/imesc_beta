

#if(file.exists("D:\\R3\\imesc2024\\WRITE_LOGS.R")){  source("D:\\R3\\imesc2024\\WRITE_LOGS.R")}

list.of.packages <- c('remotes')
if(!length(grep("connect/apps",getwd()))>0){
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {install.packages(new.packages, dependencies = TRUE)}
  remotes::install_deps("inst",upgrade="never")
}
suppressPackageStartupMessages(pkgload::load_all("inst",export_all = T,quiet =T,warn_conflicts =F))


#options(shiny.fullstacktrace = TRUE, shiny.error = write_error_logs)
#shiny::runGitHub("iMESc","DaniloCVieira","main")
shinyApp(app_ui,app_server)
#file.copy(paste0(getwd(),"/inst/R/app_ui.R"),'D:/R3/imesc2024/imesc_beta2/inst/R/app_ui.R',overwrite = T)
#file.copy(paste0(getwd(),"/inst/R/app_server.R"),'D:/R3/imesc2024/imesc_beta2/inst/R/app_server.R',overwrite = T)


#file.copy(paste0(getwd(),"/app.R"),'D:/R3/imesc2024/imesc_beta2/app.R',overwrite = T)


