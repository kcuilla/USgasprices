# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
gasprices::run_app() # add parameters here (if any)

# deploy Shiny app to shinyapps.io
library(rsconnect)

shiny_account <- Sys.getenv("SHINY_ACCOUNT")
shiny_token <- Sys.getenv("SHINY_TOKEN")
shiny_secret <- Sys.getenv("SHINY_SECRET")

rsconnect::setAccountInfo(name=shiny_account, token=shiny_token, secret=shiny_secret)

rsconnect::deployApp()