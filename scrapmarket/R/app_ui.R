#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    pageWithSidebar(
      headerPanel('Buscador de precos'),
      sidebarPanel(
        radioButtons("radio", label = h3("Escolha seu mercado"),
                     choices = list("Zona Sul" = 1, "Prix" = 2, "Mundial" = 3), 
                     selected = 1),
        textInput("text", label = h3("Escolha seu produto"),
                  value = "Enter text..."),
        actionButton("go", "Buscar")
        
      ),
      mainPanel(
        DT::DTOutput('table1')
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'scrapmarket'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

