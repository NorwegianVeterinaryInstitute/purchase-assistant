#' app_server
#'
#' Manage the server (backend) side of the shiny app.
#' @noRd
app_server <- function(input, output, session) {
  current_page <- shiny::reactive(shiny.router::get_page())
  
  user_info <- get_user()
  disease_data <- get_disease_data()
  
  disease_data <- shiny::reactiveVal(disease_data)
  
  my_herd_id <- shiny::reactiveVal(as.numeric(user_info$herd_id))
  my_name <- shiny::reactiveVal(as.character(user_info$name))
  
  greenlist <- shiny::reactiveVal(get_greenlist())
  counties <- shiny::reactiveVal(get_counties())
  sellers <- shiny::reactiveVal(get_seller_list(my_id = user_info$herd_id))
  
  output$header_title <- shiny::renderUI({
    page <- current_page()
    text <- switch(
      page,
      "/" = "Purchase Assistant",
      "my_pages" = "My pages",
      "kopa_djur" = paste0(
        "K\u00f6pa djur (k\u00e4nda bes\u00e4ttningar)"
      ),
      "djurmarknad" = "K\u00f6pa djur",
      "salja_djur" = "S\u00e4lja djur"
    )
    
    shiny::titlePanel(text)
  })
  
  output$header_description <- shiny::renderUI({
    page <- current_page()
    text <- switch(
      page,
      "/" = "",
      "my_pages" = paste0(
        "F\u00e5 en \u00f6versikt \u00f6ver statusen ",
        "i din egen bes\u00e4ttning"
      ),
      "kopa_djur" = paste0(
        "Kontrollera andra bes\u00e4ttningars ",
        "status innan du k\u00f6per"
      ),
      "djurmarknad" = "Hitta djur till salu",
      "salja_djur" = "L\u00e4gg ut djur till f\u00f6rs\u00e4ljning"
    )
    
    shiny::h5(text)
  })
  
  output$home_button <- shiny::renderUI({
    page <- current_page()
    
    if (page == "/")
      return(NULL)
    
    shiny::fluidRow(
      shiny::column(
        shiny::actionButton(
          inputId = "link_hem",
          label = "Hem",
          onclick = sprintf(
            "location.href='%s'",
            shiny.router::route_link("/")
          ),
          icon = shiny::icon("house")
        ),
        width = 12
      )
    )
  })
  
  output$header_user <- shiny::renderUI({
    shiny::div(shiny::fluidRow(
      shiny::column(
        "V\u00e4lkommen ", shiny::span(my_name(), style = "color:blue"),
        width = 12
      )
    ),
    shiny::fluidRow(
      shiny::column(
        "Din bes\u00e4ttning: ",
        shiny::span(my_herd_id(), style = "color:blue"),
        width = 12
      )
    ), style = "text-align:right")
  })
  
  shiny.router::router_server()
  
  home_server("/", my_name, my_herd_id)
  my_pages_server("my_pages", my_herd_id, greenlist, disease_data)
  animal_market_server(
    "djurmarknad", my_herd_id, greenlist, disease_data, sellers
  )
  buying_known_server(
    "kopa_djur", my_herd_id, greenlist, counties, disease_data
  )
  selling_server("salja_djur", my_herd_id, greenlist, disease_data)
  
}