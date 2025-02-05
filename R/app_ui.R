#' app_ui
#'
#' Configure the UI (frontend) side of the Shiny app
#' @noRd
app_ui <- function() {
    home <- home_ui("/")
    my_pages <- my_pages_ui("my_pages")
    animal_market <- animal_market_ui("djurmarknad")
    buying_known <- buying_known_ui("kopa_djur")
    selling <- selling_ui("salja_djur")

    shiny::fluidPage(
        theme = bslib::bs_theme(bootswatch = "flatly"),
        shinyjs::useShinyjs(),

        ## start header ##
        shiny::tags$header(
            shiny::fluidRow(
                shiny::column(
                    shiny::br(),
                    shiny::uiOutput("header_title"),
                    shiny::uiOutput("header_description"),
                    shiny::uiOutput("home_button"),
                    width = 9
                ),
                shiny::column(
                    shiny::br(),
                    shiny::fluidRow(
                        shiny::column(
                            shiny::uiOutput("header_user"),
                            width = 6
                        ),
                        shiny::column(
                            shiny::actionButton(
                                inputId = "logout",
                                label = " Logga ut",
                                icon = shiny::icon("right-from-bracket"),
                                onclick = "location.href='/logout'"
                            ),
                            width = 6
                        )
                    ),
                    width = 3
                )
            ),
            shiny::fluidRow(
                shiny::column(
                    shiny::hr(),
                    width = 12
                )
            )
        ),
        ## end header ##

        shiny.router::router_ui(
            default = shiny.router::route("/", home),
            shiny.router::route("my_pages", my_pages),
            shiny.router::route("djurmarknad", animal_market),
            shiny.router::route("kopa_djur", buying_known),
            shiny.router::route("salja_djur", selling),
            page_404 = shiny::div(
                shiny::h1("Sidan kan ej hittas"),
                shiny::br(),
                shiny::actionButton(
                    inputId = "link_hem",
                    label = shiny::h5("Hem"),
                    onclick = sprintf(
                        "location.href='%s'",
                        shiny.router::route_link("/")
                    )
                )
            )
        )
    )
}


