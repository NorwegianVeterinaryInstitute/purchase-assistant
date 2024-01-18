#' @noRd
home_ui <- function(id) {
    ns <- shiny::NS(id)

    shiny::div(
        shiny::fluidRow(
            shiny::column(
                shiny::fluidRow(
                    shiny::column(
                        shiny::uiOutput(ns("home_user")),
                        width = 12
                    )
                ),
                shiny::br(),
                shiny::includeMarkdown(
                    system.file(
                        "markdown/friskko_home.md",
                        package = "purchaseAssistant"
                    )
                ),
                width = 5
            ),
            shiny::column(
                shiny::fluidRow(
                    shiny::column(
                        home_box(
                            title = "Mitt FriskKo",
                            description = paste0(
                                "H\u00e4r kan du som \u00e4r medlem i FriskKo-",
                                "programmet f\u00e5 en \u00f6versikt \u00f6ver",
                                " dina egna provresultat och smittl\u00e4get i",
                                " din bes\u00e4ttning."
                            ),
                            page = "mitt_friskko",
                            icon = "cow"
                        ),
                        width = 6
                    ),
                    shiny::column(
                        home_box(
                            title = "K\u00f6pa djur",
                            description = paste0(
                                "Om du vill k\u00f6pa djur och beh\u00f6ver en",
                                " lista p\u00e5 bes\u00e4ttningar som lagt ut ",
                                "djur f\u00f6r f\u00f6rs\u00e4ljning som ",
                                "matchar dina \u00f6nskem\u00e5l."
                            ),
                            page = "djurmarknad",
                            icon = "cart-shopping"
                        ),
                        width = 6
                    )
                ),
                shiny::br(),
                shiny::fluidRow(
                    shiny::column(
                        home_box(
                            title = paste0(
                                "Kontrollera h\u00e4lsostatus innan ",
                                "du k\u00f6per"
                            ),
                            description = paste0(
                                "Om du vet vilka bes\u00e4ttningar du vill ",
                                "k\u00f6pa djur fr\u00e5n och vill kontrollera",
                                " deras FriskKo-status f\u00f6rst."
                            ),
                            page = "kopa_djur",
                            icon = "clipboard-check"
                        ),
                        width = 6
                    ),
                    shiny::column(
                        home_box(
                            title = "S\u00e4lja djur",
                            description = paste0(
                                "Om du vill l\u00e4gga ut djur f\u00f6r ",
                                "f\u00f6rs\u00e4ljning."
                            ),
                            page = "salja_djur",
                            icon = "store"
                        ),
                        width = 6
                    )
                ),
                width = 7
            )
        )
    )
}

#' @noRd
home_server <- function(id, user_name, herd_id) {
    shiny::moduleServer(id, function(input, output, session) {
        output$home_user <- shiny::renderUI({
            shiny::tagList(
                shiny::h4(
                    "V\u00e4lkommen ",
                    shiny::span(user_name(), style = "color: blue;")
                ),
                shiny::h5(
                    "Din bes\u00e4ttning: ",
                    shiny::span(herd_id(), style = "color: blue;")
                )
            )
        })
    })
}
