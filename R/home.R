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
                        "markdown/home.md",
                        package = "purchaseAssistant"
                    )
                ),
                width = 5
            ),
            shiny::column(
                shiny::fluidRow(
                    shiny::column(
                        home_box(
                            title = "Mine sider",
                            description = paste0(
                                "Her kan du som er medlem i",
                                "prøvetakingsprogrammet få en ",
                                "oversikt over dine egne ",
                                "prøveresultater og smittestatus i din ",
                                "besetning."
                            ),
                            page = "my_pages",
                            icon = "cow"
                        ),
                        width = 6
                    ),
                    shiny::column(
                        home_box(
                            title = "Kjøpe dyr",
                            description = paste0(
                                "Hvis du vil kjøpe dyr og trenger en",
                                " liste over besetninger som har lagt ut ",
                                "dyr for salg som ",
                                "matcher dine ønsker."
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
                                "Kontroller helsestatus før ",
                                "du kjøper"
                            ),
                            description = paste0(
                                "Hvis du vet hvilke besetninger du vil ",
                                "kjøpe dyr fra og vil kontrollere",
                                " deres status først."
                            ),
                            page = "kopa_djur",
                            icon = "clipboard-check"
                        ),
                        width = 6
                    ),
                    shiny::column(
                        home_box(
                            title = "Selge dyr",
                            description = paste0(
                                "Hvis du vil legge ut dyr for ",
                                "salg."
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
                    "Velkommen ",
                    shiny::span(user_name(), style = "color: blue;")
                ),
                shiny::h5(
                    "Din besetning: ",
                    shiny::span(herd_id(), style = "color: blue;")
                )
            )
        })
    })
}
