#' @noRd
my_friskko_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::div(
        shiny::fluidRow(
            shiny::column(
                my_herd_panel(ns, include_status = FALSE),
                width = 3
            ),
            shiny::column(
                shiny::tabsetPanel(
                    shiny::tabPanel(
                        shiny::br(),
                        shiny::h3("\u00d6versikt"),
                        shiny::fluidRow(
                            shiny::column(
                                shiny::h6(
                                    paste0(
                                        "Gr\u00f6na listan visar ",
                                        "bes\u00e4ttningar med ",
                                        "godk\u00e4nd provtagning inom ",
                                        "FriskKo. N\u00e4r du har fyra ",
                                        "smittfria provtagningsom",
                                        "g\u00e5ngar i rad, \u00e4r din ",
                                        "bes\u00e4ttning kvalificerad."
                                    )
                                ),
                                width = 12
                            )
                        ),
                        shiny::fluidRow(
                            shiny::column(
                                my_friskko_status(ns),
                                width = 12
                            )
                        ),
                        title = "\u00d6versikt",
                        value = "overview"
                    ),
                    shiny::tabPanel(
                        shiny::br(),
                        shiny::h3("Salmonella"),
                        shiny::p(
                            paste0(
                                "Resultat fr\u00e5n de senaste fyra ",
                                "provtagningarna f\u00f6r salmonella."
                            )
                        ),
                        shiny::p(
                            paste0(
                                "Positiva resultat f\u00f6r Salmonella Dublin ",
                                "och \u00f6vriga salmonellavarianter ",
                                "kombineras  till ett ",
                                "provtagningstillf\u00e4lle, och endast det ",
                                "senaste datumet visas."
                            )
                        ),
                        shiny::p(
                            "Om du har provresultat fr\u00e5n fler \u00e4n en ",
                            "mj\u0f6lktank kombineras dina tankmj\u00f6lks",
                            "resultat till ett provtagningsdatum."
                        ),
                        shiny::tableOutput(
                            outputId = ns("my_results_salmonella")
                        ),
                        shiny::br(),
                        shiny::p(
                            shiny::a(
                                href = paste0(
                                    "https://www.vxa.se/fakta/smittskydd/",
                                    "friskko/salmonellaradgivning/"
                                ),
                                target = "_blank",
                                shiny::h6(
                                    "L\u00e4s mer om salmonellar\u00e5dgivning"
                                )
                            )
                        ),
                        title = "Salmonella",
                    ),
                    shiny::tabPanel(
                        shiny::br(),
                        shiny::h3("Mycoplasma bovis"),
                        shiny::p(
                            paste0(
                                "Resultat fr\u00e5n de senaste fyra ",
                                "provtagningarna f\u00f6r mycoplasma."
                            )
                        ),
                        shiny::tableOutput(
                            outputId = ns("my_results_mycoplasma")
                        ),
                        shiny::br(),
                        shiny::p(
                            shiny::a(
                                href = paste0(
                                    "https://www.vxa.se/fakta/smittskydd/",
                                    "smittsamma-sjukdomar/mycoplasma-bovis/"
                                ),
                                target = "_blank",
                                shiny::h6(
                                    "L\u00e4s mer om Mycoplasma bovis"
                                )
                            )
                        ),
                        title = "Mycoplasma",
                    ),
                    id = ns("my_disease_tabs")
                ),
                width = 9
            )
        )
    )
}

#' @noRd
my_friskko_server <- function(id, user_id, greenlist, disease_data) {
    shiny::moduleServer(id, function(input, output, session) {
        output$my_herd_id <- shiny::renderUI({
            shiny::h6(
                "Min bes\u00e4ttning: ",
                shiny::span(user_id(), style = "color:blue")
            )
        })

        output$my_county <- shiny::renderUI({
            g_l <- greenlist()
            county <- g_l[g_l$id == user_id(), ]$county

            shiny::h5(
                "Mitt l\u00e4n: ",
                shiny::span(county, style = "color:blue")
            )
        })

        output$my_status <- shiny::renderTable({
            shiny::req(user_id() %in% greenlist()$id)

            friskko_status(disease_data(), user_id())
        }, sanitize.text.function = function(t) t, align = "c")

        output$my_greenlist <- shiny::renderUI({
            g_l <- greenlist()
            uid <- user_id()

            my_result <- g_l[id == uid]

            res <- ifelse(
                nrow(my_result) == 0,
                "Unknown",
                as.character(my_result$greenlist)
            )

            shiny::h4(
                "Min sammanlagda status: ",
                status_to_symbol(res, include_pretext = TRUE)
            )
        })

        output$my_results_salmonella <- shiny::renderTable(
            my_results(agent = "salmonella", my_id = user_id()),
            sanitize.text.function = function(t) t
        )

        output$my_results_mycoplasma <- shiny::renderTable(
            my_results(agent = "mycoplasma", my_id = user_id()),
            sanitize.text.function = function(t) t
        )
    })
}
