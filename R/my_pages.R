#' @noRd
my_pages_ui <- function(id) {
    ns <- shiny::NS(id)
    print(ns)
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
                        shiny::h3("Oversikt"),
                        shiny::fluidRow(
                            shiny::column(
                                shiny::h6(
                                    paste0(
                                        "Grønne listen viser ",
                                        "besetninger med ",
                                        "godkjent prøvetaking innen ",
                                        "programmet. Når du har fire ",
                                        "smittfrie prøvetakingsom",
                                        "ganger i rad, er din ",
                                        "besetning kvalifisert."
                                    )
                                ),
                                width = 12
                            )
                        ),
                        shiny::fluidRow(
                            shiny::column(
                                my_farm_status(ns),
                                width = 12
                            )
                        ),
                        title = "Oversikt",
                        value = "overview"
                    ),
                    shiny::tabPanel(
                        shiny::br(),
                        shiny::h3("Salmonella"),
                        shiny::p(
                            paste0(
                                "Resultater fra de siste fire ",
                                "prøvetakingene for salmonella."
                            )
                        ),
                        shiny::p(
                            paste0(
                                "Positive resultater for Salmonella Dublin ",
                                "og andre salmonellavarianter ",
                                "kombineres til en ",
                                "prøvetakingsomgang, og kun den ",
                                "siste datoen vises."
                            )
                        ),
                        shiny::p(
                            "Hvis du har prøveresultater fra mer enn en ",
                            "melketank kombineres dine tankmelk",
                            "resultater til en prøvetakingsdato."
                        ),
                        shiny::tableOutput(
                            outputId = ns("my_results_BCoV")
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
                                    "Les mer om salmonellarådgivning"
                                )
                            )
                        ),
                        title = "BCoV",
                    ),
                    shiny::tabPanel(
                        shiny::br(),
                        shiny::h3("Bovine Respiratory Disease (BRSV)"),
                        shiny::p(
                            paste0(
                                "Resultater fra de siste fire ",
                                "prøvetakingene for mycoplasma."
                            )
                        ),
                        shiny::tableOutput(
                            outputId = ns("my_results_BRSV")
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
                                    "Les mer om Bovine Resperatory Disease (BRSV)"
                                )
                            )
                        ),
                        title = "BRSV",
                    ),
                    shiny::tabPanel(
                      shiny::br(),
                      shiny::h3("Klauvstatus"),
                      shiny::p(
                        paste0(
                          "Resultater fra de siste fire ",
                          "prøvetakingene for dermatitt."
                        )
                      ),
                      shiny::tableOutput(
                        outputId = ns("my_results_Klauvstatus")
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
                            "Les mer om Klauvstatus"
                          )
                        )
                      ),
                      title = "Klauvstatus",
                    ),
                    shiny::tabPanel(
                      shiny::br(),
                      shiny::h3("Jurstatus"),
                      shiny::p(
                        paste0(
                          "Resultater fra de siste fire ",
                          "prøvetakingene for Streptococcus agalactica."
                        )
                      ),
                      shiny::tableOutput(
                        outputId = ns("my_results_Jurstatus")
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
                            "Les mer om Jurstatus"
                          )
                        )
                      ),
                      title = "Jurstatus",
                    ),
                    id = ns("my_disease_tabs")
                ),
                width = 9
            )
        )
    )
}

#' @noRd
my_pages_server <- function(id, user_id, greenlist, disease_data) {
    shiny::moduleServer(id, function(input, output, session) {
        output$my_herd_id <- shiny::renderUI({
            shiny::h6(
                "Min besetning: ",
                shiny::span(user_id(), style = "color:blue")
            )
        })

        output$my_county <- shiny::renderUI({
            g_l <- greenlist()
            county <- g_l[g_l$id == user_id(), ]$county

            shiny::h5(
                "Mitt fylke: ",
                shiny::span(county, style = "color:blue")
            )
        })

        output$my_status <- shiny::renderTable({
            shiny::req(user_id() %in% greenlist()$id)

            farm_status(disease_data(), user_id())
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
                "Min samlede status: ",
                status_to_symbol(res, include_pretext = TRUE)
            )
        })

        output$my_results_BCoV <- shiny::renderTable(
            my_results(agent = "BCoV", my_id = user_id()),
            sanitize.text.function = function(t) t
        )

        output$my_results_BRSV <- shiny::renderTable(
            my_results(agent = "BRSV", my_id = user_id()),
            sanitize.text.function = function(t) t
        )
        
        output$my_results_Klauvstatus <- shiny::renderTable(
          my_results(agent = "Klauvstatus", my_id = user_id()),
          sanitize.text.function = function(t) t
        )
        
        output$my_results_Jurstatus <- shiny::renderTable(
          my_results(agent = "Jurstatus", my_id = user_id()),
          sanitize.text.function = function(t) t
        )
    })
}
