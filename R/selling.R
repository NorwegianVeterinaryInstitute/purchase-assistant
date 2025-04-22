#' @noRd
selling_ui <- function(id) {
    ns <- shiny::NS(id)

    shiny::div(
        shiny::fluidRow(
            shiny::column(
                shiny::fluidRow(
                    shiny::column(
                        my_herd_panel(ns, include_status = TRUE),
                        width = 12
                    )
                ),
                shiny::br(),
                shiny::fluidRow(
                    shiny::column(
                        shiny::wellPanel(
                            shiny::fluidRow(
                                shiny::column(
                                    shiny::numericInput(
                                        inputId = ns("n_animals"),
                                        label = "Antall dyr å selge",
                                        value = 0,
                                        min = 0,
                                        step = 1
                                    ),
                                    width = 12
                                )
                            ),
                            shiny::fluidRow(
                                shiny::column(
                                    shiny::selectInput(
                                        inputId = ns("select_animal_type"),
                                        label = "Dyretype",
                                        choices = all_animal_types()
                                    ),
                                    width = 12
                                )
                            ),
                            shiny::fluidRow(
                                shiny::column(
                                    shiny::selectInput(
                                        inputId = ns("select_animal_breed"),
                                        label = "Rase",
                                        choices = all_animal_breeds()
                                    ),
                                    width = 12
                                )
                            ),
                            shiny::fluidRow(
                                shiny::column(
                                    shiny::actionButton(
                                        inputId = ns("add_row"),
                                        label = paste0(
                                            "Legg til i ",
                                            "salgslisten"
                                        )
                                    ),
                                    width = 6
                                )
                            ),
                            shiny::fluidRow(
                                shiny::column(
                                    shiny::p(shiny::em(
                                        paste0(
                                            "Ved å klikke på ",
                                            "\"Legg til\" godkjenner",
                                            " du at din gårds status deles ",
                                            "med potensielle kjøpere."
                                        )
                                    )),
                                    width = 12
                                )
                            )
                        ),
                        width = 12
                    )
                ),
                width = 3
            ),
            shiny::column(
                shiny::h3("Salgslisten"),
                shiny::fluidRow(
                    shiny::column(
                        shiny::actionButton(
                            inputId = ns("remove_row"),
                            "Fjern markerte rader"
                        ),
                        width = 12
                    )
                ),
                DT::dataTableOutput(outputId = ns("selling_table")),
                width = 9
            )
        )
    )
}

#' @noRd
selling_server <- function(id, user_id, greenlist, disease_data) {
    shiny::moduleServer(id, function(input, output, session) {
        selling_table <- shiny::reactiveVal(
            data.table::data.table(
                entry_date = as.Date(c(
                    "2023-11-08",
                    "2023-09-12",
                    "2023-05-09"
                )),
                n_animals = c(5, 2, 7),
                animal_type = c(
                    "Kalver på melk hunn",
                    "Okse",
                    "Ungdyr okse"
                ),
                animal_breed = c(
                    "Svensk Holstein",
                    "Svensk Rød",
                    "Svensk Rød"
                )
            )
        )

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

        output$my_status <- shiny::renderTable(
            {
                shiny::req(user_id() %in% greenlist()$id)
                freenr <- relevant_free_obs()
                farm_status(disease_data(), user_id(), freenr) |>
                    tidyr::pivot_longer(
                        cols = dplyr::everything(),
                        names_to = "diagnosis",
                        values_to = "status"
                    ) |>
                    dplyr::group_by(diagnosis) |>
                    dplyr::summarise(status = paste(status, collapse = ""))
            },
            sanitize.text.function = function(t) t,
            align = "c"
        )

        output$selling_table <- DT::renderDT(DT::datatable(
            selling_table(),
            rownames = FALSE,
            options = list(
                ordering = FALSE,
                searching = FALSE,
                paging = FALSE,
                language = DT::JS(
                    paste0(
                        '{ "url": "https://cdn.datatables.net',
                        '/plug-ins/1.11.5/i18n/nb-NO.json" }'
                    )
                ),
                columnDefs = list(list(className = "dt-right", targets = 1))
            ),
            colnames = c(
                "Utleggingsdato",
                "Antall dyr",
                "Dyretype",
                "Rase"
            )
        ))

        shiny::observeEvent(input$add_row, {
            n <- as.numeric(input$n_animals)
            shiny::req(n > 0)

            s_t <- selling_table()

            type <- input$select_animal_type
            breed <- input$select_animal_breed

            s_t <- rbind(
                s_t,
                data.table::data.table(
                    entry_date = Sys.Date(),
                    n_animals = n,
                    animal_type = type,
                    animal_breed = breed
                )
            )

            entry_date <- NULL

            s_t <- s_t[order(entry_date, rownames(s_t), decreasing = TRUE)]

            selling_table(s_t)
        })

        shiny::observeEvent(input$remove_row, {
            n <- input$selling_table_rows_selected
            shiny::req(!is.null(n))

            s_t <- selling_table()

            s_t <- s_t[-n, ]

            selling_table(s_t)
        })
    })
}
