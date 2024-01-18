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
                                        label = "Antal djur att s\u00e4lja",
                                        value = 0,
                                        min = 0,
                                        step = 1
                                    ), width = 12
                                )
                            ),
                            shiny::fluidRow(
                                shiny::column(
                                    shiny::selectInput(
                                        inputId = ns("select_animal_type"),
                                        label = "Djurtyp",
                                        choices = all_animal_types()
                                    ), width = 12
                                )
                            ),
                            shiny::fluidRow(
                                shiny::column(
                                    shiny::selectInput(
                                        inputId = ns("select_animal_breed"),
                                        label = "Ras",
                                        choices = all_animal_breeds()
                                    ), width = 12
                                )
                            ),
                            shiny::fluidRow(
                                shiny::column(
                                    shiny::actionButton(
                                        inputId = ns("add_row"),
                                        label = paste0(
                                            "L\u00e4gg till i ",
                                            "f\u00f6rs\u00e4ljningslistan"
                                        )
                                    ), width = 6
                                )
                            ),
                            shiny::fluidRow(
                                shiny::column(
                                    shiny::p(shiny::em(
                                        paste0(
                                            "Genom att klicka p\u00e5 ",
                                            "\"L\u00e4gg till\" godk\u00e4nner",
                                            " du att din FriskKo-status delas ",
                                            "med potentiella k\u00f6pare."
                                        )
                                    )), width = 12
                                )
                            )
                        ),
                        width = 12
                    )
                ),
                width = 3
            ),
            shiny::column(
                shiny::h3("F\u00f6rs\u00e4ljningslista"),
                shiny::fluidRow(
                    shiny::column(
                        shiny::actionButton(
                            inputId = ns("remove_row"),
                            "Ta bort markerade rader"
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
                    "Calves on milk female",
                    "Ox",
                    "Youngstock bull"
                ),
                animal_breed = c(
                    "Swedish Holstein",
                    "Swedish Red",
                    "Swedish Red"
                )
            )
        )

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

        output$selling_table <- DT::renderDataTable(DT::datatable(
            selling_table(),
            rownames = FALSE,
            options = list(
                ordering = FALSE,
                searching = FALSE,
                paging = FALSE,
                language = DT::JS(
                    paste0(
                        '{ "url": "https://cdn.datatables.net',
                        '/plug-ins/1.11.5/i18n/sv-SE.json" }'
                    )
                ),
                columnDefs = list(list(className = "dt-right", targets = 1))
            ),
            colnames = c(
                "Utl\u00e4ggningsdatum",
                "Antal djur",
                "Djurtyp",
                "Ras"
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
