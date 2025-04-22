#' @noRd
animal_market_ui <- function(id) {
    ns <- shiny::NS(id)
    county_names <- get_counties()$countyName
    agents <- valid_agents()

    picker_options <- shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        selectAllText = "Merk alle",
        deselectAllText = "Avmerk alle",
        noneSelectedText = "Ingenting valgt"
    )

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
                            shiny::h4("Søk dyr"),
                            shiny::p("(*) = obligatorisk valg"),
                            shinyWidgets::pickerInput(
                                inputId = ns("select_counties"),
                                label = paste0(
                                    "Velg fylke du vil kjøpe ",
                                    "fra (*)"
                                ),
                                multiple = TRUE,
                                choices = county_names,
                                options = picker_options,
                                inline = FALSE
                            ),
                            shinyWidgets::pickerInput(
                                inputId = ns("select_types"),
                                label = paste0(
                                    "Velg hvilke dyretyper du vil ",
                                    "kjøpe (*)"
                                ),
                                multiple = TRUE,
                                choices = all_animal_types(),
                                options = picker_options,
                                inline = FALSE
                            ),
                            shinyWidgets::pickerInput(
                                inputId = ns("select_breeds"),
                                label = paste0(
                                    "Velg hvilke raser du vil ",
                                    "kjøpe"
                                ),
                                multiple = TRUE,
                                choices = all_animal_breeds(),
                                options = picker_options,
                                inline = FALSE
                            ),
                            shiny::p(
                                paste0(
                                    "Kjøp dyr som er fri fra ",
                                    "(de gårdene som vises kan, men ",
                                    "trenger ikke være, fri fra ",
                                    "ikke-valgte sykdommer):"
                                )
                            ),
                            shiny::checkboxGroupInput(
                                inputId = ns("select_diseases"),
                                label = NULL,
                                choiceNames = stringr::str_to_sentence(agents),
                                choiceValues = agents,
                                inline = TRUE
                            ),
                            shiny::div(
                                shiny::fluidRow(
                                    shiny::column(
                                        shiny::span(
                                            shiny::strong(
                                                "OBS! Du risikerer å ",
                                                "innføre smittsomme sykdommer ",
                                                "på din gård.",
                                            ),
                                            style = "color:red"
                                        ),
                                        width = 12
                                    )
                                ),
                                id = ns("salmonella_warning")
                            ),
                            shiny::actionButton(
                                inputId = ns("apply_filters"),
                                label = "Vis dyr til salgs"
                            )
                        ),
                        width = 12
                    )
                ),
                width = 3
            ),
            shiny::column(
                shiny::fluidRow(
                    shiny::column(
                        shiny::h3("Dyr til salgs"),
                        shiny::br(),
                        DT::dataTableOutput(
                            outputId = ns("market_table")
                        ),
                        width = 11
                    ),
                ),
                width = 9
            )
        )
    )
}

#' @noRd
format_seller_list <- function(
    dt, free_from, counties, types, breeds = character()) {
    stopifnot(length(counties) > 0, length(types) > 0)

    county <- animalType <- animalBreed <- NULL # nolint

    dt <- dt[county %in% counties & animalType %in% types]

    if (length(breeds)) {
        dt <- dt[animalBreed %in% breeds]
    }

    if (length(free_from)) {
        stopifnot(all(free_from %in% valid_agents()))

        free_from_columns <- paste0(free_from, "Res")

        ..free_from_columns <- NULL # nolint
        dt <- dt[rowSums(
            dt[, ..free_from_columns, with = FALSE] == 0
        ) == length(free_from_columns)]
    }

    agent_columns <- paste0(valid_agents(), "Res")

    for (a in agent_columns) {
        if (a %in% colnames(dt)) {
            data.table::set(
                dt,
                j = a,
                value = sapply(dt[, get(a)], function(x) {
                    paste(utils::capture.output(
                        status_to_symbol(as.character(x))
                    ), collapse = "")
                })
            )
        }
    }

    dt
}

#' @noRd
animal_market_server <- function(
    id, user_id, greenlist, disease_data, sellers) {
    shiny::moduleServer(id, function(input, output, session) {
        dt_agents <- data.table::as.data.table(
            sapply(valid_agents(), function(x) character())
        )

        filtered_sellers <- shiny::reactiveVal(
            cbind(
                data.table::data.table(
                    herdID = integer(),
                    county = character()
                ),
                dt_agents,
                data.table::data.table(
                    animalType = character(),
                    animalBreed = character(),
                    animalNumber = integer()
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

        shiny::observeEvent(input$select_diseases,
            {
                selected_diseases <- input$select_diseases
                if ("salmonella" %in% selected_diseases) {
                    shinyjs::hide("salmonella_warning")
                } else {
                    shinyjs::show("salmonella_warning")
                }
            },
            ignoreNULL = FALSE
        )

        shiny::observeEvent(input$apply_filters, {
            selected_counties <- input$select_counties
            selected_types <- input$select_types

            if (is.null(selected_counties) || is.null(selected_types)) {
                shinyWidgets::sendSweetAlert(
                    title = NULL,
                    text = paste0(
                        "For å søke må du velge ",
                        "minst ett fylke og en dyretype."
                    ),
                    type = "warning"
                )
                return()
            }

            selected_diseases <- input$select_diseases
            if (is.null(selected_diseases)) {
                selected_diseases <- character()
            }

            selected_breeds <- input$select_breeds
            if (is.null(selected_breeds)) {
                selected_breeds <- character()
            }

            dt <- format_seller_list(
                sellers(),
                free_from = selected_diseases,
                counties = selected_counties, types = selected_types,
                breeds = selected_breeds
            )

            filtered_sellers(dt)
        })

        output$market_table <- DT::renderDT(
            {
                dt <- filtered_sellers()

                data.table::setnames(
                    dt,
                    new = c(
                        "Besetnings-ID",
                        "Fylke",
                        stringr::str_to_sentence(valid_agents()),
                        "Dyretype",
                        "Dyrrase",
                        "Antall dyr tilgjengelig"
                    )
                )

                dt
            },
            options = list(
                ordering = FALSE,
                searching = FALSE,
                paging = TRUE,
                language = list(
                    url = paste0(
                        "https://cdn.datatables.net",
                        "/plug-ins/1.11.5/i18n/nb-NO.json"
                    )
                ),
                pageLength = 15,
                lengthChange = FALSE
            ),
            escape = FALSE
        )
    })
}
