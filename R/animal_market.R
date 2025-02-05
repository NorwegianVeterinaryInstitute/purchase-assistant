#' @noRd
animal_market_ui <- function(id) {
    ns <- shiny::NS(id)
    county_names <- get_counties()$countyName
    agents <- valid_agents()

    picker_options <- shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        selectAllText = "Markera alla",
        deselectAllText = "Avmarkera alla",
        noneSelectedText = "Inget markerat"
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
                            shiny::h4("S\u00f6k djur"),
                            shiny::p("(*) = obligatoriskt val"),
                            shinyWidgets::pickerInput(
                                inputId = ns("select_counties"),
                                label = paste0(
                                    "V\u00e4lj l\u00e4n du vill k\u00f6pa ",
                                    "fr\u00e5n (*)"
                                ),
                                multiple = TRUE,
                                choices = county_names,
                                options = picker_options,
                                inline = FALSE
                            ),
                            shinyWidgets::pickerInput(
                                inputId = ns("select_types"),
                                label = paste0(
                                    "V\u00e4lj vilka djurtyper du vill ",
                                    "k\u00f6pa (*)"
                                ),
                                multiple = TRUE,
                                choices = all_animal_types(),
                                options = picker_options,
                                inline = FALSE
                            ),
                            shinyWidgets::pickerInput(
                                inputId = ns("select_breeds"),
                                label = paste0(
                                    "V\u00e4lj vilka raser du vill ",
                                    "k\u00f6pa"
                                ),
                                multiple = TRUE,
                                choices = all_animal_breeds(),
                                options = picker_options,
                                inline = FALSE
                            ),
                            shiny::p(
                                paste0(
                                    "K\u00f6p djur som \u00e4r fria fr\u00e5n ",
                                    "(de g\u00e5rdar som visas kan, men ",
                                    "beh\u00f6ver inte vara, fria fr\u00e5n ",
                                    "icke-valda smittor):"
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
                                                "OBS! Du riskerar att ",
                                                "inf\u00f6ra salmonella ",
                                                "p\u00e5 din g\u00e5rd.",
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
                                label = "Visa djur till salu"
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
                        shiny::h3("Djur till salu"),
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
    dt, free_from, counties, types, breeds = character()
) {
    stopifnot(length(counties) > 0, length(types) > 0)

    county <- animalType <- animalBreed <- NULL #nolint

    dt <- dt[county %in% counties & animalType %in% types]

    if (length(breeds))
        dt <- dt[animalBreed %in% breeds]

    if (length(free_from)) {
        stopifnot(all(free_from %in% valid_agents()))

        free_from_columns <- paste0(free_from, "Res")

        ..free_from_columns <- NULL #nolint
        dt <- dt[rowSums(
            dt[, ..free_from_columns, with = FALSE] == 0
        ) == length(free_from_columns)]
    }

    agent_columns <- paste0(valid_agents(), "Res")

    for (a in agent_columns) {
        if (a %in% colnames(dt)) {
            data.table::set(
                dt, j = a,
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
    id, user_id, greenlist, disease_data, sellers
) {
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

            farm_status(disease_data(), user_id())
        }, sanitize.text.function = function(t) t, align = "c")

        shiny::observeEvent(input$select_diseases, {
            selected_diseases <- input$select_diseases
            if ("salmonella" %in% selected_diseases)
                shinyjs::hide("salmonella_warning")
            else
                shinyjs::show("salmonella_warning")
        }, ignoreNULL = FALSE)

        shiny::observeEvent(input$apply_filters, {
            selected_counties <- input$select_counties
            selected_types <- input$select_types

            if (is.null(selected_counties) || is.null(selected_types)) {
                shinyWidgets::sendSweetAlert(
                    title = NULL,
                    text = paste0(
                        "F\u00f6r att s\u00f6ka m\u00e5ste du v\u00e4lja ",
                        "minst ett l\u00e4n och en djurtyp."
                    ),
                    type = "warning"
                )
                return()
            }

            selected_diseases <- input$select_diseases
            if (is.null(selected_diseases))
                selected_diseases <- character()

            selected_breeds <- input$select_breeds
            if (is.null(selected_breeds))
                selected_breeds <- character()

            dt <- format_seller_list(
                sellers(), free_from = selected_diseases,
                counties = selected_counties, types = selected_types,
                breeds = selected_breeds
            )

            filtered_sellers(dt)
        })

        output$market_table <- shiny::renderDataTable(
            {
                dt <- filtered_sellers()

                data.table::setnames(
                    dt,
                    new = c(
                        "Bes\u00e4ttnings-ID",
                        "L\u00e4n",
                        stringr::str_to_sentence(valid_agents()),
                        "Djurtyp",
                        "Djurras",
                        "Antal djur tillg\u00e4ngliga"
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
                        "/plug-ins/1.11.5/i18n/sv-SE.json"
                    )
                ),
                pageLength = 15,
                lengthChange = FALSE
            ),
            escape = FALSE
        )
    })
}
