#' @noRd
buying_known_ui <- function(id) {
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
                shiny::br(), shiny::br(),
                shiny::fluidRow(
                    shiny::column(
                        shiny::wellPanel(
                            shiny::numericInput(
                                inputId = ns("n_animals"),
                                label = "Antal djur att k\u00f6pa",
                                value = 0,
                                min = 0,
                                step = 1
                            ),
                            shiny::textInput(
                                inputId = ns("herd_id"),
                                label = paste0(
                                    "Bes\u00e4ttnings-ID ",
                                    "(anv\u00e4nd V\u00e4xas)"
                                ),
                                value = NULL,
                                placeholder = "Bes\u00e4ttningens ID"
                            ),
                            shiny::uiOutput(outputId = ns("county_select")),
                            shiny::actionButton(
                                inputId = ns("add_row"),
                                "L\u00e4gg till ink\u00f6p i tabell"
                            ),
                            shiny::br(),
                            shiny::p(
                                shiny::em(
                                    paste(
                                        "Om bes\u00e4ttningen redan finns i",
                                        "ink\u00f6pstabellen kommer detta",
                                        "ink\u00f6p att l\u00e4ggas ihop med ",
                                        "tidigare p\u00e5 befintlig rad."
                                    )
                                )
                            ),
                            shiny::br(),
                            shiny::actionButton(
                                ns("clear_table"), "Rensa ink\u00f6pstabell"
                            )
                        ),
                        width = 12
                    ),
                ),
                width = 3
            ),
            shiny::column(
                shiny::tabsetPanel(
                    shiny::tabPanel(
                        "\u00d6versikt ink\u00f6p",
                        shiny::fluidRow(
                            shiny::column(
                                shiny::br(),
                                shiny::h3("\u00d6versikt"),
                                width = 12
                            )
                        ),
                        shiny::fluidRow(
                            shiny::column(
                                shiny::actionButton(
                                    inputId = ns("remove_row"),
                                    "Ta bort markerade ink\u00f6p"
                                ),
                                width = 12
                            )
                        ),
                        shiny::fluidRow(
                            shiny::column(
                                DT::DTOutput(outputId = ns("greenlist_table")),
                                width = 12
                            )
                        )
                    ),
                    buying_disease_tab(ns, "salmonella"),
                    buying_disease_tab(ns, "mycoplasma"),
                    id = ns("buying_disease_tabs"),
                    type = "tabs"
                ),
                width = 9
            )
        )
    )
}

#' @noRd
buying_known_server <- function(id, user_id, greenlist,
                                counties, disease_data) {

    shiny::moduleServer(id, function(input, output, session) {
        purchase_table <- shiny::reactiveVal(
            data.frame(
                herd_id = numeric(),
                county = character(),
                n_animals = numeric()
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

        output$county_select <- shiny::renderUI({
            g_l <- greenlist()$id
            p_t <- purchase_table()

            shiny::req(
                nchar(input$herd_id) && !(as.numeric(input$herd_id) %in% g_l)
            )

            choices <-
                if (input$herd_id %in% p_t$herd_id)
                    p_t[p_t$herd_id == as.numeric(input$herd_id), ]$county
                else
                    counties()$countyName

            shiny::selectInput(
                inputId = shiny::NS(id)("county_select_pick"),
                label = paste(
                    "Bes\u00e4ttnings-ID:t finns ej i FriskKo-",
                    "listan, v\u00e4lj l\u00e4n varifr\u00e5n",
                    "k\u00f6pet skedde:"
                ),
                choices = choices,
                width = "50%"
            )
        })

        output$greenlist_table <- DT::renderDT({
            p_t <- data.table::setDT(purchase_table())

            g_l <- greenlist()[, c("id", "greenlist")]

            p_t <- data.table::merge.data.table(
                p_t,
                g_l,
                by.x = "herd_id",
                by.y = "id",
                all.x = TRUE
            )

            p_t$greenlist[is.na(p_t$greenlist)] <- 1

            data.table::set(
                p_t,
                j = "greenlist",
                value = greenlist_status(p_t$greenlist)
            )

            show_rownames <- nrow(p_t) > 0

            if (nrow(p_t))
                rownames(p_t) <- paste("Bes\u00e4ttning", seq_len(nrow(p_t)))

            DT::datatable(
                p_t,
                rownames = show_rownames,
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
                    "Bes\u00e4ttnings-ID",
                    "L\u00e4n",
                    "Antal k\u00f6pta djur",
                    "Status"
                )
            ) |>
            DT::formatStyle(
                "greenlist",
                backgroundColor = DT::styleEqual(
                    c("Gr\u00f6na listan", "Ok\u00e4nd"),
                    c("green", "white")
                ),
                color = DT::styleEqual(
                    c("Gr\u00f6na listan", "Ok\u00e4nd"),
                    c("white", "black")
                )
            )
        })

        output$salmonella_table <- DT::renderDT({
            p_t <- data.table::setDT(purchase_table())
            a_d <- disease_data()[["salmonella"]]
            agent_table(a_d, p_t)
        })

        output$mycoplasma_table <- DT::renderDT({
            p_t <- data.table::setDT(purchase_table())
            a_d <- disease_data()[["mycoplasma"]]
            agent_table(a_d, p_t)
        })

        shiny::observeEvent(input$add_row, {
            if (!is.null(input$n_animals) &&
                    as.numeric(input$n_animals) > 0 &&
                    nchar(input$herd_id) &&
                    input$herd_id != as.character(user_id())) {

                g_l <- greenlist()

                in_program <- input$herd_id %in% g_l$id

                herd_county <- ifelse(
                    in_program,
                    g_l[g_l$id == input$herd_id, ]$county,
                    input$county_select_pick
                )

                p_t <- purchase_table()

                if (input$herd_id %in% p_t$herd_id) {
                    c_current <-
                        p_t[p_t$herd_id == as.numeric(input$herd_id), ]$county

                    if (in_program || identical(
                        input$county_select_pick,
                        c_current
                    )) {
                        n_current <- p_t[p_t$herd_id == as.numeric(
                            input$herd_id
                        ), ]$n_animals

                        n_new <- n_current + as.numeric(input$n_animals)

                        p_t[p_t$herd_id == input$herd_id, ]$n_animals <- n_new
                    }
                } else {
                    p_t <- rbind(
                        p_t,
                        data.frame(
                            herd_id = as.numeric(input$herd_id),
                            county = herd_county,
                            n_animals = as.numeric(input$n_animals)
                        )
                    )
                }

                purchase_table(p_t)
            }
        })

        shiny::observeEvent(input$remove_row, {
            n <- input$greenlist_table_rows_selected
            shiny::req(!is.null(n))

            p_t <- purchase_table()

            p_t <- p_t[-n, ]

            purchase_table(p_t)
        })

        shiny::observeEvent(input$clear_table, {
            shinyWidgets::confirmSweetAlert(
                session = session,
                inputId = shiny::NS(id)("confirm_clear"),
                type = "warning",
                title = paste("S\u00e4ker att du vill rensa tabellen?",
                              "OBS! Kan ej \u00e5ngras"),
                danger_mode = TRUE,
                btn_labels = c("Avbryt", "Bekr\u00e4fta")
            )
        })

        shiny::observeEvent(input$confirm_clear, {
            if (isTRUE(input$confirm_clear)) {
                p_t <- purchase_table()

                p_t <- p_t[0, ]

                purchase_table(p_t)
            }
        })
    })
}
