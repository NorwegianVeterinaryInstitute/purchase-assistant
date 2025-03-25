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
                                label = "Antall dyr å kjøpe",
                                value = 0,
                                min = 0,
                                step = 1
                            ),
                            shiny::textInput(
                                inputId = ns("herd_id"),
                                label = paste0(
                                    "Besetnings-ID ",
                                    "(bruk Nortura eller TINE)"
                                ),
                                value = NULL,
                                placeholder = "Besetningens ID"
                            ),
                            shiny::uiOutput(outputId = ns("county_select")),
                            shiny::actionButton(
                                inputId = ns("add_row"),
                                "Legg til kjøp i tabell"
                            ),
                            shiny::br(),
                            shiny::p(
                                shiny::em(
                                    paste(
                                        "Hvis besetningen allerede finnes i",
                                        "kjøpstabellen vil dette",
                                        "kjøpet bli lagt sammen med ",
                                        "tidligere på eksisterende rad."
                                    )
                                )
                            ),
                            shiny::br(),
                            shiny::actionButton(
                                ns("clear_table"), "Tøm kjøpstabell"
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
                        "Oversikt kjøp",
                        shiny::fluidRow(
                            shiny::column(
                                shiny::br(),
                                shiny::h3("Oversikt"),
                                width = 12
                            )
                        ),
                        shiny::fluidRow(
                            shiny::column(
                                shiny::actionButton(
                                    inputId = ns("remove_row"),
                                    "Fjern markerte kjøp"
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
                    buying_disease_tab(ns, "BCoV"),
                    buying_disease_tab(ns, "BRSV"),
                    buying_disease_tab(ns, "Klauvstatus"),
                    buying_disease_tab(ns, "Jurstatus"),
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
                    "Besetnings-ID finnes ikke i bruker",
                    "listen, velg fylke hvorfra",
                    "kjøpet skjedde:"
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
                rownames(p_t) <- paste("Besetning", seq_len(nrow(p_t)))
            
            print(p_t)
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
                            '/plug-ins/1.11.5/i18n/nb-NO.json" }'
                        )
                    ),
                    columnDefs = list(list(className = "dt-right", targets = 1))
                ),
                colnames = c(
                    "Besetnings-ID",
                    "Fylke",
                    "Antall kjøpte dyr",
                    "Status"
                )
            ) |>
            DT::formatStyle(
                "greenlist",
                backgroundColor = DT::styleEqual(
                    c("Grønne listen", "Ukjent","Infisert"),
                    c("green", "white","red")
                ),
                color = DT::styleEqual(
                    c("Grønne listen", "Ukjent","Infisert"),
                    c("white", "black","white")
                )
            )
        })

       # output$salmonella_table <- DT::renderDT({
       #     p_t <- data.table::setDT(purchase_table())
       #     a_d <- disease_data()[["salmonella"]]
       #     agent_table(a_d, p_t)
       # })

      #  output$mycoplasma_table <- DT::renderDT({
      #      p_t <- data.table::setDT(purchase_table())
      #      a_d <- disease_data()[["mycoplasma"]]
      #      agent_table(a_d, p_t)
      #  })
      
        
        output$bcov_table <- DT::renderDT({
          p_t <- data.table::setDT(purchase_table())
          a_d <- disease_data()[["BCoV"]]
          agent_table(a_d, p_t)
        })
        
        output$brsv_table <- DT::renderDT({
          p_t <- data.table::setDT(purchase_table())
          a_d <- disease_data()[["BRSV"]]
          agent_table(a_d, p_t)
        })
        
        output$klauvstatus_table <- DT::renderDT({
          p_t <- data.table::setDT(purchase_table())
          a_d <- disease_data()[["Klauvstatus"]]
          agent_table(a_d, p_t)
        })
        
        output$jurstatus_table <- DT::renderDT({
          p_t <- data.table::setDT(purchase_table())
          a_d <- disease_data()[["Jurstatus"]]
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
                title = paste("Er du sikker på at du vil tømme tabellen?",
                              "OBS! Kan ikke angres"),
                danger_mode = TRUE,
                btn_labels = c("Avbryt", "Bekreft")
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
