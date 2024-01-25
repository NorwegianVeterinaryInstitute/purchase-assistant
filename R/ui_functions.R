#' @noRd
home_box <- function(title, description, page, icon) {
    shiny::wellPanel(
        shiny::fluidRow(
            shiny::column(
                shiny::strong(shiny::h5(title)),
                shiny::p(description),
                width = 12
            )
        ),
        shiny::br(),
        shiny::fluidRow(
            shiny::column(
                shiny::div(
                    shiny::actionButton(
                        inputId = paste0("link_", page),
                        label = title,
                        onclick = sprintf(
                            "location.href='%s'",
                            shiny.router::route_link(page)
                        ),
                        icon = shiny::icon(icon)
                    ),
                    style = "text-align:center;"
                ),
                width = 12
            )
        ),
        style = "min-height:30vh; width:100%; padding-bottom:10px;"
    )
}

#' @noRd
my_herd_panel <- function(ns, include_status = TRUE) {
    tags <- shiny::tagList(
        shiny::p(
            shiny::uiOutput(outputId = ns("my_herd_id"))
        ),
        shiny::p(
            shiny::uiOutput(outputId = ns("my_county"))
        )
    )

    if (isTRUE(include_status)) {
        tags <- shiny::tagAppendChild(
            tags,
            shiny::p(
                shiny::h6("Min g\u00e5rds status:"),
                shiny::tableOutput(outputId = ns("my_status")),
                shiny::tags$ul(
                    shiny::tags$li(
                        shiny::span(
                            shiny::HTML("&check;"), style = "color:green"
                        ),
                        ": De fyra senaste provtagningarna har varit smittfria."
                    ),
                    shiny::tags$li(
                        shiny::span(
                            shiny::HTML("&#9679;"), style = "color:red"
                        ),
                        paste0(
                            ": Minst en av de fyra senaste provtagningarna ",
                            "\u00e4r ej smittfri."
                        )
                    ),
                    shiny::tags$li(
                        shiny::span(
                            shiny::HTML("&#9679;"), style = "color:gray"
                        ),
                        paste0(
                            ": Fyra p\u00e5 varandra f\u00f6ljande ",
                            "provtagningar har \u00e4nnu inte gjorts."
                        )
                    ),
                    style = "list-style-type: none; font-size: 10px;"
                )
            )
        )
    }

    shiny::wellPanel(tags)
}

#' @noRd
my_farm_status <- function(ns) {
    tags <- shiny::tagList(
        shiny::uiOutput(ns("my_greenlist")),
        shiny::div(
            shiny::tableOutput(outputId = ns("my_status")),
            style = "font-size: 24px;"
        ),
        shiny::h6("Teckenf\u00f6rklaring"),
        shiny::tags$ul(
            shiny::tags$li(
                shiny::span(
                    shiny::HTML("&check;"), style = "color:green"
                ),
                ": De fyra senaste provtagningarna har varit smittfria."
            ),
            shiny::tags$li(
                shiny::span(
                    shiny::HTML("&#9679;"), style = "color:red"
                ),
                paste0(
                    ": Minst en av de fyra senaste provtagningarna \u00e4r ej ",
                    "smittfri."
                )
            ),
            shiny::tags$li(
                shiny::span(
                    shiny::HTML("&#9679;"), style = "color:gray"
                ),
                paste0(
                    ": Fyra p\u00e5 varandra f\u00f6ljande provtagningar ",
                    " har \u00e4nnu inte gjorts."
                )
            ),
            style = "list-style-type: none; font-size: 14px;"
        )
    )

    shiny::fluidRow(shiny::column(tags, width = 12))
}

#' @noRd
buying_disease_tab <- function(ns, name) {
    title <- stringr::str_to_sentence(name)
    name <- tolower(name)

    shiny::tabPanel(
        title,
        shiny::fluidRow(
            shiny::column(
                shiny::br(),
                shiny::h3(title),
                width = 12
            )
        ),
        shiny::fluidRow(
            DT::DTOutput(outputId = ns(paste0(name, "_table")))
        )
    )
}
