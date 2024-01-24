#' @noRd
greenlist_status <- function(res, personal = FALSE) {
    sapply(res, function(x) {
        if (!is.numeric(x)) {
            return(x)
        }
        if (isTRUE(personal)) {
            return(
                x
            )
        }
        ifelse(
            x == 0,
            "Gr\u00f6na listan",
            "Ok\u00e4nd"
        )
    })
}

#' @noRd
agent_table <- function(a_d, p_t) {
    p_t <- data.table::merge.data.table(
        p_t, a_d[, c("id", "date", "result")],
        by.x = "herd_id",
        by.y = "id",
        all.x = TRUE
    )

    data.table::setorderv(p_t, cols = c("herd_id", "date"), order = c(1, -1))

    agent_status <- function(x) {
        if (length(x) < 4)
            return("Ok\u00e4nd")
        x <- x[1:4]

        if (all(x == 0))
            return("Fri fr\u00e5n infektion")

        "Ok\u00e4nd"
    }

    date <- result <- NULL

    p_t <- p_t[,
        list(
            county = county[1],
            n_animals = n_animals[1],
            last_date = date[1],
            result = agent_status(result)
        ),
        by = "herd_id"
    ]

    p_t$result[is.na(p_t$result)] <- "Ok\u00e4nd"

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
            columnDefs = list(list(className = "dt-right", targets = 1)),
            selection = "none"
        ),
        colnames = c(
            "Bes\u00e4ttnings-ID",
            "L\u00e4n",
            "Antal k\u00f6pta djur",
            "Senaste analysdatum",
            "Status"
        )
    ) |> DT::formatStyle(
        "result",
        backgroundColor = DT::styleEqual(
            c("Fri fr\u00e5n infektion", "Ok\u00e4nd"),
            c("green", "white")
        ),
        color = DT::styleEqual(
            c("Fri fr\u00e5n infektion", "Ok\u00e4nd"),
            c("white", "black")
        )
    )
}

#' @noRd
friskko_status <- function(d_d, uid) {
    names(d_d) <- stringr::str_to_sentence(names(d_d))

    as.data.frame(lapply(
        d_d, function(x) {

            res <- x[x$id == uid, ]

            if (nrow(res) == 0) {
                res <- "Unknown"
            } else {
                res <- ifelse(
                    length(res$result) < 4,
                    "Unknown",
                    as.character(sum(res$result))
                )
            }

            utils::capture.output(shiny::strong(status_to_symbol(res)))
        }
    ))
}

#' @noRd
status_to_symbol <- function(x, include_pretext = FALSE, greenlist = TRUE) {
    if (is.na(x))
        x <- "Unknown"

    if (x != "Unknown" && as.numeric(x) > 1)
        x <- "1"

    res <- switch(
        x,
        "0" = shiny::HTML("&check;"),
        "1" = shiny::HTML("&#9679;"),
        "Unknown" = shiny::HTML("&#9679;"),
    )

    color <- switch(
        x, "1" = "red", "0" = "green", "Unknown" = "gray"
    )

    if (isTRUE(include_pretext)) {
        res <- shiny::tagList(ifelse(
            isTRUE(greenlist),
            switch(
                x,
                "1" = "Ej gr\u00f6n ",
                "0" = "Gr\u00f6na listan ",
                "Unknown" = "Ok\u00e4nd "
            ),
            switch(
                x,
                "1" = "Smitta p\u00e5visad ",
                "0" = "Fri fr\u00e5n infektion ",
                "Unknown" = "Ok\u00e4nd "
            )
        ), res)
    }

    shiny::span(res, style = paste0("color: ", color, ";"))
}

#' @noRd
my_results <- function(agent, my_id) {
    id <- result <- date <- NULL
    get_dataset(agent, latest = FALSE)[id == my_id, list(
        Provtagningsdatum = as.character(date),
        Provresultat = sapply(
            result[seq_len(min(4, length(result)))], function(x)  {
                paste(utils::capture.output(
                    status_to_symbol(
                        as.character(x), include_pretext = TRUE,
                        greenlist = FALSE
                    )
                ), collapse = "")
            }
        )
    )]
}
