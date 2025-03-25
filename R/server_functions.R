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
        #ifelse(
        #    x == 0,
        #    "Grønne listen",
        #    "Ukjent"
        #)
      
      if(x ==0){return("Grønne listen")}else{
        if(x ==1){return("Ukjent")}else{return("Infisert")}
      }
      
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
            return("Ukjent")
        x <- x[1:4]

        if (all(x == 0))
            return("Fri fra infeksjon")

        "Infisert"
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

    p_t$result[is.na(p_t$result)] <- "Ukjent"

    show_rownames <- nrow(p_t) > 0

    if (nrow(p_t))
        rownames(p_t) <- paste("Besetning", seq_len(nrow(p_t)))

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
            columnDefs = list(list(className = "dt-right", targets = 1)),
            selection = "none"
        ),
        colnames = c(
            "Besetnings-ID",
            "Fylke",
            "Antall kjøpte dyr",
            "Siste analysedato",
            "Status"
        )
    ) |> DT::formatStyle(
        "result",
        backgroundColor = DT::styleEqual(
            c("Fri fra infeksjon", "Ukjent","Infisert"),
            c("green", "white","red")
        ),
        color = DT::styleEqual(
            c("Fri fra infeksjon", "Ukjent","Infisert"),
            c("white", "black","white")
        )
    )
}

#' @noRd
farm_status <- function(d_d, uid) {
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
                "1" = "Ikke på grønne listen ",
                "0" = "Grønne listen ",
                "Unknown" = "Ukjent "
            ),
            switch(
                x,
                "1" = "Infeksjon påvist ",
                "0" = "Fri fra infeksjon ",
                "Unknown" = "Ukjent "
            )
        ), res)
    }

    shiny::span(res, style = paste0("color: ", color, ";"))
}

#' @noRd
my_results <- function(agent, my_id) {
    id <- result <- date <- NULL
    get_dataset(agent, latest = FALSE)[id == my_id, list(
        Prøvetakingsdato = as.character(date),
        Prøveresultat = sapply(
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
