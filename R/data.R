#' @noRd
all_animal_types <- function() {
    c(
        "Kvigekalv 1-3 uker", "Oksekalv 1-3 uker",
        "Kvigekalv 4-8 uker", "Oksekalv 4-8 uker",
        "Kvigekalv 8-16 uker", "Oksekalv 8-16 uker",
        "Kvigekalv 16-24 uker", "Oksekalv 16-24 uker",
        "Kvigekalv 24-42 uker", "Oksekalv 24-42 uker",
        "Ung Kvige", "Kvige", "Ung ku", "Ku", "Ung Okse",
        "Brukt Okse"
    )
}

#' @noRd
all_animal_breeds <- function() {
    c(
        "Aberdeen angus", "Charolais", "Hereford", "Limousin", "Simmental",
        "Norsk Rødt Fe (NRF)", "Holstein", "Jersey", "Scottish Highland",
        "Andre kjøttferaser", "Gamle norske raser", "Krysning - melkeraser",
        "Krysning - kjøttfe"
    )
}

#' valid_agents
#' @noRd
valid_agents <- function() {
    c("BCoV", "BRSV", "Klauvstatus", "Jurstatus")
}

#' relevant_period
#' @noRd
relevant_period <- function() {
    c(410, 410, 365, 365) |>
        setNames(valid_agents())
}

#' relevant_free_obs
#' @noRd
relevant_free_obs <- function() {
    c(1, 1, 2, 4) |>
        setNames(valid_agents())
}

#' get_counties
#' @noRd
get_counties <- function() {
    readRDS(system.file("datasets/counties.rds", package = "purchaseAssistant"))
}

#' @noRd
get_seller_list <- function(my_id) {
    herdID <- NULL # nolint

    dt <- data.table::fread(
        system.file(
            "datasets/dt_fake_sellers2App.csv",
            package = "purchaseAssistant"
        )
    )[herdID != my_id]

    dt <- data.table::merge.data.table(
        dt, get_counties()[, c("county", "countyName")],
        by = "county"
    )

    data.table::set(dt, j = "county", value = dt$countyName)

    cbind(dt[, "herdID"], dt[, -c("herdID", "countyName")])
}

#' get_user
#'
#' @noRd
get_user <- function() {
    user_list <- data.table::fread(
        system.file("users.csv", package = "purchaseAssistant"),
        encoding = "UTF-8"
    )
    my_user <- Sys.getenv("SHINYPROXY_USERNAME")

    if (!nchar(my_user) || !(my_user %in% user_list$uid)) {
        my_user <- "test.user"
    }

    uid <- NULL # bypass lintr
    as.list(user_list[uid == my_user])
}

#' get_dataset
#'
#' Load a dataset corresponding to one specific bacterial agent.
#'
#' @noRd
get_dataset <- function(agent, latest = TRUE, relevant_period) {
    agent <- match.arg(agent, choices = valid_agents())
    file_name <- file.path(
        system.file("datasets", package = "purchaseAssistant"),
        paste0("dt_", agent, "2App.csv")
    )
    print(file_name)
    stopifnot(file.exists(file_name))

    dt <- data.table::fread(file_name)[
        date >= Sys.Date() - relevant_period,
        c("id", "county", "date", "result")
    ][order(date)]

    stopifnot(
        identical(
            sort(names(dt)),
            c("county", "date", "id", "result")
        )
    )

    dt <- data.table::merge.data.table(
        dt,
        get_counties()[, c("county", "countyName")],
        by = "county",
        all.x = TRUE
    )

    id <- date <- result <- NULL

    data.table::setorder(dt, id, -date)

    if (isTRUE(latest)) {
        dt <- dt[
            , list(
                date = date[1],
                result = result[1]
            ),
            by = c("id", "countyName")
        ]
    }

    dt
}

#' get_disease_data
#'
#' Return all disease datasets
#'
#' @noRd
get_disease_data <- function() {
    agents <- valid_agents()
    disease_data <- lapply(
        agents,
        function(x) get_dataset(x, latest = FALSE, relevant_period()[x])
    )
    names(disease_data) <- agents
    disease_data
}

#' @noRd
result_to_greenlist <- function(result, freenr) {
    stopifnot(is.numeric(result))
    ifelse(
        length(result) <= freenr,
        1,
        2 * as.numeric(any(
            result > 0,
            na.rm = TRUE
        ))
    )
}

#' get_greenlist
#'
#' Load all datasets and calculate the total greenlist status
#'
#' @noRd
get_greenlist <- function() {
    agents <- valid_agents()
    freenr <- relevant_free_obs()
    stopifnot(length(agents) > 0)

    result <- NULL

    dt <- get_dataset(agents[1], latest = FALSE, relevant_period()[[1]])[, -"date"][,
        list(total = result_to_greenlist(result, freenr[1])),
        by = c("id", "countyName")
    ]

    if (length(agents) > 1) {
        for (a in 2:length(agents)) {
            dt_a <- get_dataset(agents[a], latest = FALSE, relevant_period()[[a]])[, -"date"][,
                list(result = result_to_greenlist(result, freenr[a])),
                by = c("id", "countyName")
            ]

            dt <- data.table::merge.data.table(
                dt, dt_a,
                by = c("id", "countyName"),
                all.x = TRUE
            )

            data.table::set(
                dt,
                j = "total",
                value = pmax(dt$total, dt$result)
            )

            dt <- dt[, -"result"]
        }
    }

    id <- countyName <- total <- NULL # nolint

    dt[, list(
        id,
        county = countyName,
        greenlist = total
    )][order(id)]
}
