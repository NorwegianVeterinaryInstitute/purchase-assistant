#' @noRd
all_animal_types <- function() {
    c("Avvent kvigekalv yngre enn 6 måneder",
      "Avvent oksekalv yngre enn 6 måneder", "Diekvigekalv",
      "Dieoksekalv", "Ku", "Kvige (drektig)",
      "Kvige (drektighet ukjent)", "Kvige (ikke drektig)", "Okse",
      "Stut", "Tjur", "Ungokse")
}

#' @noRd
all_animal_breeds <- function() {
    c("Angus", "Charolais", "Hereford", "Limousin", "Simmental",
      "Svensk Jersey-boskap (SJB)", "Svensk kullig boskap (SKB)",
      "Svensk lavlandsboskap (SLB)",
      "Svensk rød og hvit boskap (SRB)")
}

#' valid_agents
#' @noRd
valid_agents <- function() {
    c("salmonella", "mycoplasma")
}

#' get_counties
#' @noRd
get_counties <- function() {
    readRDS(system.file("datasets/counties.rds", package = "purchaseAssistant"))
}

#' @noRd
get_seller_list <- function(my_id) {
    herdID <- NULL #nolint

    dt <- data.table::fread(
        system.file(
            "datasets/dt_fake_sellers2App.csv", package = "purchaseAssistant"
        )
    )[herdID != my_id]

    dt <- data.table::merge.data.table(
        dt, get_counties()[, c("county", "countyName")], by = "county"
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

    if (!nchar(my_user) || !(my_user %in% user_list$uid))
        my_user <- "test.user"

    uid <- NULL # bypass lintr
    as.list(user_list[uid == my_user])
}

#' get_dataset
#'
#' Load a dataset corresponding to one specific bacterial agent.
#'
#' @noRd
get_dataset <- function(agent = valid_agents(), latest = TRUE) {
    agent <- match.arg(agent)

    file_name <- file.path(
        system.file("datasets", package = "purchaseAssistant"),
        paste0("dt_", agent, "2App.csv")
    )

    stopifnot(file.exists(file_name))

    dt <- data.table::fread(file_name)

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

    id <- date <- result <-  NULL

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
    agents  <- valid_agents()
    disease_data <- lapply(
        agents, function(x) get_dataset(x, latest = FALSE)
    )
    names(disease_data) <- agents
    disease_data
}

#' @noRd
result_to_greenlist <- function(result) {
    stopifnot(is.numeric(result))
    ifelse(
        length(result) < 4,
        1,
        as.numeric(any(
            result[1:4] > 0,
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
    stopifnot(length(agents) > 0)

    result <- NULL

    dt <- get_dataset(agents[1], latest = FALSE)[, -"date"][,
        list(total = result_to_greenlist(result)),
        by = c("id", "countyName")
    ]

    if (length(agents) > 1) {
        for (a in agents[2:length(agents)]) {
            dt_a <- get_dataset(a, latest = FALSE)[, -"date"][,
                list(result = result_to_greenlist(result)),
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
                value = dt$total + dt$result
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
