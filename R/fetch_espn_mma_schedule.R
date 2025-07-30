#' Fetch MMA schedule data from ESPN's unofficial API
#'
#' Retrieves MMA event schedules from ESPN's hidden API endpoints. This function
#' provides access to upcoming and past MMA events including UFC, Bellator,
#' PFL, and other major promotions covered by ESPN.
#'
#' @param league Character. MMA league/promotion to fetch. Options include:
#'   "ufc" (default), "bellator", "pfl", or NULL for all MMA events.
#' @param dates Character. Date or date range in YYYYMMDD format. Can be:
#'   - Single date: "20250801"
#'   - Date range: "20250801-20250831"
#'   - NULL for current/upcoming events (default)
#' @param limit Integer. Maximum number of events to return (default: 100).
#' @param include_results Logical. Whether to include completed fight results (default: TRUE).
#' @param timeout Numeric. Request timeout in seconds (default: 30).
#' @param raw Logical. If TRUE, assigns raw JSON to global environment. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @details
#' This function uses ESPN's unofficial/hidden API endpoints that are not
#' officially documented or supported. The API structure may change without
#' notice, and rate limiting may apply.
#'
#' **Important Notes:**
#' \itemize{
#'   \item This is an unofficial API - use at your own risk
#'   \item ESPN may block requests or change endpoints without notice
#'   \item Respect rate limits to avoid being blocked
#'   \item Consider ESPN's terms of service
#' }
#'
#' The function attempts multiple endpoint strategies and assigns cleaned data
#' to the global environment as 'mma_schedule'.
#'
#' @examples
#' \dontrun{
#' # Get current UFC events
#' fetch_espn_mma_schedule()
#' head(mma_schedule)
#'
#' # Get UFC events for a specific date
#' fetch_espn_mma_schedule(league = "ufc", dates = "20250801")
#'
#' # Get all MMA events for a date range
#' fetch_espn_mma_schedule(league = NULL, dates = "20250801-20250831", limit = 50)
#'
#' # Get raw data
#' fetch_espn_mma_schedule(league = "ufc", raw = TRUE)
#' }
#'
#' @importFrom httr GET status_code content timeout user_agent add_headers modify_url
#' @importFrom jsonlite fromJSON
fetch_espn_mma_schedule <- function(league = "ufc",
                                    dates = NULL,
                                    limit = 100,
                                    include_results = TRUE,
                                    timeout = 30,
                                    raw = FALSE) {

  # Input validation
  if (!is.null(league) && (!is.character(league) || length(league) != 1)) {
    stop("league must be a character string or NULL")
  }

  # Validate league options
  valid_leagues <- c("ufc", "bellator", "pfl", "one", "rizin")
  if (!is.null(league) && !league %in% valid_leagues) {
    warning("League '", league, "' may not be supported. Supported leagues: ",
            paste(valid_leagues, collapse = ", "))
  }

  # Validate dates
  if (!is.null(dates)) {
    if (!is.character(dates) || length(dates) != 1) {
      stop("dates must be a character string in YYYYMMDD or YYYYMMDD-YYYYMMDD format")
    }

    # Check date format
    if (!grepl("^\\d{8}(-\\d{8})?$", dates)) {
      stop("dates must be in YYYYMMDD or YYYYMMDD-YYYYMMDD format")
    }

    # Validate individual dates
    date_parts <- strsplit(dates, "-")[[1]]
    for (date_part in date_parts) {
      tryCatch({
        parsed_date <- as.Date(date_part, format = "%Y%m%d")
        if (is.na(parsed_date)) {
          stop("Invalid date: ", date_part)
        }
      }, error = function(e) {
        stop("Invalid date format: ", date_part)
      })
    }
  }

  if (!is.numeric(limit) || length(limit) != 1 || limit <= 0 || limit > 1000) {
    stop("limit must be a positive integer <= 1000")
  }

  if (!is.logical(include_results) || length(include_results) != 1) {
    stop("include_results must be TRUE or FALSE")
  }

  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    stop("timeout must be a positive numeric value")
  }

  if (!is.logical(raw) || length(raw) != 1) {
    stop("raw must be TRUE or FALSE")
  }

  # Ensure required packages are available
  required_packages <- c("httr", "jsonlite")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required but not installed")
    }
  }

  # Build URL
  base_url <- "https://site.api.espn.com/apis/site/v2/sports/mma"

  if (!is.null(league)) {
    url <- paste0(base_url, "/", league, "/scoreboard")
  } else {
    url <- paste0(base_url, "/scoreboard")
  }

  # Build query parameters
  query_params <- list(limit = limit)
  if (!is.null(dates)) {
    query_params$dates <- dates
  }

  final_url <- httr::modify_url(url, query = query_params)

  # Fetch and parse
  tryCatch({
    resp <- httr::GET(
      final_url,
      httr::timeout(timeout),
      httr::user_agent("fighteR R package - ESPN MMA Schedule Fetcher"),
      httr::add_headers(
        "Accept" = "application/json",
        "Accept-Language" = "en-US,en;q=0.9"
      )
    )

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment
    if (isTRUE(raw)) {
      assign("mma_schedule_raw", data, envir = .GlobalEnv)
      message("Raw MMA schedule data assigned to: mma_schedule_raw")
      return(invisible(data))
    }

    # Create clean schedule dataset
    schedule_df <- create_clean_mma_dataset(data, league, dates, include_results)

    # Assign to global environment
    assign("mma_schedule", schedule_df, envir = .GlobalEnv)

    message(sprintf("MMA schedule data assigned to: mma_schedule (%d events)", nrow(schedule_df)))

    return(invisible(schedule_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch MMA schedule data: %s", e$message))
  })
}

#' Create clean MMA schedule dataset from ESPN API response
#'
#' @param data Raw JSON response from ESPN API
#' @param league League parameter used in request
#' @param dates Dates parameter used in request
#' @param include_results Whether to include completed events
#' @return Clean data frame with one row per event
create_clean_mma_dataset <- function(data, league, dates, include_results) {
  # Initialize result data frame
  result_df <- data.frame(
    event_id = character(0),
    event_uid = character(0),
    event_name = character(0),
    short_name = character(0),
    event_date = character(0),
    status_type = character(0),
    status_detail = character(0),
    is_completed = logical(0),
    # Venue info
    venue_name = character(0),
    venue_city = character(0),
    venue_state = character(0),
    venue_country = character(0),
    # Event details
    promotion = character(0),
    event_type = character(0),
    # Main event info
    main_event = character(0),
    headliner_1 = character(0),
    headliner_2 = character(0),
    # Broadcast info
    broadcast_network = character(0),
    stringsAsFactors = FALSE
  )

  # Navigate to events
  events <- NULL
  if ("events" %in% names(data)) {
    events <- data[["events"]]
  }

  if (is.null(events) || length(events) == 0) {
    return(result_df)
  }

  # Process each event
  for (i in seq_along(events)) {
    event <- events[[i]]

    # Basic event info
    event_id <- if ("id" %in% names(event)) as.character(event[["id"]]) else NA_character_
    event_uid <- if ("uid" %in% names(event)) event[["uid"]] else NA_character_
    event_name <- if ("name" %in% names(event)) event[["name"]] else NA_character_
    short_name <- if ("shortName" %in% names(event)) event[["shortName"]] else NA_character_
    event_date <- if ("date" %in% names(event)) event[["date"]] else NA_character_

    # Status info
    status_info <- if ("status" %in% names(event)) event[["status"]] else list()
    status_type_info <- if ("type" %in% names(status_info)) status_info[["type"]] else list()
    status_type <- if ("name" %in% names(status_type_info)) status_type_info[["name"]] else NA_character_
    status_detail <- if ("description" %in% names(status_type_info)) status_type_info[["description"]] else NA_character_
    is_completed <- if ("completed" %in% names(status_type_info)) status_type_info[["completed"]] else FALSE

    # Initialize venue variables
    venue_name <- venue_city <- venue_state <- venue_country <- NA_character_

    # Initialize fight info
    main_event <- headliner_1 <- headliner_2 <- NA_character_

    # Initialize broadcast info
    broadcast_network <- NA_character_

    # Process competitions (fights)
    if ("competitions" %in% names(event) && length(event[["competitions"]]) > 0) {
      competition <- event[["competitions"]][[1]]

      # Venue info
      venue_info <- if ("venue" %in% names(competition)) competition[["venue"]] else list()
      venue_name <- if ("fullName" %in% names(venue_info)) venue_info[["fullName"]] else NA_character_

      venue_address <- if ("address" %in% names(venue_info)) venue_info[["address"]] else list()
      venue_city <- if ("city" %in% names(venue_address)) venue_address[["city"]] else NA_character_
      venue_state <- if ("state" %in% names(venue_address)) venue_address[["state"]] else NA_character_
      venue_country <- if ("country" %in% names(venue_address)) venue_address[["country"]] else NA_character_

      # Broadcast info
      if ("broadcasts" %in% names(competition) && length(competition[["broadcasts"]]) > 0) {
        broadcast_info <- competition[["broadcasts"]][[1]]
        broadcast_network <- if ("names" %in% names(broadcast_info) && length(broadcast_info[["names"]]) > 0) {
          paste(broadcast_info[["names"]], collapse = ", ")
        } else {
          NA_character_
        }
      }

      # Extract main event fighters
      if ("competitors" %in% names(competition) && length(competition[["competitors"]]) >= 2) {
        competitor1 <- competition[["competitors"]][[1]]
        competitor2 <- competition[["competitors"]][[2]]

        athlete1_info <- if ("athlete" %in% names(competitor1)) competitor1[["athlete"]] else list()
        athlete2_info <- if ("athlete" %in% names(competitor2)) competitor2[["athlete"]] else list()

        headliner_1 <- if ("displayName" %in% names(athlete1_info)) athlete1_info[["displayName"]] else NA_character_
        headliner_2 <- if ("displayName" %in% names(athlete2_info)) athlete2_info[["displayName"]] else NA_character_

        if (!is.na(headliner_1) && !is.na(headliner_2)) {
          main_event <- paste(headliner_1, "vs", headliner_2)
        }
      }
    }

    # Extract promotion and event type
    promotion <- extract_promotion_from_name(event_name)
    event_type <- extract_event_type_from_name(event_name)

    # Filter completed events if requested
    if (!include_results && isTRUE(is_completed)) {
      next
    }

    # Create event row
    event_row <- data.frame(
      event_id = event_id,
      event_uid = event_uid,
      event_name = event_name,
      short_name = short_name,
      event_date = event_date,
      status_type = status_type,
      status_detail = status_detail,
      is_completed = is_completed,
      venue_name = venue_name,
      venue_city = venue_city,
      venue_state = venue_state,
      venue_country = venue_country,
      promotion = promotion,
      event_type = event_type,
      main_event = main_event,
      headliner_1 = headliner_1,
      headliner_2 = headliner_2,
      broadcast_network = broadcast_network,
      stringsAsFactors = FALSE
    )

    # Add to result
    result_df <- rbind(result_df, event_row)
  }

  # Clean up row names
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
  }

  return(result_df)
}



#' Extract promotion from event name
#'
#' @param event_name Event name string
#' @return Promotion name
extract_promotion_from_name <- function(event_name) {
  if (is.na(event_name)) return(NA_character_)

  name_lower <- tolower(event_name)

  if (grepl("^ufc", name_lower)) return("UFC")
  if (grepl("bellator", name_lower)) return("Bellator")
  if (grepl("pfl", name_lower)) return("PFL")
  if (grepl("one championship", name_lower)) return("ONE Championship")
  if (grepl("rizin", name_lower)) return("RIZIN")
  if (grepl("lfa", name_lower)) return("LFA")
  if (grepl("cage warriors", name_lower)) return("Cage Warriors")

  return("Other")
}

#' Extract event type from event name
#'
#' @param event_name Event name string
#' @return Event type
extract_event_type_from_name <- function(event_name) {
  if (is.na(event_name)) return(NA_character_)

  name_lower <- tolower(event_name)

  if (grepl("fight night", name_lower)) return("Fight Night")
  if (grepl("championship", name_lower)) return("Championship")
  if (grepl("\\bufc \\d+", name_lower)) return("Numbered Event")
  if (grepl("semifinal", name_lower)) return("Semifinals")
  if (grepl("final", name_lower)) return("Finals")

  return("Regular Event")
}
