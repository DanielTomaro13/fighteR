#' Fetch MMA fight center data from ESPN's unofficial API
#'
#' Retrieves detailed fight card information from ESPN's MMA Fight Center,
#' including main card and preliminary fights with fighter details, records,
#' and fight information.
#'
#' @param event_id Character or Numeric. Specific event ID to fetch. If NULL,
#'   fetches the current featured fight center event (default: NULL).
#' @param league Character. MMA league/promotion. Options: "ufc", "bellator",
#'   "pfl", "one", "rizin", or NULL for any league (default: "ufc").
#' @param include_prelims Logical. Whether to include preliminary fights (default: TRUE).
#' @param timeout Numeric. Request timeout in seconds (default: 30).
#' @param raw Logical. If TRUE, assigns raw JSON to global environment (default: FALSE).
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @details
#' This function fetches detailed fight card information from ESPN's MMA Fight Center.
#' It provides comprehensive data about individual fights including:
#' \itemize{
#'   \item Fighter information (names, records, stats)
#'   \item Fight details (weight class, position on card)
#'   \item Event information (name, date, venue)
#'   \item Card structure (main card vs prelims)
#' }
#'
#' **Important Notes:**
#' \itemize{
#'   \item Uses ESPN's unofficial API - may change without notice
#'   \item Rate limiting may apply
#'   \item Some events may not have detailed fight data available
#' }
#'
#' The function assigns cleaned data to the global environment as 'mma_fights'.
#'
#' @examples
#' \dontrun{
#' # Get current featured fight card
#' fetch_espn_mma_fightcenter()
#' head(mma_fights)
#'
#' # Get specific event by ID
#' fetch_espn_mma_fightcenter(event_id = "600054244")
#'
#' # Get UFC fight card without prelims
#' fetch_espn_mma_fightcenter(league = "ufc", include_prelims = FALSE)
#'
#' # Get raw JSON data
#' fetch_espn_mma_fightcenter(event_id = "600054244", raw = TRUE)
#' }
#'
#' @importFrom httr GET status_code content timeout user_agent add_headers modify_url
#' @importFrom jsonlite fromJSON
fetch_espn_mma_fightcenter <- function(event_id = NULL,
                                       league = "ufc",
                                       include_prelims = TRUE,
                                       timeout = 30,
                                       raw = FALSE) {

  # Input validation
  if (!is.null(event_id) && (!is.character(event_id) && !is.numeric(event_id))) {
    stop("event_id must be a character string, numeric value, or NULL")
  }

  if (!is.null(event_id)) {
    event_id <- as.character(event_id)
  }

  if (!is.null(league) && (!is.character(league) || length(league) != 1)) {
    stop("league must be a character string or NULL")
  }

  # Validate league options
  valid_leagues <- c("ufc", "bellator", "pfl", "one", "rizin")
  if (!is.null(league) && !league %in% valid_leagues) {
    warning("League '", league, "' may not be supported. Supported leagues: ",
            paste(valid_leagues, collapse = ", "))
  }

  if (!is.logical(include_prelims) || length(include_prelims) != 1) {
    stop("include_prelims must be TRUE or FALSE")
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

  # Get event ID if not provided
  if (is.null(event_id)) {
    event_id <- get_current_featured_event(league, timeout)
    if (is.null(event_id)) {
      stop("Could not determine current featured event. Please provide event_id manually.")
    }
    message("Using featured event ID: ", event_id)
  }

  # Build URL for specific event - try multiple endpoint patterns
  base_urls <- c(
    "https://site.api.espn.com/apis/site/v2/sports/mma",
    "https://sports.core.api.espn.com/v2/sports/mma"
  )

  # Try different URL patterns
  url_patterns <- list()

  for (base_url in base_urls) {
    if (!is.null(league)) {
      url_patterns <- append(url_patterns, list(
        paste0(base_url, "/", league, "/events/", event_id),
        paste0(base_url, "/", league, "/competitions/", event_id),
        paste0(base_url, "/events/", event_id)
      ))
    } else {
      url_patterns <- append(url_patterns, list(
        paste0(base_url, "/events/", event_id),
        paste0(base_url, "/competitions/", event_id)
      ))
    }
  }

  # Try each URL pattern until one works
  successful_data <- NULL
  successful_url <- NULL

  for (url in url_patterns) {
    tryCatch({
      resp <- httr::GET(
        url,
        httr::timeout(timeout),
        httr::user_agent("fighteR R package - ESPN MMA Fight Center"),
        httr::add_headers(
          "Accept" = "application/json",
          "Accept-Language" = "en-US,en;q=0.9"
        )
      )

      if (httr::status_code(resp) == 200) {
        content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
        data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

        successful_data <- data
        successful_url <- url
        break
      }
    }, error = function(e) {
      # Continue to next URL
    })
  }

  # If no URL worked, try to get fight details from the scoreboard
  if (is.null(successful_data)) {
    message("Direct event API failed, trying to extract from scoreboard...")
    successful_data <- get_event_from_scoreboard(event_id, league, timeout)
    if (is.null(successful_data)) {
      stop("Could not fetch event data from any available endpoint. Event ID may be invalid or event may not have detailed fight data available.")
    }
  }

  # Fetch and parse
  tryCatch({
    # Handle raw data assignment
    if (isTRUE(raw)) {
      assign("mma_fights_raw", successful_data, envir = .GlobalEnv)
      message("Raw MMA fight center data assigned to: mma_fights_raw")
      return(invisible(successful_data))
    }

    # Create clean fight card dataset
    fights_df <- create_clean_fight_card_dataset(successful_data, event_id, include_prelims)

    # Assign to global environment
    assign("mma_fights", fights_df, envir = .GlobalEnv)

    message(sprintf("MMA fight center data assigned to: mma_fights (%d fights)", nrow(fights_df)))

    return(invisible(fights_df))

  }, error = function(e) {
    stop(sprintf("Failed to process MMA fight center data: %s", e$message))
  })
}

#' Get event data from scoreboard as fallback
#'
#' @param event_id Event ID to find
#' @param league League to search in
#' @param timeout Request timeout
#' @return Event data or NULL if not found
get_event_from_scoreboard <- function(event_id, league, timeout) {

  base_url <- "https://site.api.espn.com/apis/site/v2/sports/mma"

  if (!is.null(league)) {
    url <- paste0(base_url, "/", league, "/scoreboard")
  } else {
    url <- paste0(base_url, "/scoreboard")
  }

  tryCatch({
    resp <- httr::GET(
      url,
      httr::timeout(timeout),
      httr::user_agent("fighteR R package - ESPN MMA"),
      httr::add_headers("Accept" = "application/json")
    )

    if (httr::status_code(resp) != 200) {
      return(NULL)
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Look for the specific event in the events list
    if ("events" %in% names(data) && length(data[["events"]]) > 0) {
      for (event in data[["events"]]) {
        if ("id" %in% names(event) && as.character(event[["id"]]) == event_id) {
          return(event)
        }
      }
    }

    return(NULL)

  }, error = function(e) {
    return(NULL)
  })
}

#' Get current featured event ID from ESPN MMA scoreboard
#'
#' @param league League to search for featured event
#' @param timeout Request timeout
#' @return Event ID string or NULL if not found
get_current_featured_event <- function(league, timeout) {

  base_url <- "https://site.api.espn.com/apis/site/v2/sports/mma"

  if (!is.null(league)) {
    url <- paste0(base_url, "/", league, "/scoreboard")
  } else {
    url <- paste0(base_url, "/scoreboard")
  }

  tryCatch({
    resp <- httr::GET(
      url,
      httr::timeout(timeout),
      httr::user_agent("fighteR R package - ESPN MMA"),
      httr::add_headers("Accept" = "application/json")
    )

    if (httr::status_code(resp) != 200) {
      return(NULL)
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Get the first/featured event
    if ("events" %in% names(data) && length(data[["events"]]) > 0) {
      first_event <- data[["events"]][[1]]
      if ("id" %in% names(first_event)) {
        return(as.character(first_event[["id"]]))
      }
    }

    return(NULL)

  }, error = function(e) {
    return(NULL)
  })
}

#' Create clean fight card dataset from ESPN event API response
#'
#' @param data Raw JSON response from ESPN event API
#' @param event_id Event ID used in request
#' @param include_prelims Whether to include preliminary fights
#' @return Clean data frame with one row per fight
create_clean_fight_card_dataset <- function(data, event_id, include_prelims) {
  # Initialize result data frame
  result_df <- data.frame(
    event_id = character(0),
    event_name = character(0),
    event_date = character(0),
    fight_id = character(0),
    fight_order = integer(0),
    card_segment = character(0),
    weight_class = character(0),
    fight_title = character(0),
    # Fighter 1 info
    fighter1_id = character(0),
    fighter1_name = character(0),
    fighter1_record = character(0),
    fighter1_wins = character(0),
    fighter1_losses = character(0),
    fighter1_draws = character(0),
    # Fighter 2 info
    fighter2_id = character(0),
    fighter2_name = character(0),
    fighter2_record = character(0),
    fighter2_wins = character(0),
    fighter2_losses = character(0),
    fighter2_draws = character(0),
    # Fight result (if completed)
    fight_status = character(0),
    winner_id = character(0),
    winner_name = character(0),
    method = character(0),
    round_ended = character(0),
    time_ended = character(0),
    # Venue info
    venue_name = character(0),
    venue_city = character(0),
    venue_state = character(0),
    venue_country = character(0),
    stringsAsFactors = FALSE
  )

  # Extract basic event info
  event_name <- if ("header" %in% names(data) && "league" %in% names(data[["header"]]) &&
                    "name" %in% names(data[["header"]][["league"]])) {
    data[["header"]][["league"]][["name"]]
  } else if ("name" %in% names(data)) {
    data[["name"]]
  } else {
    NA_character_
  }

  event_date <- if ("header" %in% names(data) && "competitions" %in% names(data[["header"]]) &&
                    length(data[["header"]][["competitions"]]) > 0 &&
                    "date" %in% names(data[["header"]][["competitions"]][[1]])) {
    data[["header"]][["competitions"]][[1]][["date"]]
  } else {
    NA_character_
  }

  # Initialize venue info
  venue_name <- venue_city <- venue_state <- venue_country <- NA_character_

  # Extract venue information
  if ("header" %in% names(data) && "competitions" %in% names(data[["header"]]) &&
      length(data[["header"]][["competitions"]]) > 0) {
    comp <- data[["header"]][["competitions"]][[1]]
    if ("venue" %in% names(comp)) {
      venue_info <- comp[["venue"]]
      venue_name <- if ("fullName" %in% names(venue_info)) venue_info[["fullName"]] else NA_character_

      if ("address" %in% names(venue_info)) {
        venue_address <- venue_info[["address"]]
        venue_city <- if ("city" %in% names(venue_address)) venue_address[["city"]] else NA_character_
        venue_state <- if ("state" %in% names(venue_address)) venue_address[["state"]] else NA_character_
        venue_country <- if ("country" %in% names(venue_address)) venue_address[["country"]] else NA_character_
      }
    }
  }

  # Process competitions (individual fights)
  if ("competitions" %in% names(data) && length(data[["competitions"]]) > 0) {

    for (i in seq_along(data[["competitions"]])) {
      competition <- data[["competitions"]][[i]]

      # Basic fight info
      fight_id <- if ("id" %in% names(competition)) as.character(competition[["id"]]) else NA_character_

      # Determine fight order and card segment
      fight_order <- i
      card_segment <- if (i == 1) "Main Event" else if (i <= 6) "Main Card" else "Prelims"

      # Skip prelims if not requested
      if (!include_prelims && card_segment == "Prelims") {
        next
      }

      # Weight class and title
      weight_class <- if ("details" %in% names(competition) && "class" %in% names(competition[["details"]])) {
        competition[["details"]][["class"]]
      } else {
        NA_character_
      }

      fight_title <- if ("headline" %in% names(competition)) competition[["headline"]] else NA_character_

      # Fight status and results
      fight_status <- if ("status" %in% names(competition) && "type" %in% names(competition[["status"]]) &&
                          "name" %in% names(competition[["status"]][["type"]])) {
        competition[["status"]][["type"]][["name"]]
      } else {
        "Scheduled"
      }

      # Initialize fight result info
      winner_id <- winner_name <- method <- round_ended <- time_ended <- NA_character_

      # Extract result info if fight is completed
      if (fight_status %in% c("STATUS_FINAL", "Final")) {
        if ("details" %in% names(competition)) {
          details <- competition[["details"]]
          method <- if ("winType" %in% names(details)) details[["winType"]] else NA_character_
          round_ended <- if ("round" %in% names(details)) as.character(details[["round"]]) else NA_character_
          time_ended <- if ("time" %in% names(details)) details[["time"]] else NA_character_
        }
      }

      # Initialize fighter info
      fighter1_id <- fighter1_name <- fighter1_record <- fighter1_wins <- fighter1_losses <- fighter1_draws <- NA_character_
      fighter2_id <- fighter2_name <- fighter2_record <- fighter2_wins <- fighter2_losses <- fighter2_draws <- NA_character_

      # Process competitors (fighters)
      if ("competitors" %in% names(competition) && length(competition[["competitors"]]) >= 2) {

        for (j in 1:min(2, length(competition[["competitors"]]))) {
          competitor <- competition[["competitors"]][[j]]

          # Fighter info
          fighter_info <- if ("athlete" %in% names(competitor)) competitor[["athlete"]] else list()
          fighter_id <- if ("id" %in% names(fighter_info)) as.character(fighter_info[["id"]]) else NA_character_
          fighter_name <- if ("displayName" %in% names(fighter_info)) fighter_info[["displayName"]] else NA_character_

          # Fighter record
          fighter_record <- NA_character_
          fighter_wins <- fighter_losses <- fighter_draws <- NA_character_

          if ("record" %in% names(fighter_info) && length(fighter_info[["record"]]) > 0) {
            record_info <- fighter_info[["record"]][[1]]
            fighter_record <- if ("displayValue" %in% names(record_info)) record_info[["displayValue"]] else NA_character_

            # Parse individual record components if available
            if ("wins" %in% names(record_info)) fighter_wins <- as.character(record_info[["wins"]])
            if ("losses" %in% names(record_info)) fighter_losses <- as.character(record_info[["losses"]])
            if ("ties" %in% names(record_info)) fighter_draws <- as.character(record_info[["ties"]])
          }

          # Check if this fighter won
          is_winner <- FALSE
          if ("winner" %in% names(competitor)) {
            is_winner <- isTRUE(competitor[["winner"]])
          }

          if (is_winner) {
            winner_id <- fighter_id
            winner_name <- fighter_name
          }

          # Assign to fighter 1 or 2
          if (j == 1) {
            fighter1_id <- fighter_id
            fighter1_name <- fighter_name
            fighter1_record <- fighter_record
            fighter1_wins <- fighter_wins
            fighter1_losses <- fighter_losses
            fighter1_draws <- fighter_draws
          } else {
            fighter2_id <- fighter_id
            fighter2_name <- fighter_name
            fighter2_record <- fighter_record
            fighter2_wins <- fighter_wins
            fighter2_losses <- fighter_losses
            fighter2_draws <- fighter_draws
          }
        }
      }

      # Create fight row
      fight_row <- data.frame(
        event_id = event_id,
        event_name = event_name,
        event_date = event_date,
        fight_id = fight_id,
        fight_order = fight_order,
        card_segment = card_segment,
        weight_class = weight_class,
        fight_title = fight_title,
        fighter1_id = fighter1_id,
        fighter1_name = fighter1_name,
        fighter1_record = fighter1_record,
        fighter1_wins = fighter1_wins,
        fighter1_losses = fighter1_losses,
        fighter1_draws = fighter1_draws,
        fighter2_id = fighter2_id,
        fighter2_name = fighter2_name,
        fighter2_record = fighter2_record,
        fighter2_wins = fighter2_wins,
        fighter2_losses = fighter2_losses,
        fighter2_draws = fighter2_draws,
        fight_status = fight_status,
        winner_id = winner_id,
        winner_name = winner_name,
        method = method,
        round_ended = round_ended,
        time_ended = time_ended,
        venue_name = venue_name,
        venue_city = venue_city,
        venue_state = venue_state,
        venue_country = venue_country,
        stringsAsFactors = FALSE
      )

      # Add to result
      result_df <- rbind(result_df, fight_row)
    }
  }

  # Clean up row names
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
  }

  return(result_df)
}
