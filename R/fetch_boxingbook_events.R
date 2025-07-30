#' Fetch boxing events from BoxingBook API with enhanced data extraction
#'
#' Retrieves a list of boxing events from the BoxingBook API and automatically
#' extracts and cleans the nested distribution data into separate data frames.
#' This function provides access to comprehensive event data including dates,
#' venues, promoters, and detailed statistics.
#'
#' @param page Integer. Page number for pagination (default: 0).
#' @param page_size Integer. Number of records per page, max 100 (default: 50).
#' @param sort Character. Sorting criteria in format "field1,field2,ORDER"
#'   where ORDER is ASC or DESC (default: "date,id,DESC").
#' @param date_from Character. Filter events from this date in YYYY-MM-DD format (optional).
#' @param date_to Character. Filter events to this date in YYYY-MM-DD format (optional).
#' @param promoter Character. Filter by promoter name (optional).
#' @param venue Character. Filter by venue name (optional).
#' @param timeout Numeric. Request timeout in seconds (default: 30).
#' @param extract_distributions Logical. Whether to extract distribution data
#'   into separate data frames (default: TRUE).
#' @param assign_global Logical. Whether to assign extracted data frames to
#'   global environment (default: FALSE).
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{events}: Main events data frame with core event information
#'     \item \code{gender_distribution}: Gender distribution data (if extracted)
#'     \item \code{result_distribution}: Fight result distribution data (if extracted)
#'     \item \code{country_distribution}: Country distribution data (if extracted)
#'     \item \code{weight_distribution}: Weight class distribution data (if extracted)
#'   }
#'
#' @details
#' The function connects to the BoxingBook API to retrieve event data and
#' automatically cleans the complex nested structure. When \code{extract_distributions = TRUE},
#' the function separates distribution statistics into clean, analyzable data frames.
#'
#' If \code{assign_global = TRUE}, the following objects are assigned to the global environment:
#' \itemize{
#'   \item \code{boxingbook_events}: Main events data
#'   \item \code{boxingbook_gender_dist}: Gender distribution data
#'   \item \code{boxingbook_result_dist}: Result distribution data
#'   \item \code{boxingbook_country_dist}: Country distribution data
#'   \item \code{boxingbook_weight_dist}: Weight distribution data
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with data extraction
#' data <- fetch_boxingbook_events()
#' events <- data$events
#' gender_stats <- data$gender_distribution
#'
#' # Assign to global environment
#' fetch_boxingbook_events(assign_global = TRUE)
#' # Now you can access: boxingbook_events, boxingbook_gender_dist, etc.
#'
#' # Get events with filters and extract distributions
#' aus_events <- fetch_boxingbook_events(
#'   date_from = "2024-01-01",
#'   extract_distributions = TRUE
#' )
#' }
#'
#' @importFrom httr GET status_code content timeout user_agent
#' @importFrom jsonlite fromJSON
#' @export
fetch_boxingbook_events <- function(page = 0,
                                    page_size = 50,
                                    sort = "date,id,DESC",
                                    date_from = NULL,
                                    date_to = NULL,
                                    promoter = NULL,
                                    venue = NULL,
                                    timeout = 30,
                                    extract_distributions = TRUE,
                                    assign_global = FALSE) {

  # Input validation (keeping existing validation code)
  if (!is.numeric(page) || length(page) != 1 || page < 0) {
    stop("page must be a non-negative integer", call. = FALSE)
  }

  if (!is.numeric(page_size) || length(page_size) != 1 ||
      page_size <= 0 || page_size > 100) {
    stop("page_size must be a positive integer <= 100", call. = FALSE)
  }

  if (!is.character(sort) || length(sort) != 1) {
    stop("sort must be a character string", call. = FALSE)
  }

  if (!is.null(date_from)) {
    if (!is.character(date_from) || length(date_from) != 1) {
      stop("date_from must be a character string in YYYY-MM-DD format", call. = FALSE)
    }
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date_from)) {
      stop("date_from must be in YYYY-MM-DD format", call. = FALSE)
    }
    tryCatch(as.Date(date_from), error = function(e) {
      stop("date_from is not a valid date", call. = FALSE)
    })
  }

  if (!is.null(date_to)) {
    if (!is.character(date_to) || length(date_to) != 1) {
      stop("date_to must be a character string in YYYY-MM-DD format", call. = FALSE)
    }
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date_to)) {
      stop("date_to must be in YYYY-MM-DD format", call. = FALSE)
    }
    tryCatch(as.Date(date_to), error = function(e) {
      stop("date_to is not a valid date", call. = FALSE)
    })
  }

  if (!is.null(date_from) && !is.null(date_to)) {
    if (as.Date(date_from) > as.Date(date_to)) {
      stop("date_from cannot be later than date_to", call. = FALSE)
    }
  }

  if (!is.null(promoter) && (!is.character(promoter) || length(promoter) != 1)) {
    stop("promoter must be a character string", call. = FALSE)
  }

  if (!is.null(venue) && (!is.character(venue) || length(venue) != 1)) {
    stop("venue must be a character string", call. = FALSE)
  }

  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    stop("timeout must be a positive numeric value", call. = FALSE)
  }

  if (!is.logical(extract_distributions) || length(extract_distributions) != 1) {
    stop("extract_distributions must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.logical(assign_global) || length(assign_global) != 1) {
    stop("assign_global must be TRUE or FALSE", call. = FALSE)
  }

  # Base URL for BoxingBook API
  base_url <- "https://boxing-book-api-8ff329a1bf13.herokuapp.com/api/"

  # Construct API endpoint
  url <- paste0(base_url, "events")

  # Prepare query parameters
  query_params <- list(
    page = page,
    pageSize = page_size,
    sort = sort
  )

  # Add optional filters
  if (!is.null(date_from)) {
    query_params$dateFrom <- date_from
  }

  if (!is.null(date_to)) {
    query_params$dateTo <- date_to
  }

  if (!is.null(promoter)) {
    query_params$promoter <- promoter
  }

  if (!is.null(venue)) {
    query_params$venue <- venue
  }

  # Make API request with error handling
  tryCatch({
    response <- httr::GET(
      url,
      query = query_params,
      httr::timeout(timeout),
      httr::user_agent("fighteR R package")
    )

    # Check HTTP status
    status <- httr::status_code(response)

    if (status == 400) {
      stop("Bad request. Please check your filter parameters", call. = FALSE)
    }

    if (status == 429) {
      stop("API rate limit exceeded. Please wait before making more requests",
           call. = FALSE)
    }

    if (status != 200) {
      stop("API request failed with status ", status,
           ". Please check your parameters and try again", call. = FALSE)
    }

    # Parse response content
    content_raw <- httr::content(response, as = "text", encoding = "UTF-8")

    if (nchar(content_raw) == 0) {
      warning("Empty response received from API", call. = FALSE)
      return(data.frame())
    }

    # Parse JSON
    parsed_data <- tryCatch({
      jsonlite::fromJSON(content_raw, flatten = TRUE)
    }, error = function(e) {
      stop("Failed to parse API response as JSON: ", e$message, call. = FALSE)
    })

    # Extract events data
    events <- parsed_data$content

    if (is.null(events) || length(events) == 0) {
      message("No events found matching the specified criteria")
      return(list(events = data.frame()))
    }

    # Convert to data.frame
    events_df <- tryCatch({
      as.data.frame(events, stringsAsFactors = FALSE)
    }, error = function(e) {
      stop("Failed to convert events data to data.frame: ", e$message,
           call. = FALSE)
    })

    # Initialize result list
    result <- list()

    if (extract_distributions) {
      # Extract core event information (non-distribution columns)
      core_cols <- c("id", "name", "country", "state", "date", "venue", "status",
                     "calculateStats", "fightDates")

      # Keep only core columns that exist
      available_core_cols <- intersect(core_cols, names(events_df))
      events_clean <- events_df[, available_core_cols, drop = FALSE]

      # Extract distribution data
      distributions <- extract_distribution_data(events_df)

      result$events <- events_clean
      result$gender_distribution <- distributions$gender
      result$result_distribution <- distributions$results
      result$country_distribution <- distributions$countries
      result$weight_distribution <- distributions$weights

      # Assign to global environment if requested
      if (assign_global) {
        assign("boxingbook_events", events_clean, envir = .GlobalEnv)
        assign("boxingbook_gender_dist", distributions$gender, envir = .GlobalEnv)
        assign("boxingbook_result_dist", distributions$results, envir = .GlobalEnv)
        assign("boxingbook_country_dist", distributions$countries, envir = .GlobalEnv)
        assign("boxingbook_weight_dist", distributions$weights, envir = .GlobalEnv)

        message("Data assigned to global environment:")
        message("  - boxingbook_events (", nrow(events_clean), " events)")
        message("  - boxingbook_gender_dist (", nrow(distributions$gender), " records)")
        message("  - boxingbook_result_dist (", nrow(distributions$results), " records)")
        message("  - boxingbook_country_dist (", nrow(distributions$countries), " records)")
        message("  - boxingbook_weight_dist (", nrow(distributions$weights), " records)")
      }

    } else {
      # Return raw data without extraction
      result$events <- events_df

      if (assign_global) {
        assign("boxingbook_events", events_df, envir = .GlobalEnv)
        message("Raw events data assigned to: boxingbook_events")
      }
    }

    # Add metadata to main events data
    attr(result$events, "page") <- page
    attr(result$events, "page_size") <- page_size
    attr(result$events, "total_elements") <- if (is.null(parsed_data$totalElements)) NA else parsed_data$totalElements
    attr(result$events, "total_pages") <- if (is.null(parsed_data$totalPages)) NA else parsed_data$totalPages
    attr(result$events, "filters") <- list(
      date_from = date_from,
      date_to = date_to,
      promoter = promoter,
      venue = venue
    )
    attr(result$events, "retrieved_at") <- Sys.time()

    return(result)

  }, error = function(e) {
    if (inherits(e, "timeout")) {
      stop("Request timed out after ", timeout, " seconds. ",
           "Try increasing the timeout parameter", call. = FALSE)
    } else if (grepl("Could not resolve host", e$message)) {
      stop("Network connection failed. Please check your internet connection",
           call. = FALSE)
    } else {
      stop(e$message, call. = FALSE)
    }
  })
}

#' Extract distribution data from BoxingBook events
#'
#' Internal function to clean and extract distribution statistics from
#' the complex nested structure returned by the BoxingBook API.
#'
#' @param events_df Data frame containing raw events data
#' @return List containing cleaned distribution data frames
#' @keywords internal
extract_distribution_data <- function(events_df) {

  # Extract gender distribution
  gender_cols <- grep("^genderDistribution\\.", names(events_df), value = TRUE)
  gender_dist <- data.frame()

  if (length(gender_cols) > 0) {
    gender_data <- events_df[, c("id", "name", gender_cols), drop = FALSE]

    # Reshape gender data
    gender_dist <- data.frame(
      event_id = rep(events_df$id, length(gender_cols)),
      event_name = rep(events_df$name, length(gender_cols)),
      gender = rep(gsub("genderDistribution\\.", "", gender_cols), each = nrow(events_df)),
      proportion = unlist(gender_data[, gender_cols]),
      stringsAsFactors = FALSE
    )

    # Remove NA values
    gender_dist <- gender_dist[!is.na(gender_dist$proportion), ]
    rownames(gender_dist) <- NULL
  }

  # Extract result distribution
  result_cols <- grep("^resultDistribution\\.", names(events_df), value = TRUE)
  result_dist <- data.frame()

  if (length(result_cols) > 0) {
    result_data <- events_df[, c("id", "name", result_cols), drop = FALSE]

    # Reshape result data
    result_dist <- data.frame(
      event_id = rep(events_df$id, length(result_cols)),
      event_name = rep(events_df$name, length(result_cols)),
      result_type = rep(gsub("resultDistribution\\.", "", result_cols), each = nrow(events_df)),
      proportion = unlist(result_data[, result_cols]),
      stringsAsFactors = FALSE
    )

    # Remove NA values
    result_dist <- result_dist[!is.na(result_dist$proportion), ]
    rownames(result_dist) <- NULL
  }

  # Extract country distribution
  country_cols <- grep("^countryDistribution\\.", names(events_df), value = TRUE)
  country_dist <- data.frame()

  if (length(country_cols) > 0) {
    country_data <- events_df[, c("id", "name", country_cols), drop = FALSE]

    # Reshape country data
    country_dist <- data.frame(
      event_id = rep(events_df$id, length(country_cols)),
      event_name = rep(events_df$name, length(country_cols)),
      country = rep(gsub("countryDistribution\\.", "", country_cols), each = nrow(events_df)),
      proportion = unlist(country_data[, country_cols]),
      stringsAsFactors = FALSE
    )

    # Remove NA values
    country_dist <- country_dist[!is.na(country_dist$proportion), ]
    rownames(country_dist) <- NULL
  }

  # Extract weight distribution
  weight_cols <- grep("^weightDistribution\\.", names(events_df), value = TRUE)
  weight_dist <- data.frame()

  if (length(weight_cols) > 0) {
    weight_data <- events_df[, c("id", "name", weight_cols), drop = FALSE]

    # Reshape weight data
    weight_dist <- data.frame(
      event_id = rep(events_df$id, length(weight_cols)),
      event_name = rep(events_df$name, length(weight_cols)),
      weight_class = rep(gsub("weightDistribution\\.", "", weight_cols), each = nrow(events_df)),
      proportion = unlist(weight_data[, weight_cols]),
      stringsAsFactors = FALSE
    )

    # Remove NA values and clean weight class names
    weight_dist <- weight_dist[!is.na(weight_dist$proportion), ]
    weight_dist$weight_class <- gsub("\\.", "-", weight_dist$weight_class)
    rownames(weight_dist) <- NULL
  }

  return(list(
    gender = gender_dist,
    results = result_dist,
    countries = country_dist,
    weights = weight_dist
  ))
}
