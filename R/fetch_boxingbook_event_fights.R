#' Fetch fights for a given BoxingBook event
#'
#' Retrieves fight data for a specific event from the BoxingBook API.
#' This function handles pagination and provides robust error handling
#' for API interactions.
#'
#' @param event_id Numeric. The unique identifier for the boxing event.
#' @param page Integer. Page number for pagination (default: 0).
#' @param page_size Integer. Number of records per page, max 100 (default: 50).
#' @param sort Character. Sorting criteria in format "field1,field2,ORDER"
#'   where ORDER is ASC or DESC (default: "date,id,ASC").
#' @param timeout Numeric. Request timeout in seconds (default: 30).
#'
#' @return A data.frame containing fight information with columns for fight
#'   details, fighter information, and event metadata. Returns an empty
#'   data.frame if no fights are found.
#'
#' @details
#' The function connects to the BoxingBook API to retrieve fight data.
#' It includes comprehensive error handling for network issues, API errors,
#' and malformed responses. The function respects API rate limits and
#' includes appropriate timeouts.
#'
#' @examples
#' \dontrun{
#' # Fetch fights for event ID 123
#' fights <- fetch_boxingbook_event_fights(123)
#'
#' # Fetch with custom pagination
#' fights_page2 <- fetch_boxingbook_event_fights(123, page = 1, page_size = 25)
#'
#' # Fetch with custom sorting
#' fights_sorted <- fetch_boxingbook_event_fights(123, sort = "date,DESC")
#' }
#'
#' @importFrom httr GET status_code content timeout
#' @importFrom jsonlite fromJSON
#' @export
fetch_boxingbook_event_fights <- function(event_id,
                                          page = 0,
                                          page_size = 50,
                                          sort = "date,id,ASC",
                                          timeout = 30) {

  # Input validation
  if (!is.numeric(event_id) || length(event_id) != 1 || event_id <= 0) {
    stop("event_id must be a positive numeric value", call. = FALSE)
  }

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

  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    stop("timeout must be a positive numeric value", call. = FALSE)
  }

  # Ensure required packages are available
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required but not installed", call. = FALSE)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required but not installed", call. = FALSE)
  }

  # Base URL for BoxingBook API
  base_url <- "https://boxing-book-api-8ff329a1bf13.herokuapp.com/api/"

  # Construct API endpoint
  url <- paste0(base_url, "events/", event_id, "/fights")

  # Prepare query parameters
  query_params <- list(
    page = page,
    pageSize = page_size,
    sort = sort
  )

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

    if (status == 404) {
      warning("Event ID ", event_id, " not found", call. = FALSE)
      return(data.frame())
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

    # Extract fights data
    fights <- parsed_data$content

    if (is.null(fights) || length(fights) == 0) {
      message("No fights found for event ID ", event_id)
      return(data.frame())
    }

    # Convert to data.frame and ensure consistent structure
    fights_df <- tryCatch({
      as.data.frame(fights, stringsAsFactors = FALSE)
    }, error = function(e) {
      stop("Failed to convert fights data to data.frame: ", e$message,
           call. = FALSE)
    })

    # Add metadata
    attr(fights_df, "event_id") <- event_id
    attr(fights_df, "page") <- page
    attr(fights_df, "page_size") <- page_size
    attr(fights_df, "retrieved_at") <- Sys.time()

    return(fights_df)

  }, error = function(e) {
    if (inherits(e, "timeout")) {
      stop("Request timed out after ", timeout, " seconds. ",
           "Try increasing the timeout parameter", call. = FALSE)
    } else if (grepl("Could not resolve host", e$message)) {
      stop("Network connection failed. Please check your internet connection",
           call. = FALSE)
    } else {
      # Re-throw the error if it's already a custom error
      stop(e$message, call. = FALSE)
    }
  })
}
