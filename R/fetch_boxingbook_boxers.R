#' Fetch boxers from BoxingBook API
#'
#' Retrieves a list of boxers from the BoxingBook API with support for
#' pagination, sorting, and comprehensive filtering options. This function
#' provides access to detailed boxer profiles including personal information,
#' fight records, and career statistics.
#'
#' @param page Integer. Page number for pagination (default: 0).
#' @param page_size Integer. Number of records per page, max 100 (default: 50).
#' @param sort Character. Sorting criteria in format "field1,field2,ORDER"
#'   where ORDER is ASC or DESC (default: NULL for API default).
#' @param first_name Character. Filter by boxer's first name (optional).
#' @param last_name Character. Filter by boxer's last name (optional).
#' @param nick_name Character. Filter by boxer's nickname (optional).
#' @param quick_search Character. General search across name fields (optional).
#' @param country Character. Filter by boxer's current country (optional).
#' @param birth_country Character. Filter by boxer's birth country (optional).
#' @param state Character. Filter by boxer's state/province (optional).
#' @param gender Character. Filter by gender: "male" or "female" (optional).
#' @param age Integer. Filter by specific age (optional).
#' @param age_query_type Character. Age comparison type: "EQUAL", "GREATER_THAN",
#'   "LESS_THAN", "GREATER_THAN_OR_EQUAL", "LESS_THAN_OR_EQUAL" (optional).
#' @param fights Integer. Filter by number of fights (optional).
#' @param fights_query_type Character. Fights comparison type: same options as age_query_type (optional).
#' @param weight Numeric. Filter by weight in kg (optional).
#' @param weight_query_type Character. Weight comparison type: same options as age_query_type (optional).
#' @param include_exhibitions Logical. Whether to include exhibition matches in counts (optional).
#' @param boxer_id Integer. Filter by specific boxer ID - will return single boxer (optional).
#' @param timeout Numeric. Request timeout in seconds (default: 30).
#'
#' @return A data.frame containing boxer information with columns for personal
#'   details, fight records, physical attributes, and career statistics.
#'   Returns an empty data.frame if no boxers are found.
#'
#' @details
#' The function connects to the BoxingBook API to retrieve boxer data with
#' comprehensive filtering capabilities. It includes error handling for network
#' issues, API errors, and malformed responses.
#'
#' The API contains over 25,000 boxer records with detailed information including:
#' \itemize{
#'   \item Personal information (name, age, nationality, stance)
#'   \item Fight statistics (wins, losses, draws, exhibitions)
#'   \item Physical attributes (weight, calculated weight)
#'   \item Career details (gym, verification status, activity)
#' }
#'
#' @examples
#' \dontrun{
#' # Get first 50 boxers (default)
#' boxers <- fetch_boxingbook_boxers()
#'
#' # Search by name
#' ali_boxers <- fetch_boxingbook_boxers(last_name = "Ali")
#'
#' # Filter by country and gender
#' aus_female <- fetch_boxingbook_boxers(
#'   country = "Australia",
#'   gender = "female"
#' )
#'
#' # Active boxers with more than 10 fights
#' experienced <- fetch_boxingbook_boxers(
#'   fights = 10,
#'   fights_query_type = "GREATER_THAN"
#' )
#'
#' # Quick search across all name fields
#' search_results <- fetch_boxingbook_boxers(quick_search = "Mike")
#'
#' # Get specific boxer by ID (alternative to fetch_boxingbook_boxer)
#' specific_boxer <- fetch_boxingbook_boxers(boxer_id = 30794)
#'
#' # Heavyweights (over 90kg)
#' heavyweights <- fetch_boxingbook_boxers(
#'   weight = 90,
#'   weight_query_type = "GREATER_THAN_OR_EQUAL"
#' )
#' }
#'
#' @importFrom httr GET status_code content timeout user_agent
#' @importFrom jsonlite fromJSON
#' @export
fetch_boxingbook_boxers <- function(page = 0,
                                    page_size = 50,
                                    sort = NULL,
                                    first_name = NULL,
                                    last_name = NULL,
                                    nick_name = NULL,
                                    quick_search = NULL,
                                    country = NULL,
                                    birth_country = NULL,
                                    state = NULL,
                                    gender = NULL,
                                    age = NULL,
                                    age_query_type = NULL,
                                    fights = NULL,
                                    fights_query_type = NULL,
                                    weight = NULL,
                                    weight_query_type = NULL,
                                    include_exhibitions = NULL,
                                    boxer_id = NULL,
                                    timeout = 30) {

  # Input validation
  if (!is.numeric(page) || length(page) != 1 || page < 0) {
    stop("page must be a non-negative integer", call. = FALSE)
  }

  if (!is.numeric(page_size) || length(page_size) != 1 ||
      page_size <= 0 || page_size > 100) {
    stop("page_size must be a positive integer <= 100", call. = FALSE)
  }

  if (!is.null(sort) && (!is.character(sort) || length(sort) != 1)) {
    stop("sort must be a character string", call. = FALSE)
  }

  # Validate character parameters
  char_params <- list(
    first_name = first_name, last_name = last_name, nick_name = nick_name,
    quick_search = quick_search, country = country, birth_country = birth_country,
    state = state
  )

  for (param_name in names(char_params)) {
    param_value <- char_params[[param_name]]
    if (!is.null(param_value) && (!is.character(param_value) || length(param_value) != 1)) {
      stop(paste(param_name, "must be a character string"), call. = FALSE)
    }
  }

  # Validate gender
  if (!is.null(gender)) {
    if (!is.character(gender) || length(gender) != 1 ||
        !gender %in% c("male", "female")) {
      stop("gender must be 'male' or 'female'", call. = FALSE)
    }
  }

  # Validate numeric parameters
  if (!is.null(age) && (!is.numeric(age) || length(age) != 1 || age < 0)) {
    stop("age must be a non-negative numeric value", call. = FALSE)
  }

  if (!is.null(fights) && (!is.numeric(fights) || length(fights) != 1 || fights < 0)) {
    stop("fights must be a non-negative numeric value", call. = FALSE)
  }

  if (!is.null(weight) && (!is.numeric(weight) || length(weight) != 1 || weight <= 0)) {
    stop("weight must be a positive numeric value", call. = FALSE)
  }

  # Validate query types
  valid_query_types <- c("EQUAL", "GREATER_THAN", "LESS_THAN",
                         "GREATER_THAN_OR_EQUAL", "LESS_THAN_OR_EQUAL")

  query_types <- list(
    age_query_type = age_query_type,
    fights_query_type = fights_query_type,
    weight_query_type = weight_query_type
  )

  for (param_name in names(query_types)) {
    param_value <- query_types[[param_name]]
    if (!is.null(param_value)) {
      if (!is.character(param_value) || length(param_value) != 1 ||
          !param_value %in% valid_query_types) {
        stop(paste(param_name, "must be one of:", paste(valid_query_types, collapse = ", ")),
             call. = FALSE)
      }
    }
  }

  # Validate include_exhibitions
  if (!is.null(include_exhibitions) &&
      (!is.logical(include_exhibitions) || length(include_exhibitions) != 1)) {
    stop("include_exhibitions must be TRUE or FALSE", call. = FALSE)
  }

  # Validate boxer_id
  if (!is.null(boxer_id) && (!is.numeric(boxer_id) || length(boxer_id) != 1 || boxer_id <= 0)) {
    stop("boxer_id must be a positive numeric value", call. = FALSE)
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

  # If searching by boxer_id, use the individual boxer endpoint
  if (!is.null(boxer_id)) {
    # Use the individual boxer endpoint for direct ID lookup
    return(fetch_boxingbook_boxer(boxer_id, timeout))
  }

  # Construct API endpoint for list
  url <- paste0(base_url, "boxers")

  # Prepare query parameters
  query_params <- list(
    page = page,
    pageSize = page_size
  )

  # Add optional parameters
  if (!is.null(sort)) query_params$sort <- sort
  if (!is.null(first_name)) query_params$firstName <- first_name
  if (!is.null(last_name)) query_params$lastName <- last_name
  if (!is.null(nick_name)) query_params$nickName <- nick_name
  if (!is.null(quick_search)) query_params$quickSearch <- quick_search
  if (!is.null(country)) query_params$country <- country
  if (!is.null(birth_country)) query_params$birthCountry <- birth_country
  if (!is.null(state)) query_params$state <- state
  if (!is.null(gender)) query_params$gender <- gender
  if (!is.null(age)) query_params$age <- age
  if (!is.null(age_query_type)) query_params$ageQueryType <- age_query_type
  if (!is.null(fights)) query_params$fights <- fights
  if (!is.null(fights_query_type)) query_params$fightsQueryType <- fights_query_type
  if (!is.null(weight)) query_params$weight <- weight
  if (!is.null(weight_query_type)) query_params$weightQueryType <- weight_query_type
  if (!is.null(include_exhibitions)) query_params$includeExhibitions <- tolower(as.character(include_exhibitions))

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

    # Extract boxers data
    boxers <- parsed_data$content

    if (is.null(boxers) || length(boxers) == 0) {
      message("No boxers found matching the specified criteria")
      return(data.frame())
    }

    # Convert to data.frame and ensure consistent structure with robust handling
    boxers_df <- tryCatch({
      safe_convert_to_dataframe(boxers)
    }, error = function(e) {
      stop("Failed to convert boxers data to data.frame: ", e$message,
           call. = FALSE)
    })

    # Clean and standardize the data
    boxers_df <- clean_boxers_data(boxers_df)

    # Add metadata
    attr(boxers_df, "page") <- page
    attr(boxers_df, "page_size") <- page_size
    attr(boxers_df, "total_elements") <- if (is.null(parsed_data$totalElements)) NA else parsed_data$totalElements
    attr(boxers_df, "total_pages") <- if (is.null(parsed_data$totalPages)) NA else parsed_data$totalPages
    attr(boxers_df, "filters") <- list(
      first_name = first_name,
      last_name = last_name,
      nick_name = nick_name,
      quick_search = quick_search,
      country = country,
      birth_country = birth_country,
      state = state,
      gender = gender,
      age = age,
      fights = fights,
      weight = weight,
      boxer_id = boxer_id
    )
    attr(boxers_df, "retrieved_at") <- Sys.time()

    return(boxers_df)

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

#' Fetch individual boxer details from BoxingBook API
#'
#' Retrieves detailed information for a specific boxer using their unique ID.
#' This function provides complete boxer profile data including personal
#' information, fight records, and career statistics.
#'
#' @param boxer_id Integer. The unique ID of the boxer to retrieve.
#' @param timeout Numeric. Request timeout in seconds (default: 30).
#'
#' @return A data.frame with a single row containing comprehensive boxer
#'   information, or an empty data.frame if the boxer is not found.
#'
#' @details
#' This function retrieves detailed information for a single boxer, including
#' all available fields such as personal details, fight statistics, physical
#' attributes, and career information.
#'
#' @examples
#' \dontrun{
#' # Get specific boxer by ID
#' boxer <- fetch_boxingbook_boxer(30794)
#'
#' # Extract boxer details
#' if (nrow(boxer) > 0) {
#'   cat("Name:", boxer$firstName, boxer$lastName, "\n")
#'   cat("Record:", boxer$wins, "-", boxer$losses, "-", boxer$draws, "\n")
#'   cat("Country:", boxer$country, "\n")
#' }
#' }
#'
#' @importFrom httr GET status_code content timeout user_agent
#' @importFrom jsonlite fromJSON
#' @export
fetch_boxingbook_boxer <- function(boxer_id, timeout = 30) {

  # Input validation
  if (!is.numeric(boxer_id) || length(boxer_id) != 1 || boxer_id <= 0) {
    stop("boxer_id must be a positive numeric value", call. = FALSE)
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
  url <- paste0(base_url, "boxers/", boxer_id)

  # Make API request with error handling
  tryCatch({
    response <- httr::GET(
      url,
      httr::timeout(timeout),
      httr::user_agent("fighteR R package")
    )

    # Check HTTP status
    status <- httr::status_code(response)

    if (status == 404) {
      warning("Boxer ID ", boxer_id, " not found", call. = FALSE)
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
    boxer_data <- tryCatch({
      jsonlite::fromJSON(content_raw, flatten = TRUE)
    }, error = function(e) {
      stop("Failed to parse API response as JSON: ", e$message, call. = FALSE)
    })

    if (is.null(boxer_data)) {
      warning("No data received for boxer ID ", boxer_id, call. = FALSE)
      return(data.frame())
    }

    # Convert to data.frame with robust handling
    boxer_df <- tryCatch({
      # Handle the data conversion more robustly
      safe_convert_to_dataframe(boxer_data)
    }, error = function(e) {
      stop("Failed to convert boxer data to data.frame: ", e$message,
           call. = FALSE)
    })

    # Clean and standardize the data
    boxer_df <- clean_boxers_data(boxer_df)

    # Add metadata
    attr(boxer_df, "boxer_id") <- boxer_id
    attr(boxer_df, "retrieved_at") <- Sys.time()

    return(boxer_df)

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

#' Safe conversion of API data to data.frame
#'
#' Internal function to safely convert API response data to data.frame,
#' handling edge cases like empty lists, NULL values, and mismatched lengths.
#'
#' @param data Raw data from API (list or data.frame)
#' @return Cleaned data.frame
#' @keywords internal
safe_convert_to_dataframe <- function(data) {

  # If already a data.frame, return as-is
  if (is.data.frame(data)) {
    return(data)
  }

  # If it's a list, process each element
  if (is.list(data)) {
    # Handle each element to ensure consistent types and lengths
    cleaned_data <- list()

    for (name in names(data)) {
      element <- data[[name]]

      # Handle NULL values
      if (is.null(element)) {
        cleaned_data[[name]] <- NA
        next
      }

      # Handle empty lists
      if (is.list(element) && length(element) == 0) {
        cleaned_data[[name]] <- NA
        next
      }

      # Handle lists with content - flatten if possible
      if (is.list(element) && length(element) > 0) {
        # If it's a simple list, try to collapse to string or take first element
        if (all(sapply(element, function(x) length(x) <= 1))) {
          if (all(sapply(element, is.character))) {
            cleaned_data[[name]] <- paste(unlist(element), collapse = ", ")
          } else {
            cleaned_data[[name]] <- unlist(element)[1]
          }
        } else {
          # Complex nested structure - convert to string representation
          cleaned_data[[name]] <- as.character(jsonlite::toJSON(element, auto_unbox = TRUE))
        }
        next
      }

      # Handle vectors - ensure length 1 for single-row data.frame
      if (is.vector(element) && length(element) > 1) {
        # If it's a character vector, collapse with commas
        if (is.character(element)) {
          cleaned_data[[name]] <- paste(element, collapse = ", ")
        } else {
          # For numeric vectors, take the first value
          cleaned_data[[name]] <- element[1]
        }
      } else {
        # Single values or length-1 vectors
        cleaned_data[[name]] <- element
      }
    }

    # Convert to data.frame
    return(as.data.frame(cleaned_data, stringsAsFactors = FALSE))
  }

  # For other types, try direct conversion
  return(as.data.frame(data, stringsAsFactors = FALSE))
}

#' Clean and standardize boxer data
#'
#' Internal function to clean and standardize boxer data from the BoxingBook API.
#'
#' @param boxers_df Raw data frame from API
#' @return Cleaned data frame
#' @keywords internal
clean_boxers_data <- function(boxers_df) {

  # Handle date of birth formatting
  if ("dateOfBirth" %in% names(boxers_df)) {
    boxers_df$dateOfBirth <- as.Date(boxers_df$dateOfBirth)
  }

  # Handle created/updated timestamps
  timestamp_cols <- c("createdAt", "updatedAt")
  for (col in timestamp_cols) {
    if (col %in% names(boxers_df)) {
      boxers_df[[col]] <- as.POSIXct(boxers_df[[col]], format = "%Y-%m-%dT%H:%M:%OS")
    }
  }

  # Standardize logical columns
  logical_cols <- c("active", "verified", "needsUpdate", "hidden")
  for (col in logical_cols) {
    if (col %in% names(boxers_df)) {
      boxers_df[[col]] <- as.logical(boxers_df[[col]])
    }
  }

  # Clean numeric columns
  numeric_cols <- c("weight", "calculatedWeight", "wins", "losses", "draws",
                    "walkoverWins", "walkoverLosses", "exhibitions", "fights",
                    "rounds", "age", "points")
  for (col in numeric_cols) {
    if (col %in% names(boxers_df)) {
      boxers_df[[col]] <- as.numeric(boxers_df[[col]])
    }
  }

  # Clean character columns (trim whitespace, handle empty strings)
  char_cols <- c("firstName", "lastName", "nickName", "stance", "gym",
                 "gender", "state", "country", "birthCountry", "imageUrl",
                 "registration", "memberId")
  for (col in char_cols) {
    if (col %in% names(boxers_df)) {
      boxers_df[[col]] <- trimws(boxers_df[[col]])
      boxers_df[[col]][boxers_df[[col]] == ""] <- NA
    }
  }

  # Create derived columns
  if (all(c("wins", "losses", "draws") %in% names(boxers_df))) {
    boxers_df$total_fights_excluding_exhibitions <-
      boxers_df$wins + boxers_df$losses + boxers_df$draws

    boxers_df$win_percentage <- ifelse(
      boxers_df$total_fights_excluding_exhibitions > 0,
      round((boxers_df$wins / boxers_df$total_fights_excluding_exhibitions) * 100, 2),
      NA
    )
  }

  # Create full name column
  if (all(c("firstName", "lastName") %in% names(boxers_df))) {
    boxers_df$full_name <- paste(
      trimws(boxers_df$firstName),
      trimws(boxers_df$lastName)
    )
    boxers_df$full_name <- trimws(boxers_df$full_name)

    # Add nickname to display name if available
    if ("nickName" %in% names(boxers_df)) {
      boxers_df$display_name <- ifelse(
        !is.na(boxers_df$nickName) & boxers_df$nickName != "",
        paste0(boxers_df$full_name, ' "', boxers_df$nickName, '"'),
        boxers_df$full_name
      )
    }
  }

  # Reorder columns for better readability
  preferred_order <- c("id", "full_name", "display_name", "firstName", "lastName",
                       "nickName", "country", "state", "gender", "age", "weight",
                       "stance", "wins", "losses", "draws", "total_fights_excluding_exhibitions",
                       "win_percentage", "exhibitions", "fights", "rounds", "gym",
                       "active", "verified")

  existing_preferred <- intersect(preferred_order, names(boxers_df))
  other_cols <- setdiff(names(boxers_df), preferred_order)

  if (length(existing_preferred) > 0) {
    boxers_df <- boxers_df[, c(existing_preferred, other_cols), drop = FALSE]
  }

  return(boxers_df)
}
