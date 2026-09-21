# fighteR 🥊

<!-- badges: start -->
[![Docs](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://danieltomaro13.github.io/fighteR/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)
[![GitHub release](https://img.shields.io/github/v/release/DanielTomaro13/fighteR)](https://github.com/DanielTomaro13/fighteR/releases)
<!-- badges: end -->

An R package for combat sports data. Boxing records and fight cards from BoxingBook, and UFC and MMA schedules and fight cards from ESPN, all returned as clean data frames.

📖 **Full function reference: [danieltomaro13.github.io/fighteR](https://danieltomaro13.github.io/fighteR/)**

## Installation

```r
# install.packages("devtools")
devtools::install_github("DanielTomaro13/fighteR")
```

## Quick start

All output below is real, captured from the package.

### UFC schedule for a date range

```r
library(fighteR)
library(dplyr)

fetch_espn_mma_schedule(league = "ufc", dates = "20250101-20250331") |>
  mutate(event_date = substr(event_date, 1, 10)) |>
  select(short_name, event_date, venue_name, venue_city, venue_country, is_completed)
#> # A tibble: 11 x 6
#>   short_name      event_date venue_name       venue_city venue_country is_completed
#>   <chr>           <chr>      <chr>            <chr>      <chr>         <lgl>
#> 1 UFC Fight Night 2025-01-11 Meta APEX        Las Vegas  USA           TRUE
#> 2 UFC 311         2025-01-18 Intuit Dome      Inglewood  USA           TRUE
#> 3 UFC Fight Night 2025-02-01 anb Arena        Riyadh     Saudi Arabia  TRUE
#> 4 UFC 312         2025-02-08 Qudos Bank Arena Sydney     Australia     TRUE
#> 5 UFC Fight Night 2025-02-15 Meta APEX        Las Vegas  USA           TRUE
#> # i 6 more rows
```

The full result has 18 columns, including the promotion, event type, both headliners and the broadcast network. Pass an `event_id` to `fetch_espn_mma_fightcenter()` to get the full fight card for that event, prelims included.

### Boxing events and fight cards

```r
events <- fetch_boxingbook_events(page_size = 50)
names(events)
#> [1] "events"               "gender_distribution"  "result_distribution"
#> [4] "country_distribution" "weight_distribution"
```

`events$events` is the event list, and the four distribution tables summarise the fights on those cards by gender, result, country and weight class. Use `date_from`, `date_to`, `promoter` and `venue` to filter, then pass an event ID to `fetch_boxingbook_event_fights()` for the bouts.

## Functions

| Function | What it returns |
|---|---|
| `fetch_espn_mma_schedule()` | MMA events for a league and date range, with venue, headliners and status |
| `fetch_espn_mma_fightcenter()` | The full fight card for one event, including prelims |
| `fetch_boxingbook_events()` | Boxing events, filterable by date, promoter and venue |
| `fetch_boxingbook_event_fights()` | Every bout on one boxing event |
| `fetch_boxingbook_boxers()` | Searchable boxer directory with records, stance, weight and location |
| `fetch_boxingbook_boxer()` | The full profile for one boxer |

The ESPN functions take `raw = TRUE` to return the unprocessed API response.

## Disclaimer

This package uses unofficial, undocumented endpoints that may change or break without notice. Please keep request rates modest and use the data for personal and educational work.

## License

MIT © [Daniel Tomaro](https://github.com/DanielTomaro13). Not affiliated with ESPN, BoxingBook or the UFC.
