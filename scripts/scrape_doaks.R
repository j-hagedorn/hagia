scrape_doaks <- function(
    keys = NULL,
    start_text_id = "oehc_000001",
    pause = 1
) {
  
  # Required packages:
  # install.packages(c(
  #   "httr2", "rvest", "xml2",
  #   "stringr", "dplyr", "purrr", "tibble"
  # ))
  
  # ----------------------------------------------------------
  # Configuration
  # ----------------------------------------------------------
  
  base_url <- paste0(
    "https://www.doaks.org/research/byzantine/",
    "resources/hagiography/database"
  )
  
  list_url <- paste0(
    base_url,
    "/dohp.asp?cmd=SList"
  )
  
  metadata_url <- function(key) {
    paste0(
      base_url,
      "/dohp.asp?cmd=SShow&key=",
      key
    )
  }
  
  text_url <- function(key) {
    paste0(
      base_url,
      "/texts/",
      key,
      ".html"
    )
  }
  
  
  # ----------------------------------------------------------
  # Retrieve and parse an HTML page
  # ----------------------------------------------------------
  
  get_page <- function(url) {
    
    httr2::request(url) |>
      httr2::req_user_agent(
        paste(
          "Mozilla/5.0",
          "(academic research;",
          "Open English Hagiography Corpus)"
        )
      ) |>
      httr2::req_timeout(30) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform() |>
      httr2::resp_body_string() |>
      xml2::read_html()
  }
  
  
  # ----------------------------------------------------------
  # Discover all saint keys when none are supplied
  # ----------------------------------------------------------
  
  if (is.null(keys)) {
    
    message("Discovering saint records...")
    
    list_doc <- get_page(list_url)
    
    hrefs <- list_doc |>
      rvest::html_elements("a") |>
      rvest::html_attr("href")
    
    keys <- hrefs |>
      stringr::str_extract(
        "(?<=key=)[0-9]+"
      ) |>
      stats::na.omit() |>
      as.integer() |>
      unique() |>
      sort()
    
    if (length(keys) == 0) {
      stop("No saint record keys were found on the Saints page.")
    }
    
    message(
      "Found ",
      length(keys),
      " saint records."
    )
  }
  
  
  # ----------------------------------------------------------
  # Parse starting OEHC text ID
  #
  # Example:
  # oehc_000251
  #
  # becomes:
  # prefix = "oehc_"
  # number = 251
  # width  = 6
  # ----------------------------------------------------------
  
  id_parts <- stringr::str_match(
    start_text_id,
    "^(.*?)([0-9]+)$"
  )
  
  if (is.na(id_parts[1, 1])) {
    stop(
      "start_text_id must end in a number, ",
      "for example 'oehc_000001'."
    )
  }
  
  id_prefix <- id_parts[1, 2]
  id_start  <- as.integer(id_parts[1, 3])
  id_width  <- nchar(id_parts[1, 3])
  
  text_ids <- paste0(
    id_prefix,
    stringr::str_pad(
      id_start + seq_along(keys) - 1,
      width = id_width,
      pad = "0"
    )
  )
  
  
  # ----------------------------------------------------------
  # Extract a labeled <li> value
  #
  # Examples:
  # Vita: 850–916
  # Domain: RURAL
  # Region: AEGINA.CONSTANTINOPLE
  # Author: Anonymous
  # ----------------------------------------------------------
  
  get_list_field <- function(items, label) {
    
    match <- items[
      stringr::str_detect(
        items,
        stringr::regex(
          paste0("^", label, "\\s*:"),
          ignore_case = TRUE
        )
      )
    ]
    
    if (length(match) == 0) {
      return(NA_character_)
    }
    
    match[[1]] |>
      stringr::str_remove(
        stringr::regex(
          paste0("^", label, "\\s*:\\s*"),
          ignore_case = TRUE
        )
      ) |>
      stringr::str_squish()
  }
  
  
  # ----------------------------------------------------------
  # Extract BHG, BHL, or BHO independently
  #
  # Each classification is stored in its own field.
  #
  # Multiple identifiers are retained as a semicolon-separated
  # value, e.g. "123; 123a".
  # ----------------------------------------------------------
  
  get_classification <- function(record, scheme) {
    
    headings <- record |>
      rvest::html_elements("h3") |>
      rvest::html_text2() |>
      paste(collapse = " ") |>
      stringr::str_squish()
    
    # Capture only the material following this classification,
    # stopping at another BHG/BHL/BHO label, "See Text", or end.
    pattern <- paste0(
      "\\b",
      scheme,
      "(?:\\s+Number)?\\s*:?\\s*",
      "(.*?)",
      "(?=\\bB(?:HG|HL|HO)\\b|\\bSee\\s+Text\\b|$)"
    )
    
    value <- stringr::str_match(
      headings,
      stringr::regex(
        pattern,
        ignore_case = TRUE
      )
    )[, 2]
    
    if (is.na(value)) {
      return(NA_character_)
    }
    
    ids <- stringr::str_extract_all(
      value,
      "[0-9]+[A-Za-z]*(?:\\s*[-–]\\s*[0-9]+[A-Za-z]*)?"
    )[[1]]
    
    if (length(ids) == 0) {
      return(NA_character_)
    }
    
    paste(
      unique(stringr::str_squish(ids)),
      collapse = "; "
    )
  }
  
  
  # ----------------------------------------------------------
  # Extract source text
  # ----------------------------------------------------------
  
  extract_text <- function(doc) {
    
    paragraphs <- doc |>
      rvest::html_elements("p") |>
      rvest::html_text2() |>
      stringr::str_replace_all("\u00A0", " ") |>
      stringr::str_trim()
    
    paragraphs <- paragraphs[nzchar(paragraphs)]
    
    if (length(paragraphs) > 0) {
      return(
        paste(
          paragraphs,
          collapse = "\n\n"
        )
      )
    }
    
    doc |>
      rvest::html_element("body") |>
      rvest::html_text2() |>
      stringr::str_replace_all("\u00A0", " ") |>
      stringr::str_trim()
  }
  
  
  # ----------------------------------------------------------
  # Scrape one saint record
  # ----------------------------------------------------------
  
  scrape_one <- function(key, text_id) {
    
    source_url <- metadata_url(key)
    text_source_url <- text_url(key)
    
    tryCatch({
      
      message(
        "Scraping key ",
        key,
        " -> ",
        text_id
      )
      
      # ------------------------------------------------------
      # Metadata page
      # ------------------------------------------------------
      
      doc <- get_page(source_url)
      
      # Isolate the saint record from site navigation.
      record <- doc |>
        rvest::html_element(
          xpath = paste0(
            "//div[",
            "h2 and ",
            "(h3[contains(., 'BHG')] ",
            "or h3[contains(., 'BHL')] ",
            "or h3[contains(., 'BHO')])",
            "]"
          )
        )
      
      # ------------------------------------------------------
      # Name and saint dates
      # ------------------------------------------------------
      
      title_full <- record |>
        rvest::html_element("h2") |>
        rvest::html_text2() |>
        stringr::str_squish()
      
      date_node <- record |>
        rvest::html_element("h2 em")
      
      saint_dates <- if (
        inherits(date_node, "xml_missing")
      ) {
        NA_character_
      } else {
        date_node |>
          rvest::html_text2() |>
          stringr::str_squish()
      }
      
      title <- title_full
      
      if (!is.na(saint_dates)) {
        title <- title |>
          stringr::str_remove(
            paste0(
              ",\\s*",
              stringr::fixed(saint_dates)
            )
          ) |>
          stringr::str_squish()
      }
      
      
      # ------------------------------------------------------
      # Hagiographic classifications
      # ------------------------------------------------------
      
      bhg <- get_classification(
        record,
        "BHG"
      )
      
      bhl <- get_classification(
        record,
        "BHL"
      )
      
      bho <- get_classification(
        record,
        "BHO"
      )
      
      
      # ------------------------------------------------------
      # Vita, domain, region, author
      # ------------------------------------------------------
      
      items <- record |>
        rvest::html_elements("ul > li") |>
        rvest::html_text2() |>
        stringr::str_squish()
      
      original_date <- get_list_field(
        items,
        "Vita"
      )
      
      domain <- get_list_field(
        items,
        "Domain"
      )
      
      region <- get_list_field(
        items,
        "Region"
      )
      
      author <- get_list_field(
        items,
        "Author"
      )
      
      
      # ------------------------------------------------------
      # Edition
      # ------------------------------------------------------
      
      edition_node <- record |>
        rvest::html_element(
          xpath = paste0(
            ".//h4[contains(., 'Edition')]",
            "/following-sibling::p[1]"
          )
        )
      
      source_collection <- if (
        inherits(edition_node, "xml_missing")
      ) {
        NA_character_
      } else {
        edition_node |>
          rvest::html_text2() |>
          stringr::str_squish()
      }
      
      
      # ------------------------------------------------------
      # Source text
      # ------------------------------------------------------
      
      source_text <- tryCatch(
        {
          get_page(text_source_url) |>
            extract_text()
        },
        error = function(e) {
          warning(
            "Text unavailable for key ",
            key,
            ": ",
            conditionMessage(e)
          )
          NA_character_
        }
      )
      
      
      # ------------------------------------------------------
      # Return one tidy record
      # ------------------------------------------------------
      
      tibble::tibble(
        
        key = key,
        
        schema_version = "1.0",
        
        text_id = text_id,
        
        bhg = bhg,
        
        bhl = bhl,
        
        bho = bho,
        
        title = title,
        
        saint_dates = saint_dates,
        
        original_date = original_date,
        
        domain = domain,
        
        region = region,
        
        author = author,
        
        source_collection = source_collection,
        
        source_url = source_url,
        
        text_source_url = text_source_url,
        
        rights_status = "review_required",
        
        genre = "vita",
        
        collection = "medieval_translations",
        
        source_text = source_text
      )
      
    }, error = function(e) {
      
      message(
        "Key ",
        key,
        " failed: ",
        conditionMessage(e)
      )
      
      tibble::tibble(
        
        key = key,
        
        schema_version = "1.0",
        
        text_id = text_id,
        
        bhg = NA_character_,
        
        bhl = NA_character_,
        
        bho = NA_character_,
        
        title = NA_character_,
        
        saint_dates = NA_character_,
        
        original_date = NA_character_,
        
        domain = NA_character_,
        
        region = NA_character_,
        
        author = NA_character_,
        
        source_collection = NA_character_,
        
        source_url = source_url,
        
        text_source_url = text_source_url,
        
        rights_status = "review_required",
        
        genre = "vita",
        
        collection = "medieval_translations",
        
        source_text = NA_character_
      )
    })
  }
  
  
  # ----------------------------------------------------------
  # Scrape requested records
  # ----------------------------------------------------------
  
  results <- purrr::map2_dfr(
    keys,
    text_ids,
    function(key, text_id) {
      
      result <- scrape_one(
        key,
        text_id
      )
      
      if (pause > 0) {
        Sys.sleep(pause)
      }
      
      result
    }
  )
  
  results
}


## Scrape and save

# arrow::write_parquet(df_doaks, "derived/df_doaks.parquet")

# df_doaks <- arrow::read_parquet("derived/df_doaks.parquet")