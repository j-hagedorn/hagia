scrape_aelfric <- function(
    works = NULL,
    start_text_id = "oehc_000001",
    contact = Sys.getenv("OEHC_CONTACT"),
    cache_dir = "cache/aelfric"
) {
  
  # ==========================================================
  # ÆLFRIC'S LIVES OF SAINTS — WIKISOURCE API SCRAPER
  #
  # Returns one dataframe row per hagiographic narrative.
  #
  # The scraper targets the English translations in:
  #
  #   Ælfric's Lives of Saints
  #   ed. Walter W. Skeat
  #   EETS OS 76, 82, 94, 114
  #   1881–1900
  #
  # Required packages:
  #
  # install.packages(c(
  #   "httr2",
  #   "rvest",
  #   "xml2",
  #   "stringr",
  #   "dplyr",
  #   "purrr",
  #   "tibble",
  #   "digest"
  # ))
  #
  # Before running:
  #
  # Sys.setenv(
  #   OEHC_CONTACT = "your-email@example.com"
  # )
  #
  # Examples:
  #
  # all_aelfric <- scrape_aelfric(
  #   start_text_id = "oehc_002000"
  # )
  #
  # test <- scrape_aelfric(
  #   works = c("eugenia", "basil", "sebastian")
  # )
  #
  # ==========================================================
  
  
  # ----------------------------------------------------------
  # Configuration
  # ----------------------------------------------------------
  
  api_url <- "https://en.wikisource.org/w/api.php"
  
  parent_page <- "Ælfric's Lives of Saints"
  
  if (!nzchar(contact)) {
    stop(
      paste(
        "Please supply contact information for Wikimedia",
        "using contact = '...' or the OEHC_CONTACT",
        "environment variable."
      )
    )
  }
  
  user_agent <- paste0(
    "OpenEnglishHagiographyCorpus/0.1 (",
    contact,
    ")"
  )
  
  dir.create(
    cache_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Curated hagiographic manifest
  #
  # These are the 30 narrative texts retained for the core
  # hagiographic corpus.
  #
  # page_title is the title of the English translation page
  # beneath "Ælfric's Lives of Saints".
  #
  # chapter preserves Skeat/Wikisource chapter numbering.
  #
  # feast_date is retained where Wikisource supplies it.
  # ----------------------------------------------------------
  
  manifest <- tibble::tribble(
    
    ~work_id,
    ~chapter,
    ~page_title,
    ~title,
    ~feast_date,
    ~genre,
    
    
    "eugenia",
    "II",
    "Of Saint Eugenia",
    "Saint Eugenia",
    "December 25",
    "vita",
    
    
    "basil",
    "III",
    "Of Saint Basil",
    "Saint Basil",
    "January 1",
    "vita",
    
    
    "julian_basilissa",
    "IV",
    "Of Saint Julian and Basilissa",
    "Saint Julian and Basilissa",
    "January 9",
    "vita",
    
    
    "sebastian",
    "V",
    "Of Saint Sebastian",
    "Saint Sebastian",
    "January 20",
    "martyr_act",
    
    
    "maurus",
    "VI",
    "Of Saint Maurus",
    "Saint Maurus",
    "January 15",
    "vita",
    
    
    "agnes",
    "VII",
    "Of Saint Agnes",
    "Saint Agnes",
    "January 21",
    "martyr_act",
    
    
    "agatha",
    "VIII",
    "Of Saint Agatha",
    "Saint Agatha",
    "February 5",
    "martyr_act",
    
    
    "lucy",
    "IX",
    "and Saint Lucy",
    "Saint Lucy",
    "December 13",
    "martyr_act",
    
    
    "forty_soldiers",
    "XI",
    "Of the Forty Soldiers",
    "The Forty Soldiers",
    "March 9",
    "martyr_act",
    
    
    "george",
    "XIV",
    "Of Saint George",
    "Saint George",
    "April 23",
    "martyr_act",
    
    
    "mark",
    "XV",
    "Of Saint Mark the Evangelist",
    "Saint Mark the Evangelist",
    "April 25",
    "vita",
    
    
    "alban",
    "XIX",
    "Of Saint Alban",
    "Saint Alban",
    "June 22",
    "martyr_act",
    
    
    "aetheldrytha",
    "XX",
    "Of Saint Æðeldryða",
    "Saint Æðeldryða",
    "June 23",
    "vita",
    
    
    "swythun",
    "XXI",
    "Of Saint Swythun",
    "Saint Swythun",
    "July 2",
    "vita",
    
    
    "apollinaris",
    "XXII",
    "Of Saint Apollinaris",
    "Saint Apollinaris",
    "July 23",
    "vita",
    
    
    "seven_sleepers",
    "XXIII",
    "Of the Seven Sleepers",
    "The Seven Sleepers",
    "July 27",
    "legend",
    
    
    "mary_egypt",
    "XXIIIb",
    "Death of St. Mary of Egypt",
    "Saint Mary of Egypt",
    "April 2",
    "vita",
    
    
    "abdon_sennes",
    "XXIV",
    "Of Abdon and Sennes",
    "Abdon and Sennes",
    NA_character_,
    "martyr_act",
    
    
    "machabees",
    "XXV",
    "Of the Machabees",
    "The Machabees",
    "August 1",
    "martyr_act",
    
    
    "oswold",
    "XXVI",
    "Of Saint Oswold",
    "Saint Oswold",
    "August 5",
    "vita",
    
    
    "theban_legion",
    "XXVIII",
    "Of the Theban Legion",
    "The Theban Legion",
    "September 22",
    "martyr_act",
    
    
    "dionysius",
    "XXIX",
    "Of Saint Dionysius",
    "Saint Dionysius",
    "October 9",
    "martyr_act",
    
    
    "eustace",
    "XXX",
    "Of Saint Eustace",
    "Saint Eustace",
    "November 2",
    "legend",
    
    
    "martin",
    "XXXI",
    "Of Saint Martin",
    "Saint Martin",
    "November 11",
    "vita",
    
    
    "edmund",
    "XXXII",
    "Of Saint Edmund",
    "Saint Edmund",
    "November 20",
    "martyr_act",
    
    
    "euphrasia",
    "XXXIII",
    "Of Saint Euphrasia",
    "Saint Euphrasia",
    "February 11",
    "vita",
    
    
    "cecilia",
    "XXXIV",
    "Of Saint Cecilia",
    "Saint Cecilia",
    "November 22",
    "martyr_act",
    
    
    "crisantus_daria",
    "XXXV",
    "Of Crisantus and Daria",
    "Crisantus and Daria",
    "December 1",
    "martyr_act",
    
    
    "thomas",
    "XXXVI",
    "Of Saint Thomas the Apostle",
    "Saint Thomas the Apostle",
    "December 21",
    "martyr_act",
    
    
    "vincent",
    NA_character_,
    "(The Martyrdom of St. Vincent)",
    "Saint Vincent",
    NA_character_,
    "martyr_act"
  )
  
  
  # ----------------------------------------------------------
  # Select requested works
  # ----------------------------------------------------------
  
  if (!is.null(works)) {
    
    unknown <- setdiff(
      works,
      manifest$work_id
    )
    
    if (length(unknown) > 0) {
      stop(
        "Unknown work_id: ",
        paste(unknown, collapse = ", ")
      )
    }
    
    manifest <- manifest |>
      dplyr::filter(
        work_id %in% works
      )
  }
  
  
  # ----------------------------------------------------------
  # Generate sequential OEHC IDs
  # ----------------------------------------------------------
  
  id_parts <- stringr::str_match(
    start_text_id,
    "^(.*?)([0-9]+)$"
  )
  
  if (is.na(id_parts[1, 1])) {
    stop(
      "start_text_id must end with a number, ",
      "for example 'oehc_000001'."
    )
  }
  
  id_prefix <- id_parts[1, 2]
  id_start  <- as.integer(id_parts[1, 3])
  id_width  <- nchar(id_parts[1, 3])
  
  text_ids <- paste0(
    id_prefix,
    stringr::str_pad(
      id_start + seq_len(nrow(manifest)) - 1,
      width = id_width,
      pad = "0"
    )
  )
  
  
  # ----------------------------------------------------------
  # Construct full Wikisource page title
  # ----------------------------------------------------------
  
  full_page_title <- function(page_title) {
    
    paste0(
      parent_page,
      "/",
      page_title
    )
  }
  
  
  # ----------------------------------------------------------
  # Construct browser-facing Wikisource URL
  # ----------------------------------------------------------
  
  page_url <- function(page) {
    
    page <- stringr::str_replace_all(
      page,
      " ",
      "_"
    )
    
    paste0(
      "https://en.wikisource.org/wiki/",
      utils::URLencode(
        page,
        reserved = FALSE
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Common Wikimedia API request
  #
  # All API traffic:
  #
  #   * identifies the project
  #   * uses maxlag=5
  #   * is limited to one request per second
  #   * retries only 429 and 503 responses
  # ----------------------------------------------------------
  
  make_request <- function() {
    
    httr2::request(
      api_url
    ) |>
      
      httr2::req_user_agent(
        user_agent
      ) |>
      
      httr2::req_url_query(
        maxlag = 5
      ) |>
      
      httr2::req_throttle(
        capacity = 1,
        fill_time_s = 1,
        realm = "oehc-wikisource-api"
      ) |>
      
      httr2::req_retry(
        max_tries = 3,
        is_transient = function(resp) {
          
          httr2::resp_status(resp) %in%
            c(429, 503)
        }
      ) |>
      
      httr2::req_timeout(
        30
      )
  }
  
  
  # ----------------------------------------------------------
  # Cache filename
  #
  # Wikisource titles contain punctuation, Unicode, and
  # slashes, so hashed filenames are safer.
  # ----------------------------------------------------------
  
  cache_file <- function(page) {
    
    hash <- digest::digest(
      page,
      algo = "xxhash64"
    )
    
    file.path(
      cache_dir,
      paste0(
        "page_",
        hash,
        ".html"
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Retrieve rendered Wikisource page
  #
  # Cached HTML is used whenever available.
  # ----------------------------------------------------------
  
  get_page_doc <- function(page) {
    
    cached <- cache_file(
      page
    )
    
    
    if (file.exists(cached)) {
      
      return(
        xml2::read_html(
          cached
        )
      )
    }
    
    
    response <- make_request() |>
      
      httr2::req_url_query(
        action = "parse",
        page = page,
        prop = "text",
        redirects = 1,
        format = "json",
        formatversion = 2
      ) |>
      
      httr2::req_perform()
    
    
    body <- httr2::resp_body_json(
      response,
      simplifyVector = FALSE
    )
    
    
    if (!is.null(body$error)) {
      
      stop(
        "Wikisource API error for '",
        page,
        "': ",
        body$error$info
      )
    }
    
    
    html <- body$parse$text
    
    
    writeLines(
      html,
      cached,
      useBytes = TRUE
    )
    
    
    xml2::read_html(
      html
    )
  }
  
  
  # ----------------------------------------------------------
  # Clean a text block without destroying meaningful lineation
  #
  # Unlike str_squish(), this keeps embedded line breaks.
  # ----------------------------------------------------------
  
  clean_block <- function(x) {
    
    x <- stringr::str_replace_all(
      x,
      "\u00A0",
      " "
    )
    
    x <- stringr::str_replace_all(
      x,
      "\r\n?",
      "\n"
    )
    
    lines <- stringr::str_split(
      x,
      "\n"
    )[[1]]
    
    lines <- stringr::str_trim(
      lines
    )
    
    # Remove repeated empty lines while otherwise preserving
    # line boundaries.
    x <- paste(
      lines,
      collapse = "\n"
    )
    
    x <- stringr::str_replace_all(
      x,
      "\n{3,}",
      "\n\n"
    )
    
    stringr::str_trim(
      x
    )
  }
  
  
  # ----------------------------------------------------------
  # Extract primary English translation text
  # ----------------------------------------------------------
  
  extract_text <- function(page) {
    
    doc <- get_page_doc(
      page
    )
    
    
    # Remove Wikisource/editorial apparatus.
    unwanted <- rvest::html_elements(
      doc,
      paste(
        ".reference",
        ".references",
        ".mw-references-wrap",
        ".mw-editsection",
        ".ws-noexport",
        ".noprint",
        ".licenseContainer",
        "#licenseContainer",
        ".authority-control",
        "table",
        "style",
        "script",
        sep = ", "
      )
    )
    
    
    if (length(unwanted) > 0) {
      xml2::xml_remove(
        unwanted
      )
    }
    
    
    # The translations are normally contained in paragraphs.
    paragraphs <- doc |>
      
      rvest::html_elements(
        ".mw-parser-output p"
      ) |>
      
      rvest::html_text2()
    
    
    paragraphs <- purrr::map_chr(
      paragraphs,
      clean_block
    )
    
    
    paragraphs <- paragraphs[
      !is.na(paragraphs) &
        nzchar(paragraphs)
    ]
    
    
    # Fallback in case a Wikisource page uses unusual markup.
    if (length(paragraphs) == 0) {
      
      content <- rvest::html_element(
        doc,
        ".mw-parser-output"
      )
      
      
      if (
        inherits(
          content,
          "xml_missing"
        )
      ) {
        
        content <- rvest::html_element(
          doc,
          "body"
        )
      }
      
      
      text <- content |>
        rvest::html_text2() |>
        clean_block()
      
      
      if (!nzchar(text)) {
        return(NA_character_)
      }
      
      
      return(text)
    }
    
    
    paste(
      paragraphs,
      collapse = "\n\n"
    )
  }
  
  
  # ----------------------------------------------------------
  # Scrape one Ælfric narrative
  # ----------------------------------------------------------
  
  scrape_one <- function(
    row,
    text_id
  ) {
    
    row <- as.list(
      row
    )
    
    page <- full_page_title(
      row$page_title
    )
    
    
    tryCatch({
      
      message(
        "Scraping ",
        text_id,
        ": ",
        row$title
      )
      
      
      source_text <- extract_text(
        page
      )
      
      
      if (
        is.na(source_text) ||
        !nzchar(source_text)
      ) {
        
        stop(
          "No usable source text extracted."
        )
      }
      
      
      tibble::tibble(
        
        schema_version = "1.0",
        
        text_id = text_id,
        
        bhg = NA_character_,
        bhl = NA_character_,
        bho = NA_character_,
        
        title = row$title,
        
        saint_dates = NA_character_,
        
        # Scholars date the collection around the end
        # of the tenth / beginning of the eleventh century.
        original_date = "c. 998–1002",
        
        domain = NA_character_,
        region = NA_character_,
        
        author = "Ælfric of Eynsham",
        
        # Collection-level translation credits.
        # Individual contributions vary within the edition.
        translator = paste(
          "Walter W. Skeat;",
          "Catherine Gunning;",
          "J. E. Wilkinson"
        ),
        
        translation_year = "1881–1900",
        
        source_collection = paste(
          "Ælfric's Lives of Saints,",
          "ed. Walter W. Skeat,",
          "EETS OS 76, 82, 94, 114",
          "(1881–1900)"
        ),
        
        chapter = row$chapter,
        
        feast_date = row$feast_date,
        
        text_source_url =
          page_url(page),
        
        rights_status =
          "public_domain",
        
        genre = row$genre,
        
        collection =
          "historical_english",
        
        source_text =
          source_text
      )
      
      
    }, error = function(e) {
      
      message(
        "Failed: ",
        row$title,
        " — ",
        conditionMessage(e)
      )
      
      
      # Preserve metadata and sequencing even when one
      # Wikisource page fails.
      tibble::tibble(
        
        schema_version = "1.0",
        
        text_id = text_id,
        
        bhg = NA_character_,
        bhl = NA_character_,
        bho = NA_character_,
        
        title = row$title,
        
        saint_dates = NA_character_,
        
        original_date = "c. 998–1002",
        
        domain = NA_character_,
        region = NA_character_,
        
        author = "Ælfric of Eynsham",
        
        translator = paste(
          "Walter W. Skeat;",
          "Catherine Gunning;",
          "J. E. Wilkinson"
        ),
        
        translation_year = "1881–1900",
        
        source_collection = paste(
          "Ælfric's Lives of Saints,",
          "ed. Walter W. Skeat,",
          "EETS OS 76, 82, 94, 114",
          "(1881–1900)"
        ),
        
        chapter = row$chapter,
        
        feast_date = row$feast_date,
        
        text_source_url =
          page_url(page),
        
        rights_status =
          "public_domain",
        
        genre = row$genre,
        
        collection =
          "historical_english",
        
        source_text =
          NA_character_
      )
    })
  }
  
  
  # ----------------------------------------------------------
  # Scrape all selected narratives
  # ----------------------------------------------------------
  
  purrr::map2_dfr(
    
    seq_len(
      nrow(manifest)
    ),
    
    text_ids,
    
    ~ scrape_one(
      manifest[.x, ],
      .y
    )
  )
}

# df_aelfric <- 
#   scrape_aelfric(
#    works = manifest$work_id,
#    start_text_id = "oehc_000333"
#   )

## Scrape and save

# arrow::write_parquet(df_aelfric, "derived/df_aelfric.parquet")

# df_aelfric <- arrow::read_parquet("derived/df_aelfric.parquet")

