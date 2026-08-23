
scrape_wikisource <- function(
    works = NULL,
    start_text_id = "oehc_000001",
    contact = Sys.getenv("OEHC_CONTACT"),
    cache_dir = "cache/wikisource"
) {
  
  # ==========================================================
  # Wikisource ANF/NPNF Hagiography Scraper
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
  # The function returns one dataframe row per hagiographic
  # WORK, not one row per Wikisource chapter.
  #
  # Multi-page works are assembled automatically from their
  # component Wikisource pages.
  #
  # Usage:
  #
  #   Sys.setenv(
  #     OEHC_CONTACT = "your-email@example.com"
  #   )
  #
  #   anf_npnf <- scrape_wikisource(
  #     start_text_id = "oehc_001000"
  #   )
  #
  # Or scrape selected works:
  #
  #   test <- scrape_wikisource(
  #     works = c("antony", "hilarion", "perpetua")
  #   )
  #
  # ==========================================================
  
  
  # ----------------------------------------------------------
  # Configuration
  # ----------------------------------------------------------
  
  api_url <- "https://en.wikisource.org/w/api.php"
  
  if (!nzchar(contact)) {
    stop(
      paste(
        "Please supply contact information for the Wikimedia",
        "User-Agent, either with contact = '...' or by setting",
        "the OEHC_CONTACT environment variable."
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
  # Acquisition manifest
  #
  # mode:
  #
  #   single
  #     Entire primary text occurs on one Wikisource page.
  #
  #   subpages
  #     Work is distributed across component pages.
  #
  # include_regex identifies which subpages belong to the
  # primary text and excludes editorial material.
  #
  # start_regex can trim editorial introductory paragraphs
  # appearing at the beginning of a single-page source.
  # ----------------------------------------------------------
  
  manifest <- tibble::tribble(
    
    ~work_id,
    ~title,
    ~page,
    ~mode,
    ~include_regex,
    ~start_regex,
    ~bhg,
    ~bhl,
    ~bho,
    ~saint_dates,
    ~original_date,
    ~region,
    ~author,
    ~genre,
    ~source_collection,
    
    
    # --------------------------------------------------------
    # Athanasius
    # --------------------------------------------------------
    
    "antony",
    "Life of Antony",
    paste0(
      "Nicene and Post-Nicene Fathers: Series II/",
      "Volume IV/Life of Antony/Vita Antoni"
    ),
    "subpages",
    "^(Preface|Chapter[ _][0-9]+)$",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "356–362",
    "EGYPT",
    "Athanasius of Alexandria",
    "vita",
    "Nicene and Post-Nicene Fathers, Second Series, Volume IV",
    
    
    # --------------------------------------------------------
    # Jerome
    # --------------------------------------------------------
    
    "paul",
    "Life of Paulus the First Hermit",
    paste0(
      "Nicene and Post-Nicene Fathers: Series II/",
      "Volume VI/Treatises/The Life of Paulus the First Hermit"
    ),
    "single",
    NA_character_,
    "^1\\.",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "374–375",
    "EGYPT",
    "Jerome",
    "vita",
    "Nicene and Post-Nicene Fathers, Second Series, Volume VI",
    
    
    "hilarion",
    "Life of Hilarion",
    paste0(
      "Nicene and Post-Nicene Fathers: Series II/",
      "Volume VI/Treatises/The Life of S. Hilarion"
    ),
    "single",
    NA_character_,
    "^1\\.",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "390",
    "PALESTINE.CYPRUS",
    "Jerome",
    "vita",
    "Nicene and Post-Nicene Fathers, Second Series, Volume VI",
    
    
    "malchus",
    "Life of Malchus, the Captive Monk",
    paste0(
      "Nicene and Post-Nicene Fathers: Series II/",
      "Volume VI/Treatises/",
      "The Life of Malchus, the Captive Monk"
    ),
    "single",
    NA_character_,
    "^1\\.",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "391",
    "SYRIA",
    "Jerome",
    "vita",
    "Nicene and Post-Nicene Fathers, Second Series, Volume VI",
    
    
    # --------------------------------------------------------
    # Sulpicius Severus
    # --------------------------------------------------------
    
    "martin",
    "Life of St. Martin of Tours",
    paste0(
      "Nicene and Post-Nicene Fathers: Series II/",
      "Volume XI/Sulpitius Severus/On the Life of St. Martin"
    ),
    "subpages",
    "^Chapter[ _][IVXLCDM]+$",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "late 4th century",
    "GAUL",
    "Sulpicius Severus",
    "vita",
    "Nicene and Post-Nicene Fathers, Second Series, Volume XI",
    
    
    "martin_dialogues",
    "Dialogues concerning St. Martin",
    paste0(
      "Nicene and Post-Nicene Fathers: Series II/",
      "Volume XI/Sulpitius Severus/Dialogues"
    ),
    "subpages",
    "^Dialogue[ _](I|II|III)/Chapter[ _][IVXLCDM]+$",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "early 5th century",
    "GAUL.EGYPT",
    "Sulpicius Severus",
    "dialogue",
    "Nicene and Post-Nicene Fathers, Second Series, Volume XI",
    
    
    # --------------------------------------------------------
    # Pontius
    # --------------------------------------------------------
    
    "cyprian",
    "Life and Passion of Cyprian",
    paste0(
      "Ante-Nicene Fathers/Volume V/Cyprian/",
      "The Life and Passion of Cyprian, Bishop and Martyr. ",
      "By Pontius the Deacon"
    ),
    "single",
    NA_character_,
    "^1\\.",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "3rd century",
    "NORTH_AFRICA",
    "Pontius the Deacon",
    "vita",
    "Ante-Nicene Fathers, Volume V",
    
    
    # --------------------------------------------------------
    # Perpetua and Felicitas
    #
    # Introductory Notice and Elucidations are excluded.
    # --------------------------------------------------------
    
    "perpetua",
    "Passion of Perpetua and Felicitas",
    paste0(
      "Ante-Nicene Fathers/Volume III/Ethical/",
      "The Passion of the Holy Martyrs Perpetua and Felicitas"
    ),
    "subpages",
    "^(Preface|Argument[ _](I|II|III|IV|V|VI))$",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "203",
    "NORTH_AFRICA",
    "Anonymous / Perpetua",
    "martyr_act",
    "Ante-Nicene Fathers, Volume III",
    
    
    # --------------------------------------------------------
    # Polycarp
    # --------------------------------------------------------
    
    "polycarp",
    "Martyrdom of Polycarp",
    "Ante-Nicene Fathers/Volume I/The Martyrdom of Polycarp",
    "single",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "2nd century",
    "ASIA_MINOR",
    "Church of Smyrna",
    "martyr_act",
    "Ante-Nicene Fathers, Volume I",
    
    
    # --------------------------------------------------------
    # Scillitan Martyrs
    # --------------------------------------------------------
    
    "scillitan",
    "Passion of the Scillitan Martyrs",
    paste0(
      "Ante-Nicene Fathers/Volume IX/",
      "The Passion of the Scillitan Martyrs/",
      "The Passion of the Scillitan Martyrs"
    ),
    "single",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "180",
    "NORTH_AFRICA",
    "Anonymous",
    "martyr_act",
    "Ante-Nicene Fathers, Volume IX",
    
    
    # --------------------------------------------------------
    # Eusebius: Martyrs of Palestine
    #
    # NPNF editorial Introduction is excluded.
    # --------------------------------------------------------
    
    "palestinian_martyrs",
    "Martyrs of Palestine",
    paste0(
      "Nicene and Post-Nicene Fathers: Series II/",
      "Volume I/Church History of Eusebius/Martyrs of Palestine"
    ),
    "subpages",
    "^Chapter[ _][IVXLCDM]+$",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "early 4th century",
    "PALESTINE",
    "Eusebius of Caesarea",
    "martyrdom_collection",
    "Nicene and Post-Nicene Fathers, Second Series, Volume I",
    
    
    # --------------------------------------------------------
    # Eusebius: selected ecclesiastical-history narratives
    # --------------------------------------------------------
    
    "lyons_vienne",
    "Martyrs of Lyons and Vienne",
    paste0(
      "Nicene and Post-Nicene Fathers: Series II/",
      "Volume I/Church History of Eusebius/",
      "Book V/Chapter 1"
    ),
    "single",
    NA_character_,
    "^1\\.",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "early 4th century",
    "GAUL",
    "Eusebius of Caesarea",
    "ecclesiastical_history_excerpt",
    "Nicene and Post-Nicene Fathers, Second Series, Volume I",
    
    
    "caesarea_martyrs",
    "Martyrs at Caesarea in Palestine",
    paste0(
      "Nicene and Post-Nicene Fathers: Series II/",
      "Volume I/Church History of Eusebius/",
      "Book VII/Chapter 12"
    ),
    "single",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "early 4th century",
    "PALESTINE",
    "Eusebius of Caesarea",
    "ecclesiastical_history_excerpt",
    "Nicene and Post-Nicene Fathers, Second Series, Volume I"
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
  # Generate sequential OEHC text IDs
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
      id_start + seq_len(nrow(manifest)) - 1,
      width = id_width,
      pad = "0"
    )
  )
  
  
  # ----------------------------------------------------------
  # Canonical Wikisource URL
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
  # Common API request
  #
  # All requests share:
  #
  #   * identifying User-Agent
  #   * maxlag=5
  #   * one-request-per-second throttle
  #   * retry only for HTTP 429 / 503
  #
  # Because all requests use the same throttle realm, nested
  # chapter calls cannot inadvertently generate a burst.
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
  # Wikisource page titles can contain slashes, punctuation,
  # spaces, etc. A hash provides a safe local filename.
  # ----------------------------------------------------------
  
  cache_file <- function(
    value,
    type,
    extension
  ) {
    
    hash <- digest::digest(
      value,
      algo = "xxhash64"
    )
    
    file.path(
      cache_dir,
      paste0(
        type,
        "_",
        hash,
        ".",
        extension
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Retrieve rendered HTML for one Wikisource page
  #
  # Once downloaded, the parsed HTML is stored locally.
  # Subsequent runs use the cached copy without another API
  # request.
  # ----------------------------------------------------------
  
  get_page_doc <- function(page) {
    
    cached <- cache_file(
      page,
      "page",
      "html"
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
  # Discover descendant pages beneath a parent work
  #
  # Results are also cached because the chapter structure of
  # these historical Wikisource works rarely changes.
  # ----------------------------------------------------------
  
  get_subpages <- function(parent) {
    
    cached <- cache_file(
      parent,
      "subpages",
      "rds"
    )
    
    
    if (file.exists(cached)) {
      
      return(
        readRDS(
          cached
        )
      )
    }
    
    
    prefix <- paste0(
      parent,
      "/"
    )
    
    pages <- character()
    
    continue_from <- NULL
    
    
    repeat {
      
      request <- make_request() |>
        
        httr2::req_url_query(
          action = "query",
          list = "allpages",
          apprefix = prefix,
          apnamespace = 0,
          aplimit = "max",
          format = "json",
          formatversion = 2
        )
      
      
      if (!is.null(continue_from)) {
        
        request <- request |>
          
          httr2::req_url_query(
            apcontinue = continue_from
          )
      }
      
      
      response <- request |>
        httr2::req_perform()
      
      
      body <- httr2::resp_body_json(
        response,
        simplifyVector = FALSE
      )
      
      
      if (!is.null(body$error)) {
        
        stop(
          "Wikisource API error while discovering subpages: ",
          body$error$info
        )
      }
      
      
      if (
        !is.null(body$query$allpages) &&
        length(body$query$allpages) > 0
      ) {
        
        pages <- c(
          pages,
          purrr::map_chr(
            body$query$allpages,
            ~ .x$title
          )
        )
      }
      
      
      continue_from <-
        body[["continue"]][["apcontinue"]]
      
      
      if (is.null(continue_from)) {
        break
      }
    }
    
    
    pages <- unique(
      pages
    )
    
    
    saveRDS(
      pages,
      cached
    )
    
    
    pages
  }
  
  
  # ----------------------------------------------------------
  # Convert Arabic or Roman section numbers to integers
  #
  # This prevents alphabetical ordering such as:
  #
  # Chapter 1
  # Chapter 10
  # Chapter 11
  # Chapter 2
  # ----------------------------------------------------------
  
  number_value <- function(x) {
    
    roman_values <- c(
      I = 1,
      V = 5,
      X = 10,
      L = 50,
      C = 100,
      D = 500,
      M = 1000
    )
    
    
    parse_one <- function(value) {
      
      if (
        is.na(value) ||
        !nzchar(value)
      ) {
        return(NA_integer_)
      }
      
      
      if (
        stringr::str_detect(
          value,
          "^[0-9]+$"
        )
      ) {
        
        return(
          as.integer(value)
        )
      }
      
      
      chars <- strsplit(
        value,
        ""
      )[[1]]
      
      
      nums <- unname(
        roman_values[chars]
      )
      
      
      if (any(is.na(nums))) {
        return(NA_integer_)
      }
      
      
      total <- 0L
      
      
      for (i in seq_along(nums)) {
        
        if (
          i < length(nums) &&
          nums[i] < nums[i + 1]
        ) {
          
          total <- total - nums[i]
          
        } else {
          
          total <- total + nums[i]
        }
      }
      
      
      as.integer(total)
    }
    
    
    vapply(
      x,
      parse_one,
      integer(1)
    )
  }
  
  
  # ----------------------------------------------------------
  # Put discovered subpages into reading order
  # ----------------------------------------------------------
  
  order_subpages <- function(
    pages,
    parent
  ) {
    
    relative <- stringr::str_remove(
      pages,
      stringr::fixed(
        paste0(
          parent,
          "/"
        )
      )
    )
    
    
    # Dialogue number, where applicable.
    dialogue_token <- stringr::str_match(
      relative,
      "^Dialogue[ _]([IVXLCDM]+)/"
    )[, 2]
    
    
    dialogue_number <- number_value(
      dialogue_token
    )
    
    
    dialogue_number[
      is.na(dialogue_number)
    ] <- 0L
    
    
    # Last part of page path:
    #
    # Chapter 1
    # Chapter XII
    # Argument III
    # Preface
    leaf <- stringr::str_extract(
      relative,
      "[^/]+$"
    )
    
    
    section_token <- stringr::str_match(
      leaf,
      "^(?:Chapter|Argument)[ _]([0-9IVXLCDM]+)$"
    )[, 2]
    
    
    section_number <- number_value(
      section_token
    )
    
    
    section_number[
      is.na(section_number)
    ] <- 9999L
    
    
    # Prefaces precede numbered sections.
    section_group <- ifelse(
      leaf == "Preface",
      0L,
      1L
    )
    
    
    section_number[
      leaf == "Preface"
    ] <- 0L
    
    
    pages[
      order(
        dialogue_number,
        section_group,
        section_number,
        relative
      )
    ]
  }
  
  
  # ----------------------------------------------------------
  # Extract primary prose from one rendered Wikisource page
  # ----------------------------------------------------------
  
  extract_page_text <- function(
    page,
    start_regex = NA_character_
  ) {
    
    doc <- get_page_doc(
      page
    )
    
    
    # Remove Wikisource apparatus and references before
    # extracting the prose.
    unwanted <- rvest::html_elements(
      doc,
      paste(
        ".reference",
        ".references",
        ".mw-references-wrap",
        ".mw-editsection",
        ".ws-noexport",
        ".noprint",
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
    
    
    # ANF/NPNF prose is normally organized in paragraphs.
    paragraphs <- doc |>
      
      rvest::html_elements("p") |>
      
      rvest::html_text2() |>
      
      stringr::str_replace_all(
        "\u00A0",
        " "
      ) |>
      
      stringr::str_squish()
    
    
    paragraphs <- paragraphs[
      !is.na(paragraphs) &
        nzchar(paragraphs)
    ]
    
    
    # Some single-page works begin with short editorial text.
    # Where specified in the manifest, begin with the first
    # paragraph matching start_regex.
    if (
      !is.na(start_regex) &&
      nzchar(start_regex) &&
      length(paragraphs) > 0
    ) {
      
      start <- which(
        stringr::str_detect(
          paragraphs,
          stringr::regex(
            start_regex
          )
        )
      )
      
      
      if (length(start) > 0) {
        
        paragraphs <- paragraphs[
          start[[1]]:length(paragraphs)
        ]
      }
    }
    
    
    # Fallback for unusual pages without <p> elements.
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
        
        stringr::str_replace_all(
          "\u00A0",
          " "
        ) |>
        
        stringr::str_squish()
      
      
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
  # Generate human-readable section labels for assembled works
  #
  # Examples:
  #
  # Chapter 1
  # Argument III
  # Dialogue II — Chapter VI
  # ----------------------------------------------------------
  
  section_label <- function(
    page,
    parent
  ) {
    
    page |>
      
      stringr::str_remove(
        stringr::fixed(
          paste0(
            parent,
            "/"
          )
        )
      ) |>
      
      stringr::str_replace_all(
        "_",
        " "
      ) |>
      
      stringr::str_replace_all(
        "/",
        " — "
      )
  }
  
  
  # ----------------------------------------------------------
  # Scrape and assemble one complete work
  # ----------------------------------------------------------
  
  scrape_one <- function(
    row,
    text_id
  ) {
    
    row <- as.list(
      row
    )
    
    
    tryCatch({
      
      message(
        "Scraping ",
        text_id,
        ": ",
        row$title
      )
      
      
      # ------------------------------------------------------
      # Identify Wikisource pages belonging to the work
      # ------------------------------------------------------
      
      if (row$mode == "single") {
        
        pages <- row$page
        
      } else {
        
        pages <- get_subpages(
          row$page
        )
        
        
        relative <- stringr::str_remove(
          pages,
          stringr::fixed(
            paste0(
              row$page,
              "/"
            )
          )
        )
        
        
        pages <- pages[
          stringr::str_detect(
            relative,
            stringr::regex(
              row$include_regex
            )
          )
        ]
        
        
        pages <- order_subpages(
          pages,
          row$page
        )
      }
      
      
      if (length(pages) == 0) {
        
        stop(
          "No text pages found."
        )
      }
      
      
      # ------------------------------------------------------
      # Extract the primary text from each page
      # ------------------------------------------------------
      
      texts <- purrr::map_chr(
        pages,
        ~ extract_page_text(
          .x,
          start_regex = row$start_regex
        )
      )
      
      
      good <- !is.na(texts) &
        nzchar(texts)
      
      
      pages <- pages[good]
      texts <- texts[good]
      
      
      if (length(texts) == 0) {
        
        stop(
          "No usable source text extracted."
        )
      }
      
      
      # ------------------------------------------------------
      # Assemble a multi-page work
      # ------------------------------------------------------
      
      if (row$mode == "subpages") {
        
        labels <- purrr::map_chr(
          pages,
          ~ section_label(
            .x,
            row$page
          )
        )
        
        
        blocks <- paste0(
          "## ",
          labels,
          "\n\n",
          texts
        )
        
        
        source_text <- paste(
          blocks,
          collapse = "\n\n"
        )
        
      } else {
        
        source_text <- texts[[1]]
      }
      
      
      # ------------------------------------------------------
      # Return one OEHC-style record
      # ------------------------------------------------------
      
      tibble::tibble(
        
        schema_version = "1.0",
        
        text_id = text_id,
        
        bhg = row$bhg,
        bhl = row$bhl,
        bho = row$bho,
        
        title = row$title,
        
        saint_dates = row$saint_dates,
        
        original_date = row$original_date,
        
        domain = NA_character_,
        
        region = row$region,
        
        author = row$author,
        
        source_collection =
          row$source_collection,
        
        text_source_url =
          page_url(row$page),
        
        rights_status =
          "public_domain",
        
        genre = row$genre,
        
        collection =
          "ancient",
        
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
      
      
      tibble::tibble(
        
        schema_version = "1.0",
        
        text_id = text_id,
        
        bhg = row$bhg,
        bhl = row$bhl,
        bho = row$bho,
        
        title = row$title,
        
        saint_dates = row$saint_dates,
        
        original_date = row$original_date,
        
        domain = NA_character_,
        
        region = row$region,
        
        author = row$author,
        
        source_collection =
          row$source_collection,
        
        text_source_url =
          page_url(row$page),
        
        rights_status =
          "public_domain",
        
        genre = row$genre,
        
        collection =
          "ancient",
        
        source_text =
          NA_character_
      )
    })
  }
  
  
  # ----------------------------------------------------------
  # Scrape the selected acquisition manifest
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

# 
# df_npnf <- scrape_wikisource(
#   works = manifest$work_id,
#   start_text_id = "oehc_000320"
# )

## Scrape and save

# arrow::write_parquet(df_npnf, "derived/df_npnf.parquet")

# df_npnf <- arrow::read_parquet("derived/df_npnf.parquet")


