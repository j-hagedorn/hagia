scrape_historia_tripartita <- function(
    output = c("stories", "chapters", "both"),
    units = NULL,
    start_text_id = "oehc_000001",
    contact = Sys.getenv("OEHC_CONTACT"),
    cache_dir = "cache/historia_tripartita",
    force_refresh = FALSE,
    strip_pl_markers = TRUE,
    strict = TRUE
) {
  
  # ==========================================================
  # CASSIODORUS / EPIPHANIUS SCHOLASTICUS
  # HISTORIA ECCLESIASTICA TRIPARTITA
  #
  # Source:
  # Latin Wikisource
  #
  # Edition:
  # Patrologia Latina 69, cols. 879D-1214C
  #
  # Digital source:
  # Corpus Corporum -> Latin Wikisource
  #
  # Architecture:
  #
  #   Wikisource page
  #       |
  #       +--> canonical chapter table
  #       |
  #       +--> curated hagiographic story table
  #
  # output = "stories"
  #   OEHC-style person/group narrative records
  #
  # output = "chapters"
  #   complete chapter-level transcription
  #
  # output = "both"
  #   list(chapters = ..., stories = ...)
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
  #   "tibble"
  # ))
  #
  # Before running:
  #
  # Sys.setenv(
  #   OEHC_CONTACT = "your-email@example.com"
  # )
  #
  # ==========================================================
  
  
  output <- match.arg(output)
  
  
  # ----------------------------------------------------------
  # Source configuration
  # ----------------------------------------------------------
  
  api_url <-
    "https://la.wikisource.org/w/api.php"
  
  page_title <-
    "Historia tripartita"
  
  page_url <-
    "https://la.wikisource.org/wiki/Historia_tripartita"
  
  
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
  
  
  cache_file <- file.path(
    cache_dir,
    "historia_tripartita.html"
  )
  
  
  # ----------------------------------------------------------
  # Common Wikimedia request
  # ----------------------------------------------------------
  
  make_request <- function() {
    
    httr2::request(
      api_url
    ) |>
      
      httr2::req_user_agent(
        user_agent
      ) |>
      
      httr2::req_retry(
        max_tries = 3,
        is_transient = function(resp) {
          
          httr2::resp_status(resp) %in%
            c(429, 503)
        }
      ) |>
      
      httr2::req_timeout(
        60
      )
  }
  
  
  # ----------------------------------------------------------
  # Download complete rendered work
  #
  # Only one Wikimedia request is required.
  # ----------------------------------------------------------
  
  if (
    force_refresh ||
    !file.exists(cache_file)
  ) {
    
    message(
      "Downloading Historia Tripartita..."
    )
    
    
    response <- make_request() |>
      
      httr2::req_url_query(
        action = "parse",
        page = page_title,
        prop = "text",
        redirects = 1,
        format = "json",
        formatversion = 2,
        maxlag = 5
      ) |>
      
      httr2::req_perform()
    
    
    body <- httr2::resp_body_json(
      response,
      simplifyVector = FALSE
    )
    
    
    if (!is.null(body$error)) {
      
      stop(
        "Wikisource API error: ",
        body$error$info
      )
    }
    
    
    writeLines(
      body$parse$text,
      cache_file,
      useBytes = TRUE
    )
  }
  
  
  # ----------------------------------------------------------
  # Parse page
  # ----------------------------------------------------------
  
  doc <- xml2::read_html(
    cache_file
  )
  
  
  container <- rvest::html_element(
    doc,
    ".mw-parser-output"
  )
  
  
  if (
    inherits(
      container,
      "xml_missing"
    )
  ) {
    
    container <- rvest::html_element(
      doc,
      "body"
    )
  }
  
  
  if (
    inherits(
      container,
      "xml_missing"
    )
  ) {
    
    stop(
      "Could not locate Wikisource text container."
    )
  }
  
  
  # ----------------------------------------------------------
  # Remove Wikimedia apparatus
  # ----------------------------------------------------------
  
  unwanted <- rvest::html_elements(
    container,
    paste(
      ".mw-editsection",
      ".reference",
      ".references",
      ".mw-references-wrap",
      ".ws-noexport",
      ".noprint",
      ".printfooter",
      ".catlinks",
      ".mw-normal-catlinks",
      ".licenseContainer",
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
  
  
  # ----------------------------------------------------------
  # Flatten rendered work to structured plain text
  #
  # The source supplies:
  #
  #   LIBER PRIMUS.
  #   CAPUT PRIMUM.
  #   ...
  #
  # Therefore chapter parsing does not depend on MediaWiki
  # HTML heading wrappers.
  # ----------------------------------------------------------
  
  full_text <- rvest::html_text2(
    container
  )
  
  
  full_text <- full_text |>
    
    stringr::str_replace_all(
      "\u00A0",
      " "
    ) |>
    
    stringr::str_replace_all(
      "\r\n?",
      "\n"
    ) |>
    
    stringr::str_replace_all(
      "[ \t]+\n",
      "\n"
    ) |>
    
    stringr::str_replace_all(
      "\n[ \t]+",
      "\n"
    ) |>
    
    stringr::str_replace_all(
      "\n{3,}",
      "\n\n"
    )
  
  
  # ----------------------------------------------------------
  # Remove Wikisource footer if present
  # ----------------------------------------------------------
  
  footer_location <- stringr::str_locate(
    full_text,
    stringr::regex(
      "\\bReceptum de\\b",
      ignore_case = TRUE
    )
  )
  
  
  if (!is.na(footer_location[1, "start"])) {
    
    full_text <- stringr::str_sub(
      full_text,
      1,
      footer_location[1, "start"] - 1
    )
  }
  
  
  # ----------------------------------------------------------
  # Roman numeral converter
  # ----------------------------------------------------------
  
  roman_to_int <- function(x) {
    
    values <- c(
      I = 1L,
      V = 5L,
      X = 10L,
      L = 50L,
      C = 100L,
      D = 500L,
      M = 1000L
    )
    
    
    chars <- strsplit(
      toupper(x),
      ""
    )[[1]]
    
    
    nums <- unname(
      values[chars]
    )
    
    
    if (
      length(nums) == 0 ||
      any(is.na(nums))
    ) {
      
      return(
        NA_integer_
      )
    }
    
    
    if (length(nums) == 1) {
      
      return(
        nums[[1]]
      )
    }
    
    
    total <- 0L
    
    
    for (i in seq_along(nums)) {
      
      if (
        i < length(nums) &&
        nums[[i]] <
        nums[[i + 1]]
      ) {
        
        total <- total -
          nums[[i]]
        
      } else {
        
        total <- total +
          nums[[i]]
      }
    }
    
    
    as.integer(
      total
    )
  }
  
  
  # ----------------------------------------------------------
  # Book labels
  # ----------------------------------------------------------
  
  book_numbers <- c(
    
    PRIMUS = 1L,
    SECUNDUS = 2L,
    TERTIUS = 3L,
    QUARTUS = 4L,
    QUINTUS = 5L,
    SEXTUS = 6L,
    SEPTIMUS = 7L,
    OCTAVUS = 8L,
    NONUS = 9L,
    DECIMUS = 10L,
    UNDECIMUS = 11L,
    DUODECIMUS = 12L
  )
  
  
  book_pattern <- paste0(
    
    "\\bLIBER\\s+(",
    
    paste(
      names(book_numbers),
      collapse = "|"
    ),
    
    ")\\."
  )
  
  
  book_locations <- stringr::str_locate_all(
    
    full_text,
    
    stringr::regex(
      book_pattern
    )
  )[[1]]
  
  
  book_matches <- stringr::str_match_all(
    
    full_text,
    
    stringr::regex(
      book_pattern
    )
  )[[1]]
  
  
  if (nrow(book_locations) != 12) {
    
    stop(
      paste0(
        "Expected 12 books but detected ",
        nrow(book_locations),
        ". The source structure should be reviewed."
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Clean body text while retaining paragraph breaks
  # ----------------------------------------------------------
  
  clean_source_text <- function(x) {
    
    if (
      is.na(x) ||
      !nzchar(x)
    ) {
      
      return(
        NA_character_
      )
    }
    
    
    x <- stringr::str_replace_all(
      x,
      "\u00A0",
      " "
    )
    
    
    if (strip_pl_markers) {
      
      # Example:
      #
      #   (0943B) 1024
      #
      # or:
      #
      #   (0943B)
      
      x <- stringr::str_replace_all(
        x,
        "\\(\\d{4}[A-D]\\)\\s*\\d{3,4}\\b",
        ""
      )
      
      
      x <- stringr::str_replace_all(
        x,
        "\\(\\d{4}[A-D]\\)",
        ""
      )
    }
    
    
    lines <- stringr::str_split(
      x,
      "\n"
    )[[1]]
    
    
    lines <- stringr::str_squish(
      lines
    )
    
    
    lines <- lines[
      !is.na(lines) &
        nzchar(lines)
    ]
    
    
    if (length(lines) == 0) {
      
      return(
        NA_character_
      )
    }
    
    
    paste(
      lines,
      collapse = "\n\n"
    ) |>
      
      stringr::str_trim()
  }
  
  
  # ----------------------------------------------------------
  # Separate chapter rubric from chapter body
  # ----------------------------------------------------------
  
  split_heading_body <- function(x) {
    
    x <- stringr::str_trim(
      x
    )
    
    
    match <- stringr::str_match(
      
      x,
      
      stringr::regex(
        "^(.{1,700}?\\.)\\s+(?=(?:\\(\\d{4}[A-D]\\)|[[:upper:]]))",
        dotall = TRUE
      )
    )
    
    
    if (!is.na(match[1, 2])) {
      
      heading <- stringr::str_squish(
        match[1, 2]
      )
      
      
      body <- stringr::str_sub(
        x,
        nchar(match[1, 2]) + 1L
      )
      
      
      return(
        list(
          heading = heading,
          body = body
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Fallback:
    # first PL column marker
    # --------------------------------------------------------
    
    pl_location <- stringr::str_locate(
      x,
      "\\(\\d{4}[A-D]\\)"
    )
    
    
    if (
      !is.na(pl_location[1, "start"]) &&
      pl_location[1, "start"] <= 700
    ) {
      
      return(
        list(
          
          heading =
            stringr::str_squish(
              stringr::str_sub(
                x,
                1,
                pl_location[1, "start"] - 1
              )
            ),
          
          body =
            stringr::str_sub(
              x,
              pl_location[1, "start"]
            )
        )
      )
    }
    
    
    list(
      heading = NA_character_,
      body = x
    )
  }
  
  
  # ----------------------------------------------------------
  # Parse one book into chapters
  # ----------------------------------------------------------
  
  parse_book <- function(
    book_number,
    book_text
  ) {
    
    chapter_pattern <-
      "\\bCAPUT\\s+(PRIMUM|[IVXLCDM]+)\\."
    
    
    chapter_locations <- stringr::str_locate_all(
      
      book_text,
      
      stringr::regex(
        chapter_pattern
      )
    )[[1]]
    
    
    chapter_matches <- stringr::str_match_all(
      
      book_text,
      
      stringr::regex(
        chapter_pattern
      )
    )[[1]]
    
    
    if (nrow(chapter_locations) == 0) {
      
      return(
        tibble::tibble()
      )
    }
    
    
    purrr::map_dfr(
      
      seq_len(
        nrow(chapter_locations)
      ),
      
      function(i) {
        
        start <-
          chapter_locations[i, "start"]
        
        
        end <- if (
          i <
          nrow(chapter_locations)
        ) {
          
          chapter_locations[
            i + 1,
            "start"
          ] - 1L
          
        } else {
          
          nchar(
            book_text
          )
        }
        
        
        chunk <- stringr::str_sub(
          book_text,
          start,
          end
        )
        
        
        chapter_token <-
          chapter_matches[i, 2]
        
        
        chapter_number <- if (
          chapter_token ==
          "PRIMUM"
        ) {
          
          1L
          
        } else {
          
          roman_to_int(
            chapter_token
          )
        }
        
        
        chunk <- stringr::str_remove(
          
          chunk,
          
          stringr::regex(
            paste0(
              "^\\s*CAPUT\\s+",
              chapter_token,
              "\\.\\s*"
            )
          )
        )
        
        
        pieces <- split_heading_body(
          chunk
        )
        
        
        tibble::tibble(
          
          book =
            book_number,
          
          chapter =
            chapter_number,
          
          chapter_token =
            chapter_token,
          
          source_heading =
            pieces$heading,
          
          source_text =
            clean_source_text(
              pieces$body
            )
        )
      }
    )
  }
  
  
  # ----------------------------------------------------------
  # Parse all twelve books
  # ----------------------------------------------------------
  
  chapter_tables <- vector(
    "list",
    12
  )
  
  
  for (i in seq_len(12)) {
    
    book_label <-
      book_matches[i, 2]
    
    
    book_number <-
      unname(
        book_numbers[
          book_label
        ]
      )
    
    
    book_start <-
      book_locations[i, "end"] +
      1L
    
    
    book_end <- if (
      i < 12
    ) {
      
      book_locations[
        i + 1,
        "start"
      ] - 1L
      
    } else {
      
      nchar(
        full_text
      )
    }
    
    
    book_text <- stringr::str_sub(
      full_text,
      book_start,
      book_end
    )
    
    
    chapter_tables[[i]] <- parse_book(
      book_number =
        book_number,
      book_text =
        book_text
    )
  }
  
  
  df_chapters <- dplyr::bind_rows(
    chapter_tables
  ) |>
    
    dplyr::arrange(
      book,
      chapter
    ) |>
    
    dplyr::mutate(
      
      chapter_id =
        sprintf(
          "ht_%02d_%03d",
          book,
          chapter
        ),
      
      source_passage =
        paste0(
          "Book ",
          book,
          ", Chapter ",
          chapter
        ),
      
      source_collection =
        paste(
          "Cassiodorus / Epiphanius Scholasticus,",
          "Historia ecclesiastica tripartita,",
          "Patrologia Latina 69, cols. 879D-1214C"
        ),
      
      source_language =
        "Latin",
      
      text_source_url =
        page_url,
      
      rights_status =
        "public_domain",
      
      digital_source_license =
        "CC BY-SA",
      
      digital_source =
        "Latin Wikisource; Corpus Corporum transcription",
      
      text_characters =
        nchar(
          source_text
        )
    )
  
  
  message(
    "Parsed ",
    nrow(df_chapters),
    " canonical chapters."
  )
  
  
  # ----------------------------------------------------------
  # If only chapters are requested, return now.
  # ----------------------------------------------------------
  
  if (output == "chapters") {
    
    return(
      df_chapters
    )
  }
  
  
  # ==========================================================
  # CURATED HAGIOGRAPHIC STORY MANIFEST
  #
  # whole_chapter = TRUE
  #
  #   Entire canonical chapter becomes one OEHC record.
  #
  # whole_chapter = FALSE
  #
  #   start_regex / end_regex delimit a narrative within
  #   a chapter.
  #
  # If end_regex is NA, the next curated story's start marker
  # within the SAME chapter is used as the endpoint.
  #
  # ==========================================================
  
  story_manifest <- tibble::tribble(
    
    ~story_id,
    ~book,
    ~chapter,
    ~subject,
    ~unit_type,
    ~genre,
    ~start_regex,
    ~end_regex,
    ~whole_chapter,
    
    
    # ========================================================
    # BOOK I
    # ========================================================
    
    "i10_confessors",
    1, 10,
    "Osius, Amphion, Maximus, and Paphnutius",
    "group",
    "confessor_notice",
    "Itaque persecutionibus jam cessantibus",
    "Secundum eos autem fuisse percepimus Spiridionem",
    FALSE,
    
    
    "i10_spiridion",
    1, 10,
    "Spiridion of Trimythous",
    "individual",
    "saint_episode",
    "Secundum eos autem fuisse percepimus Spiridionem",
    NA_character_,
    FALSE,
    
    
    # ========================================================
    # BOOK III
    # ========================================================
    
    "iii1_conversions",
    3, 1,
    "Conversion of the Indians, Iberians, Armenians, and Persians",
    "group",
    "conversion_narrative",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "iii2_persian_martyrs",
    3, 2,
    "Persian Martyrs under Shapur II",
    "group",
    "martyrdom_collection",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    # ========================================================
    # BOOK VI
    # ========================================================
    
    "vi31_babylas",
    6, 31,
    "Babylas of Antioch",
    "individual",
    "martyr_cult",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "vi45_julian_saba",
    6, 45,
    "Julian Saba",
    "individual",
    "saint_episode",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    # ========================================================
    # BOOK VIII, CHAPTER 1
    # PERSON-LEVEL ASCETIC SEGMENTATION
    # ========================================================
    
    "viii1_arsenius",
    8, 1,
    "Arsenius",
    "individual",
    "ascetic_notice",
    "Eo tempore fuit Arsenius monasterii praesul",
    NA_character_,
    FALSE,
    
    
    "viii1_pior_1",
    8, 1,
    "Pior: ascetic eating",
    "individual",
    "ascetic_notice",
    "Alius nomine Pior ambulans comedebat",
    NA_character_,
    FALSE,
    
    
    "viii1_isidorus_1",
    8, 1,
    "Isidorus",
    "individual",
    "ascetic_notice",
    "Isidorus dicebat quadragesimum",
    NA_character_,
    FALSE,
    
    
    "viii1_pambo",
    8, 1,
    "Pambo",
    "individual",
    "ascetic_notice",
    "Pambo autem, cum sine litteris esset",
    "Alter quidam dicebat",
    FALSE,
    
    
    "viii1_piterius",
    8, 1,
    "Piterius",
    "individual",
    "ascetic_notice",
    "Piterius multas expositiones",
    NA_character_,
    FALSE,
    
    
    "viii1_macarius_egyptian",
    8, 1,
    "Macarius the Egyptian",
    "individual",
    "ascetic_notice",
    "Illo siquidem tempore inter monachos fuerunt duo viri",
    "Porro Macarius Alexandrinus",
    FALSE,
    
    
    "viii1_macarius_alexandrian",
    8, 1,
    "Macarius of Alexandria",
    "individual",
    "ascetic_notice",
    "Porro Macarius Alexandrinus",
    NA_character_,
    FALSE,
    
    
    "viii1_evagrius_1",
    8, 1,
    "Evagrius Ponticus: first notice",
    "individual",
    "ascetic_notice",
    "Horum discipulus Evagrius",
    NA_character_,
    FALSE,
    
    
    "viii1_ammonius_1",
    8, 1,
    "Ammonius: refusal of episcopacy",
    "individual",
    "ascetic_notice",
    "Fuit autem et alius vir mirabilis inter monachos, cui nomen Ammonius",
    NA_character_,
    FALSE,
    
    
    "viii1_john",
    8, 1,
    "John of Egypt",
    "individual",
    "ascetic_notice",
    "Quorum unus quidem in Aegypto Joannes",
    NA_character_,
    FALSE,
    
    
    "viii1_ammon_theonas",
    8, 1,
    "Ammon and Theonas",
    "group",
    "ascetic_notice",
    "In hac itaque regione philosophabatur Ammon",
    NA_character_,
    FALSE,
    
    
    "viii1_copres",
    8, 1,
    "Copres",
    "individual",
    "ascetic_notice",
    "Copres quidem",
    NA_character_,
    FALSE,
    
    
    "viii1_hellin",
    8, 1,
    "Hellin",
    "individual",
    "ascetic_notice",
    "Hellin vero",
    NA_character_,
    FALSE,
    
    
    "viii1_helias",
    8, 1,
    "Helias",
    "individual",
    "ascetic_notice",
    "Verum Helias",
    NA_character_,
    FALSE,
    
    
    "viii1_apelles",
    8, 1,
    "Apelles",
    "individual",
    "ascetic_notice",
    "Fuit etiam Apelles",
    NA_character_,
    FALSE,
    
    
    "viii1_isidorus_2",
    8, 1,
    "Isidorus: enclosed monastery",
    "individual",
    "ascetic_notice",
    "Inter hos etiam Isidorus fuit",
    NA_character_,
    FALSE,
    
    
    "viii1_serapion",
    8, 1,
    "Serapion",
    "individual",
    "ascetic_notice",
    "Nec Serapion",
    NA_character_,
    FALSE,
    
    
    "viii1_dioscorus",
    8, 1,
    "Dioscorus",
    "individual",
    "ascetic_notice",
    "Dioscorus quoque",
    NA_character_,
    FALSE,
    
    
    "viii1_eulogius",
    8, 1,
    "Eulogius",
    "individual",
    "ascetic_notice",
    "Eulogius presbyter",
    NA_character_,
    FALSE,
    
    
    "viii1_apollo",
    8, 1,
    "Apollo of the Thebaid",
    "individual",
    "ascetic_notice",
    "His itaque similis erat Apollo",
    NA_character_,
    FALSE,
    
    
    "viii1_dorotheus",
    8, 1,
    "Dorotheus the Theban",
    "individual",
    "ascetic_notice",
    "Excellenter autem inter alios emicabat Dorotheus",
    NA_character_,
    FALSE,
    
    
    "viii1_piamon",
    8, 1,
    "Piamon",
    "individual",
    "ascetic_notice",
    "Piamonem vero presbyterum",
    NA_character_,
    FALSE,
    
    
    "viii1_john_healer",
    8, 1,
    "John the Healer",
    "individual",
    "ascetic_notice",
    "Joanni quoque tantam Deus",
    NA_character_,
    FALSE,
    
    
    "viii1_benjamin",
    8, 1,
    "Benjamin",
    "individual",
    "ascetic_notice",
    "Inter hos Benjamin senior",
    NA_character_,
    FALSE,
    
    
    "viii1_mark",
    8, 1,
    "Mark of Scetis",
    "individual",
    "ascetic_notice",
    "Marcum in Schyti",
    NA_character_,
    FALSE,
    
    
    "viii1_macarius_conversion",
    8, 1,
    "Macarius: conversion after accidental homicide",
    "individual",
    "ascetic_notice",
    "Macario vero datum est",
    NA_character_,
    FALSE,
    
    
    "viii1_apollonius",
    8, 1,
    "Apollonius",
    "individual",
    "ascetic_notice",
    "Apollonius autem",
    NA_character_,
    FALSE,
    
    
    "viii1_moses",
    8, 1,
    "Moses the Ethiopian",
    "individual",
    "ascetic_notice",
    "Moyses autem cum servus esset",
    NA_character_,
    FALSE,
    
    
    "viii1_paul",
    8, 1,
    "Paul of Libya",
    "individual",
    "ascetic_notice",
    "Sub hoc igitur imperio fuit Paulus",
    NA_character_,
    FALSE,
    
    
    "viii1_pachomius",
    8, 1,
    "Pachomius",
    "individual",
    "ascetic_notice",
    "Tunc enim Pachomius",
    NA_character_,
    FALSE,
    
    
    "viii1_stephanus",
    8, 1,
    "Stephanus of Mareotis",
    "individual",
    "ascetic_notice",
    "Stephanus autem circa Mareotem",
    NA_character_,
    FALSE,
    
    
    "viii1_pior_2",
    8, 1,
    "Pior: family and holy well",
    "individual",
    "ascetic_notice",
    "Pior autem cum de domo patris",
    NA_character_,
    FALSE,
    
    
    "viii1_ammonius_2",
    8, 1,
    "Ammonius: second episcopal refusal",
    "individual",
    "ascetic_notice",
    "Ammonium itaque ferunt nimis eruditum",
    NA_character_,
    FALSE,
    
    
    "viii1_evagrius_2",
    8, 1,
    "Evagrius Ponticus: vision and withdrawal",
    "individual",
    "ascetic_notice",
    "Eo siquidem tempore fuit Evagrius vir eloquentissimus",
    NA_character_,
    FALSE,
    
    
    "viii1_melas_salomon",
    8, 1,
    "Melas and Salomon",
    "group",
    "ascetic_notice",
    "Fuerunt etiam in Rinocorura",
    NA_character_,
    FALSE,
    
    
    "viii1_epiphanius",
    8, 1,
    "Epiphanius of Salamis",
    "individual",
    "ascetic_notice",
    "Hoc tempore Epiphanius",
    NA_character_,
    FALSE,
    
    
    "viii1_protogenes",
    8, 1,
    "Protogenes of Carrhae",
    "individual",
    "ascetic_notice",
    "Fuit etiam in Carris Protogenes",
    "Alios vero quosdam",
    FALSE,
    
    
    "viii1_heliodorus",
    8, 1,
    "Heliodorus",
    "individual",
    "ascetic_notice",
    "Heliodorum plurimas insomnes",
    NA_character_,
    FALSE,
    
    
    # ========================================================
    # OTHER BOOK VIII NARRATIVES
    # ========================================================
    
    "viii2_burned_clergy",
    8, 2,
    "Orthodox Clergy Burned at Sea",
    "group",
    "martyr_act",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "viii4_aphraates",
    8, 4,
    "Aphraates",
    "individual",
    "saint_episode",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "viii5_julian",
    8, 5,
    "Julian Saba at Antioch",
    "individual",
    "saint_episode",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "viii6_acepsemas",
    8, 6,
    "Acepsemas",
    "individual",
    "ascetic_notice",
    "Fuit igitur illo tempore.*?Acepsemas",
    "Zeugmatius quoque",
    FALSE,
    
    
    "viii6_zeugmatius",
    8, 6,
    "Zeugmatius",
    "individual",
    "ascetic_notice",
    "Zeugmatius quoque",
    "Hoc itaque tempore fuit Ephraem",
    FALSE,
    
    
    "viii6_ephraem",
    8, 6,
    "Ephraem the Syrian",
    "individual",
    "ascetic_notice",
    "Hoc itaque tempore fuit Ephraem",
    NA_character_,
    FALSE,
    
    
    "viii7_macarii",
    8, 7,
    "Macarius the Egyptian and Macarius of Alexandria",
    "group",
    "miracle_narrative",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "viii8_didymus",
    8, 8,
    "Didymus the Blind",
    "individual",
    "saint_episode",
    "Eo siquidem tempore virum alium fidelem",
    "Et quoniam aliqui ex nominis similitudine",
    FALSE,
    
    
    "viii8_gregory",
    8, 8,
    "Gregory Thaumaturgus",
    "individual",
    "saint_episode",
    "Et quoniam aliqui ex nominis similitudine",
    NA_character_,
    FALSE,
    
    
    "viii13_isaac",
    8, 13,
    "Isaac the Monk",
    "individual",
    "saint_episode",
    "Aiunt etenim Isaac monachum",
    "Betranion autem",
    FALSE,
    
    
    "viii13_betranion",
    8, 13,
    "Betranion of Tomi",
    "individual",
    "saint_episode",
    "Betranion autem",
    "Qua tamen gratia barbari",
    FALSE,
    
    
    # ========================================================
    # BOOK IX
    # ========================================================
    
    "ix46_donatus",
    9, 46,
    "Donatus of Euroea",
    "individual",
    "miracle_narrative",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "ix47_theotimus",
    9, 47,
    "Theotimus of Tomi",
    "individual",
    "miracle_narrative",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "ix48_epiphanius",
    9, 48,
    "Epiphanius of Salamis",
    "individual",
    "miracle_narrative",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "ix49_prophet_relics",
    9, 49,
    "Discovery of the Bodies of Habakkuk and Micah",
    "group",
    "relic_discovery",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    # ========================================================
    # BOOK X
    # ========================================================
    
    "x2_telemachus",
    10, 2,
    "Telemachus",
    "individual",
    "martyr_act",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "x3_chrysostom",
    10, 3,
    "John Chrysostom: Ordination and Early Acts",
    "individual",
    "saint_episode",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "x26_chrysostom_relics",
    10, 26,
    "Translation of the Relics of John Chrysostom",
    "individual",
    "translation_narrative",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "x32_sanne",
    10, 32,
    "Sanne the Persian Confessor",
    "individual",
    "confessor_act",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "x33_benjamin",
    10, 33,
    "Benjamin the Persian Deacon",
    "individual",
    "martyr_act",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    # ========================================================
    # BOOK XI
    # ========================================================
    
    "xi2_atticus",
    11, 2,
    "Atticus of Constantinople",
    "individual",
    "saint_episode",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    # ========================================================
    # BOOK XII
    # ========================================================
    
    "xii2_atticus",
    12, 2,
    "Atticus: Works of Charity",
    "individual",
    "saint_episode",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "xii11_proclus",
    12, 11,
    "Proclus of Constantinople",
    "individual",
    "saint_episode",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    "xii14_chrysostom_relics",
    12, 14,
    "Return of John Chrysostom's Body to Constantinople",
    "individual",
    "translation_narrative",
    "^.*?Corpus enim Joannis",
    "Paulus quoque Novatianorum episcopus",
    FALSE
  )
  
  
  # ----------------------------------------------------------
  # Preserve original manifest order
  # ----------------------------------------------------------
  
  story_manifest <- story_manifest |>
    
    dplyr::mutate(
      manifest_order =
        dplyr::row_number()
    )
  
  
  # ----------------------------------------------------------
  # Compute automatic next-story boundaries BEFORE filtering
  # units.
  #
  # This means:
  #
  # units = "viii1_moses"
  #
  # still ends Moses at the beginning of the next canonical
  # curated story, rather than running to the end of VIII.1.
  # ----------------------------------------------------------
  
  story_manifest <- story_manifest |>
    
    dplyr::arrange(
      book,
      chapter,
      manifest_order
    ) |>
    
    dplyr::group_by(
      book,
      chapter
    ) |>
    
    dplyr::mutate(
      
      next_start_regex =
        dplyr::lead(
          start_regex
        )
    ) |>
    
    dplyr::ungroup()
  
  
  # ----------------------------------------------------------
  # Optional story selection
  # ----------------------------------------------------------
  
  if (!is.null(units)) {
    
    unknown <- setdiff(
      units,
      story_manifest$story_id
    )
    
    
    if (length(unknown) > 0) {
      
      stop(
        "Unknown story_id: ",
        paste(
          unknown,
          collapse = ", "
        )
      )
    }
    
    
    story_manifest <- story_manifest |>
      
      dplyr::filter(
        story_id %in% units
      )
  }
  
  
  # ----------------------------------------------------------
  # Safe regex locator
  # ----------------------------------------------------------
  
  locate_after <- function(
    text,
    pattern,
    after = 1L
  ) {
    
    if (
      is.na(pattern) ||
      !nzchar(pattern)
    ) {
      
      return(
        list(
          start = NA_integer_,
          end = NA_integer_
        )
      )
    }
    
    
    remainder <- stringr::str_sub(
      text,
      after
    )
    
    
    location <- stringr::str_locate(
      
      remainder,
      
      stringr::regex(
        pattern,
        ignore_case = TRUE,
        dotall = TRUE
      )
    )
    
    
    if (
      is.na(
        location[1, "start"]
      )
    ) {
      
      return(
        list(
          start = NA_integer_,
          end = NA_integer_
        )
      )
    }
    
    
    list(
      
      start = as.integer(
        after +
          unname(
            location[1, "start"]
          ) -
          1L
      ),
      
      end = as.integer(
        after +
          unname(
            location[1, "end"]
          ) -
          1L
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Extract one story from its canonical chapter
  #
  # NOTE:
  #
  # book_value and chapter_value deliberately have names
  # different from dataframe columns.
  #
  # This prevents dplyr from interpreting:
  #
  #   book == book
  #
  # instead of comparing the column to the function argument.
  # ----------------------------------------------------------
  
  extract_story <- function(
    book_value,
    chapter_value,
    whole_chapter,
    start_regex,
    end_regex,
    next_start_regex,
    story_id
  ) {
    
    chapter_row <- df_chapters |>
      
      dplyr::filter(
        .data$book == .env$book_value,
        .data$chapter == .env$chapter_value
      )
    
    
    if (nrow(chapter_row) != 1) {
      
      msg <- paste0(
        "Could not uniquely identify source chapter for ",
        story_id,
        " (Book ",
        book_value,
        ", Chapter ",
        chapter_value,
        "). Found ",
        nrow(chapter_row),
        " matching rows."
      )
      
      
      if (strict) {
        
        stop(
          msg
        )
      }
      
      
      warning(
        msg
      )
      
      
      return(
        list(
          source_text = NA_character_,
          status = "missing_chapter"
        )
      )
    }
    
    
    text <-
      chapter_row$source_text[[1]]
    
    
    if (
      is.na(text) ||
      !nzchar(text)
    ) {
      
      return(
        list(
          source_text = NA_character_,
          status = "empty_chapter"
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Whole chapter = one narrative
    # --------------------------------------------------------
    
    if (isTRUE(whole_chapter)) {
      
      return(
        list(
          source_text = text,
          status = "ok"
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Locate story beginning
    # --------------------------------------------------------
    
    start_location <- locate_after(
      text = text,
      pattern = start_regex
    )
    
    
    if (
      is.na(
        start_location$start
      )
    ) {
      
      msg <- paste0(
        "Could not locate start marker for ",
        story_id,
        " (Book ",
        book_value,
        ", Chapter ",
        chapter_value,
        ")."
      )
      
      
      if (strict) {
        
        stop(
          msg
        )
      }
      
      
      warning(
        msg
      )
      
      
      return(
        list(
          source_text = NA_character_,
          status = "missing_start"
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Determine endpoint
    #
    # Explicit end_regex takes precedence.
    #
    # Otherwise use next curated story's start marker.
    # --------------------------------------------------------
    
    effective_end <- if (
      !is.na(end_regex) &&
      nzchar(end_regex)
    ) {
      
      end_regex
      
    } else {
      
      next_start_regex
    }
    
    
    # --------------------------------------------------------
    # If no endpoint exists, continue to end of chapter.
    # --------------------------------------------------------
    
    if (
      is.na(effective_end) ||
      !nzchar(effective_end)
    ) {
      
      text_end <-
        nchar(text)
      
      status <-
        "ok"
      
    } else {
      
      end_location <- locate_after(
        
        text = text,
        
        pattern =
          effective_end,
        
        after =
          start_location$end +
          1L
      )
      
      
      if (
        is.na(
          end_location$start
        )
      ) {
        
        msg <- paste0(
          "Could not locate end marker for ",
          story_id,
          " (Book ",
          book_value,
          ", Chapter ",
          chapter_value,
          ")."
        )
        
        
        if (strict) {
          
          stop(
            msg
          )
        }
        
        
        warning(
          msg
        )
        
        
        text_end <-
          nchar(text)
        
        status <-
          "missing_end"
        
      } else {
        
        text_end <-
          end_location$start -
          1L
        
        status <-
          "ok"
      }
    }
    
    
    # --------------------------------------------------------
    # Validate boundaries
    # --------------------------------------------------------
    
    if (
      text_end <
      start_location$start
    ) {
      
      msg <- paste0(
        "Invalid story boundaries for ",
        story_id,
        " (Book ",
        book_value,
        ", Chapter ",
        chapter_value,
        ")."
      )
      
      
      if (strict) {
        
        stop(
          msg
        )
      }
      
      
      warning(
        msg
      )
      
      
      return(
        list(
          source_text = NA_character_,
          status = "invalid_boundary"
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Extract story
    # --------------------------------------------------------
    
    passage <- stringr::str_sub(
      
      text,
      
      start =
        start_location$start,
      
      end =
        text_end
    ) |>
      
      stringr::str_trim()
    
    
    if (!nzchar(passage)) {
      
      return(
        list(
          source_text = NA_character_,
          status = "empty_story"
        )
      )
    }
    
    
    list(
      source_text = passage,
      status = status
    )
  }
  
  
  # ----------------------------------------------------------
  # Extract curated hagiographic stories
  #
  # Named list prevents argument-position ambiguity.
  # ----------------------------------------------------------
  
  extracted <- purrr::pmap(
    
    list(
      
      book_value =
        story_manifest$book,
      
      chapter_value =
        story_manifest$chapter,
      
      whole_chapter =
        story_manifest$whole_chapter,
      
      start_regex =
        story_manifest$start_regex,
      
      end_regex =
        story_manifest$end_regex,
      
      next_start_regex =
        story_manifest$next_start_regex,
      
      story_id =
        story_manifest$story_id
    ),
    
    extract_story
  )
  
  
  # ----------------------------------------------------------
  # Sequential OEHC text IDs
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
  
  
  id_prefix <-
    id_parts[1, 2]
  
  id_start <-
    as.integer(
      id_parts[1, 3]
    )
  
  id_width <-
    nchar(
      id_parts[1, 3]
    )
  
  
  text_ids <- paste0(
    
    id_prefix,
    
    stringr::str_pad(
      
      id_start +
        seq_len(
          nrow(story_manifest)
        ) -
        1L,
      
      width =
        id_width,
      
      pad =
        "0"
    )
  )
  
  
  # ----------------------------------------------------------
  # Join chapter rubrics onto story table
  # ----------------------------------------------------------
  
  story_headings <- story_manifest |>
    
    dplyr::select(
      book,
      chapter
    ) |>
    
    dplyr::left_join(
      
      df_chapters |>
        
        dplyr::select(
          book,
          chapter,
          source_heading
        ),
      
      by = c(
        "book",
        "chapter"
      )
    ) |>
    
    dplyr::pull(
      source_heading
    )
  
  
  # ----------------------------------------------------------
  # OEHC story dataframe
  # ----------------------------------------------------------
  
  df_stories <- story_manifest |>
    
    dplyr::mutate(
      
      schema_version =
        "1.0",
      
      text_id =
        text_ids,
      
      bhg =
        NA_character_,
      
      bhl =
        NA_character_,
      
      bho =
        NA_character_,
      
      title =
        paste0(
          "Historia Tripartita: ",
          subject
        ),
      
      saint_dates =
        NA_character_,
      
      original_date =
        "6th century",
      
      domain =
        NA_character_,
      
      region =
        NA_character_,
      
      author =
        "Cassiodorus / Epiphanius Scholasticus",
      
      source_authors =
        paste(
          "Socrates Scholasticus;",
          "Sozomen;",
          "Theodoret of Cyrrhus"
        ),
      
      compiler =
        "Cassiodorus",
      
      translator =
        "Epiphanius Scholasticus",
      
      source_language =
        "Latin",
      
      source_passage =
        paste0(
          "Book ",
          book,
          ", Chapter ",
          chapter
        ),
      
      source_heading =
        story_headings,
      
      source_collection =
        paste(
          "Historia ecclesiastica tripartita;",
          "Patrologia Latina 69, cols. 879D-1214C"
        ),
      
      text_source_url =
        page_url,
      
      rights_status =
        "public_domain",
      
      digital_source_license =
        "CC BY-SA",
      
      digital_source =
        "Latin Wikisource; Corpus Corporum transcription",
      
      collection =
        "historical_latin_reception",
      
      extraction_status =
        purrr::map_chr(
          extracted,
          "status"
        ),
      
      source_text =
        purrr::map_chr(
          extracted,
          "source_text"
        ),
      
      text_characters =
        nchar(
          source_text
        )
    ) |>
    
    dplyr::arrange(
      manifest_order
    ) |>
    
    dplyr::select(
      
      schema_version,
      text_id,
      
      bhg,
      bhl,
      bho,
      
      title,
      subject,
      unit_type,
      
      saint_dates,
      original_date,
      
      domain,
      region,
      
      author,
      source_authors,
      compiler,
      translator,
      source_language,
      
      book,
      chapter,
      source_passage,
      source_heading,
      
      source_collection,
      text_source_url,
      
      rights_status,
      digital_source_license,
      digital_source,
      
      genre,
      collection,
      
      extraction_status,
      text_characters,
      
      source_text
    )
  
  
  # ----------------------------------------------------------
  # QA
  # ----------------------------------------------------------
  
  failed <- df_stories |>
    
    dplyr::filter(
      extraction_status != "ok" |
        is.na(source_text) |
        !nzchar(source_text)
    )
  
  
  if (nrow(failed) > 0) {
    
    warning(
      paste0(
        nrow(failed),
        " Historia Tripartita story records require review."
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Return requested object
  # ----------------------------------------------------------
  
  if (output == "stories") {
    
    return(
      df_stories
    )
  }
  
  
  list(
    
    chapters =
      df_chapters,
    
    stories =
      df_stories
  )
}

## Scrape and save

# df_historia_tripartita <- 
#   scrape_historia_tripartita(
#     output = "stories", 
#     start_text_id = "oehc_000690"
#   )
# 
# arrow::write_parquet(df_historia_tripartita, "derived/df_historia_tripartita.parquet")
# 
# df_historia_tripartita <- arrow::read_parquet("derived/df_historia_tripartita.parquet")
