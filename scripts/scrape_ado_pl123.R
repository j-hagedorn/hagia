scrape_ado_pl123 <- function(
    works = NULL,
    start_text_id = "oehc_000001",
    contact = Sys.getenv("OEHC_CONTACT"),
    cache_dir = "cache/ado_pl123",
    pause = 1,
    force_refresh = FALSE,
    strip_pl_markers = TRUE
) {
  
  # ==========================================================
  # ADO / PL 123 NARRATIVE HAGIOGRAPHY
  #
  # Machine-readable source:
  # Latin Wikisource
  #
  # Textual source:
  # Patrologia Latina 123
  # via Corpus Corporum / Latin Wikisource
  #
  # Output:
  # One OEHC row per substantial narrative work.
  #
  # Full output:
  #
  #   1. Passio S. Desiderii
  #   2. Vita S. Theuderii
  #   3. De translatione S. Barnardi
  #   4. De Miraculis S. Barnardi
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
  
  
  # ----------------------------------------------------------
  # Configuration
  # ----------------------------------------------------------
  
  api_url <-
    "https://la.wikisource.org/w/api.php"
  
  
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
  # Source manifest
  #
  # start_regex identifies the beginning of the actual work,
  # excluding Wikisource bibliographic/template material.
  #
  # The dedicatory prefaces of Desiderius and Theuderius are
  # retained because they form part of the transmitted works.
  # ----------------------------------------------------------
  
  manifest <- tibble::tribble(
    
    ~work_id,
    ~title,
    ~subject,
    ~author,
    ~page,
    ~original_date,
    ~date_note,
    ~genre,
    ~pl_reference,
    
    
    "desiderius",
    
    "Passio S. Desiderii",
    
    "Desiderius of Vienne",
    
    "Ado of Vienne",
    
    "Passio S. Desiderii (Ado Viennensis)",
    
    "870",
    
    paste(
      "Ado's preface explicitly dates the work",
      "to the year 870."
    ),
    
    "passion",
    
    "PL 123, cols. 435B-442D",
    
    
    "theuderius",
    
    "Vita S. Theuderii",
    
    "Theuderius of Vienne",
    
    "Ado of Vienne",
    
    "Vita S. Theuderii (Ado Viennensis)",
    
    "9th century",
    
    NA_character_,
    
    "vita",
    
    "PL 123, cols. 443C-450C",
    
    
    "barnard_translation",
    
    "De translatione S. Barnardi",
    
    "Barnard of Vienne",
    
    "Anonymous",
    
    "De translatione S. Barnardi",
    
    "after 914",
    
    paste(
      "The narrative dates the elevation and translation",
      "of Barnard's body to 914; the Wikisource metadata",
      "label 'saeculo IX' is therefore not used here."
    ),
    
    "translation_narrative",
    
    "PL 123, cols. 449D-452A",
    
    
    "barnard_miracles",
    
    "De Miraculis S. Barnardi",
    
    "Barnard of Vienne",
    
    "Anonymous",
    
    "De Miraculis S. Barnardi",
    
    "after 914",
    
    paste(
      "The collection concerns the posthumous cult and",
      "relic miracles of Barnard; date should be reviewed",
      "if more precise manuscript evidence is added."
    ),
    
    "miracle_collection",
    
    "PL 123, cols. 451B-452C"
  )
  
  
  # ----------------------------------------------------------
  # Text-start markers
  #
  # Kept separately for readability.
  # ----------------------------------------------------------
  
  start_markers <- tibble::tribble(
    
    ~work_id,
    ~start_regex,
    
    
    "desiderius",
    "PRAEFATIUNCULA\\s+ADONIS",
    
    
    "theuderius",
    "ADO,\\s*Viennensis\\s+episcopus",
    
    
    "barnard_translation",
    "Regni\\s+coelorum\\s+civibus",
    
    
    "barnard_miracles",
    "Quamvis\\s+imperitia\\s+mea"
  )
  
  
  manifest <- manifest |>
    
    dplyr::left_join(
      start_markers,
      by = "work_id"
    )
  
  
  # ----------------------------------------------------------
  # Select requested works
  #
  # Examples:
  #
  # works = "desiderius"
  #
  # works = c(
  #   "desiderius",
  #   "theuderius"
  # )
  # ----------------------------------------------------------
  
  if (!is.null(works)) {
    
    unknown <- setdiff(
      works,
      manifest$work_id
    )
    
    
    if (length(unknown) > 0) {
      
      stop(
        "Unknown work_id: ",
        paste(
          unknown,
          collapse = ", "
        )
      )
    }
    
    
    manifest <- manifest |>
      
      dplyr::filter(
        work_id %in% works
      )
  }
  
  
  # ----------------------------------------------------------
  # Sequential OEHC IDs
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
          nrow(manifest)
        ) -
        1,
      
      width =
        id_width,
      
      pad =
        "0"
    )
  )
  
  
  # ----------------------------------------------------------
  # Browser-facing Wikisource URL
  # ----------------------------------------------------------
  
  page_url <- function(page) {
    
    page <- stringr::str_replace_all(
      page,
      " ",
      "_"
    )
    
    
    paste0(
      "https://la.wikisource.org/wiki/",
      utils::URLencode(
        page,
        reserved = FALSE
      )
    )
  }
  
  
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
      
      httr2::req_throttle(
        capacity = 1,
        fill_time_s = 1,
        realm = "oehc-latin-wikisource"
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
  # Retrieve one Wikisource page
  # ----------------------------------------------------------
  
  get_page_doc <- function(
    page,
    work_id
  ) {
    
    cached <- file.path(
      cache_dir,
      paste0(
        work_id,
        ".html"
      )
    )
    
    
    if (
      !force_refresh &&
      file.exists(cached)
    ) {
      
      return(
        xml2::read_html(
          cached
        )
      )
    }
    
    
    message(
      "Fetching ",
      page
    )
    
    
    response <- make_request() |>
      
      httr2::req_url_query(
        action = "parse",
        page = page,
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
        "Wikisource API error for ",
        page,
        ": ",
        body$error$info
      )
    }
    
    
    html <-
      body$parse$text
    
    
    writeLines(
      html,
      cached,
      useBytes = TRUE
    )
    
    
    Sys.sleep(
      pause
    )
    
    
    xml2::read_html(
      html
    )
  }
  
  
  # ----------------------------------------------------------
  # Clean one textual block
  # ----------------------------------------------------------
  
  clean_block <- function(x) {
    
    x <- stringr::str_replace_all(
      x,
      "\u00A0",
      " "
    )
    
    
    # Remove inline PL column milestones from source_text.
    #
    # Example:
    #
    #   123.0443C|
    #   123.0452B|
    #
    # The complete PL range is retained separately in the
    # pl_reference metadata field.
    #
    if (strip_pl_markers) {
      
      x <- stringr::str_replace_all(
        x,
        "\\b123\\.\\d{4}[A-D]\\|?\\s*",
        ""
      )
    }
    
    
    x |>
      
      stringr::str_replace_all(
        "[ \t]+",
        " "
      ) |>
      
      stringr::str_replace_all(
        "\\s*\\n\\s*",
        " "
      ) |>
      
      stringr::str_squish()
  }
  
  
  # ----------------------------------------------------------
  # Extract one complete work
  #
  # We use paragraphs and headings rather than scraping the
  # entire rendered page text. Bibliographic templates occur
  # before the first source-specific start marker and are
  # discarded.
  #
  # Internal headings are retained in Markdown form.
  # ----------------------------------------------------------
  
  extract_work <- function(
    page,
    work_id,
    start_regex
  ) {
    
    doc <- get_page_doc(
      page = page,
      work_id = work_id
    )
    
    
    # --------------------------------------------------------
    # Remove Wikimedia apparatus
    # --------------------------------------------------------
    
    unwanted <- rvest::html_elements(
      doc,
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
    
    
    # --------------------------------------------------------
    # Retrieve prose and headings in document order
    # --------------------------------------------------------
    
    nodes <- rvest::html_elements(
      doc,
      "p, h2, h3, h4, h5, h6"
    )
    
    
    if (length(nodes) == 0) {
      
      stop(
        "No textual blocks found for ",
        page,
        "."
      )
    }
    
    
    blocks <- tibble::tibble(
      
      node_type =
        xml2::xml_name(
          nodes
        ),
      
      text =
        rvest::html_text2(
          nodes
        )
    ) |>
      
      dplyr::mutate(
        
        text =
          purrr::map_chr(
            text,
            clean_block
          )
      ) |>
      
      dplyr::filter(
        !is.na(text),
        nzchar(text)
      )
    
    
    # --------------------------------------------------------
    # Find actual beginning of the work
    # --------------------------------------------------------
    
    start_index <- which(
      
      stringr::str_detect(
        blocks$text,
        
        stringr::regex(
          start_regex,
          ignore_case = TRUE
        )
      )
    )[1]
    
    
    if (is.na(start_index)) {
      
      stop(
        paste0(
          "Could not locate the start of ",
          work_id,
          ". Expected marker: ",
          start_regex
        )
      )
    }
    
    
    blocks <- blocks[
      seq.int(
        start_index,
        nrow(blocks)
      ),
    ]
    
    
    # --------------------------------------------------------
    # Render internal headings as Markdown headings
    # --------------------------------------------------------
    
    blocks <- blocks |>
      
      dplyr::mutate(
        
        text = dplyr::if_else(
          
          node_type %in%
            c(
              "h2",
              "h3",
              "h4",
              "h5",
              "h6"
            ),
          
          paste0(
            "## ",
            text
          ),
          
          text
        )
      )
    
    
    source_text <- paste(
      blocks$text,
      collapse = "\n\n"
    ) |>
      
      stringr::str_replace_all(
        "\n{3,}",
        "\n\n"
      ) |>
      
      stringr::str_trim()
    
    
    if (!nzchar(source_text)) {
      
      stop(
        "Empty source text extracted for ",
        work_id,
        "."
      )
    }
    
    
    list(
      
      source_text =
        source_text,
      
      block_count =
        nrow(blocks),
      
      text_characters =
        nchar(source_text),
      
      extraction_status =
        "ok"
    )
  }
  
  
  # ----------------------------------------------------------
  # Extract all requested works
  # ----------------------------------------------------------
  
  extracted <- purrr::pmap(
    
    list(
      manifest$page,
      manifest$work_id,
      manifest$start_regex
    ),
    
    function(
    page,
    work_id,
    start_regex
    ) {
      
      extract_work(
        page = page,
        work_id = work_id,
        start_regex = start_regex
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # Construct OEHC dataframe
  # ----------------------------------------------------------
  
  result <- manifest |>
    
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
      
      saint_dates =
        NA_character_,
      
      domain =
        NA_character_,
      
      region =
        NA_character_,
      
      source_language =
        "Latin",
      
      editor =
        "J. P. Migne",
      
      source_collection =
        "Patrologia Latina 123",
      
      digital_source =
        "Latin Wikisource; Corpus Corporum transcription",
      
      text_source_url =
        purrr::map_chr(
          page,
          page_url
        ),
      
      rights_status =
        "public_domain",
      
      digital_source_license =
        "CC BY-SA (Wikisource)",
      
      collection =
        "medieval_latin_hagiography",
      
      extraction_status =
        purrr::map_chr(
          extracted,
          "extraction_status"
        ),
      
      source_text =
        purrr::map_chr(
          extracted,
          "source_text"
        ),
      
      source_block_count =
        purrr::map_int(
          extracted,
          "block_count"
        ),
      
      text_characters =
        purrr::map_int(
          extracted,
          "text_characters"
        )
    ) |>
    
    dplyr::select(
      
      schema_version,
      text_id,
      
      bhg,
      bhl,
      bho,
      
      title,
      subject,
      
      saint_dates,
      original_date,
      date_note,
      
      domain,
      region,
      
      author,
      source_language,
      
      editor,
      pl_reference,
      
      source_collection,
      digital_source,
      text_source_url,
      
      rights_status,
      digital_source_license,
      
      genre,
      collection,
      
      extraction_status,
      source_block_count,
      text_characters,
      
      source_text
    )
  
  
  # ----------------------------------------------------------
  # Sanity checks
  #
  # These are substantial works. Extremely short records
  # indicate an extraction problem rather than a genuinely
  # tiny source.
  # ----------------------------------------------------------
  
  short_records <- result |>
    
    dplyr::filter(
      is.na(source_text) |
        text_characters < 500
    )
  
  
  if (nrow(short_records) > 0) {
    
    warning(
      paste0(
        nrow(short_records),
        " PL 123 narrative records contain fewer than ",
        "500 characters and should be reviewed."
      )
    )
  }
  
  
  result
}


## Scrape and save

# df_ado_pl123 <- scrape_ado_pl123(start_text_id = "oehc_000686")
# 
# arrow::write_parquet(df_ado_pl123, "derived/df_ado_pl123.parquet")
# 
# df_ado_pl123 <- arrow::read_parquet("derived/df_ado_pl123.parquet")
