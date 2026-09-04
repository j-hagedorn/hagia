scrape_jean_de_mailly <- function(
    part = c("core", "supplement", "all"),
    start_text_id = "oehc_000001",
    contact = Sys.getenv("OEHC_CONTACT"),
    cache_dir = "cache/jean_de_mailly",
    pause = 1,
    force_refresh = FALSE
) {
  
  # ==========================================================
  # JEAN DE MAILLY
  # ABBREVIATIO IN GESTIS ET MIRACULIS SANCTORUM
  #
  # Source:
  # Latin Wikisource
  #
  # This version DOES NOT depend on Wikisource HTML heading
  # wrappers. It uses MediaWiki's TOC metadata to identify
  # source sections, then retrieves each section separately.
  #
  # Expected records:
  #
  #   core       = 177
  #   supplement = 36
  #   all        = 213
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
  # ==========================================================
  
  
  part <- match.arg(part)
  
  
  # ----------------------------------------------------------
  # Configuration
  # ----------------------------------------------------------
  
  api_url <-
    "https://la.wikisource.org/w/api.php"
  
  page_title <-
    "Abbreviatio in gestis et miraculis sanctorum"
  
  page_url <- paste0(
    "https://la.wikisource.org/wiki/",
    "Abbreviatio_in_gestis_et_miraculis_sanctorum"
  )
  
  
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
  # Common request builder
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
  # Convert a small HTML heading fragment to plain text
  #
  # tocdata$line may contain HTML markup.
  # ----------------------------------------------------------
  
  clean_heading <- function(x) {
    
    if (
      is.null(x) ||
      length(x) == 0 ||
      is.na(x) ||
      !nzchar(x)
    ) {
      
      return("")
    }
    
    
    doc <- xml2::read_html(
      paste0(
        "<div>",
        x,
        "</div>"
      )
    )
    
    
    rvest::html_text2(
      rvest::html_element(
        doc,
        "div"
      )
    ) |>
      stringr::str_squish()
  }
  
  
  # ----------------------------------------------------------
  # Get MediaWiki table-of-contents data
  #
  # This replaces all previous H2-wrapper detection.
  # ----------------------------------------------------------
  
  toc_cache <- file.path(
    cache_dir,
    "toc.rds"
  )
  
  
  if (
    !force_refresh &&
    file.exists(toc_cache)
  ) {
    
    toc_raw <- readRDS(
      toc_cache
    )
    
  } else {
    
    message(
      "Retrieving Wikisource section structure..."
    )
    
    
    response <- make_request() |>
      
      httr2::req_url_query(
        action = "parse",
        page = page_title,
        prop = "tocdata",
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
    
    
    toc_raw <-
      body$parse$tocdata$sections
    
    
    saveRDS(
      toc_raw,
      toc_cache
    )
    
    
    Sys.sleep(
      pause
    )
  }
  
  
  if (length(toc_raw) == 0) {
    
    stop(
      "MediaWiki returned no section metadata."
    )
  }
  
  
  # ----------------------------------------------------------
  # Convert TOC metadata to dataframe
  # ----------------------------------------------------------
  
  get_value <- function(
    x,
    name,
    default = NA_character_
  ) {
    
    value <- x[[name]]
    
    
    if (
      is.null(value) ||
      length(value) == 0
    ) {
      
      default
      
    } else {
      
      value
    }
  }
  
  
  toc <- purrr::map_dfr(
    seq_along(toc_raw),
    function(i) {
      
      section <- toc_raw[[i]]
      
      
      tibble::tibble(
        
        toc_order =
          i,
        
        h_level =
          as.integer(
            get_value(
              section,
              "hLevel",
              NA_integer_
            )
          ),
        
        section_index =
          as.character(
            get_value(
              section,
              "index",
              NA_character_
            )
          ),
        
        from_title =
          as.character(
            get_value(
              section,
              "fromTitle",
              page_title
            )
          ),
        
        source_heading =
          clean_heading(
            get_value(
              section,
              "line",
              ""
            )
          ),
        
        anchor =
          as.character(
            get_value(
              section,
              "anchor",
              NA_character_
            )
          ),
        
        link_anchor =
          as.character(
            get_value(
              section,
              "linkAnchor",
              get_value(
                section,
                "anchor",
                NA_character_
              )
            )
          )
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # Keep H2-level source divisions
  #
  # The numbered legends and subordinate headings on this
  # page are H2 divisions.
  # ----------------------------------------------------------
  
  toc <- toc |>
    
    dplyr::filter(
      h_level == 2
    ) |>
    
    dplyr::arrange(
      toc_order
    )
  
  
  if (nrow(toc) == 0) {
    
    stop(
      "No H2 sections were returned by MediaWiki tocdata."
    )
  }
  
  
  # ----------------------------------------------------------
  # Identify numbered entries
  #
  # The transcription sometimes uses U instead of V in its
  # Roman numerals:
  #
  #   XLIU
  #   XLU
  #   CXLIU
  #
  # Therefore U is deliberately included.
  # ----------------------------------------------------------
  
  roman_pattern <-
    "[IVXLCDMU]+"
  
  
  toc <- toc |>
    
    dplyr::mutate(
      
      numbered =
        stringr::str_detect(
          source_heading,
          paste0(
            "^",
            roman_pattern,
            "(?:\\s*[-–]\\s*|\\s*$)"
          )
        )
    )
  
  
  # ----------------------------------------------------------
  # Identify boundaries of the core Abbreviatio
  # ----------------------------------------------------------
  
  core_start_pos <- which(
    
    toc$numbered &
      
      stringr::str_detect(
        toc$source_heading,
        stringr::regex(
          "Sancti\\s+Andree\\s+apostoli",
          ignore_case = TRUE
        )
      )
  )[1]
  
  
  core_end_pos <- which(
    
    toc$numbered &
      
      stringr::str_detect(
        toc$source_heading,
        stringr::regex(
          "Saturnini\\s+martyris",
          ignore_case = TRUE
        )
      )
  )[1]
  
  
  if (
    is.na(core_start_pos) ||
    is.na(core_end_pos)
  ) {
    
    stop(
      paste(
        "Could not identify the Andrew-to-Saturninus",
        "boundaries of the main Abbreviatio."
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Core sections
  #
  # cumsum(numbered) assigns unnumbered subordinate sections
  # to the preceding numbered legend.
  #
  # Thus:
  #
  #   CXX - Assumption
  #   De miraculis Marie
  #
  # remain one OEHC record.
  # ----------------------------------------------------------
  
  core_components <- toc[
    core_start_pos:core_end_pos,
  ] |>
    
    dplyr::mutate(
      
      source_part =
        "core",
      
      section_order =
        cumsum(numbered)
    )
  
  
  # ----------------------------------------------------------
  # Supplement
  #
  # Begins with the first numbered H2 after Saturninus.
  # ----------------------------------------------------------
  
  after_core <- toc[
    seq.int(
      core_end_pos + 1,
      nrow(toc)
    ),
  ]
  
  
  supplement_start <- which(
    after_core$numbered
  )[1]
  
  
  if (is.na(supplement_start)) {
    
    stop(
      "Could not locate the Supplementum."
    )
  }
  
  
  supplement_components <-
    after_core[
      supplement_start:nrow(after_core),
    ] |>
    
    dplyr::mutate(
      
      source_part =
        "supplement",
      
      section_order =
        cumsum(numbered)
    )
  
  
  # ----------------------------------------------------------
  # Validate expected numbers of numbered source entries
  # ----------------------------------------------------------
  
  n_core <- sum(
    core_components$numbered
  )
  
  n_supplement <- sum(
    supplement_components$numbered
  )
  
  
  if (n_core != 177) {
    
    stop(
      paste0(
        "Expected 177 core entries but identified ",
        n_core,
        "."
      )
    )
  }
  
  
  if (n_supplement != 36) {
    
    stop(
      paste0(
        "Expected 36 supplement entries but identified ",
        n_supplement,
        "."
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Combine component map
  # ----------------------------------------------------------
  
  components <- dplyr::bind_rows(
    core_components,
    supplement_components
  )
  
  
  # ----------------------------------------------------------
  # Build one metadata row per numbered legend
  # ----------------------------------------------------------
  
  entries <- components |>
    
    dplyr::filter(
      numbered
    ) |>
    
    dplyr::mutate(
      
      source_entry_number =
        stringr::str_extract(
          source_heading,
          paste0(
            "^",
            roman_pattern
          )
        ),
      
      title =
        stringr::str_remove(
          source_heading,
          paste0(
            "^",
            roman_pattern,
            "\\s*[-–]?\\s*"
          )
        ) |>
        stringr::str_squish()
    ) |>
    
    dplyr::mutate(
      
      title =
        dplyr::if_else(
          !nzchar(title),
          
          paste(
            dplyr::if_else(
              source_part == "core",
              "Abbreviatio",
              "Supplementum"
            ),
            source_entry_number
          ),
          
          title
        )
    )
  
  
  # ----------------------------------------------------------
  # Select requested collection component
  # ----------------------------------------------------------
  
  selected_entries <- switch(
    
    part,
    
    core =
      entries |>
      dplyr::filter(
        source_part == "core"
      ),
    
    supplement =
      entries |>
      dplyr::filter(
        source_part == "supplement"
      ),
    
    all =
      entries
  )
  
  
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
          nrow(selected_entries)
        ) -
        1,
      
      width =
        id_width,
      
      pad =
        "0"
    )
  )
  
  
  # ----------------------------------------------------------
  # Section cache
  # ----------------------------------------------------------
  
  section_cache_dir <- file.path(
    cache_dir,
    "sections"
  )
  
  
  dir.create(
    section_cache_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Retrieve one MediaWiki section
  #
  # This is the key change from previous versions.
  #
  # We ask MediaWiki itself for the section rather than
  # attempting to infer boundaries from rendered HTML.
  # ----------------------------------------------------------
  
  get_section_text <- function(
    section_index,
    from_title,
    toc_order
  ) {
    
    if (
      is.na(section_index) ||
      !nzchar(section_index)
    ) {
      
      stop(
        "A source section has no MediaWiki section index."
      )
    }
    
    
    # Template-generated TOC indexes may have T- prefixes.
    #
    # When querying the source page itself, remove the T-
    # prefix and use fromTitle.
    parse_index <- stringr::str_remove(
      section_index,
      "^T-"
    )
    
    
    source_page <- if (
      is.na(from_title) ||
      !nzchar(from_title)
    ) {
      
      page_title
      
    } else {
      
      from_title
    }
    
    
    cached <- file.path(
      section_cache_dir,
      sprintf(
        "section_%03d.html",
        toc_order
      )
    )
    
    
    if (
      !force_refresh &&
      file.exists(cached)
    ) {
      
      html <- paste(
        readLines(
          cached,
          warn = FALSE,
          encoding = "UTF-8"
        ),
        collapse = "\n"
      )
      
    } else {
      
      message(
        "Fetching section ",
        toc_order,
        ": ",
        source_page,
        " [",
        parse_index,
        "]"
      )
      
      
      response <- make_request() |>
        
        httr2::req_url_query(
          action = "parse",
          page = source_page,
          section = parse_index,
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
          "Wikisource API error for section ",
          section_index,
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
    }
    
    
    # --------------------------------------------------------
    # Parse section HTML
    # --------------------------------------------------------
    
    doc <- xml2::read_html(
      html
    )
    
    
    # Remove apparatus.
    unwanted <- rvest::html_elements(
      doc,
      paste(
        ".mw-editsection",
        ".reference",
        ".references",
        ".mw-references-wrap",
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
    
    
    # Remove section headings themselves.
    #
    # We already have headings from tocdata and add subordinate
    # headings manually where appropriate.
    headings <- rvest::html_elements(
      doc,
      "h1, h2, h3, h4, h5, h6"
    )
    
    
    if (length(headings) > 0) {
      
      xml2::xml_remove(
        headings
      )
    }
    
    
    # Extract primary prose.
    paragraphs <- rvest::html_elements(
      doc,
      "p"
    ) |>
      
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
    
    
    if (length(paragraphs) == 0) {
      
      return(
        NA_character_
      )
    }
    
    
    paste(
      paragraphs,
      collapse = "\n\n"
    )
  }
  
  
  # ----------------------------------------------------------
  # Assemble one OEHC record from one or more MediaWiki
  # sections.
  #
  # Numbered heading:
  #
  #   CXX - De Assumptione...
  #
  # plus subordinate heading:
  #
  #   De miraculis Marie
  #
  # are combined into the same source_text.
  # ----------------------------------------------------------
  
  extract_entry <- function(
    source_part_value,
    section_order_value
  ) {
    
    entry_components <- components |>
      
      dplyr::filter(
        source_part ==
          source_part_value,
        
        section_order ==
          section_order_value
      )
    
    
    blocks <- purrr::pmap_chr(
      
      entry_components,
      
      function(
    toc_order,
    h_level,
    section_index,
    from_title,
    source_heading,
    anchor,
    link_anchor,
    numbered,
    source_part,
    section_order
      ) {
        
        text <- get_section_text(
          section_index =
            section_index,
          
          from_title =
            from_title,
          
          toc_order =
            toc_order
        )
        
        
        if (
          is.na(text) ||
          !nzchar(text)
        ) {
          
          return("")
        }
        
        
        # Preserve subordinate headings within the record.
        if (!numbered) {
          
          paste0(
            "## ",
            source_heading,
            "\n\n",
            text
          )
          
        } else {
          
          text
        }
      }
    )
    
    
    blocks <- blocks[
      nzchar(blocks)
    ]
    
    
    if (length(blocks) == 0) {
      
      NA_character_
      
    } else {
      
      paste(
        blocks,
        collapse = "\n\n"
      )
    }
  }
  
  
  # ----------------------------------------------------------
  # Extract selected entries
  # ----------------------------------------------------------
  
  source_texts <- purrr::map2_chr(
    
    selected_entries$source_part,
    
    selected_entries$section_order,
    
    extract_entry
  )
  
  
  # ----------------------------------------------------------
  # Construct direct source URLs
  # ----------------------------------------------------------
  
  text_source_urls <- purrr::map_chr(
    selected_entries$link_anchor,
    function(anchor) {
      
      if (
        is.na(anchor) ||
        !nzchar(anchor)
      ) {
        
        page_url
        
      } else {
        
        paste0(
          page_url,
          "#",
          anchor
        )
      }
    }
  )
  
  
  # ----------------------------------------------------------
  # Construct OEHC dataframe
  # ----------------------------------------------------------
  
  result <- selected_entries |>
    
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
      
      original_date =
        "post 1225",
      
      domain =
        NA_character_,
      
      region =
        NA_character_,
      
      author =
        "Jean de Mailly",
      
      source_language =
        "Latin",
      
      editor =
        "Giovanni Paolo Maggioni",
      
      edition_year =
        "2013",
      
      source_collection = paste(
        "Jean de Mailly,",
        "Abbreviatio in gestis et miraculis sanctorum.",
        "Supplementum hagiographicum,",
        "ed. Giovanni Paolo Maggioni",
        "(SISMEL / Edizioni del Galluzzo, 2013)"
      ),
      
      text_source_url =
        text_source_urls,
      
      rights_status =
        "review_required",
      
      genre =
        "legendary",
      
      collection =
        "medieval_compilations",
      
      source_text =
        source_texts
    ) |>
    
    dplyr::select(
      
      schema_version,
      text_id,
      
      bhg,
      bhl,
      bho,
      
      title,
      
      saint_dates,
      original_date,
      
      domain,
      region,
      
      author,
      source_language,
      
      source_part,
      section_order,
      source_entry_number,
      source_heading,
      
      editor,
      edition_year,
      
      source_collection,
      text_source_url,
      
      rights_status,
      genre,
      collection,
      
      source_text
    )
  
  
  # ----------------------------------------------------------
  # Final validation
  # ----------------------------------------------------------
  
  empty_records <- result |>
    
    dplyr::filter(
      is.na(source_text) |
        !nzchar(source_text)
    )
  
  
  if (nrow(empty_records) > 0) {
    
    warning(
      paste0(
        nrow(empty_records),
        " entries produced empty source text."
      )
    )
  }
  
  
  result
}

## Scrape and save

# df_jean_de_mailly <- scrape_jean_de_mailly(start_text_id = "oehc_000410")
# 
# arrow::write_parquet(df_jean_de_mailly, "derived/df_jean_de_mailly.parquet")
# 
# df_jean_de_mailly <- arrow::read_parquet("derived/df_jean_de_mailly.parquet")
