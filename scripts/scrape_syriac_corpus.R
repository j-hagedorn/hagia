scrape_syriac_corpus <- function(
    ids = NULL,
    selection = c("hagiography", "all"),
    start_text_id = "oehc_000001",
    cache_dir = "cache/syriac_corpus",
    force_refresh = FALSE,
    extra_include_regex = NULL,
    strict = TRUE
) {
  
  # ==========================================================
  # DIGITAL SYRIAC CORPUS
  #
  # Source:
  # Oxford-BYU Digital Syriac Corpus
  #
  # Repository:
  # https://github.com/srophe/syriac-corpus
  #
  # Data:
  # data/tei/<numeric_id>.xml
  #
  # Strategy:
  #
  #   GitHub repository ZIP
  #        |
  #        +--> local data/tei/*.xml
  #        |
  #        +--> read TEI headers
  #        |
  #        +--> identify hagiographic works
  #        |
  #        +--> extract Syriac body
  #        |
  #        +--> one OEHC row per TEI work
  #
  #
  # selection = "hagiography"
  #
  #   Conservative automatic selection based on:
  #
  #     - martyr / martyrdom titles
  #     - lives
  #     - miracle narratives
  #     - Persian Martyr Acts
  #     - Apocryphal Acts of the Apostles
  #     - named apostolic acts
  #     - histories of named saints / "Mar ..."
  #     - Doctrine of Addai
  #     - printed source collections such as
  #       Acta martyrum et sanctorum
  #
  #
  # selection = "all"
  #
  #   Return every Digital Syriac Corpus TEI work.
  #
  #
  # ids =
  #
  #   Optional numeric Digital Syriac Corpus IDs.
  #   When supplied, these override automatic selection.
  #
  #   Example:
  #
  #   ids = c(335, 387, 389)
  #
  #
  # Required packages:
  #
  # install.packages(c(
  #   "httr2",
  #   "xml2",
  #   "stringr",
  #   "dplyr",
  #   "purrr",
  #   "tibble"
  # ))
  #
  # ==========================================================
  
  
  selection <- match.arg(
    selection
  )
  
  
  # ----------------------------------------------------------
  # Source configuration
  # ----------------------------------------------------------
  
  archive_url <- paste0(
    "https://github.com/srophe/syriac-corpus/",
    "archive/refs/heads/main.zip"
  )
  
  
  raw_base_url <- paste0(
    "https://raw.githubusercontent.com/",
    "srophe/syriac-corpus/main/data/tei/"
  )
  
  
  github_base_url <- paste0(
    "https://github.com/srophe/syriac-corpus/",
    "blob/main/data/tei/"
  )
  
  
  dir.create(
    cache_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  archive_file <- file.path(
    cache_dir,
    "syriac-corpus-main.zip"
  )
  
  
  repo_dir <- file.path(
    cache_dir,
    "repository"
  )
  
  
  # ----------------------------------------------------------
  # Generic text helper
  # ----------------------------------------------------------
  
  clean_text <- function(x) {
    
    if (
      length(x) == 0 ||
      is.null(x) ||
      is.na(x) ||
      !nzchar(x)
    ) {
      
      return(
        NA_character_
      )
    }
    
    
    x |>
      
      stringr::str_replace_all(
        "\u00A0",
        " "
      ) |>
      
      stringr::str_squish() |>
      
      stringr::str_trim()
  }
  
  
  collapse_unique <- function(
    x,
    default = NA_character_
  ) {
    
    x <- as.character(x)
    
    
    x <- x[
      !is.na(x) &
        nzchar(
          stringr::str_trim(x)
        )
    ]
    
    
    x <- unique(
      stringr::str_squish(x)
    )
    
    
    if (length(x) == 0) {
      
      default
      
    } else {
      
      paste(
        x,
        collapse = "; "
      )
    }
  }
  
  
  # ----------------------------------------------------------
  # Extract text from a TEI node while removing embedded
  # <foreign> elements.
  #
  # This gives:
  #
  #   "Martyrdom of Jacob and His Sister Mary..."
  #
  # rather than mixing the English title with its Syriac
  # title in the same metadata field.
  # ----------------------------------------------------------
  
  node_text_without_foreign <- function(node) {
    
    if (
      inherits(
        node,
        "xml_missing"
      )
    ) {
      
      return(
        NA_character_
      )
    }
    
    
    fragment <- tryCatch(
      
      xml2::read_xml(
        as.character(node)
      ),
      
      error = function(e) {
        NULL
      }
    )
    
    
    if (is.null(fragment)) {
      
      return(
        clean_text(
          xml2::xml_text(node)
        )
      )
    }
    
    
    foreign_nodes <- xml2::xml_find_all(
      fragment,
      ".//*[local-name()='foreign']"
    )
    
    
    if (length(foreign_nodes) > 0) {
      
      xml2::xml_remove(
        foreign_nodes
      )
    }
    
    
    clean_text(
      xml2::xml_text(
        fragment
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Download repository archive
  #
  # One download replaces hundreds of individual web calls.
  # ----------------------------------------------------------
  
  if (
    force_refresh ||
    !file.exists(archive_file)
  ) {
    
    message(
      "Downloading Digital Syriac Corpus repository..."
    )
    
    
    response <- httr2::request(
      archive_url
    ) |>
      
      httr2::req_user_agent(
        "OpenEnglishHagiographyCorpus/0.1"
      ) |>
      
      httr2::req_retry(
        max_tries = 3,
        is_transient = function(resp) {
          
          httr2::resp_status(resp) %in%
            c(
              429,
              502,
              503,
              504
            )
        }
      ) |>
      
      httr2::req_timeout(
        120
      ) |>
      
      httr2::req_perform()
    
    
    writeBin(
      httr2::resp_body_raw(
        response
      ),
      archive_file
    )
  }
  
  
  # ----------------------------------------------------------
  # Extract repository
  # ----------------------------------------------------------
  
  if (
    force_refresh &&
    dir.exists(repo_dir)
  ) {
    
    unlink(
      repo_dir,
      recursive = TRUE,
      force = TRUE
    )
  }
  
  
  if (!dir.exists(repo_dir)) {
    
    dir.create(
      repo_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    
    message(
      "Extracting Digital Syriac Corpus repository..."
    )
    
    
    utils::unzip(
      archive_file,
      exdir = repo_dir
    )
  }
  
  
  # ----------------------------------------------------------
  # Locate extracted data/tei directory
  #
  # ZIP normally creates:
  #
  #   repository/
  #     syriac-corpus-main/
  #       data/
  #         tei/
  # ----------------------------------------------------------
  
  repo_roots <- list.dirs(
    repo_dir,
    recursive = FALSE,
    full.names = TRUE
  )
  
  
  tei_candidates <- file.path(
    repo_roots,
    "data",
    "tei"
  )
  
  
  tei_candidates <- tei_candidates[
    dir.exists(
      tei_candidates
    )
  ]
  
  
  if (length(tei_candidates) == 0) {
    
    stop(
      "Could not locate data/tei in the extracted repository."
    )
  }
  
  
  tei_dir <- tei_candidates[[1]]
  
  
  # ----------------------------------------------------------
  # Discover numeric TEI files
  # ----------------------------------------------------------
  
  tei_files <- list.files(
    tei_dir,
    pattern = "^[0-9]+\\.xml$",
    full.names = TRUE
  )
  
  
  if (length(tei_files) == 0) {
    
    stop(
      "No Digital Syriac Corpus TEI files were found."
    )
  }
  
  
  # ----------------------------------------------------------
  # Optional numeric ID restriction
  #
  # Applied before parsing for efficient testing.
  # ----------------------------------------------------------
  
  if (!is.null(ids)) {
    
    ids <- as.integer(ids)
    
    
    file_ids <- as.integer(
      stringr::str_remove(
        basename(tei_files),
        "\\.xml$"
      )
    )
    
    
    missing_ids <- setdiff(
      ids,
      file_ids
    )
    
    
    if (length(missing_ids) > 0) {
      
      stop(
        "Unknown Digital Syriac Corpus ID(s): ",
        paste(
          missing_ids,
          collapse = ", "
        )
      )
    }
    
    
    tei_files <- tei_files[
      file_ids %in% ids
    ]
  }
  
  
  # ----------------------------------------------------------
  # Extract source text
  #
  # Common structure:
  #
  # <text>
  #   <body>
  #     <div type="rubric">...</div>
  #     <div type="body">
  #       <p>...</p>
  #     </div>
  #   </body>
  # </text>
  #
  # Notes are removed before extraction.
  #
  # Paragraph / verse boundaries are retained using blank
  # lines.
  # ----------------------------------------------------------
  
  extract_body <- function(doc) {
    
    body <- xml2::xml_find_first(
      doc,
      "//*[local-name()='text']/*[local-name()='body']"
    )
    
    
    if (
      inherits(
        body,
        "xml_missing"
      )
    ) {
      
      return(
        list(
          source_heading = NA_character_,
          source_text = NA_character_
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Work with a copy so removing notes does not alter
    # metadata extraction from the original document.
    # --------------------------------------------------------
    
    body_copy <- tryCatch(
      
      xml2::read_xml(
        as.character(body)
      ),
      
      error = function(e) {
        NULL
      }
    )
    
    
    if (is.null(body_copy)) {
      
      return(
        list(
          source_heading = NA_character_,
          source_text = clean_text(
            xml2::xml_text(body)
          )
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Rubric / original heading
    # --------------------------------------------------------
    
    rubric_node <- xml2::xml_find_first(
      
      body_copy,
      
      ".//*[local-name()='div'][@type='rubric']"
    )
    
    
    source_heading <- if (
      inherits(
        rubric_node,
        "xml_missing"
      )
    ) {
      
      NA_character_
      
    } else {
      
      clean_text(
        xml2::xml_text(
          rubric_node
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Prefer explicit div type="body"
    # --------------------------------------------------------
    
    body_div <- xml2::xml_find_first(
      
      body_copy,
      
      ".//*[local-name()='div'][@type='body']"
    )
    
    
    if (
      inherits(
        body_div,
        "xml_missing"
      )
    ) {
      
      content_node <- body_copy
      
      
      rubric_nodes <- xml2::xml_find_all(
        
        content_node,
        
        ".//*[local-name()='div'][@type='rubric']"
      )
      
      
      if (length(rubric_nodes) > 0) {
        
        xml2::xml_remove(
          rubric_nodes
        )
      }
      
    } else {
      
      content_node <- body_div
    }
    
    
    # --------------------------------------------------------
    # Remove editorial / textual notes
    # --------------------------------------------------------
    
    notes <- xml2::xml_find_all(
      
      content_node,
      
      ".//*[local-name()='note']"
    )
    
    
    if (length(notes) > 0) {
      
      xml2::xml_remove(
        notes
      )
    }
    
    
    # --------------------------------------------------------
    # Extract prose and verse blocks in document order
    #
    # <p> = prose
    # <l> = verse line
    # <ab> = anonymous block
    #
    # Avoid duplicates where <ab> contains <p>, or <l>
    # occurs inside a paragraph.
    # --------------------------------------------------------
    
    blocks <- xml2::xml_find_all(
      
      content_node,
      
      paste0(
        ".//*[local-name()='p']",
        " | ",
        ".//*[local-name()='l'",
        " and not(ancestor::*[local-name()='p'])]",
        " | ",
        ".//*[local-name()='ab'",
        " and not(descendant::*[local-name()='p'])",
        " and not(descendant::*[local-name()='l'])]"
      )
    )
    
    
    if (length(blocks) > 0) {
      
      block_text <- purrr::map_chr(
        
        blocks,
        
        function(node) {
          
          clean_text(
            xml2::xml_text(node)
          )
        }
      )
      
      
      block_text <- block_text[
        !is.na(block_text) &
          nzchar(block_text)
      ]
      
      
      source_text <- if (
        length(block_text) == 0
      ) {
        
        NA_character_
        
      } else {
        
        paste(
          block_text,
          collapse = "\n\n"
        )
      }
      
    } else {
      
      source_text <- clean_text(
        xml2::xml_text(
          content_node
        )
      )
    }
    
    
    list(
      source_heading = source_heading,
      source_text = source_text
    )
  }
  
  
  # ----------------------------------------------------------
  # Parse one TEI file
  # ----------------------------------------------------------
  
  parse_tei <- function(path) {
    
    corpus_id <- as.integer(
      stringr::str_remove(
        basename(path),
        "\\.xml$"
      )
    )
    
    
    doc <- tryCatch(
      
      xml2::read_xml(
        path
      ),
      
      error = function(e) {
        
        if (strict) {
          
          stop(
            paste0(
              "Could not parse Digital Syriac Corpus ",
              corpus_id,
              ": ",
              conditionMessage(e)
            )
          )
        }
        
        
        warning(
          "Could not parse Digital Syriac Corpus ",
          corpus_id
        )
        
        
        return(NULL)
      }
    )
    
    
    if (is.null(doc)) {
      
      return(
        tibble::tibble(
          corpus_id = corpus_id,
          extraction_status = "tei_parse_error"
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Main work title
    # --------------------------------------------------------
    
    title_node <- xml2::xml_find_first(
      
      doc,
      
      paste0(
        "//*[local-name()='titleStmt']",
        "/*[local-name()='title'][@level='a']"
      )
    )
    
    
    title <- node_text_without_foreign(
      title_node
    )
    
    
    work_uri <- if (
      inherits(
        title_node,
        "xml_missing"
      )
    ) {
      
      NA_character_
      
    } else {
      
      xml2::xml_attr(
        title_node,
        "ref"
      )
    }
    
    
    # --------------------------------------------------------
    # Series titles
    # --------------------------------------------------------
    
    series_nodes <- xml2::xml_find_all(
      
      doc,
      
      paste0(
        "//*[local-name()='titleStmt']",
        "/*[local-name()='title'][@level='s']"
      )
    )
    
    
    series_titles <- purrr::map_chr(
      
      series_nodes,
      
      node_text_without_foreign
    )
    
    
    series_refs <- xml2::xml_attr(
      series_nodes,
      "ref"
    )
    
    
    keep_series <-
      !is.na(series_titles) &
      nzchar(series_titles) &
      stringr::str_to_lower(
        series_titles
      ) !=
      "digital syriac corpus"
    
    
    series_title <- collapse_unique(
      series_titles[
        keep_series
      ]
    )
    
    
    series_uri <- collapse_unique(
      series_refs[
        keep_series
      ]
    )
    
    
    # --------------------------------------------------------
    # Author
    # --------------------------------------------------------
    
    author_nodes <- xml2::xml_find_all(
      
      doc,
      
      paste0(
        "//*[local-name()='titleStmt']",
        "/*[local-name()='author']"
      )
    )
    
    
    authors <- purrr::map_chr(
      
      author_nodes,
      
      function(node) {
        
        clean_text(
          xml2::xml_text(node)
        )
      }
    )
    
    
    author <- collapse_unique(
      authors,
      "Anonymous"
    )
    
    
    # --------------------------------------------------------
    # Digital editors
    # --------------------------------------------------------
    
    digital_editor_nodes <- xml2::xml_find_all(
      
      doc,
      
      paste0(
        "//*[local-name()='titleStmt']",
        "/*[local-name()='editor']"
      )
    )
    
    
    digital_editors <- collapse_unique(
      
      purrr::map_chr(
        
        digital_editor_nodes,
        
        function(node) {
          
          clean_text(
            xml2::xml_text(node)
          )
        }
      )
    )
    
    
    # --------------------------------------------------------
    # Composition date
    # --------------------------------------------------------
    
    date_node <- xml2::xml_find_first(
      
      doc,
      
      paste0(
        "//*[local-name()='profileDesc']",
        "//*[local-name()='origDate'][@type='composition']"
      )
    )
    
    
    if (
      inherits(
        date_node,
        "xml_missing"
      )
    ) {
      
      date_node <- xml2::xml_find_first(
        doc,
        "//*[local-name()='profileDesc']//*[local-name()='origDate']"
      )
    }
    
    
    original_date <- if (
      inherits(
        date_node,
        "xml_missing"
      )
    ) {
      
      NA_character_
      
    } else {
      
      clean_text(
        xml2::xml_text(
          date_node
        )
      )
    }
    
    
    composition_from <- if (
      inherits(
        date_node,
        "xml_missing"
      )
    ) {
      
      NA_character_
      
    } else {
      
      xml2::xml_attr(
        date_node,
        "from"
      )
    }
    
    
    composition_to <- if (
      inherits(
        date_node,
        "xml_missing"
      )
    ) {
      
      NA_character_
      
    } else {
      
      xml2::xml_attr(
        date_node,
        "to"
      )
    }
    
    
    # --------------------------------------------------------
    # Language
    # --------------------------------------------------------
    
    language_nodes <- xml2::xml_find_all(
      
      doc,
      
      paste0(
        "//*[local-name()='langUsage']",
        "/*[local-name()='language']"
      )
    )
    
    
    language_codes <- xml2::xml_attr(
      language_nodes,
      "ident"
    )
    
    
    language_labels <- purrr::map_chr(
      
      language_nodes,
      
      function(node) {
        
        clean_text(
          xml2::xml_text(node)
        )
      }
    )
    
    
    language_code <- collapse_unique(
      language_codes,
      "syr"
    )
    
    
    language_description <- collapse_unique(
      language_labels
    )
    
    
    # --------------------------------------------------------
    # Digital license
    # --------------------------------------------------------
    
    license_node <- xml2::xml_find_first(
      
      doc,
      
      paste0(
        "//*[local-name()='publicationStmt']",
        "//*[local-name()='licence']"
      )
    )
    
    
    license_url <- if (
      inherits(
        license_node,
        "xml_missing"
      )
    ) {
      
      NA_character_
      
    } else {
      
      xml2::xml_attr(
        license_node,
        "target"
      )
    }
    
    
    license_text <- if (
      inherits(
        license_node,
        "xml_missing"
      )
    ) {
      
      NA_character_
      
    } else {
      
      clean_text(
        xml2::xml_text(
          license_node
        )
      )
    }
    
    
    digital_source_license <- if (
      !is.na(license_url) &&
      stringr::str_detect(
        license_url,
        "creativecommons\\.org/licenses/by/4\\.0"
      )
    ) {
      
      "CC BY 4.0"
      
    } else if (
      !is.na(license_text) &&
      stringr::str_detect(
        stringr::str_to_lower(
          license_text
        ),
        "cc.?by.?4\\.0|attribution 4\\.0"
      )
    ) {
      
      "CC BY 4.0"
      
    } else {
      
      license_text
    }
    
    
    # --------------------------------------------------------
    # Public corpus record URL
    # --------------------------------------------------------
    
    publication_ids <- xml2::xml_find_all(
      
      doc,
      
      paste0(
        "//*[local-name()='publicationStmt']",
        "/*[local-name()='idno'][@type='URI']"
      )
    )
    
    
    publication_urls <- xml2::xml_text(
      publication_ids
    )
    
    
    corpus_url <- publication_urls[
      stringr::str_detect(
        publication_urls,
        "syriaccorpus\\.org"
      )
    ]
    
    
    corpus_url <- if (
      length(corpus_url) == 0
    ) {
      
      paste0(
        "https://syriaccorpus.org/",
        corpus_id
      )
      
    } else {
      
      corpus_url[[1]]
    }
    
    
    # --------------------------------------------------------
    # Printed-source bibliography
    # --------------------------------------------------------
    
    source_title_node <- xml2::xml_find_first(
      
      doc,
      
      paste0(
        "//*[local-name()='sourceDesc']",
        "//*[local-name()='monogr']",
        "/*[local-name()='title'][@level='m']"
      )
    )
    
    
    source_title <- if (
      inherits(
        source_title_node,
        "xml_missing"
      )
    ) {
      
      NA_character_
      
    } else {
      
      clean_text(
        xml2::xml_text(
          source_title_node
        )
      )
    }
    
    
    source_author_nodes <- xml2::xml_find_all(
      
      doc,
      
      paste0(
        "//*[local-name()='sourceDesc']",
        "//*[local-name()='monogr']",
        "/*[local-name()='author']"
      )
    )
    
    
    source_editor_nodes <- xml2::xml_find_all(
      
      doc,
      
      paste0(
        "//*[local-name()='sourceDesc']",
        "//*[local-name()='monogr']",
        "/*[local-name()='editor']"
      )
    )
    
    
    source_author <- collapse_unique(
      
      purrr::map_chr(
        
        source_author_nodes,
        
        function(node) {
          
          clean_text(
            xml2::xml_text(node)
          )
        }
      )
    )
    
    
    source_editor <- collapse_unique(
      
      purrr::map_chr(
        
        source_editor_nodes,
        
        function(node) {
          
          clean_text(
            xml2::xml_text(node)
          )
        }
      )
    )
    
    
    source_date_node <- xml2::xml_find_first(
      
      doc,
      
      paste0(
        "//*[local-name()='sourceDesc']",
        "//*[local-name()='imprint']",
        "/*[local-name()='date']"
      )
    )
    
    
    source_date <- if (
      inherits(
        source_date_node,
        "xml_missing"
      )
    ) {
      
      NA_character_
      
    } else {
      
      clean_text(
        xml2::xml_text(
          source_date_node
        )
      )
    }
    
    
    source_bibl_ids <- xml2::xml_find_all(
      
      doc,
      
      paste0(
        "//*[local-name()='sourceDesc']",
        "//*[local-name()='idno'][@type='URI']"
      )
    )
    
    
    source_bibl_urls <- xml2::xml_text(
      source_bibl_ids
    )
    
    
    source_bibl_uri <- source_bibl_urls[
      stringr::str_detect(
        source_bibl_urls,
        "syriaca\\.org/bibl"
      )
    ]
    
    
    source_bibl_uri <- if (
      length(source_bibl_uri) == 0
    ) {
      
      NA_character_
      
    } else {
      
      source_bibl_uri[[1]]
    }
    
    
    # --------------------------------------------------------
    # Digital edition status
    # --------------------------------------------------------
    
    revision_node <- xml2::xml_find_first(
      doc,
      "//*[local-name()='revisionDesc']"
    )
    
    
    edition_status <- if (
      inherits(
        revision_node,
        "xml_missing"
      )
    ) {
      
      NA_character_
      
    } else {
      
      xml2::xml_attr(
        revision_node,
        "status"
      )
    }
    
    
    # --------------------------------------------------------
    # Extract source text
    # --------------------------------------------------------
    
    body <- extract_body(
      doc
    )
    
    
    source_text <- body$source_text
    
    
    # --------------------------------------------------------
    # URLs
    # --------------------------------------------------------
    
    tei_source_url <- paste0(
      raw_base_url,
      corpus_id,
      ".xml"
    )
    
    
    github_source_url <- paste0(
      github_base_url,
      corpus_id,
      ".xml"
    )
    
    
    # --------------------------------------------------------
    # Return one TEI record
    # --------------------------------------------------------
    
    tibble::tibble(
      
      corpus_id =
        corpus_id,
      
      title =
        title,
      
      work_uri =
        work_uri,
      
      series_title =
        series_title,
      
      series_uri =
        series_uri,
      
      author =
        author,
      
      digital_editors =
        digital_editors,
      
      original_date =
        original_date,
      
      composition_from =
        composition_from,
      
      composition_to =
        composition_to,
      
      language_code =
        language_code,
      
      language_description =
        language_description,
      
      source_heading =
        body$source_heading,
      
      source_title =
        source_title,
      
      source_author =
        source_author,
      
      source_editor =
        source_editor,
      
      source_date =
        source_date,
      
      source_bibl_uri =
        source_bibl_uri,
      
      edition_status =
        edition_status,
      
      corpus_url =
        corpus_url,
      
      github_source_url =
        github_source_url,
      
      tei_source_url =
        tei_source_url,
      
      digital_source_license =
        digital_source_license,
      
      license_url =
        license_url,
      
      source_text =
        source_text,
      
      text_characters =
        ifelse(
          is.na(source_text),
          0L,
          nchar(source_text)
        ),
      
      extraction_status =
        ifelse(
          !is.na(source_text) &&
            nzchar(source_text),
          "ok",
          "empty_text"
        )
    )
  }
  
  
  # ----------------------------------------------------------
  # Parse corpus
  # ----------------------------------------------------------
  
  message(
    "Parsing ",
    length(tei_files),
    " Digital Syriac Corpus TEI files..."
  )
  
  
  metadata <- purrr::map_dfr(
    tei_files,
    parse_tei
  )
  
  
  # ----------------------------------------------------------
  # Hagiographic classification
  #
  # This is deliberately conservative.
  # ----------------------------------------------------------
  
  metadata <- metadata |>
    
    dplyr::mutate(
      
      title_search =
        stringr::str_to_lower(
          dplyr::coalesce(
            title,
            ""
          )
        ),
      
      series_search =
        stringr::str_to_lower(
          dplyr::coalesce(
            series_title,
            ""
          )
        ),
      
      source_search =
        stringr::str_to_lower(
          dplyr::coalesce(
            source_title,
            ""
          )
        ),
      
      
      # ------------------------------------------------------
      # Strong collection-level signals
      # ------------------------------------------------------
      
      hagiographic_series =
        stringr::str_detect(
          
          series_search,
          
          paste0(
            "persian martyr acts",
            "|apocryphal acts of the apostles",
            "|lives? of ",
            "|martyrs?"
          )
        ),
      
      
      hagiographic_source =
        stringr::str_detect(
          
          source_search,
          
          paste0(
            "acta martyrum et sanctorum",
            "|apocryphal acts of the apostles",
            "|history of the monks",
            "|paradise of the fathers"
          )
        ),
      
      
      # ------------------------------------------------------
      # Strong title-level signals
      # ------------------------------------------------------
      
      hagiographic_title =
        stringr::str_detect(
          
          title_search,
          
          paste0(
            "martyrdom",
            "|martyrs? of",
            "|passion of",
            "|life of",
            "|miracles? of",
            "|history of mar\\b",
            "|history of saint",
            "|story of mar\\b",
            "|travels of ",
            "|doctrine of addai",
            "|acts? of (",
            "thomas",
            "|peter",
            "|andrew",
            "|john",
            "|philip",
            "|paul",
            "|matthew",
            "|bartholomew",
            "|thaddeus",
            "|addai",
            ")"
          )
        ),
      
      
      # ------------------------------------------------------
      # Optional additional user-supplied inclusion pattern
      # ------------------------------------------------------
      
      extra_match =
        if (is.null(extra_include_regex)) {
          
          FALSE
          
        } else {
          
          stringr::str_detect(
            paste(
              title_search,
              series_search,
              source_search
            ),
            stringr::regex(
              extra_include_regex,
              ignore_case = TRUE
            )
          )
        },
      
      
      # ------------------------------------------------------
      # Exclude obvious biblical Peshitta works unless another
      # strong hagiographic signal is present.
      # ------------------------------------------------------
      
      peshitta =
        stringr::str_detect(
          paste(
            title_search,
            series_search
          ),
          "peshitta"
        ),
      
      
      is_hagiographic =
        (
          hagiographic_series |
            hagiographic_source |
            hagiographic_title |
            extra_match
        ) &
        !(
          peshitta &
            !hagiographic_series &
            !hagiographic_source
        ),
      
      
      selection_reason =
        dplyr::case_when(
          
          extra_match ~
            "user-supplied pattern",
          
          hagiographic_series ~
            "hagiographic series",
          
          hagiographic_source ~
            "hagiographic printed source",
          
          hagiographic_title ~
            "hagiographic title",
          
          TRUE ~
            NA_character_
        )
    )
  
  
  # ----------------------------------------------------------
  # Exact IDs override automatic selection
  # ----------------------------------------------------------
  
  if (!is.null(ids)) {
    
    result <- metadata |>
      
      dplyr::filter(
        corpus_id %in% ids
      )
    
  } else if (
    selection == "hagiography"
  ) {
    
    result <- metadata |>
      
      dplyr::filter(
        is_hagiographic
      )
    
  } else {
    
    result <- metadata
  }
  
  
  if (nrow(result) == 0) {
    
    stop(
      "No Digital Syriac Corpus records matched the requested selection."
    )
  }
  
  
  # ----------------------------------------------------------
  # Infer OEHC genre
  # ----------------------------------------------------------
  
  result <- result |>
    
    dplyr::mutate(
      
      genre =
        dplyr::case_when(
          
          stringr::str_detect(
            title_search,
            "martyr|passion"
          ) |
            stringr::str_detect(
              series_search,
              "martyr"
            ) ~
            "martyr_act",
          
          
          stringr::str_detect(
            title_search,
            "miracle"
          ) ~
            "miracle_collection",
          
          
          stringr::str_detect(
            title_search,
            "life of"
          ) ~
            "vita",
          
          
          stringr::str_detect(
            title_search,
            paste0(
              "acts? of ",
              "|travels of ",
              "|doctrine of addai"
            )
          ) |
            stringr::str_detect(
              series_search,
              "apocryphal acts of the apostles"
            ) ~
            "apostolic_act",
          
          
          stringr::str_detect(
            title_search,
            "history of mar|story of mar"
          ) ~
            "saint_narrative",
          
          
          TRUE ~
            "hagiography"
        )
    )
  
  
  # ----------------------------------------------------------
  # Deterministic ordering
  # ----------------------------------------------------------
  
  result <- result |>
    
    dplyr::arrange(
      corpus_id
    )
  
  
  # ----------------------------------------------------------
  # Generate sequential OEHC IDs
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
          nrow(result)
        ) -
        1L,
      
      width =
        id_width,
      
      pad =
        "0"
    )
  )
  
  
  # ----------------------------------------------------------
  # Construct OEHC dataframe
  # ----------------------------------------------------------
  
  result <- result |>
    
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
      
      subject =
        title,
      
      saint_dates =
        NA_character_,
      
      domain =
        NA_character_,
      
      region =
        NA_character_,
      
      textual_tradition =
        "Syriac",
      
      source_language =
        "Syriac",
      
      source_collection =
        "Digital Syriac Corpus",
      
      rights_status =
        dplyr::coalesce(
          digital_source_license,
          "CC BY 4.0"
        ),
      
      underlying_text_rights =
        "public_domain",
      
      collection =
        "syriac_hagiography"
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
      composition_from,
      composition_to,
      
      domain,
      region,
      
      author,
      
      textual_tradition,
      source_language,
      language_code,
      language_description,
      
      genre,
      collection,
      
      corpus_id,
      work_uri,
      series_title,
      series_uri,
      
      source_heading,
      
      source_collection,
      
      source_title,
      source_author,
      source_editor,
      source_date,
      source_bibl_uri,
      
      digital_editors,
      edition_status,
      
      corpus_url,
      github_source_url,
      tei_source_url,
      
      rights_status,
      underlying_text_rights,
      digital_source_license,
      license_url,
      
      selection_reason,
      
      extraction_status,
      text_characters,
      
      source_text
    )
  
  
  # ----------------------------------------------------------
  # QA
  # ----------------------------------------------------------
  
  failed <- result |>
    
    dplyr::filter(
      extraction_status != "ok" |
        is.na(source_text) |
        !nzchar(source_text)
    )
  
  
  if (nrow(failed) > 0) {
    
    warning(
      paste0(
        nrow(failed),
        " Digital Syriac Corpus records require review."
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Duplicate Syriaca work URI check
  # ----------------------------------------------------------
  
  duplicated_work_uris <- result |>
    
    dplyr::filter(
      !is.na(work_uri),
      nzchar(work_uri)
    ) |>
    
    dplyr::count(
      work_uri
    ) |>
    
    dplyr::filter(
      n > 1
    )
  
  
  if (nrow(duplicated_work_uris) > 0) {
    
    message(
      nrow(duplicated_work_uris),
      " Syriaca work URI(s) occur in more than one TEI record; ",
      "these have been retained as separate digital witnesses."
    )
  }
  
  
  message(
    "Created ",
    nrow(result),
    " OEHC Syriac records."
  )
  
  
  result
}

## Scrape and save

# df_syriac_corpus <- scrape_syriac_corpus(start_text_id = "oehc_000758")
# 
# arrow::write_parquet(df_syriac_corpus, "derived/df_syriac_corpus.parquet")
# 
# df_syriac_corpus <- arrow::read_parquet("derived/df_syriac_corpus.parquet")
