scrape_theodoret <- function(
    chapters = 1:30,
    start_text_id = "oehc_000363",
    cache_file = "cache/theodoret_historia_religiosa.xml",
    force_refresh = FALSE
) {
  
  # ==========================================================
  # THEODORET OF CYRRHUS — HISTORIA RELIGIOSA
  #
  # Source:
  # OpenGreekAndLatin / First1KGreek
  #
  # TEI edition:
  # Theodoretus Cyrensis Episcopus Opera Omnia, vol. 3
  # Patrologia Graeca 82
  # ed. Johann Ludwig Schulze
  # Paris: J. P. Migne, 1864
  #
  # Digital transcription:
  # CC BY-SA 4.0
  #
  # Returns one row per chapter / ascetic narrative.
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
  
  
  # ----------------------------------------------------------
  # Source configuration
  # ----------------------------------------------------------
  
  tei_url <- paste0(
    "https://raw.githubusercontent.com/",
    "OpenGreekAndLatin/First1KGreek/master/",
    "data/tlg4089/tlg004/",
    "tlg4089.tlg004.1st1K-grc1.xml"
  )
  
  atlas_url <- paste0(
    "https://atlas.perseus.tufts.edu/library/",
    "urn%3Acts%3AgreekLit%3Atlg4089.tlg004.1st1K-grc1/"
  )
  
  
  # ----------------------------------------------------------
  # Curated chapter manifest
  #
  # The TEI itself numbers the chapters but does not provide
  # useful English subject headings, so these are supplied as
  # project metadata.
  # ----------------------------------------------------------
  
  manifest <- tibble::tribble(
    
    ~chapter, ~subject,
    
    1, "James of Nisibis",
    2, "Julian Saba",
    3, "Marcianus",
    4, "Eusebius of Teleda",
    5, "Publius",
    6, "Symeon the Elder",
    7, "Palladius",
    8, "Aphrahat",
    9, "Peter the Galatian",
    10, "Theodosius",
    11, "Romanus",
    12, "Zeno",
    13, "Macedonius",
    14, "Maesymas",
    15, "Acepsimas",
    16, "Maron",
    17, "Abraham",
    18, "Eusebius of Asikha",
    19, "Salamanes",
    20, "Maris",
    21, "James of Cyrrhestica",
    22, "Thalassius and Limnaeus",
    23, "John",
    24, "Zebinas and Polychronius",
    25, "Asclepius",
    26, "Symeon Stylites",
    27, "Baradatus",
    28, "Thalelaeus",
    29, "Marana and Cyra",
    30, "Domnina"
  )
  
  
  # ----------------------------------------------------------
  # Validate requested chapters
  # ----------------------------------------------------------
  
  chapters <- unique(
    as.integer(chapters)
  )
  
  invalid <- setdiff(
    chapters,
    manifest$chapter
  )
  
  if (length(invalid) > 0) {
    stop(
      "Invalid chapter number(s): ",
      paste(invalid, collapse = ", ")
    )
  }
  
  manifest <- manifest |>
    dplyr::filter(
      chapter %in% chapters
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
  
  id_prefix <- id_parts[1, 2]
  
  id_start <- as.integer(
    id_parts[1, 3]
  )
  
  id_width <- nchar(
    id_parts[1, 3]
  )
  
  text_ids <- paste0(
    id_prefix,
    stringr::str_pad(
      id_start + seq_len(nrow(manifest)) - 1,
      width = id_width,
      pad = "0"
    )
  )
  
  
  # ----------------------------------------------------------
  # Download and cache TEI
  #
  # The complete work is contained in one XML document, so
  # only one network request is necessary.
  # ----------------------------------------------------------
  
  dir.create(
    dirname(cache_file),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  if (
    force_refresh ||
    !file.exists(cache_file)
  ) {
    
    message(
      "Downloading Historia Religiosa TEI..."
    )
    
    response <- httr2::request(
      tei_url
    ) |>
      
      httr2::req_user_agent(
        "OpenEnglishHagiographyCorpus/0.1"
      ) |>
      
      httr2::req_retry(
        max_tries = 3
      ) |>
      
      httr2::req_timeout(
        60
      ) |>
      
      httr2::req_perform()
    
    
    writeBin(
      httr2::resp_body_raw(response),
      cache_file
    )
  }
  
  
  # ----------------------------------------------------------
  # Parse TEI XML
  # ----------------------------------------------------------
  
  doc <- xml2::read_xml(
    cache_file
  )
  
  
  # ----------------------------------------------------------
  # Confirm that all thirty numbered chapters exist
  # ----------------------------------------------------------
  
  chapter_nodes <- xml2::xml_find_all(
    doc,
    paste0(
      ".//*[local-name()='div'",
      " and @type='textpart'",
      " and @subtype='chapter']"
    )
  )
  
  chapter_numbers <- xml2::xml_attr(
    chapter_nodes,
    "n"
  )
  
  numbered_chapters <- suppressWarnings(
    as.integer(chapter_numbers)
  )
  
  numbered_chapters <- numbered_chapters[
    !is.na(numbered_chapters)
  ]
  
  
  missing_chapters <- setdiff(
    manifest$chapter,
    numbered_chapters
  )
  
  if (length(missing_chapters) > 0) {
    stop(
      "Requested chapter(s) missing from TEI: ",
      paste(
        missing_chapters,
        collapse = ", "
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Extract one chapter
  # ----------------------------------------------------------
  
  extract_chapter <- function(chapter_number) {
    
    xpath <- paste0(
      ".//*[local-name()='div'",
      " and @type='textpart'",
      " and @subtype='chapter'",
      " and @n='",
      chapter_number,
      "']"
    )
    
    
    node <- xml2::xml_find_first(
      doc,
      xpath
    )
    
    
    if (
      inherits(
        node,
        "xml_missing"
      )
    ) {
      
      return(
        list(
          source_text = NA_character_,
          pg_reference = NA_character_
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Preserve paragraph structure.
    #
    # Inline TEI elements such as quotations and page
    # milestones remain incorporated into the surrounding
    # paragraph text.
    # --------------------------------------------------------
    
    paragraph_nodes <- xml2::xml_find_all(
      node,
      "./*[local-name()='p']"
    )
    
    
    paragraphs <- xml2::xml_text(
      paragraph_nodes
    )
    
    
    paragraphs <- paragraphs |>
      
      stringr::str_replace_all(
        "\u00A0",
        " "
      ) |>
      
      stringr::str_squish()
    
    
    paragraphs <- paragraphs[
      !is.na(paragraphs) &
        nzchar(paragraphs)
    ]
    
    
    source_text <- if (
      length(paragraphs) == 0
    ) {
      
      NA_character_
      
    } else {
      
      paste(
        paragraphs,
        collapse = "\n\n"
      )
    }
    
    
    # --------------------------------------------------------
    # Extract Migne column markers supplied by the TEI.
    # --------------------------------------------------------
    
    milestone_nodes <- xml2::xml_find_all(
      node,
      paste0(
        ".//*[local-name()='milestone'",
        " and @unit='mignepage']"
      )
    )
    
    
    milestones <- xml2::xml_attr(
      milestone_nodes,
      "n"
    )
    
    milestones <- milestones[
      !is.na(milestones) &
        nzchar(milestones)
    ]
    
    
    pg_reference <- if (
      length(milestones) == 0
    ) {
      
      NA_character_
      
    } else if (
      length(milestones) == 1
    ) {
      
      paste0(
        "PG 82, col. ",
        milestones[[1]]
      )
      
    } else {
      
      paste0(
        "PG 82, cols. ",
        milestones[[1]],
        "–",
        milestones[[length(milestones)]]
      )
    }
    
    
    list(
      source_text = source_text,
      pg_reference = pg_reference
    )
  }
  
  
  # ----------------------------------------------------------
  # Extract selected chapters
  # ----------------------------------------------------------
  
  extracted <- purrr::map(
    manifest$chapter,
    extract_chapter
  )
  
  
  # ----------------------------------------------------------
  # Construct OEHC dataframe
  # ----------------------------------------------------------
  
  result <- manifest |>
    
    dplyr::mutate(
      
      schema_version = "1.0",
      
      text_id = text_ids,
      
      bhg = NA_character_,
      bhl = NA_character_,
      bho = NA_character_,
      
      title = paste0(
        "Historia Religiosa: ",
        subject
      ),
      
      saint_dates = NA_character_,
      
      # Project-level dating, not supplied by the TEI.
      original_date = "c. 444",
      
      domain = NA_character_,
      region = NA_character_,
      
      author = "Theodoret of Cyrrhus",
      
      source_collection = paste(
        "Theodoret, Historia Religiosa;",
        "Theodoretus Cyrensis Episcopus Opera Omnia, vol. 3;",
        "PG 82;",
        "ed. Johann Ludwig Schulze;",
        "Paris: J. P. Migne, 1864"
      ),
      
      source_language = "grc",
      
      tei_urn = paste0(
        "urn:cts:greekLit:",
        "tlg4089.tlg004.1st1K-grc1:",
        chapter
      ),
      
      pg_reference = purrr::map_chr(
        extracted,
        "pg_reference"
      ),
      
      text_source_url = tei_url,
      
      source_record_url = atlas_url,
      
      rights_status = "CC BY-SA 4.0",
      
      genre = "vita",
      
      collection = "ancient",
      
      source_text = purrr::map_chr(
        extracted,
        "source_text"
      )
    ) |>
    
    dplyr::select(
      -subject
    )
  
  
  result
}

## Scrape and save

# df_theodoret <- scrape_theodoret(start_text_id = "oehc_000363")

# arrow::write_parquet(df_theodoret, "derived/df_theodoret.parquet")

# df_theodoret <- arrow::read_parquet("derived/df_theodoret.parquet")

