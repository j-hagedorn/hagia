scrape_theodoret_eh <- function(
    units = NULL,
    start_text_id = "oehc_000001",
    cache_file = "cache/theodoret_ecclesiastical_history.xml",
    force_refresh = FALSE
) {
  
  # ==========================================================
  # THEODORET OF CYRRHUS — ECCLESIASTICAL HISTORY
  #
  # Source:
  # OpenGreekAndLatin / First1KGreek
  #
  # TEI edition:
  # Theodoretus Cyrensis Episcopus Opera Omnia
  # ed. Johann Ludwig Schulze
  # Patrologia Graeca 82
  #
  # Digital transcription:
  # CC BY-SA 4.0
  #
  # Returns one row per curated hagiographic narrative.
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
    "data/tlg4089/tlg003/",
    "tlg4089.tlg003.1st1K-grc1.xml"
  )
  
  
  atlas_url <- paste0(
    "https://atlas.perseus.tufts.edu/library/",
    "urn%3Acts%3AgreekLit%3A",
    "tlg4089.tlg003.1st1K-grc1/"
  )
  
  
  # ----------------------------------------------------------
  # Curated hagiographic manifest
  #
  # unit_type:
  #
  #   individual
  #   group
  #
  # The same saint may appear in multiple chapters.
  # These remain separate textual witnesses.
  # ----------------------------------------------------------
  
  manifest <- tibble::tribble(
    
    ~unit_id,
    ~book,
    ~chapter,
    ~subject,
    ~unit_type,
    ~genre,
    
    
    # ========================================================
    # BOOK I
    # ========================================================
    
    "i22_frumentius_aedesius",
    1, 22,
    "Frumentius and Aedesius",
    "group",
    "conversion_narrative",
    
    
    "i23_captive_woman_iberia",
    1, 23,
    "The Captive Woman of Iberia",
    "individual",
    "conversion_narrative",
    
    
    # ========================================================
    # BOOK III
    # ========================================================
    
    "iii5_athanasius",
    3, 5,
    "Athanasius: Fourth Exile and Flight",
    "individual",
    "saint_episode",
    
    
    "iii6_babylas",
    3, 6,
    "Babylas and His Companions",
    "group",
    "martyr_cult",
    
    
    "iii7_theodorus",
    3, 7,
    "Theodorus the Confessor",
    "individual",
    "confessor_act",
    
    
    "iii10_priests_son",
    3, 10,
    "The Priest's Son",
    "individual",
    "conversion_narrative",
    
    
    "iii11_juventinus_maximinus",
    3, 11,
    "Juventinus and Maximinus",
    "group",
    "martyr_act",
    
    
    "iii19_julian_saba",
    3, 19,
    "Julian Saba and the Death of Julian",
    "individual",
    "saint_episode",
    
    
    # ========================================================
    # BOOK IV
    # ========================================================
    
    "iv6_ambrose",
    4, 6,
    "Ambrose of Milan",
    "individual",
    "saint_episode",
    
    
    "iv20_moses",
    4, 20,
    "Moses the Monk and Mavia",
    "individual",
    "saint_episode",
    
    
    "iv21_presbyter_martyrs",
    4, 21,
    "The Martyred Presbyters of Constantinople",
    "group",
    "martyr_act",
    
    
    "iv23_aphraates",
    4, 23,
    "Aphraates",
    "individual",
    "saint_episode",
    
    
    "iv24_julian_saba",
    4, 24,
    "Julian Saba at Antioch",
    "individual",
    "saint_episode",
    
    
    "iv26_ephraim_didymus",
    4, 26,
    "Ephraim the Syrian and Didymus of Alexandria",
    "group",
    "ascetic_notice",
    
    
    "iv31_isaac_bretanio",
    4, 31,
    "Isaac and Bretanio",
    "group",
    "saint_episode",
    
    
    # ========================================================
    # BOOK V
    # ========================================================
    
    "v4_eusebius_samosata",
    5, 4,
    "Eusebius of Samosata",
    "individual",
    "saint_episode",
    
    
    "v16_amphilochius",
    5, 16,
    "Amphilochius of Iconium",
    "individual",
    "saint_episode",
    
    
    "v18_placilla",
    5, 18,
    "Placilla",
    "individual",
    "saint_episode",
    
    
    "v26_telemachus",
    5, 26,
    "Telemachus the Monk",
    "individual",
    "martyr_act",
    
    
    # --------------------------------------------------------
    # John Chrysostom sequence
    #
    # These are retained as distinct episodes because
    # Theodoret himself separates them into chapters.
    # --------------------------------------------------------
    
    "v28_chrysostom_boldness",
    5, 28,
    "John Chrysostom: Boldness for God",
    "individual",
    "saint_episode",
    
    
    "v29_chrysostom_phoenicia",
    5, 29,
    "John Chrysostom and the Temples of Phoenicia",
    "individual",
    "saint_episode",
    
    
    "v30_chrysostom_goths",
    5, 30,
    "John Chrysostom and the Gothic Church",
    "individual",
    "saint_episode",
    
    
    "v31_chrysostom_scythians",
    5, 31,
    "John Chrysostom and the Scythians",
    "individual",
    "saint_episode",
    
    
    "v32_chrysostom_gainas",
    5, 32,
    "John Chrysostom and Gainas",
    "individual",
    "saint_episode",
    
    
    "v33_chrysostom_embassy",
    5, 33,
    "John Chrysostom's Embassy to Gainas",
    "individual",
    "saint_episode",
    
    
    "v38_persian_martyrs",
    5, 38,
    "The Martyrs of Persia",
    "group",
    "martyrdom_collection"
  )
  
  
  # ----------------------------------------------------------
  # Select requested units
  #
  # Example:
  #
  # units = c(
  #   "iii6_babylas",
  #   "iii11_juventinus_maximinus"
  # )
  # ----------------------------------------------------------
  
  if (!is.null(units)) {
    
    unknown <- setdiff(
      units,
      manifest$unit_id
    )
    
    
    if (length(unknown) > 0) {
      
      stop(
        "Unknown unit_id: ",
        paste(
          unknown,
          collapse = ", "
        )
      )
    }
    
    
    manifest <- manifest |>
      dplyr::filter(
        unit_id %in% units
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
  # Cache TEI
  #
  # The entire Ecclesiastical History is one XML document,
  # so only one network request is required.
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
      "Downloading Theodoret, Ecclesiastical History TEI..."
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
      httr2::resp_body_raw(
        response
      ),
      cache_file
    )
  }
  
  
  # ----------------------------------------------------------
  # Parse TEI
  # ----------------------------------------------------------
  
  doc <- xml2::read_xml(
    cache_file
  )
  
  
  # ----------------------------------------------------------
  # Clean a paragraph
  # ----------------------------------------------------------
  
  clean_text <- function(x) {
    
    x |>
      
      stringr::str_replace_all(
        "\u00A0",
        " "
      ) |>
      
      stringr::str_replace_all(
        "\\s+",
        " "
      ) |>
      
      stringr::str_trim()
  }
  
  
  # ----------------------------------------------------------
  # Extract one chapter
  #
  # The TEI hierarchy is:
  #
  #   book
  #     chapter
  #
  # This is substantially more stable than scraping HTML.
  # ----------------------------------------------------------
  
  extract_chapter <- function(
    book,
    chapter
  ) {
    
    xpath <- paste0(
      
      ".//*[local-name()='div'",
      " and @type='textpart'",
      " and @subtype='book'",
      " and @n='",
      book,
      "']",
      
      "/*[local-name()='div'",
      " and @type='textpart'",
      " and @subtype='chapter'",
      " and @n='",
      chapter,
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
          
          source_heading =
            NA_character_,
          
          source_text =
            NA_character_,
          
          pg_reference =
            NA_character_,
          
          status =
            "missing_chapter"
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Remove critical/editorial notes
    #
    # Inline quotations remain part of the prose.
    # --------------------------------------------------------
    
    note_nodes <- xml2::xml_find_all(
      node,
      ".//*[local-name()='note']"
    )
    
    
    if (length(note_nodes) > 0) {
      
      xml2::xml_remove(
        note_nodes
      )
    }
    
    
    # --------------------------------------------------------
    # Extract paragraph text
    # --------------------------------------------------------
    
    paragraph_nodes <- xml2::xml_find_all(
      node,
      "./*[local-name()='p']"
    )
    
    
    paragraphs <- xml2::xml_text(
      paragraph_nodes
    )
    
    
    paragraphs <- purrr::map_chr(
      paragraphs,
      clean_text
    )
    
    
    paragraphs <- paragraphs[
      !is.na(paragraphs) &
        nzchar(paragraphs)
    ]
    
    
    if (length(paragraphs) == 0) {
      
      return(
        list(
          
          source_heading =
            NA_character_,
          
          source_text =
            NA_character_,
          
          pg_reference =
            NA_character_,
          
          status =
            "empty_chapter"
        )
      )
    }
    
    
    # --------------------------------------------------------
    # In this TEI edition the first paragraph normally
    # contains the Greek chapter rubric.
    #
    # Keep it separately when there is more than one paragraph.
    # --------------------------------------------------------
    
    if (length(paragraphs) > 1) {
      
      source_heading <-
        paragraphs[[1]]
      
      text_paragraphs <-
        paragraphs[-1]
      
    } else {
      
      source_heading <-
        NA_character_
      
      text_paragraphs <-
        paragraphs
    }
    
    
    source_text <- paste(
      text_paragraphs,
      collapse = "\n\n"
    )
    
    
    # --------------------------------------------------------
    # Extract PG 82 column references
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
      
      source_heading =
        source_heading,
      
      source_text =
        source_text,
      
      pg_reference =
        pg_reference,
      
      status =
        "ok"
    )
  }
  
  
  # ----------------------------------------------------------
  # Extract all selected units
  # ----------------------------------------------------------
  
  extracted <- purrr::map2(
    
    manifest$book,
    
    manifest$chapter,
    
    extract_chapter
  )
  
  
  # ----------------------------------------------------------
  # Construct CTS URNs
  # ----------------------------------------------------------
  
  tei_urns <- paste0(
    
    "urn:cts:greekLit:",
    "tlg4089.tlg003.1st1K-grc1:",
    
    manifest$book,
    ".",
    manifest$chapter
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
      
      title =
        paste0(
          "Theodoret, Ecclesiastical History: ",
          subject
        ),
      
      saint_dates =
        NA_character_,
      
      original_date =
        "c. 450",
      
      domain =
        NA_character_,
      
      region =
        NA_character_,
      
      author =
        "Theodoret of Cyrrhus",
      
      source_language =
        "grc",
      
      source_passage =
        paste0(
          "Book ",
          book,
          ", Chapter ",
          chapter
        ),
      
      source_heading =
        purrr::map_chr(
          extracted,
          "source_heading"
        ),
      
      source_collection = paste(
        "Theodoret, Historia Ecclesiastica;",
        "Theodoretus Cyrensis Episcopus Opera Omnia;",
        "PG 82;",
        "ed. Johann Ludwig Schulze"
      ),
      
      tei_urn =
        tei_urns,
      
      pg_reference =
        purrr::map_chr(
          extracted,
          "pg_reference"
        ),
      
      text_source_url =
        tei_url,
      
      source_record_url =
        atlas_url,
      
      rights_status =
        "CC BY-SA 4.0",
      
      collection =
        "ancient",
      
      extraction_status =
        purrr::map_chr(
          extracted,
          "status"
        ),
      
      source_text =
        purrr::map_chr(
          extracted,
          "source_text"
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
      unit_type,
      
      saint_dates,
      original_date,
      
      domain,
      region,
      
      author,
      source_language,
      
      book,
      chapter,
      source_passage,
      source_heading,
      
      source_collection,
      tei_urn,
      pg_reference,
      
      text_source_url,
      source_record_url,
      
      rights_status,
      genre,
      collection,
      
      extraction_status,
      source_text
    )
  
  
  # ----------------------------------------------------------
  # Final validation
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
        " Theodoret EH records require review."
      )
    )
  }
  
  
  result
}

## Scrape and save

# df_theodoret_eh <- scrape_theodoret_eh(start_text_id = "oehc_000660")

# arrow::write_parquet(df_theodoret_eh, "derived/df_theodoret_eh.parquet")

# df_theodoret_eh <- arrow::read_parquet("derived/df_theodoret_eh.parquet")
