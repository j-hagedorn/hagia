scrape_sozomen <- function(
    units = NULL,
    start_text_id = "oehc_000001",
    contact = Sys.getenv("OEHC_CONTACT"),
    cache_dir = "cache/sozomen",
    pause = 1,
    force_refresh = FALSE,
    strict = TRUE
) {
  
  # ==========================================================
  # SOZOMEN — ECCLESIASTICAL HISTORY
  # CURATED HAGIOGRAPHIC / ASCETIC PASSAGES
  #
  # Source:
  # Nicene and Post-Nicene Fathers, Second Series, Volume II
  # English Wikisource
  #
  # Translation:
  # Revised by Chester D. Hartranft
  #
  # Output:
  # One OEHC row per separable hagiographic narrative.
  #
  # Individual notices are split where Sozomen provides
  # distinguishable prose.
  #
  # Genuinely collective notices remain group records.
  #
  # Expected full output: 73 rows.
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
    "https://en.wikisource.org/w/api.php"
  
  page_root <- paste0(
    "Nicene and Post-Nicene Fathers: Series II/",
    "Volume II/Sozomen"
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
  # Curated segmentation manifest
  #
  # start_regex:
  #   beginning of the source passage for this unit
  #
  # end_regex:
  #   optional explicit stopping point
  #
  # If end_regex is NA, the beginning of the next curated
  # passage in the same chapter is used as the endpoint.
  #
  # whole_chapter:
  #   use the complete chapter as a single record
  #
  # unit_type:
  #   individual = separable notice of one person
  #   group      = genuinely collective source notice
  # ----------------------------------------------------------
  
  manifest <- tibble::tribble(
    
    ~unit_id,
    ~book,
    ~chapter,
    ~subject,
    ~unit_type,
    ~start_regex,
    ~end_regex,
    ~whole_chapter,
    
    
    # ========================================================
    # BOOK I
    # ========================================================
    
    "i11_spyridon",
    1, 11,
    "Spyridon",
    "individual",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    # --------------------------------------------------------
    # I.13 — Antony and Paul the Simple
    # --------------------------------------------------------
    
    "i13_antony",
    1, 13,
    "Antony the Great",
    "individual",
    paste0(
      "Whether the Egyptians or others are to be regarded ",
      "as the founders of this philosophy"
    ),
    NA_character_,
    FALSE,
    
    
    "i13_paul_simple",
    1, 13,
    "Paul the Simple",
    "individual",
    paste0(
      "We must relate, in chronological order, ",
      "the history of the most celebrated disciples of Antony"
    ),
    NA_character_,
    FALSE,
    
    
    # --------------------------------------------------------
    # I.14 — Ammon and Eutychianus
    # --------------------------------------------------------
    
    "i14_ammon",
    1, 14,
    "Ammon",
    "individual",
    "It was about this period that Ammon",
    NA_character_,
    FALSE,
    
    
    "i14_eutychianus",
    1, 14,
    "Eutychianus of Olympus",
    "individual",
    paste0(
      "I am convinced that it was likewise during ",
      "this reign that Eutychianus"
    ),
    NA_character_,
    FALSE,
    
    
    # ========================================================
    # BOOK III
    # ========================================================
    
    # --------------------------------------------------------
    # III.14 — Egyptian and other holy men
    # --------------------------------------------------------
    
    "iii14_macarius_egyptian",
    3, 14,
    "Macarius the Egyptian",
    "individual",
    "The Egyptian, the story says",
    NA_character_,
    FALSE,
    
    
    "iii14_macarius_alexandrian",
    3, 14,
    "Macarius of Alexandria",
    "individual",
    "The other Macarius became a presbyter",
    NA_character_,
    FALSE,
    
    
    "iii14_egyptian_elders",
    3, 14,
    paste(
      "Pambo, Heraclides, Cronius, Paphnutius,",
      "Putubastus, Arsisius, Serapion, and Piturion"
    ),
    "group",
    "Pambo, Heraclides, Cronius",
    NA_character_,
    FALSE,
    
    
    "iii14_pachomius",
    3, 14,
    "Pachomius",
    "individual",
    "It is said that Pachomius at first practiced philosophy",
    NA_character_,
    FALSE,
    
    
    "iii14_apollonius",
    3, 14,
    "Apollonius",
    "individual",
    "About the same period, Apollonius became celebrated",
    NA_character_,
    FALSE,
    
    
    "iii14_anuph",
    3, 14,
    "Anuph",
    "individual",
    "I believe that Anuph",
    NA_character_,
    FALSE,
    
    
    "iii14_hilarion",
    3, 14,
    "Hilarion",
    "individual",
    paste0(
      "The same species of philosophy was about this time ",
      "cultivated in Palestine"
    ),
    NA_character_,
    FALSE,
    
    
    "iii14_julian",
    3, 14,
    "Julian of Edessa",
    "individual",
    "About the same period, Julian practiced philosophy",
    NA_character_,
    FALSE,
    
    
    "iii14_daniel_simeon",
    3, 14,
    "Daniel and Simeon",
    "group",
    paste0(
      "Besides the above, many other ecclesiastical ",
      "philosophers flourished"
    ),
    NA_character_,
    FALSE,
    
    
    "iii14_eustathius",
    3, 14,
    "Eustathius of Sebaste",
    "individual",
    "It is said that Eustathius",
    NA_character_,
    FALSE,
    
    
    "iii14_martin",
    3, 14,
    "Martin of Tours",
    "individual",
    "Although the Thracians, the Illyrians",
    NA_character_,
    FALSE,
    
    
    "iii14_hilary",
    3, 14,
    "Hilary",
    "individual",
    "We have heard that Hilary",
    "I have now related what I have been able",
    FALSE,
    
    
    # --------------------------------------------------------
    # III.16 — Ephraim
    # --------------------------------------------------------
    
    "iii16_ephraim",
    3, 16,
    "Ephraim the Syrian",
    "individual",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    # ========================================================
    # BOOK VI
    # ========================================================
    
    # --------------------------------------------------------
    # VI.28 — Holy men of Egypt
    # --------------------------------------------------------
    
    "vi28_john",
    6, 28,
    "John",
    "individual",
    paste0(
      "There was not, it appears, a more celebrated ",
      "man in Egypt than John"
    ),
    NA_character_,
    FALSE,
    
    
    "vi28_or",
    6, 28,
    "Or",
    "individual",
    "Or was another eminent man of this period",
    NA_character_,
    FALSE,
    
    
    "vi28_ammon",
    6, 28,
    "Ammon",
    "individual",
    "Ammon, the leader of the monks",
    NA_character_,
    FALSE,
    
    
    "vi28_theonas",
    6, 28,
    "Theonas",
    "individual",
    "Benus and Theonas likewise presided",
    NA_character_,
    FALSE,
    
    
    "vi28_benus",
    6, 28,
    "Benus",
    "individual",
    "Benus was never seen to manifest",
    NA_character_,
    FALSE,
    
    
    "vi28_copres",
    6, 28,
    "Copres",
    "individual",
    "Copres, Helles, and Elias also flourished",
    NA_character_,
    FALSE,
    
    
    "vi28_helles",
    6, 28,
    "Helles",
    "individual",
    "Helles had from his youth upwards",
    NA_character_,
    FALSE,
    
    
    "vi28_elias",
    6, 28,
    "Elias",
    "individual",
    "Elias, who practiced philosophy",
    NA_character_,
    FALSE,
    
    
    "vi28_apelles",
    6, 28,
    "Apelles",
    "individual",
    "Apelles flourished at the same period",
    NA_character_,
    FALSE,
    
    
    "vi28_isidore",
    6, 28,
    "Isidore",
    "individual",
    "Isidore, Serapion, and Dioscorus",
    NA_character_,
    FALSE,
    
    
    "vi28_serapion",
    6, 28,
    "Serapion",
    "individual",
    "Serapion lived in the neighborhood",
    NA_character_,
    FALSE,
    
    
    "vi28_dioscorus",
    6, 28,
    "Dioscorus",
    "individual",
    "Dioscorus had not more than",
    NA_character_,
    FALSE,
    
    
    "vi28_eulogius",
    6, 28,
    "Eulogius",
    "individual",
    "The presbyter Eulogius",
    NA_character_,
    FALSE,
    
    
    # --------------------------------------------------------
    # VI.29 — Monks of Thebaïs and Scetis
    # --------------------------------------------------------
    
    "vi29_apollos",
    6, 29,
    "Apollos",
    "individual",
    "Apollos flourished about the same period",
    NA_character_,
    FALSE,
    
    
    "vi29_dorotheus",
    6, 29,
    "Dorotheus",
    "individual",
    "Dorotheus, a native of Thebes",
    NA_character_,
    FALSE,
    
    
    "vi29_piammon",
    6, 29,
    "Piammon",
    "individual",
    "Piammon and John presided",
    NA_character_,
    FALSE,
    
    
    "vi29_john",
    6, 29,
    "John of Diolcus",
    "individual",
    "John had received from God",
    NA_character_,
    FALSE,
    
    
    "vi29_benjamin",
    6, 29,
    "Benjamin",
    "individual",
    "A very old man, named Benjamin",
    NA_character_,
    FALSE,
    
    
    "vi29_mark",
    6, 29,
    "Mark of Scetis",
    "individual",
    "About the same time the celebrated Mark",
    NA_character_,
    FALSE,
    
    
    "vi29_macarius",
    6, 29,
    "Macarius the Younger",
    "individual",
    "Macarius had received from God",
    NA_character_,
    FALSE,
    
    
    "vi29_apollonius",
    6, 29,
    "Apollonius of Scetis",
    "individual",
    "Apollonius, after passing his life",
    NA_character_,
    FALSE,
    
    
    "vi29_moses_egyptian",
    6, 29,
    "Moses the Egyptian",
    "individual",
    "Moses was originally a slave",
    NA_character_,
    FALSE,
    
    
    "vi29_paul",
    6, 29,
    "Paul of Ferme",
    "individual",
    "Paul, Pachon, Stephen, and Moses",
    NA_character_,
    FALSE,
    
    
    "vi29_pachon",
    6, 29,
    "Pachon",
    "individual",
    "Pachon also flourished",
    NA_character_,
    FALSE,
    
    
    "vi29_stephen",
    6, 29,
    "Stephen",
    "individual",
    "Stephen dwelt at Mareotis",
    NA_character_,
    FALSE,
    
    
    "vi29_moses_libyan",
    6, 29,
    "Moses the Libyan",
    "individual",
    "Moses was celebrated for his meekness",
    NA_character_,
    FALSE,
    
    
    "vi29_pior",
    6, 29,
    "Pior",
    "individual",
    "Pior determined, from his youth",
    NA_character_,
    FALSE,
    
    
    # --------------------------------------------------------
    # VI.30 — Monks of Scetis
    # --------------------------------------------------------
    
    "vi30_senior_monks",
    6, 30,
    paste(
      "Origen, Didymus, Cronion, Arsisius,",
      "Putubatus, Arsion, and Serapion"
    ),
    "group",
    "At this period, Origen",
    NA_character_,
    FALSE,
    
    
    "vi30_ammon",
    6, 30,
    "Ammon",
    "individual",
    "It is said that Ammon attained",
    NA_character_,
    FALSE,
    
    
    "vi30_evagrius",
    6, 30,
    "Evagrius Ponticus",
    "individual",
    paste0(
      "Some time afterwards, during the ensuing reign, ",
      "the wise Evagrius"
    ),
    NA_character_,
    FALSE,
    
    
    # --------------------------------------------------------
    # VI.31 — Rhinocorura
    # --------------------------------------------------------
    
    "vi31_melas",
    6, 31,
    "Melas",
    "individual",
    "the most eminent philosophers among them were Melas",
    NA_character_,
    FALSE,
    
    
    "vi31_solon",
    6, 31,
    "Solon",
    "individual",
    "Solon quitted the pursuits of commerce",
    NA_character_,
    FALSE,
    
    
    # --------------------------------------------------------
    # VI.32 — Palestine
    # --------------------------------------------------------
    
    "vi32_hesycas",
    6, 32,
    "Hesycas",
    "individual",
    "among them Hesycas",
    NA_character_,
    FALSE,
    
    
    "vi32_epiphanius",
    6, 32,
    "Epiphanius of Salamis",
    "individual",
    "Epiphanius, afterwards bishop of Salamis",
    NA_character_,
    FALSE,
    
    
    "vi32_four_brothers",
    6, 32,
    "Salamines, Phuscon, Malachion, and Crispion",
    "group",
    paste0(
      "At the same period in the monasteries, ",
      "Salamines"
    ),
    NA_character_,
    FALSE,
    
    
    "vi32_ammonius",
    6, 32,
    "Ammonius of Capharcobra",
    "individual",
    "Ammonius lived at a distance",
    NA_character_,
    FALSE,
    
    
    "vi32_silvanus",
    6, 32,
    "Silvanus",
    "individual",
    "I think that Silvanus",
    NA_character_,
    FALSE,
    
    
    # --------------------------------------------------------
    # VI.33 — Syria and Persia
    # --------------------------------------------------------
    
    "vi33_nisibis_shepherds",
    6, 33,
    paste(
      "Battheus, Eusebius, Barges, Halas, Abbo,",
      "Lazarus, Abdaleus, Zeno, and Heliodorus"
    ),
    "group",
    "Battheus, Eusebius, Barges",
    NA_character_,
    FALSE,
    
    
    "vi33_eusebius_carrae",
    6, 33,
    "Eusebius of Carræ",
    "individual",
    "Eusebius voluntarily shut himself",
    NA_character_,
    FALSE,
    
    
    "vi33_protogenes",
    6, 33,
    "Protogenes",
    "individual",
    "Protogenes dwelt in the same locality",
    NA_character_,
    FALSE,
    
    
    "vi33_aones",
    6, 33,
    "Aones",
    "individual",
    "Aones had a monastery",
    NA_character_,
    FALSE,
    
    
    # --------------------------------------------------------
    # VI.34 — Syria, Galatia, Cappadocia
    # --------------------------------------------------------
    
    "vi34_gaddanas_azizus",
    6, 34,
    "Gaddanas and Azizus",
    "group",
    "Gaddanas and Azizus",
    NA_character_,
    FALSE,
    
    
    "vi34_ephraim_julian",
    6, 34,
    "Ephraim and Julian",
    "group",
    "Ephraim the Syrian",
    NA_character_,
    FALSE,
    
    
    "vi34_barses_eulogius_lazarus",
    6, 34,
    "Barses, Eulogius, and Lazarus",
    "group",
    "Barses",
    NA_character_,
    FALSE,
    
    
    "vi34_battheus",
    6, 34,
    "Battheus",
    "individual",
    "Battheus, for instance",
    NA_character_,
    FALSE,
    
    
    "vi34_halas",
    6, 34,
    "Halas",
    "individual",
    "Halas, again",
    NA_character_,
    FALSE,
    
    
    "vi34_heliodorus",
    6, 34,
    "Heliodorus",
    "individual",
    "Heliodorus passed many nights",
    NA_character_,
    FALSE,
    
    
    "vi34_valentian",
    6, 34,
    "Valentian",
    "individual",
    "Such, I found, was the course pursued by Valentian",
    NA_character_,
    FALSE,
    
    
    "vi34_valentian_theodore",
    6, 34,
    "Second Valentian and Theodore",
    "group",
    "Another individual of the same name distinguished",
    NA_character_,
    FALSE,
    
    
    "vi34_marosas_bassus_bassones",
    6, 34,
    "Marosas, Bassus, and Bassones",
    "group",
    "not less distinguished were Marosas",
    NA_character_,
    FALSE,
    
    
    "vi34_paul_telmison",
    6, 34,
    "Paul of Telmison",
    "individual",
    "This latter was from the village of Telmison",
    NA_character_,
    FALSE,
    
    
    "vi34_leontius_prapidius",
    6, 34,
    "Leontius and Prapidius",
    "group",
    "Leontius and Prapidius",
    NA_character_,
    FALSE,
    
    
    # --------------------------------------------------------
    # VI.40 — Isaac
    # --------------------------------------------------------
    
    "vi40_isaac",
    6, 40,
    "Isaac",
    "individual",
    NA_character_,
    NA_character_,
    TRUE,
    
    
    # ========================================================
    # BOOK VII
    # ========================================================
    
    # --------------------------------------------------------
    # VII.26 — Donatus and Theotimus
    # --------------------------------------------------------
    
    "vii26_donatus",
    7, 26,
    "Donatus of Eurœa",
    "individual",
    "There were at this period many other bishops",
    NA_character_,
    FALSE,
    
    
    "vii26_theotimus",
    7, 26,
    "Theotimus of Tomi",
    "individual",
    "The church of Tomi",
    NA_character_,
    FALSE
  )
  
  
  # ----------------------------------------------------------
  # Add within-chapter order
  # ----------------------------------------------------------
  
  manifest <- manifest |>
    
    dplyr::group_by(
      book,
      chapter
    ) |>
    
    dplyr::mutate(
      unit_order =
        dplyr::row_number()
    ) |>
    
    dplyr::ungroup()
  
  
  if (nrow(manifest) != 73) {
    
    stop(
      paste0(
        "Internal manifest error: expected 73 units, found ",
        nrow(manifest),
        "."
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Select requested units
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
  # Construct Wikisource page title
  # ----------------------------------------------------------
  
  chapter_page <- function(
    book,
    chapter
  ) {
    
    paste0(
      page_root,
      "/Book_",
      as.character(
        as.roman(book)
      ),
      "/Chapter_",
      chapter
    )
  }
  
  
  # ----------------------------------------------------------
  # Browser-facing source URL
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
  # Retrieve and cache one chapter
  # ----------------------------------------------------------
  
  get_chapter_doc <- function(
    book,
    chapter
  ) {
    
    cached <- file.path(
      cache_dir,
      paste0(
        "book_",
        book,
        "_chapter_",
        chapter,
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
    
    
    page <- chapter_page(
      book,
      chapter
    )
    
    
    message(
      "Fetching Sozomen ",
      book,
      ".",
      chapter
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
  # Extract chapter prose
  # ----------------------------------------------------------
  
  get_chapter_text <- function(
    book,
    chapter
  ) {
    
    doc <- get_chapter_doc(
      book,
      chapter
    )
    
    
    unwanted <- rvest::html_elements(
      doc,
      paste(
        ".reference",
        ".references",
        ".mw-references-wrap",
        ".mw-editsection",
        ".ws-noexport",
        ".noprint",
        "sup.reference",
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
    
    
    if (length(paragraphs) == 0) {
      
      stop(
        "No prose extracted from Sozomen ",
        book,
        ".",
        chapter
      )
    }
    
    
    paste(
      paragraphs,
      collapse = "\n\n"
    )
  }
  
  
  # ----------------------------------------------------------
  # Locate regex after a specific character position
  #
  # IMPORTANT:
  #
  # This returns a LIST with explicit $start and $end fields.
  #
  # Earlier versions returned a named vector constructed from
  # already-named matrix values, which could produce names
  # such as "start.start" and cause:
  #
  #   subscript out of bounds
  #
  # when using:
  #
  #   location[["start"]]
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
      start = after
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
  # Extract curated units from one chapter
  # ----------------------------------------------------------
  
  segment_chapter <- function(
    rows,
    text
  ) {
    
    rows <- rows |>
      dplyr::arrange(
        unit_order
      )
    
    
    output <- vector(
      "list",
      nrow(rows)
    )
    
    
    for (i in seq_len(nrow(rows))) {
      
      row <- rows[i, ]
      
      
      # ------------------------------------------------------
      # Whole-chapter narrative
      # ------------------------------------------------------
      
      if (
        isTRUE(
          row$whole_chapter[[1]]
        )
      ) {
        
        output[[i]] <- tibble::tibble(
          
          unit_id =
            row$unit_id,
          
          source_text =
            stringr::str_trim(
              text
            ),
          
          segment_status =
            "ok"
        )
        
        next
      }
      
      
      # ------------------------------------------------------
      # Locate passage start
      # ------------------------------------------------------
      
      start_location <- locate_after(
        
        text = text,
        
        pattern =
          row$start_regex[[1]],
        
        after = 1L
      )
      
      
      if (
        is.na(
          start_location[["start"]]
        )
      ) {
        
        msg <- paste0(
          "Could not locate start marker for ",
          row$unit_id,
          " (Sozomen ",
          row$book,
          ".",
          row$chapter,
          ")."
        )
        
        
        if (strict) {
          stop(msg)
        }
        
        
        warning(msg)
        
        
        output[[i]] <- tibble::tibble(
          
          unit_id =
            row$unit_id,
          
          source_text =
            NA_character_,
          
          segment_status =
            "missing_start"
        )
        
        next
      }
      
      
      # ------------------------------------------------------
      # Explicit end marker takes precedence
      # ------------------------------------------------------
      
      if (
        !is.na(
          row$end_regex[[1]]
        ) &&
        nzchar(
          row$end_regex[[1]]
        )
      ) {
        
        end_location <- locate_after(
          
          text = text,
          
          pattern =
            row$end_regex[[1]],
          
          after =
            start_location[["end"]] +
            1L
        )
        
        
        if (
          is.na(
            end_location[["start"]]
          )
        ) {
          
          msg <- paste0(
            "Could not locate end marker for ",
            row$unit_id,
            "."
          )
          
          
          if (strict) {
            stop(msg)
          }
          
          
          warning(msg)
          
          
          text_end <-
            nchar(text)
          
          status <-
            "missing_end"
          
        } else {
          
          text_end <-
            end_location[["start"]] -
            1L
          
          status <-
            "ok"
        }
        
        
        # ------------------------------------------------------
        # Otherwise use next curated unit as boundary
        # ------------------------------------------------------
        
      } else if (
        i < nrow(rows)
      ) {
        
        next_pattern <-
          rows$start_regex[[i + 1]]
        
        
        if (
          is.na(next_pattern) ||
          !nzchar(next_pattern)
        ) {
          
          text_end <-
            nchar(text)
          
          status <-
            "ok"
          
        } else {
          
          next_location <- locate_after(
            
            text = text,
            
            pattern =
              next_pattern,
            
            after =
              start_location[["end"]] +
              1L
          )
          
          
          if (
            is.na(
              next_location[["start"]]
            )
          ) {
            
            msg <- paste0(
              "Could not locate next boundary after ",
              row$unit_id,
              "."
            )
            
            
            if (strict) {
              stop(msg)
            }
            
            
            warning(msg)
            
            
            text_end <-
              nchar(text)
            
            status <-
              "missing_next_boundary"
            
          } else {
            
            text_end <-
              next_location[["start"]] -
              1L
            
            status <-
              "ok"
          }
        }
        
        
        # ------------------------------------------------------
        # Last curated unit in chapter
        # ------------------------------------------------------
        
      } else {
        
        text_end <-
          nchar(text)
        
        status <-
          "ok"
      }
      
      
      # ------------------------------------------------------
      # Validate calculated range
      # ------------------------------------------------------
      
      if (
        text_end <
        start_location[["start"]]
      ) {
        
        msg <- paste0(
          "Invalid passage boundaries for ",
          row$unit_id,
          "."
        )
        
        
        if (strict) {
          stop(msg)
        }
        
        
        warning(msg)
        
        
        output[[i]] <- tibble::tibble(
          
          unit_id =
            row$unit_id,
          
          source_text =
            NA_character_,
          
          segment_status =
            "invalid_boundary"
        )
        
        next
      }
      
      
      # ------------------------------------------------------
      # Extract passage
      # ------------------------------------------------------
      
      passage <- stringr::str_sub(
        
        text,
        
        start =
          start_location[["start"]],
        
        end =
          text_end
      )
      
      
      passage <- passage |>
        
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
        ) |>
        
        stringr::str_trim()
      
      
      output[[i]] <- tibble::tibble(
        
        unit_id =
          row$unit_id,
        
        source_text =
          passage,
        
        segment_status =
          status
      )
    }
    
    
    dplyr::bind_rows(
      output
    )
  }
  
  
  # ----------------------------------------------------------
  # Fetch each required chapter once
  # ----------------------------------------------------------
  
  chapters <- manifest |>
    
    dplyr::distinct(
      book,
      chapter
    ) |>
    
    dplyr::arrange(
      book,
      chapter
    )
  
  
  chapter_data <- purrr::pmap_dfr(
    
    chapters,
    
    function(book, chapter) {
      
      tibble::tibble(
        
        book =
          book,
        
        chapter =
          chapter,
        
        chapter_text =
          get_chapter_text(
            book,
            chapter
          )
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # Segment all chapters
  # ----------------------------------------------------------
  
  segmented <- purrr::map_dfr(
    
    seq_len(
      nrow(chapters)
    ),
    
    function(i) {
      
      current_book <-
        chapters$book[[i]]
      
      current_chapter <-
        chapters$chapter[[i]]
      
      
      rows <- manifest |>
        
        dplyr::filter(
          book == current_book,
          chapter == current_chapter
        )
      
      
      text <- chapter_data |>
        
        dplyr::filter(
          book == current_book,
          chapter == current_chapter
        ) |>
        
        dplyr::pull(
          chapter_text
        )
      
      
      segment_chapter(
        rows = rows,
        text = text[[1]]
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # Join segmentation to manifest
  # ----------------------------------------------------------
  
  result <- manifest |>
    
    dplyr::left_join(
      segmented,
      by = "unit_id"
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
          nrow(result)
        ) -
        1,
      
      width =
        id_width,
      
      pad =
        "0"
    )
  )
  
  
  # ----------------------------------------------------------
  # Construct source page and URL
  # ----------------------------------------------------------
  
  source_pages <- purrr::map2_chr(
    
    result$book,
    
    result$chapter,
    
    chapter_page
  )
  
  
  source_urls <- purrr::map_chr(
    
    source_pages,
    
    page_url
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
      
      title =
        paste0(
          "Sozomen: ",
          subject
        ),
      
      saint_dates =
        NA_character_,
      
      original_date =
        "c. 440s",
      
      domain =
        NA_character_,
      
      region =
        NA_character_,
      
      author =
        "Sozomen",
      
      original_language =
        "Greek",
      
      translator =
        "Chester D. Hartranft",
      
      translation_year =
        "1890",
      
      source_collection =
        paste(
          "Nicene and Post-Nicene Fathers,",
          "Second Series, Volume II;",
          "The Ecclesiastical History of Sozomen"
        ),
      
      source_passage =
        paste0(
          "Book ",
          as.character(
            as.roman(book)
          ),
          ", Chapter ",
          chapter
        ),
      
      text_source_url =
        source_urls,
      
      rights_status =
        "public_domain",
      
      digital_source_license =
        "CC BY-SA",
      
      genre =
        "ecclesiastical_history_hagiography",
      
      collection =
        "ancient"
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
      original_language,
      
      translator,
      translation_year,
      
      book,
      chapter,
      source_passage,
      
      source_collection,
      text_source_url,
      
      rights_status,
      digital_source_license,
      
      genre,
      collection,
      
      segment_status,
      source_text
    )
  
  
  # ----------------------------------------------------------
  # Final checks
  # ----------------------------------------------------------
  
  failed <- result |>
    
    dplyr::filter(
      segment_status != "ok" |
        is.na(source_text) |
        !nzchar(source_text)
    )
  
  
  if (nrow(failed) > 0) {
    
    warning(
      paste0(
        nrow(failed),
        " Sozomen units require segmentation review."
      )
    )
  }
  
  
  result
}

## Scrape and save

# df_sozomen <- scrape_sozomen(start_text_id = "oehc_000587")
# 
# arrow::write_parquet(df_sozomen, "derived/df_sozomen.parquet")
# 
# df_sozomen <- arrow::read_parquet("derived/df_sozomen.parquet")
