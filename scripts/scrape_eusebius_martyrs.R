scrape_eusebius_martyrs <- function(
    start_text_id = "oehc_000001",
    cache_file = "cache/eusebius_martyrs_long.html",
    force_refresh = FALSE
) {
  
  # ==========================================================
  # EUSEBIUS — MARTYRS OF PALESTINE, LONG RECENSION
  #
  # Source:
  # William Cureton, ed. and trans.,
  # History of the Martyrs in Palestine
  # (Williams and Norgate, 1861)
  #
  # Digital transcription:
  # Tertullian.org
  #
  # Returns ONE ROW PER "CONFESSION" SECTION.
  #
  # Expected output: 17 OEHC records.
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
  
  
  # ----------------------------------------------------------
  # Source
  # ----------------------------------------------------------
  
  source_url <- paste0(
    "https://tertullian.org/fathers/",
    "eusebius_martyrs.htm"
  )
  
  
  # ----------------------------------------------------------
  # Curated section manifest
  #
  # One row corresponds to one source-defined "Confession."
  # Group martyrdoms remain single textual units.
  # ----------------------------------------------------------
  
  manifest <- tibble::tribble(
    
    ~section_order, ~title,
    
    1, "Confession of Procopius",
    
    2, paste(
      "Confession of Alphaeus,",
      "Zacchaeus, and Romanus"
    ),
    
    3, "Confession of Timotheus",
    
    4, paste(
      "Confession of Agapius,",
      "the Two Alexanders,",
      "the Two Dionysiuses, Timotheus,",
      "Romulus, and Paesis"
    ),
    
    5, "Confession of Epiphanius (Apphianus)",
    
    6, "Confession of Alosis (Aedesius)",
    
    7, "Confession of Agapius",
    
    8, "Confession of Theodosia",
    
    9, "Confession of Domninus",
    
    10, paste(
      "Confession of Paulus,",
      "Valentina, and Hatha"
    ),
    
    11, paste(
      "Confession of Antoninus,",
      "Zebinas, Germanus, and Mannathus"
    ),
    
    12, paste(
      "Confession of Ares,",
      "Primus, and Elias"
    ),
    
    13, "Confession of Peter Absalom",
    
    14, paste(
      "Confession of Pamphilus, Vales,",
      "Seleucus, Paulus, Porphyrius,",
      "Theophilus, Julianus, and One Egyptian"
    ),
    
    15, "Confession of Hadrianus and Eubulus",
    
    16, paste(
      "Confession of Paulus, Nilus,",
      "Patrimytheas, and Elias"
    ),
    
    17, "Confession of Silvanus and Companions"
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
  # Cache source HTML
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
      "Downloading Eusebius, Martyrs of Palestine..."
    )
    
    response <- httr2::request(
      source_url
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
  # Parse HTML
  # ----------------------------------------------------------
  
  doc <- xml2::read_html(
    cache_file
  )
  
  body <- rvest::html_element(
    doc,
    "body"
  )
  
  if (inherits(body, "xml_missing")) {
    stop(
      "Could not locate the HTML body."
    )
  }
  
  
  page_text <- rvest::html_text2(
    body
  )
  
  page_text <- page_text |>
    
    stringr::str_replace_all(
      "\u00A0",
      " "
    ) |>
    
    stringr::str_replace_all(
      "\r\n?",
      "\n"
    )
  
  
  # ----------------------------------------------------------
  # Identify beginning of the first martyr narrative
  #
  # Starting with Procopius avoids Cureton's title pages,
  # preface, introduction, and other editorial material.
  # ----------------------------------------------------------
  
  start_pattern <- stringr::regex(
    paste0(
      "THE\\s+CONFESSION\\s+OF\\s+",
      "PROCOPIUS\\s*,?"
    ),
    ignore_case = TRUE
  )
  
  start_location <- stringr::str_locate(
    page_text,
    start_pattern
  )
  
  
  if (is.na(start_location[1, "start"])) {
    stop(
      "Could not locate the Confession of Procopius."
    )
  }
  
  
  # ----------------------------------------------------------
  # Identify end of Eusebius' primary text
  #
  # Flexible whitespace is important because html_text2()
  # may insert line breaks or multiple spaces.
  # ----------------------------------------------------------
  
  closing_pattern <- stringr::regex(
    paste0(
      "Here\\s+end\\s+the\\s+chapters\\s+",
      "of\\s+the\\s+narrative\\s+",
      "of\\s+the\\s+victories\\s+",
      "of\\s+the\\s+holy\\s+confessors\\s+",
      "in\\s+Palestine\\s*\\."
    ),
    ignore_case = TRUE
  )
  
  
  closing_location <- stringr::str_locate(
    page_text,
    closing_pattern
  )
  
  
  # ----------------------------------------------------------
  # Preferred endpoint: Eusebius' explicit closing colophon
  # ----------------------------------------------------------
  
  if (!is.na(closing_location[1, "end"])) {
    
    primary_end <- closing_location[
      1,
      "end"
    ]
    
  } else {
    
    # --------------------------------------------------------
    # Fallback 1:
    # Tertullian.org editorial note introducing Cureton's
    # notes.
    # --------------------------------------------------------
    
    editorial_notes_pattern <- stringr::regex(
      paste0(
        "\\[\\[These\\s+notes\\s+",
        "have\\s+been\\s+scanned"
      ),
      ignore_case = TRUE
    )
    
    editorial_notes_location <-
      stringr::str_locate(
        page_text,
        editorial_notes_pattern
      )
    
    
    if (
      !is.na(
        editorial_notes_location[
          1,
          "start"
        ]
      )
    ) {
      
      warning(
        paste(
          "Could not locate Eusebius' closing colophon;",
          "using the beginning of the editorial notes",
          "as the endpoint."
        )
      )
      
      primary_end <-
        editorial_notes_location[
          1,
          "start"
        ] - 1
      
      
    } else {
      
      # ------------------------------------------------------
      # Fallback 2:
      # Explicit NOTES. heading
      # ------------------------------------------------------
      
      notes_pattern <- stringr::regex(
        "^\\s*NOTES\\.\\s*$",
        ignore_case = TRUE,
        multiline = TRUE
      )
      
      notes_location <- stringr::str_locate(
        page_text,
        notes_pattern
      )
      
      
      if (
        is.na(
          notes_location[
            1,
            "start"
          ]
        )
      ) {
        
        stop(
          paste(
            "Could not identify the end of the primary text:",
            "neither the closing colophon nor either",
            "notes marker was found."
          )
        )
      }
      
      
      warning(
        paste(
          "Could not locate Eusebius' closing colophon;",
          "using the NOTES heading as the endpoint."
        )
      )
      
      primary_end <-
        notes_location[
          1,
          "start"
        ] - 1
    }
  }
  
  
  # ----------------------------------------------------------
  # Extract only the martyr narratives
  # ----------------------------------------------------------
  
  primary_text <- stringr::str_sub(
    page_text,
    start = start_location[1, "start"],
    end = primary_end
  )
  
  
  # ----------------------------------------------------------
  # Find all source section headings
  #
  # Examples:
  #
  # THE CONFESSION OF PROCOPIUS,
  #
  # THE CONFESSION OF TIMOTHEUS, IN THE CITY OF GAZA,
  #
  # THE CONFESSION OF SILVANUS, AND OF THOSE WITH HIM,
  #
  # Leading/trailing whitespace is tolerated.
  # ----------------------------------------------------------
  
  heading_pattern <- stringr::regex(
    "^\\s*THE\\s+CONFESSION\\s+OF\\s+[^\\n]+?\\s*$",
    multiline = TRUE
  )
  
  
  heading_locations <- stringr::str_locate_all(
    primary_text,
    heading_pattern
  )[[1]]
  
  
  headings <- stringr::str_sub(
    primary_text,
    heading_locations[, "start"],
    heading_locations[, "end"]
  )
  
  headings <- headings |>
    stringr::str_squish()
  
  
  # ----------------------------------------------------------
  # Validate section count
  # ----------------------------------------------------------
  
  expected_sections <- nrow(
    manifest
  )
  
  if (length(headings) != expected_sections) {
    
    stop(
      paste0(
        "Expected ",
        expected_sections,
        " confession sections but found ",
        length(headings),
        ". Inspect the extracted headings before continuing."
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Extract one narrative per heading
  #
  # Each story begins immediately after its heading and runs
  # to the beginning of the next source heading.
  # ----------------------------------------------------------
  
  sections <- purrr::map_chr(
    seq_along(headings),
    function(i) {
      
      text_start <-
        heading_locations[
          i,
          "end"
        ] + 1
      
      
      text_end <- if (
        i < length(headings)
      ) {
        
        heading_locations[
          i + 1,
          "start"
        ] - 1
        
      } else {
        
        nchar(primary_text)
      }
      
      
      text <- stringr::str_sub(
        primary_text,
        start = text_start,
        end = text_end
      )
      
      
      # ------------------------------------------------------
      # Remove Eusebius' work-level colophon from the final
      # Silvanus narrative.
      # ------------------------------------------------------
      
      if (i == length(headings)) {
        
        text <- stringr::str_remove(
          text,
          stringr::regex(
            paste0(
              "\\s*Here\\s+end\\s+the\\s+chapters\\s+",
              "of\\s+the\\s+narrative\\s+",
              "of\\s+the\\s+victories\\s+",
              "of\\s+the\\s+holy\\s+confessors\\s+",
              "in\\s+Palestine\\s*\\.\\s*$"
            ),
            ignore_case = TRUE
          )
        )
        
        
        # Remove any trailing separator retained by a
        # fallback endpoint.
        text <- stringr::str_remove(
          text,
          "\\s*(?:\\*\\s*){3,}$"
        )
      }
      
      
      # ------------------------------------------------------
      # Remove printed-page markers
      #
      # Examples:
      #
      # [p. 21]
      # [P. 50.]
      # |48
      # |iii
      #
      # \\s* allows page numbers broken across lines.
      # ------------------------------------------------------
      
      text <- text |>
        
        stringr::str_replace_all(
          stringr::regex(
            "\\[p\\.\\s*[0-9]+\\.?\\]",
            ignore_case = TRUE
          ),
          ""
        ) |>
        
        stringr::str_replace_all(
          stringr::regex(
            "\\|\\s*(?:[0-9]+|[ivxlcdm]+)",
            ignore_case = TRUE
          ),
          ""
        )
      
      
      # ------------------------------------------------------
      # Normalize whitespace while preserving paragraphs
      # ------------------------------------------------------
      
      text <- text |>
        
        stringr::str_replace_all(
          "[ \t]+\n",
          "\n"
        ) |>
        
        stringr::str_replace_all(
          "\n[ \t]+",
          "\n"
        ) |>
        
        stringr::str_replace_all(
          "[ \t]{2,}",
          " "
        ) |>
        
        stringr::str_replace_all(
          "\n{3,}",
          "\n\n"
        ) |>
        
        stringr::str_trim()
      
      
      text
    }
  )
  
  
  # ----------------------------------------------------------
  # Build OEHC dataframe
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
      
      original_date =
        "early 4th century",
      
      domain =
        NA_character_,
      
      region =
        "PALESTINE",
      
      author =
        "Eusebius of Caesarea",
      
      translator =
        "William Cureton",
      
      translation_year =
        "1861",
      
      translation_source_language =
        "Syriac",
      
      recension =
        "long",
      
      source_heading =
        headings,
      
      source_collection = paste(
        "Eusebius of Caesarea,",
        "History of the Martyrs in Palestine,",
        "ed. and trans. William Cureton",
        "(Williams and Norgate, 1861)"
      ),
      
      text_source_url =
        source_url,
      
      rights_status =
        "public_domain",
      
      genre =
        "martyr_act",
      
      collection =
        "ancient",
      
      source_text =
        sections
    )
  
  
  # ----------------------------------------------------------
  # Final validation
  # ----------------------------------------------------------
  
  if (
    any(
      is.na(result$source_text) |
      !nzchar(result$source_text)
    )
  ) {
    
    warning(
      "One or more confession sections produced empty text."
    )
  }
  
  
  if (nrow(result) != 17) {
    
    stop(
      paste0(
        "Expected 17 OEHC records but produced ",
        nrow(result),
        "."
      )
    )
  }
  
  
  result
}

## Scrape and save

# df_eusebius_martyrs <- scrape_eusebius_martyrs(start_text_id = "oehc_000393")
# 
# arrow::write_parquet(df_eusebius_martyrs, "derived/df_eusebius_martyrs.parquet")
# 
# df_eusebius_martyrs <- arrow::read_parquet("derived/df_eusebius_martyrs.parquet")
