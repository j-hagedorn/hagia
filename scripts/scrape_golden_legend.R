scrape_golden_legend <- function(
    volumes = 1:7,
    start_text_id = "oehc_000120",
    saints_only = TRUE,
    pause = 1
) {
  
  # Required packages:
  #
  # install.packages(c(
  #   "httr2", "rvest", "xml2",
  #   "stringr", "dplyr", "purrr", "tibble"
  # ))
  
  # ----------------------------------------------------------
  # Configuration
  # ----------------------------------------------------------
  
  base_url <- paste0(
    "https://sourcebooks.web.fordham.edu/",
    "basis/goldenlegend"
  )
  
  volume_url <- function(volume) {
    paste0(
      base_url,
      "/GoldenLegend-Volume",
      volume,
      ".asp"
    )
  }
  
  
  # ----------------------------------------------------------
  # Retrieve and parse an HTML page
  # ----------------------------------------------------------
  
  get_page <- function(url) {
    
    httr2::request(url) |>
      httr2::req_user_agent(
        paste(
          "Mozilla/5.0",
          "(academic research;",
          "Open English Hagiography Corpus)"
        )
      ) |>
      httr2::req_timeout(30) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform() |>
      httr2::resp_body_string() |>
      xml2::read_html()
  }
  
  
  # ----------------------------------------------------------
  # Extract the table of contents
  #
  # The complete volume pages use internal links such as:
  #
  #   <a href="#...">The Life of S. Nicholas</a>
  #
  # We use those anchors to identify the beginning and end
  # of each story.
  # ----------------------------------------------------------
  
  get_contents <- function(doc) {
    
    links <- doc |>
      rvest::html_elements("a[href^='#']")
    
    toc <- tibble::tibble(
      
      title = links |>
        rvest::html_text2() |>
        stringr::str_squish(),
      
      href = links |>
        rvest::html_attr("href")
      
    ) |>
      dplyr::mutate(
        
        fragment = href |>
          stringr::str_remove("^#") |>
          URLdecode()
        
      ) |>
      dplyr::filter(
        !is.na(fragment),
        nzchar(fragment),
        nzchar(title)
      ) |>
      
      # Back-links elsewhere in the document can repeat anchors.
      # The first occurrence should be the table-of-contents link.
      dplyr::distinct(
        fragment,
        .keep_all = TRUE
      )
    
    
    # Keep only anchors that actually have a corresponding
    # target somewhere in the document.
    has_target <- purrr::map_lgl(
      toc$fragment,
      function(fragment) {
        
        xpath <- paste0(
          "//*[@id='",
          fragment,
          "' or @name='",
          fragment,
          "']"
        )
        
        node <- xml2::xml_find_first(
          doc,
          xpath
        )
        
        !inherits(
          node,
          "xml_missing"
        )
      }
    )
    
    
    toc |> dplyr::filter(has_target)
  }
  
  
  # ----------------------------------------------------------
  # Extract text between two internal anchors
  #
  # start_fragment marks the current story.
  # next_fragment marks the beginning of the next story.
  #
  # Selecting text nodes between the two anchors avoids
  # relying on <p>, <div>, <h2>, etc., which is useful for
  # older HTML.
  # ----------------------------------------------------------
  
  extract_section <- function(
    doc,
    start_fragment,
    next_fragment
  ) {
    
    # ----------------------------------------------------------
    # Locate the anchor marking the beginning of this story
    # ----------------------------------------------------------
    
    start_xpath <- paste0(
      "//*[@id='",
      start_fragment,
      "' or @name='",
      start_fragment,
      "']"
    )
    
    start_node <- xml2::xml_find_first(
      doc,
      start_xpath
    )
    
    
    if (inherits(start_node, "xml_missing")) {
      return(NA_character_)
    }
    
    
    # ----------------------------------------------------------
    # Select text between this anchor and the next anchor
    # ----------------------------------------------------------
    
    if (!is.na(next_fragment)) {
      
      text_xpath <- paste0(
        "following::text()[",
        "following::*[",
        "@id='", next_fragment,
        "' or @name='", next_fragment,
        "']",
        "]"
      )
      
    } else {
      
      # Final story in the volume
      text_xpath <- "following::text()"
    }
    
    
    text_nodes <- xml2::xml_find_all(
      start_node,
      text_xpath
    )
    
    
    # ----------------------------------------------------------
    # Extract and clean individual text nodes
    # ----------------------------------------------------------
    
    text <- xml2::xml_text(
      text_nodes
    )
    
    text <- text |>
      stringr::str_replace_all("\u00A0", " ") |>
      stringr::str_squish()
    
    
    # Remove empty text nodes
    text <- text[
      !is.na(text) &
        nzchar(text)
    ]
    
    
    # If nothing remains, return one NA value
    if (length(text) == 0) {
      return(NA_character_)
    }
    
    
    # ----------------------------------------------------------
    # IMPORTANT:
    # Collapse all text nodes into ONE character string.
    #
    # map2_chr() requires one character value per story.
    # ----------------------------------------------------------
    
    paste(
      text,
      collapse = "\n\n"
    )
  }
  
  
  # ----------------------------------------------------------
  # Identify likely hagiographic sections
  #
  # Fordham also includes:
  #   - liturgical feasts
  #   - biblical histories
  #   - prologues
  #   - glossaries
  #   - indexes
  #
  # The initial rule is intentionally inclusive. It captures
  # titles referring to lives, saints, martyrs, and several
  # well-known collective hagiographic stories.
  #
  # Set saints_only = FALSE to retain every chapter instead.
  # ----------------------------------------------------------
  
  is_saint_story <- function(title) {
    
    stringr::str_detect(
      title,
      stringr::regex(
        paste(
          c(
            "\\bLife\\b",
            "\\bLives\\b",
            "\\bS\\.\\s",
            "\\bSS\\.\\s",
            "\\bSaint\\b",
            "\\bSaints\\b",
            "\\bMartyr\\b",
            "\\bMartyrs\\b",
            "Holy Innocents",
            "Seven Sleepers",
            "Seven Brethren",
            "Four Crowned",
            "Maccabees",
            "Barlaam and Josaphat"
          ),
          collapse = "|"
        ),
        ignore_case = TRUE
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Scrape one complete volume
  # ----------------------------------------------------------
  
  scrape_volume <- function(volume) {
    
    url <- volume_url(volume)
    
    message(
      "Scraping Golden Legend volume ",
      volume,
      "..."
    )
    
    doc <- get_page(url)
    
    toc <- get_contents(doc)
    
    
    if (nrow(toc) == 0) {
      stop(
        "No internal chapter anchors found in volume ",
        volume
      )
    }
    
    
    # The next anchor marks the endpoint for each section.
    toc <- toc |>
      dplyr::mutate(
        
        next_fragment = dplyr::lead(
          fragment
        ),
        
        chapter_order = dplyr::row_number()
      )
    
    
    # Extract each chapter/story.
    toc <- toc |>
      dplyr::mutate(
        
        source_text = purrr::map2_chr(
          fragment,
          next_fragment,
          ~ extract_section(
            doc,
            .x,
            .y
          )
        )
      )
    
    
    # Remove obvious paratext.
    toc <- toc |>
      dplyr::filter(
        !stringr::str_detect(
          title,
          stringr::regex(
            paste(
              c(
                "^Prologue$",
                "^The Golden Legend$",
                "^Glossary$",
                "^Appendix$",
                "Index of Saints",
                "^General Index$"
              ),
              collapse = "|"
            ),
            ignore_case = TRUE
          )
        )
      )
    
    
    # Optionally retain only saint-centered chapters.
    if (saints_only) {
      
      toc <- toc |>
        dplyr::filter(
          is_saint_story(title)
        )
    }
    
    
    toc |>
      dplyr::mutate(
        
        volume = volume,
        
        text_source_url = url
        
      ) |>
      dplyr::select(
        volume,
        chapter_order,
        title,
        text_source_url,
        source_text
      )
  }
  
  
  # ----------------------------------------------------------
  # Scrape requested volumes
  # ----------------------------------------------------------
  
  stories <- purrr::map_dfr(
    volumes,
    function(volume) {
      
      result <- scrape_volume(
        volume
      )
      
      if (pause > 0) {
        Sys.sleep(pause)
      }
      
      result
    }
  )
  
  
  # ----------------------------------------------------------
  # Generate sequential OEHC text IDs
  #
  # Example:
  # start_text_id = "oehc_000251"
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
      id_start + seq_len(nrow(stories)) - 1,
      width = id_width,
      pad = "0"
    )
  )
  
  
  # ----------------------------------------------------------
  # Construct OEHC-style metadata
  # ----------------------------------------------------------
  
  df_golden_legend <- stories |>
    dplyr::mutate(
      
      schema_version = "1.0",
      
      text_id = text_ids,
      
      # BHG/BHL/BHO are not supplied by Fordham.
      bhg = NA_character_,
      bhl = NA_character_,
      bho = NA_character_,
      
      saint_dates = NA_character_,
      
      # Date assigned to the Golden Legend compilation.
      original_date = "1275",
      
      domain = NA_character_,
      region = NA_character_,
      
      author = "Jacobus de Voragine",
      
      source_collection = paste(
        "The Golden Legend or Lives of the Saints;",
        "Temple Classics edition"
      ),
      
      rights_status = "public_domain",
      
      genre = "legendary",
      
      collection = "medieval_translations"
      
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
      source_collection,
      
      volume,
      chapter_order,
      
      text_source_url,
      
      rights_status,
      genre,
      collection,
      
      source_text
    )
}

## Scrape and save

# arrow::write_parquet(df_golden_legend, "derived/df_golden_legend.parquet")

# df_golden_legend <- arrow::read_parquet("derived/df_golden_legend.parquet")