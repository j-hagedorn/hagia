
library(dplyr)
library(purrr)

latin_datasets <- list(
  jean_de_mailly = df_jean_de_mailly,
  ado_pl123 = df_ado_pl123,
  historia_tripartita = df_historia_tripartita
) |>
  imap(
    ~ mutate(
      .x,
      dataset = .y
    )
  )

common_cols <- Reduce(
  intersect,
  lapply(latin_datasets, names)
)

df_latin <- latin_datasets |>
  map(
    ~ select(
      .x,
      all_of(common_cols)
    )
  ) |>
  bind_rows()

# ============================================================
# OEHC LATIN -> ENGLISH TRANSLATION PIPELINE
#
# Input:
#   df_latin
#
# Required columns:
#   text_id
#   source_text
#
# Strategy:
#   - translate each source record whole
#   - one fresh Claude conversation per record
#   - checkpoint after every translation
#   - resume interrupted runs
# ============================================================

library(tidyverse)
library(purrr)
library(ellmer)

# ------------------------------------------------------------
# 1. Translation specification
# ------------------------------------------------------------

translation_prompt <- paste(
  "You are translating premodern Christian hagiographic prose",
  "from Latin into English for computational analysis of motifs,",
  "themes, entities, actions, and recurrent concepts.",
  "",
  "Translate the complete supplied Latin text into clear modern",
  "English while prioritizing semantic fidelity and terminological",
  "consistency over literary elegance.",
  "",
  "Requirements:",
  "- Preserve the meaning of the Latin closely.",
  "- Preserve explicit repetition. Do not replace repeated words",
  "  with synonyms merely for stylistic variety.",
  "- Use consistent English equivalents for recurring Latin words",
  "  and phrases when they have the same sense.",
  "- Do not embellish, summarize, omit, intensify, soften, or",
  "  unnecessarily paraphrase the source.",
  "- Preserve divine and supernatural terminology closely.",
  "- Do not secularize or naturalize supernatural claims.",
  "- Preserve agency, negation, modality, temporal sequence,",
  "  and explicit causal relationships.",
  "- Do not silently resolve ambiguous pronouns, referents, syntax,",
  "  or lexical meanings. Preserve ambiguity where English permits.",
  "- If English requires a choice, use the least interpretive",
  "  reasonable wording.",
  "- Preserve proper names and ecclesiastical or social titles",
  "  consistently.",
  "- Do not over-classicize medieval Latin usage.",
  "- Do not harmonize wording with familiar biblical translations",
  "  unless the Latin itself requires it.",
  "- Preserve paragraph boundaries where possible.",
  "- Translate all substantive material, including repetitive,",
  "  formulaic, awkward, or difficult passages.",
  "",
  "Return only the English translation.",
  "Do not provide commentary, notes, explanations, headings,",
  "confidence scores, or summaries.",
  sep = "\n"
)


translation_prompt_version <- "oehc_latin_v1.0"


# ------------------------------------------------------------
# 2. Word count helper
# ------------------------------------------------------------

count_words <- function(x) {
  
  if (
    is.na(x) ||
    !nzchar(str_trim(x))
  ) {
    return(0L)
  }
  
  str_count(
    str_squish(x),
    "\\S+"
  )
}


# ------------------------------------------------------------
# 3. Claude factory
#
# Change only `model` here if you later choose another Claude
# model.
#
# Requires ANTHROPIC_API_KEY in the environment.
# ------------------------------------------------------------

make_claude_chat <- function(
    model = "claude-sonnet-5",
    max_tokens = 20000
) {
  
  ellmer::chat_anthropic(
    model = model,
    system_prompt = translation_prompt,
    params = ellmer::params(
      max_tokens = max_tokens
    ),
    cache = "none"
  )
}


# ------------------------------------------------------------
# 4. Translate one text
# ------------------------------------------------------------

translate_latin_text <- function(
    source_text,
    text_id,
    model = "claude-sonnet-5",
    max_tokens = 20000
) {
  
  if (
    is.na(source_text) ||
    !nzchar(str_trim(source_text))
  ) {
    
    return(
      list(
        translation = NA_character_,
        status = "empty_source",
        error = NA_character_
      )
    )
  }
  
  
  result <- tryCatch(
    
    {
      
      # Fresh conversation for every OEHC record.
      
      chat <- make_claude_chat(
        model = model,
        max_tokens = max_tokens
      )
      
      
      translation <- chat$chat(
        paste0(
          "Text ID: ",
          text_id,
          "\n\n",
          "LATIN SOURCE TEXT:\n\n",
          source_text
        ),
        echo = "none"
      )
      
      
      list(
        translation = str_trim(
          translation
        ),
        status = "translated",
        error = NA_character_
      )
    },
    
    error = function(e) {
      
      list(
        translation = NA_character_,
        status = "error",
        error = conditionMessage(e)
      )
    }
  )
  
  
  result
}


# ------------------------------------------------------------
# 5. Translate complete dataframe
# ------------------------------------------------------------

translate_latin_corpus <- function(
    df,
    model = "claude-sonnet-5",
    max_tokens = 20000,
    checkpoint_file = "df_latin_translation_checkpoint.rds",
    resume = TRUE
) {
  
  # ----------------------------------------------------------
  # Validate input
  # ----------------------------------------------------------
  
  required_columns <- c(
    "text_id",
    "source_text"
  )
  
  
  missing_columns <- setdiff(
    required_columns,
    names(df)
  )
  
  
  if (length(missing_columns) > 0) {
    
    stop(
      "Missing required column(s): ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  }
  
  
  if (anyDuplicated(df$text_id)) {
    
    stop(
      "text_id must be unique."
    )
  }
  
  
  # ----------------------------------------------------------
  # Resume or initialize
  # ----------------------------------------------------------
  
  if (
    resume &&
    file.exists(checkpoint_file)
  ) {
    
    message(
      "Loading checkpoint: ",
      checkpoint_file
    )
    
    
    output <- readRDS(
      checkpoint_file
    )
    
    
    if (!setequal(
      output$text_id,
      df$text_id
    )) {
      
      stop(
        "Checkpoint text_ids do not match the current dataframe."
      )
    }
    
    
    output <- output[
      match(
        df$text_id,
        output$text_id
      ),
    ]
    
  } else {
    
    output <- df |>
      
      mutate(
        
        source_word_count =
          map_int(
            source_text,
            count_words
          ),
        
        translation =
          NA_character_,
        
        translation_status =
          "pending",
        
        translation_provider =
          "Anthropic",
        
        translation_model =
          model,
        
        translation_prompt_version =
          translation_prompt_version,
        
        translation_max_tokens =
          max_tokens,
        
        translated_at =
          as.POSIXct(
            NA
          ),
        
        translation_error =
          NA_character_
      )
  }
  
  
  # ----------------------------------------------------------
  # Translate pending or previously failed records
  # ----------------------------------------------------------
  
  pending <- which(
    output$translation_status %in%
      c(
        "pending",
        "error"
      )
  )
  
  
  if (length(pending) == 0) {
    
    message(
      "No untranslated records remain."
    )
    
    return(
      output
    )
  }
  
  
  for (j in seq_along(pending)) {
    
    i <- pending[[j]]
    
    
    message(
      "[",
      j,
      "/",
      length(pending),
      "] Translating ",
      output$text_id[[i]],
      " — ",
      output$source_word_count[[i]],
      " words"
    )
    
    
    translated <- translate_latin_text(
      
      source_text =
        output$source_text[[i]],
      
      text_id =
        output$text_id[[i]],
      
      model =
        model,
      
      max_tokens =
        max_tokens
    )
    
    
    output$translation[[i]] <-
      translated$translation
    
    
    output$translation_status[[i]] <-
      translated$status
    
    
    output$translation_provider[[i]] <-
      "Anthropic"
    
    
    output$translation_model[[i]] <-
      model
    
    
    output$translation_prompt_version[[i]] <-
      translation_prompt_version
    
    
    output$translation_max_tokens[[i]] <-
      max_tokens
    
    
    output$translated_at[[i]] <-
      Sys.time()
    
    
    output$translation_error[[i]] <-
      translated$error
    
    
    # Save immediately after each record.
    
    saveRDS(
      output,
      checkpoint_file
    )
  }
  
  
  message(
    "Translation run complete."
  )
  
  
  output
}


###

df_latin_translated <- 
  translate_latin_corpus(
    df = df_latin,
    model = "claude-sonnet-5",
    checkpoint_file = "df_latin_translation_checkpoint.rds"
  )