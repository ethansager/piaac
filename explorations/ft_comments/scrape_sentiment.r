#!/usr/bin/env Rscript
# =============================================================================
# FT Article Comments: Scrape + Sentiment Analysis
# Article: https://www.ft.com/content/e2ddd496-4f07-4dc8-a47c-314354da8d46
#
# Coral endpoint discovered via Network tab:
#   GET https://ft.coral.coralproject.net/api/graphql
#   Uses persisted query ID: fe6db435838489b39afeb993214af799
#
# AUTHENTICATION:
#   Copy the cookie: header from the graphql request in Firefox DevTools
#   Run:
#     FT_COOKIE="paste_full_cookie_string_here" \
#       Rscript explorations/ft_comments/scrape_sentiment.r
# =============================================================================

library(tidyverse)
library(httr2)
library(tidytext)
library(ggplot2)
library(jsonlite)

# Load AFINN lexicon directly (avoids textdata package dependency)
load_afinn <- function() {
  cache_path <- here::here("explorations/ft_comments/output/afinn.rds")
  if (file.exists(cache_path)) return(readRDS(cache_path))
  message("Downloading AFINN lexicon...")
  raw <- read_tsv(
    "https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-111.txt",
    col_names = c("word", "value"),
    col_types = "ci",
    show_col_types = FALSE
  )
  saveRDS(raw, cache_path)
  raw
}

set.seed(20260414)

# --- Config ------------------------------------------------------------------

STORY_ID       <- "e2ddd496-4f07-4dc8-a47c-314354da8d46"
CORAL_ENDPOINT <- "https://ft.coral.coralproject.net/api/graphql"
QUERY_ID       <- "fe6db435838489b39afeb993214af799"  # FT's persisted query

# No auth needed — Coral API is public for this article

OUT_DIR <- here::here("explorations/ft_comments/output")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Colours -----------------------------------------------------------------

positive_green <- "#15803d"
negative_red   <- "#b91c1c"
neutral_gray   <- "#525252"
primary_blue   <- "#012169"

# --- Fetch comments ----------------------------------------------------------

make_request <- function(cursor = NULL) {
  variables <- list(
    count          = 200L,
    orderBy        = "CREATED_AT_DESC",
    storyID        = STORY_ID,
    tag            = NULL,
    flattenReplies = TRUE,
    ratingFilter   = NULL,
    refreshStream  = FALSE
  )
  if (!is.null(cursor)) variables$cursor <- cursor

  request(CORAL_ENDPOINT) |>
    req_method("GET") |>
    req_url_query(
      query     = "",
      id        = QUERY_ID,
      variables = toJSON(variables, auto_unbox = TRUE, null = "null")
    ) |>
    req_headers(
      "User-Agent"        = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:149.0) Gecko/20100101 Firefox/149.0",
      "Accept"            = "*/*",
      "Content-Type"      = "application/json",
      "Origin"            = "https://www.ft.com",
      "Referer"           = "https://www.ft.com/",
      "X-Coral-Client-ID" = "aea778f0-3856-11f1-9037-53c7772ee4aa"
    ) |>
    req_error(is_error = \(r) FALSE)
}

strip_html <- function(x) {
  x |>
    str_replace_all("<[^>]+>", " ") |>
    str_replace_all("&amp;", "&") |>
    str_replace_all("&lt;", "<") |>
    str_replace_all("&gt;", ">") |>
    str_replace_all("&quot;", '"') |>
    str_replace_all("&#39;", "'") |>
    str_squish()
}

parse_edges <- function(edges) {
  map_dfr(edges, function(e) {
    n <- e$node
    tibble(
      comment_id  = n$id %||% NA_character_,
      created_at  = n$createdAt %||% NA_character_,
      author      = n$author$username %||% "[anonymous]",
      body        = n$body %||% "",
      reply_count = as.integer(length(n$replies$edges) %||% 0L),
      reactions   = as.integer(n$actionCounts$reaction$total %||% 0L)
    )
  })
}

fetch_comments <- function() {
  all_edges <- list()
  cursor    <- NULL
  page      <- 1L

  message("Fetching comments from Coral API (paginating)...")

  repeat {
    resp <- tryCatch(
      req_perform(make_request(cursor)),
      error = \(e) stop("Request failed: ", e$message)
    )

    if (resp_status(resp) != 200) {
      stop(sprintf("HTTP %d on page %d", resp_status(resp), page))
    }

    parsed    <- resp_body_json(resp, simplifyVector = FALSE)
    story     <- parsed$data$story
    edges     <- story$comments$edges
    page_info <- story$comments$pageInfo

    message(sprintf("  Page %d: %d comments", page, length(edges)))
    all_edges <- c(all_edges, edges)

    if (!isTRUE(page_info$hasNextPage)) break
    cursor <- page_info$endCursor
    page   <- page + 1L
    Sys.sleep(0.5)
  }

  message(sprintf("Total comments retrieved: %d", length(all_edges)))

  parse_edges(all_edges) |>
    mutate(
      created_at = lubridate::ymd_hms(created_at, tz = "UTC"),
      body       = strip_html(body)
    ) |>
    filter(nchar(body) > 0)
}

comments <- fetch_comments()
saveRDS(comments, file.path(OUT_DIR, "comments_raw.rds"))
write_csv(comments, file.path(OUT_DIR, "comments_raw.csv"))

# --- Classification: Denial / Acceptance / Other -----------------------------
# Keyword-based classifier. A comment is scored on denial and acceptance
# signal words; the stronger signal wins. Ties and weak signals -> Other.

message("Classifying comments...")

# Denial: disputes or pushes back on the article's claims
denial_patterns <- paste(c(
  "don't believe", "do not believe", "disagree", "i doubt",
  "not convinced", "not sure i", "actually", "misleading", "wrong",
  "nonsense", "rubbish", "overstated", "exaggerated", "overblown",
  "not the case", "flawed", "questionable", "debatable", "correlation",
  "causation", "methodology", "sample size", "anecdotal", "cherry.pick",
  "counterpoint", "counter.point", "however", "but actually",
  "what about", "in reality", "in fact", "on the contrary",
  "i would argue", "i'd argue", "not true", "false premise",
  "confirmation bias", "nuance", "more complex", "not that simple",
  "doesn't follow", "does not follow", "selective"
), collapse = "|")

# Acceptance: agrees with or validates the article
acceptance_patterns <- paste(c(
  "i agree", "absolutely", "spot on", "exactly right", "well said",
  "confirms", "as someone who", "in my experience", "i have seen",
  "i've seen", "this is true", "this rings true", "clearly",
  "obviously", "unfortunately", "sadly", "alarming", "concerning",
  "worrying", "worried", "we must", "we need to", "must act",
  "well.written", "great article", "important article", "thank.*author",
  "thank.*writ", "heartfelt", "resonates", "couldn't agree more",
  "could not agree more", "same here", "my experience too",
  "this is real", "happening here", "happening in", "i see this",
  "witnessed", "observed", "no surprise", "unsurprising",
  "as expected", "as predicted"
), collapse = "|")

classify_comment <- function(text) {
  t <- tolower(text)
  denial_hits     <- lengths(regmatches(t, gregexpr(denial_patterns,     t, perl = TRUE)))
  acceptance_hits <- lengths(regmatches(t, gregexpr(acceptance_patterns, t, perl = TRUE)))
  case_when(
    denial_hits == 0 & acceptance_hits == 0 ~ "Other",
    denial_hits > acceptance_hits           ~ "Denial",
    acceptance_hits > denial_hits           ~ "Acceptance",
    TRUE                                    ~ "Other"   # tie -> Other
  )
}

comment_classified <- comments |>
  mutate(category = classify_comment(body),
         category = factor(category, levels = c("Acceptance", "Denial", "Other")))

saveRDS(comment_classified, file.path(OUT_DIR, "comment_classified.rds"))
write_csv(
  comment_classified |> select(-body),
  file.path(OUT_DIR, "comment_classified.csv")
)

# --- Summary -----------------------------------------------------------------

cat("\n=== CLASSIFICATION SUMMARY ===\n")
cat(sprintf("Total comments: %d\n\n", nrow(comment_classified)))

comment_classified |>
  count(category) |>
  mutate(pct = scales::percent(n / sum(n), accuracy = 0.1)) |>
  print()

cat("\nSample Acceptance comments:\n")
comment_classified |>
  filter(category == "Acceptance") |>
  slice_head(n = 3) |>
  select(author, body) |>
  mutate(body = str_trunc(body, 160)) |>
  print(width = Inf)

cat("\nSample Denial comments:\n")
comment_classified |>
  filter(category == "Denial") |>
  slice_head(n = 3) |>
  select(author, body) |>
  mutate(body = str_trunc(body, 160)) |>
  print(width = Inf)

# --- Figures -----------------------------------------------------------------

theme_custom <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", color = primary_blue),
      plot.subtitle   = element_text(color = neutral_gray),
      legend.position = "bottom"
    )
}

pal <- c(Acceptance = positive_green, Denial = negative_red, Other = neutral_gray)

# 1. Breakdown bar
p_bar <- comment_classified |>
  count(category) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = category, y = pct, fill = category)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), vjust = -0.4, size = 4.5) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "FT reader response to literacy decline article",
    subtitle = sprintf("n = %d comments | keyword-based classification", nrow(comment_classified)),
    x        = NULL,
    y        = "Share of comments"
  ) +
  theme_custom()

ggsave(file.path(OUT_DIR, "classification_breakdown.pdf"), p_bar, width = 8, height = 5)
ggsave(file.path(OUT_DIR, "classification_breakdown.png"), p_bar, width = 8, height = 5, dpi = 150)

# 2. Top denial keywords
denial_kw <- c(
  "disagree", "actually", "misleading", "wrong", "flawed", "nonsense",
  "questionable", "correlation", "however", "nuance", "overstated",
  "exaggerated", "methodology", "not the case", "in reality"
)

acceptance_kw <- c(
  "agree", "absolutely", "unfortunately", "sadly", "alarming",
  "concerning", "worrying", "clearly", "confirms", "resonates",
  "witnessed", "observed", "unsurprising", "heartfelt", "spot on"
)

count_kw <- function(texts, keywords) {
  map_dfr(keywords, function(kw) {
    tibble(
      keyword = kw,
      n       = sum(str_detect(tolower(texts), fixed(kw)))
    )
  }) |> filter(n > 0) |> arrange(desc(n))
}

denial_counts     <- count_kw(comments$body, denial_kw)     |> mutate(group = "Denial")
acceptance_counts <- count_kw(comments$body, acceptance_kw) |> mutate(group = "Acceptance")

kw_combined <- bind_rows(denial_counts, acceptance_counts) |>
  group_by(group) |>
  slice_max(n, n = 12) |>
  ungroup() |>
  mutate(keyword = reorder_within(keyword, n, group))

p_kw <- ggplot(kw_combined, aes(x = n, y = keyword, fill = group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~group, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_manual(values = c(Denial = negative_red, Acceptance = positive_green)) +
  labs(
    title = "Signal keywords by category",
    x     = "Number of comments containing keyword",
    y     = NULL
  ) +
  theme_custom()

ggsave(file.path(OUT_DIR, "classification_keywords.pdf"), p_kw, width = 12, height = 6)
ggsave(file.path(OUT_DIR, "classification_keywords.png"), p_kw, width = 12, height = 6, dpi = 150)

# --- Sub-classify "Other" comments ------------------------------------------

message("Sub-classifying Other comments...")

lament_patterns <- paste(c(
  "we are doomed", "going downhill", "declining", "decay", "collapse",
  "dumbing down", "dumb", "idiocracy", "thick", "stupid", "mediocrity",
  "worrying trend", "terrifying", "frightening", "bleak", "depressing",
  "no surprise", "unsurprising", "inevitable", "too late", "lost cause",
  "reverting", "phase", "end of", "sad state", "dire", "grim",
  "gulf", "chilling", "uncomfortable", "gulp", "alas", "lament",
  "regrettably", "unfortunately", "sadly", "shame", "pity"
), collapse = "|")

anecdote_patterns <- paste(c(
  "in my", "i work", "i teach", "i have", "i've", "my students",
  "my children", "my daughter", "my son", "my kids", "my school",
  "my workplace", "at my", "where i", "i see this", "i notice",
  "i struggle", "i used to", "when i was", "years ago i",
  "as a teacher", "as a parent", "as someone", "can totally see",
  "i had to deal", "i asked"
), collapse = "|")

political_patterns <- paste(c(
  "trump", "labour", "tory", "tories", "republican", "democrat",
  "conservative", "liberal", "starmer", "gove", "maga", "right.wing",
  "left.wing", "government", "politicians", "minister", "party",
  "brexit", "election", "vote", "voters", "elected"
), collapse = "|")

prescription_patterns <- paste(c(
  "we should", "we must", "we need", "should be taught", "should teach",
  "ought to", "have to", "need to invest", "need to focus",
  "solution is", "answer is", "fix is", "recommend", "encourage",
  "delete your", "ringfence", "invest in", "reform", "overhaul",
  "curriculum", "policy", "mandate"
), collapse = "|")

reference_patterns <- paste(c(
  "postman", "mcluhan", "mccluhan", "orwell", "huxley", "plato",
  "study", "research", "paper", "professor", "university", "lse",
  "according to", "published", "found that", "data show", "survey",
  "percent", "per cent", "statistics", "evidence", "empirical",
  "aristotle", "kant", "darwin", "evolution", "neuroscien",
  "book", "wrote", "argued", "essay", "analysis", "report"
), collapse = "|")

praise_patterns <- paste(c(
  "wonderful article", "great article", "good article", "excellent article",
  "well written", "well.written", "thank you", "thanks for", "more like this",
  "share it", "share this", "post it", "kudos", "bravo",
  "appreciate", "much appreciated", "love this", "brilliant piece",
  "important piece", "timely piece", "good read"
), collapse = "|")

critique_patterns <- paste(c(
  "confused", "conflated", "misses the mark", "poorly written",
  "atrocious", "embarrassing", "rambling", "meandering",
  "no clear point", "no argument", "what is the point",
  "inconsistencies", "contradicts", "own goal", "ill-thought",
  "missed the point", "surface level", "shallow analysis",
  "clickbait", "vague", "platitude"
), collapse = "|")

humor_patterns <- paste(c(
  "idiocracy", "prophecies", "that says it all", "er yeh", "wot she said",
  "innit", "that'll fix it", "next question", "becoming\\?",
  "green's theorem", "stable geni", "if those.*could read",
  "roman toilet", "circa.*ad", "very stable", "fanstastic",
  "upside down", "surreal", "ha ", "haha", "lol", ":-",
  "pseudonym", "apologies.*sarah", "tongue.*cheek"
), collapse = "|")

question_patterns <- paste(c(
  "what is finland", "does this article explain", "what does.*mean",
  "what is the reading", "fleisch", "kincaid", "grade level",
  "i wonder why", "i'm curious", "just curious", "curious.*impact",
  "does fertility", "why finland", "why does finland", "how does finland",
  "\\?$", "explain it\\?", "next question"
), collapse = "|")

nuance_patterns <- paste(c(
  "caution when conflat", "not the same thing", "literacy.*not.*intelligence",
  "intelligence.*not.*literacy", "worth noting", "it is worth",
  "distinction between", "difference between", "more nuanced",
  "not that simple", "complex.*issue", "encouraging that",
  "shows the difference", "bucks the trend", "technology to acquire",
  "past the accent", "look past"
), collapse = "|")

contentious_patterns <- paste(c(
  "bell curve", "low-skilled migrants", "backward nations",
  "hordes", "flooded it", "censor me", "diversity.*strength",
  "mantra", "pandora"
), collapse = "|")

classify_other <- function(text) {
  t <- tolower(text)
  scores <- c(
    Lament       = lengths(regmatches(t, gregexpr(lament_patterns,       t, perl = TRUE))),
    Anecdote     = lengths(regmatches(t, gregexpr(anecdote_patterns,     t, perl = TRUE))),
    Political    = lengths(regmatches(t, gregexpr(political_patterns,    t, perl = TRUE))),
    Prescription = lengths(regmatches(t, gregexpr(prescription_patterns, t, perl = TRUE))),
    Reference    = lengths(regmatches(t, gregexpr(reference_patterns,    t, perl = TRUE))),
    Praise       = lengths(regmatches(t, gregexpr(praise_patterns,       t, perl = TRUE))),
    Critique     = lengths(regmatches(t, gregexpr(critique_patterns,     t, perl = TRUE))),
    Humor        = lengths(regmatches(t, gregexpr(humor_patterns,        t, perl = TRUE))),
    Question     = lengths(regmatches(t, gregexpr(question_patterns,     t, perl = TRUE))),
    Nuance       = lengths(regmatches(t, gregexpr(nuance_patterns,       t, perl = TRUE))),
    Contentious  = lengths(regmatches(t, gregexpr(contentious_patterns,  t, perl = TRUE)))
  )
  if (max(scores) == 0) return("Unclassified")
  names(which.max(scores))
}

other_subclassified <- comment_classified |>
  filter(category == "Other") |>
  mutate(subcategory = map_chr(body, classify_other),
         subcategory = factor(subcategory, levels = c(
           "Lament", "Anecdote", "Political", "Prescription",
           "Reference", "Praise", "Critique",
           "Humor", "Question", "Nuance", "Contentious", "Unclassified"
         )))

cat("\n=== OTHER SUB-CATEGORIES ===\n")
other_subclassified |>
  count(subcategory, sort = TRUE) |>
  mutate(pct = scales::percent(n / sum(n), accuracy = 0.1)) |>
  print()

cat("\nSamples from each sub-category:\n")
walk(levels(other_subclassified$subcategory), function(cat) {
  rows <- other_subclassified |> filter(subcategory == cat)
  if (nrow(rows) == 0) return()
  cat(sprintf("\n--- %s (%d) ---\n", cat, nrow(rows)))
  rows |> slice_head(n = 2) |> pull(body) |>
    str_trunc(200) |> walk(~ cat(.x, "\n\n"))
})

saveRDS(other_subclassified, file.path(OUT_DIR, "other_subclassified.rds"))
write_csv(
  other_subclassified |> select(-body),
  file.path(OUT_DIR, "other_subclassified.csv")
)

# Figure: full breakdown — Acceptance/Denial as single bars, Other broken out
# Lament -> Acceptance; Critique -> Denial
full_breakdown <- bind_rows(
  comment_classified |>
    filter(category != "Other") |>
    mutate(label = as.character(category), group = as.character(category)),
  other_subclassified |>
    mutate(
      group = case_when(
        subcategory == "Lament"   ~ "Acceptance",
        subcategory == "Critique" ~ "Denial",
        TRUE                      ~ "Other"
      ),
      label = case_when(
        subcategory == "Lament"   ~ "Acceptance",
        subcategory == "Critique" ~ "Denial",
        TRUE                      ~ paste0("Other: ", subcategory)
      )
    )
) |>
  count(label, group) |>
  mutate(
    label = factor(label, levels = rev(c(
      "Acceptance",
      "Denial",
      "Other: Reference", "Other: Lament", "Other: Political",
      "Other: Humor", "Other: Question", "Other: Nuance",
      "Other: Prescription", "Other: Anecdote", "Other: Praise",
      "Other: Contentious", "Other: Unclassified"
    )))
  )

group_pal <- c(Acceptance = positive_green, Denial = negative_red, Other = neutral_gray)

p_full <- ggplot(full_breakdown, aes(x = n, y = label, fill = group)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  scale_fill_manual(values = group_pal, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "FT comment response — full breakdown",
    subtitle = sprintf("n = %d comments, literacy decline article", nrow(comment_classified)),
    x        = "Number of comments",
    y        = NULL
  ) +
  theme_custom() +
  theme(legend.position = "right")

ggsave(file.path(OUT_DIR, "full_breakdown.pdf"), p_full, width = 11, height = 7)
ggsave(file.path(OUT_DIR, "full_breakdown.png"), p_full, width = 11, height = 7, dpi = 150)

message("\nDone. Outputs written to: ", OUT_DIR)
