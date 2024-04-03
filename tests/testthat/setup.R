# Extra libraries (suggested) for tests
library(dplyr)
library(tidyr)
library(lubridate)

# Helper function to reduce the number of levels in a column of a data frame
level_reducer <- function(dt, variable, p_to_keep = 0.7,
                          num_max_values = NULL, num_of_rare_values = 0, explorative = FALSE,
                          add_specific_value = NULL, keep_spec_rows = NULL) { # Latter no exploration
  checkmate::assert_number(p_to_keep, lower = 0, upper = 1)
  checkmate::assert_data_frame(dt)
  checkmate::assert_string(variable)
  checkmate::assert_character(add_specific_value, null.ok = TRUE)
  checkmate::assert_choice(variable, names(dt))
  checkmate::assert_integer(keep_spec_rows,
    null.ok = TRUE,
    lower = 1, upper = nrow(dt), unique = TRUE
  )
  checkmate::assert_flag(explorative)
  cur_vec <- dt[[variable]]

  if (is.factor(cur_vec)) {
    cur_vec <- as.character(cur_vec)
  }

  lev_freq <- sort(table(cur_vec), decreasing = TRUE)

  # Explorative plot
  if (explorative && interactive()) {
    require(ggplot2)
    plot_tbl <- tibble(
      level = names(lev_freq),
      freq = lev_freq
    ) %>%
      arrange(desc(freq)) %>%
      mutate(level = factor(level, levels = level))

    how_many_to_plot <- min(10, nrow(plot_tbl))
    spacing <- round(nrow(plot_tbl) / how_many_to_plot)
    gg <- ggplot(plot_tbl) +
      geom_col(aes(x = level, y = freq), width = 1) +
      theme_classic() +
      labs(title = paste0("Frequency of levels in ", variable), x = "Level", y = "Frequency") +
      scale_x_discrete(
        breaks = plot_tbl$level[seq(1, nrow(plot_tbl), by = spacing)],
        labels = seq(1, nrow(plot_tbl))[seq(1, nrow(plot_tbl), by = spacing)]
      )

    # Adding % annotation
    annot_x <- tail(plot_tbl$level[cumsum(plot_tbl$freq) <= sum(plot_tbl$freq) * p_to_keep], 1)
    annot_y <- 0.9 * max(plot_tbl$freq)
    annot_label <- paste0(
      "Desired: ", round(p_to_keep * 100, 1), "%",
      "\n", "Levels to keep: ", sum(cumsum(plot_tbl$freq) <= sum(plot_tbl$freq) * p_to_keep)
    )
    gg <- gg + geom_vline(aes(xintercept = tail(level[cumsum(freq) <= sum(freq) * p_to_keep], 1)), color = "red") +
      annotate("text", x = annot_x, y = annot_y, label = annot_label, vjust = 0, hjust = 0)

    if (!is.null(num_max_values)) {
      annot_x <- num_max_values - num_of_rare_values
      annot_y <- 0.5 * max(plot_tbl$freq)
      annot_label <- paste0(
        "Desired: ", num_max_values - num_of_rare_values,
        "\n Kept: ",
        round(sum(plot_tbl$freq[seq(1, num_max_values - num_of_rare_values)]) * 100 / sum(plot_tbl$freq), 1),
        "%"
      )
      gg <- gg + geom_vline(aes(xintercept = num_max_values - num_of_rare_values), color = "blue") +
        annotate("text", x = annot_x, y = annot_y, label = annot_label, vjust = 0, hjust = 0)
    }

    if (!is.null(add_specific_value)) {
      xint <- which(plot_tbl$level %in% add_specific_value)
      annot_x <- tail(which(plot_tbl$level %in% add_specific_value), 1)
      annot_y <- 0.1 * max(plot_tbl$freq)
      if (length(add_specific_value) == 1L) {
        annot_label <- paste0(
          "Specific value: ", add_specific_value,
          "\nAdded freq: ",
          round(plot_tbl$freq[which(plot_tbl$level == add_specific_value)] * 100 / sum(plot_tbl$freq), 1),
          "%\n",
          "Rank: ", which(plot_tbl$level == add_specific_value)
        )
      } else {
        xint <- max(xint)
        annot_label <- paste0(
          "Num spec values: ", length(add_specific_value),
          "\nAdded freq: ",
          round(sum(plot_tbl$freq[which(plot_tbl$level %in% add_specific_value)]) * 100 / sum(plot_tbl$freq), 1),
          "%\n",
          "Max rank: ", max(which(plot_tbl$level %in% add_specific_value))
        )
      }
      gg <- gg +
        geom_vline(aes(xintercept = xint), color = "black") +
        annotate("text", x = annot_x, y = annot_y, label = annot_label, vjust = 0, hjust = 0)
    }

    print(gg)

    #  Effective calculations
  } else {
    checkmate::assert_int(num_of_rare_values, lower = 0, upper = length(lev_freq))
    checkmate::assert_int(num_max_values, lower = num_of_rare_values, upper = length(lev_freq), null.ok = TRUE)

    if (!is.null(num_max_values)) {
      lev_to_keep <- names(lev_freq)[seq(1, num_max_values - num_of_rare_values)]

      if (num_of_rare_values > 0) {
        lev_to_keep <- c(
          lev_to_keep,
          names(lev_freq)[seq(length(lev_freq) - num_of_rare_values + 1, length(lev_freq))]
        )
      }
    } else {
      cum_freq <- cumsum(lev_freq) / sum(lev_freq)
      if (p_to_keep < min(cum_freq)) {
        stop(paste0("p_to_keep is too low. The minimum value of p_to_keep is ", round(min(cum_freq), 3)))
      }
      lev_to_keep <- names(lev_freq)[cum_freq <= p_to_keep]
    }

    if (!is.null(add_specific_value)) {
      checkmate::assert_subset(add_specific_value, names(lev_freq))
      lev_to_keep <- unique(c(lev_to_keep, add_specific_value))
    }

    if (!is.null(keep_spec_rows)) {
      lev_to_keep <- c(lev_to_keep, keep_spec_rows)
      filter1 <- which(cur_vec %in% lev_to_keep)
      keep_spec_rows <- keep_spec_rows[!keep_spec_rows %in% filter1]
    }

    if (interactive()) {
      msg <- paste0(
        "Reducing levels of ", deparse(substitute(dt)), " for variable ",
        variable, ": keeping ", length(lev_to_keep),
        " levels out of ", length(lev_freq), " levels. Total kept (%): ",
        round(sum(lev_freq[lev_to_keep]) * 100 / sum(lev_freq), 1)
      )
      if (length(keep_spec_rows) > 0) {
        msg <- paste0(
          msg, "\n", "Keeping other rows for a total: ",
          round((sum(lev_freq[lev_to_keep]) + length(keep_spec_rows)) * 100 / sum(lev_freq), 1)
        )
      }
      message(msg)
    }

    out <- dt %>% filter(!!sym(variable) %in% lev_to_keep)

    if (length(keep_spec_rows) > 0) {
      out <- rbind(out, dt %>% slice(keep_spec_rows))
    }

    # Simple check of filtering
    stopifnot(nrow(out) == sum(cur_vec %in% lev_to_keep))

    out
  }
}

# Data loading for tests
adsl_raw <- random.cdisc.data::cadsl
adab_raw <- random.cdisc.data::cadab
adae_raw <- random.cdisc.data::cadae
adaette_raw <- random.cdisc.data::cadaette
adcm_raw <- random.cdisc.data::cadcm
addv_raw <- random.cdisc.data::caddv
adeg_raw <- random.cdisc.data::cadeg
adex_raw <- random.cdisc.data::cadex
adhy_raw <- random.cdisc.data::cadhy
adlb_raw <- random.cdisc.data::cadlb
admh_raw <- random.cdisc.data::cadmh
adpc_raw <- random.cdisc.data::cadpc
adpp_raw <- random.cdisc.data::cadpp
adpc_raw <- random.cdisc.data::cadpc
adqs_raw <- random.cdisc.data::cadqs
adrs_raw <- random.cdisc.data::cadrs
adsub_raw <- random.cdisc.data::cadsub
adtte_raw <- random.cdisc.data::cadtte
advs_raw <- random.cdisc.data::cadvs

# Data loading for pharmaverse
adpp_pharmaverse <- pharmaverseadam::adpp
adpc_pharmaverse <- pharmaverseadam::adpc
set.seed(99)
adsl_pharmaverse <- pharmaverseadam::adsl %>%
  mutate(
    DCSREAS = sample(c("ADVERSE EVENT", ""), nrow(.), replace = TRUE, prob = c(0.08, 0.92)),
    DCSREAS = with_label(DCSREAS, "Discontinuation Reason")
  ) %>%
  filter(ACTARM != "Screen Failure")
adae_pharmaverse <- pharmaverseadam::adae %>%
  mutate(
    AETOXGR = sample(
      c("1", "2", "3", "4", "5"),
      nrow(.),
      replace = TRUE,
      prob = c(0.70, 0.20, 0.05, 0.045, 0.005)
    ),
    AECONTRT = sample(c("Y"), nrow(.), replace = TRUE, prob = c(0.95)),
    ANL01FL = "Y",
    SMQ01NAM = ifelse(AEDECOD %in% c("NAUSEA", "VOMITING"), "SMQ01", NA_character_),
    SMQ01SC = ifelse(SMQ01NAM == "SMQ01", "BROAD", NA_character_),
    SMQ02NAM = ifelse(AEDECOD == "SKIN IRRITATION", "SMQ02", NA_character_),
    SMQ02SC = ifelse(SMQ02NAM == "SMQ02", "NARROW", NA_character_),
    CQ01NAM = ifelse(AEDECOD == "HEADACHE", "CQ01", NA_character_)
  )
# adae_pharmaverse trimming of variables with too many levels
adae_pharmaverse <- level_reducer(adae_pharmaverse, "AEDECOD",
  num_max_values = 7, num_of_rare_values = 1,
  add_specific_value = c("VOMITING", "NAUSEA", "SKIN IRRITATION", "HEADACHE")
)

set.seed(NULL)
adlb_pharmaverse <- pharmaverseadam::adlb %>%
  mutate(AVALU = LBORRESU)
needed_paramcd <- adlb_pharmaverse %>%
  filter(PARAMCD %in% c("AST", "ALT", "BILI", "ALB")) %>% # needed for lbt11 and lbt13
  pull(PARAM) %>%
  unique()

adlb_pharmaverse <- level_reducer(
  adlb_pharmaverse, "PARAM",
  num_max_values = 10, num_of_rare_values = 1,
  add_specific_value = c(needed_paramcd, "Sodium (mmol/L)") # for ATOXGR == "-3" in lbt07
)
advs_pharmaverse <- pharmaverseadam::advs


# skip_if_too_deep
skip_if_too_deep <- function(depth) { # nolint
  checkmate::assert_number(depth, lower = 0, upper = 5)

  testing_depth <- getOption("TESTING_DEPTH")
  if (is.null(testing_depth)) testing_depth <- Sys.getenv("TESTING_DEPTH")

  testing_depth <- tryCatch(
    as.numeric(testing_depth),
    error = function(error) 3,
    warning = function(warning) 3
  )

  if (length(testing_depth) != 1 || is.na(testing_depth)) testing_depth <- 3

  if (testing_depth < depth) {
    testthat::skip(paste("testing depth", testing_depth, "is below current testing specification", depth))
  }
}
