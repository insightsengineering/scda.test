library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(rlistings)
library(junco)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSIDEM02"
fileid <- write_path(opath, tblid)
popfl <- "RANDFL"
trtvar <- "TRT01P"
key_cols <- c("COL1")
sort_cols <- c("COL1")
disp_cols <- paste0("COL", 1:9)
concat_sep <- " / "
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)


###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c(
        "Xanomeline Low Dose",
        "Xanomeline High Dose",
        "Placebo"
      )
    ),
    SEX = factor(
      case_when(SEX == "M" ~ "Male", SEX == "F" ~ "Female", TRUE ~ SEX),
      levels = c("Male", "Female", "Intersex", "Unknown")
    ),
    COUNTRY = factor(
      case_when(
        COUNTRY == "USA" ~ "United States",
        TRUE ~ NA_character_
      )
    ),
    RACE = factor(
      case_when(
        RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ "American Indian or Alaska Native",
        RACE == "ASIAN" ~ "Asian",
        RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black or African American",
        RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ "Native Hawaiian or other Pacific Islander",
        RACE == "WHITE" ~ "White",
        RACE == "MULTIPLE" ~ "Multiple",
        RACE == "NOT REPORTED" ~ "Not reported",
        RACE == "UNKNOWN" ~ "Unknown",
        RACE == "OTHER" ~ "Other"
      ),
      levels = c(
        "American Indian or Alaska Native",
        "Asian",
        "Black or African American",
        "Native Hawaiian or other Pacific Islander",
        "White",
        "Multiple",
        "Not reported",
        "Unknown",
        "Other"
      )
    ),
    ETHNIC = factor(
      case_when(
        ETHNIC == "HISPANIC OR LATINO" ~ "Hispanic or Latino",
        ETHNIC == "NOT HISPANIC OR LATINO" ~ "Not Hispanic or Latino",
        ETHNIC == "NOT REPORTED" ~ "Not reported",
        ETHNIC == "UNKNOWN" ~ "Unknown"
      ),
      levels = c("Hispanic or Latino", "Not Hispanic or Latino", "Not reported", "Unknown")
    )
  ) |>
  filter(!!rlang::sym(popfl) == "Y")

# To determine the count of STRAT variables and it's expressions programmatically
# Identify the STRAT columns
strat_cols <- grep("^STRAT\\d+R$", names(adsl), value = TRUE)

# Sort them in numeric order
strat_cols <- strat_cols[order(as.numeric(gsub("\\D", "", strat_cols)))]

# To build expressions for STRAT vars
strat_expr <- lapply(strat_cols, function(x) {
  expr(explicit_na(!!sym(x), ""))
})

# To build fixed expression of mutate for fixed columns
fixed_expr <- list(
  # col1
  expr(explicit_na(USUBJID, "")),
  # col2
  expr(explicit_na(REGION1, "")),
  # col3
  expr(explicit_na(COUNTRY, "")),
  # col4
  expr(case_when(
    !is.na(RANDDT) & any(names(adsl) == "RANDDTM") & !is.na(RANDDTM) ~
      paste0(
        toupper(format(RANDDT, "%d%b%Y")),
        concat_sep,
        substr(RANDDTM, 12, 16)
      ),
    !is.na(RANDDT) & any(names(adsl) == "RANDDTM") & is.na(RANDDTM) ~
      paste0(toupper(format(RANDDT, "%d%b%Y")), concat_sep, "--:--"),
    !is.na(RANDDT) & !any(names(adsl) == "RANDDTM") ~ paste0(toupper(format(RANDDT, "%d%b%Y"))),
  )),
  # col5
  expr(explicit_na(RANUM, ""))
)

# final list including the STRAT columns
expr_list <- c(fixed_expr, strat_expr)

# assign dynamic column names and define end col number
names(expr_list) <- paste0("COL", seq(from = 1, length.out = length(expr_list)))

# end of STRAT col number to assign next coloumn name dynamically
end_num <- length(expr_list)

# Adding last two variables expressions based on end STRAT COL number
expr_list[[paste0("COL", end_num + 1)]] <- expr(explicit_na(TRT01P, ""))
expr_list[[paste0("COL", end_num + 2)]] <- expr(explicit_na(TRT01A, ""))

# Adding COL names and it's expressions
lsting <- adsl |>
  mutate(!!!expr_list) |>
  arrange(COL1)

# To create a list of variables and it's labels programmatically to include in the output
# list of COL names
col_names <- colnames(lsting)[grep("^COL\\d+$", colnames(lsting))]

# Fixed COL labels
fixed_labels <- c(
  # col1
  "Subject ID",
  # col2
  "Region",
  # col3
  paste("Country", "Territory", sep = concat_sep),
  # col4
  paste("Randomization Date", "Time", sep = concat_sep),
  # col5
  "Randomization Number"
)

# To pull labels of STRATxD variables from ADSL
stratd_cols <- sub(".$", "D", strat_cols)

strat_labels <- setNames(
  lapply(stratd_cols, function(str) {
    lbl <- attr(adsl[[str]], "label")

    if (is.null(lbl) || is.na(lbl) || lbl == "") {
      str
    } else {
      lbl
    }
  }),
  stratd_cols
)

# End fixed column labels
end_labels <- c(
  "Randomization Treatment Assignment",
  "Actual Treatment"
)

# All labels
all_labels <- c(fixed_labels, strat_labels, end_labels)

# COL and Label list
label_list <- as.list(setNames(all_labels, col_names))

# Adding COL names and it's labels
for (nm in names(label_list)) {
  attr(lsting[[nm]], "label") <- label_list[[nm]]
}

###############################################################################
# Build listing
###############################################################################

result <- rlistings::as_listing(
  df = lsting,
  key_cols = c("COL1"),
  disp_cols = disp_cols,
  sort_cols = sort_cols,
  round_type = "sas"
)

###############################################################################
# Add titles and footnotes
###############################################################################

result <- set_titles(result, tab_titles)

###############################################################################
# Output listing
###############################################################################


colwidth <- c(25, 16, 17, 26, 26, 72, 48, 26, 21)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
