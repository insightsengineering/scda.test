library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(rlistings)
library(junco)
library(haven)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LPK01"
fileid <- write_path(opath, tblid)
popfl <- "PKFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1", "COL2")
sort_cols <- c("COL0", "COL1", "COL2")
disp_cols <- paste0("COL", 0:6)
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)
concat_sep <- " / "
paramcd <- "XAN"

# Set to NULL to keep all records including derived (DTYPE) records
exclude_dtype <- TRUE

###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj |>
  filter(.data[[popfl]] == "Y") |>
  select(USUBJID, all_of(c(trtvar, popfl))) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c(
        "Xanomeline Low Dose",
        "Xanomeline High Dose",
        "Placebo"
      )
    )
  )


adpc <- adpc_jnj |>
  filter(PARAMCD == paramcd, PKRFL == "N") |>
  filter(if (!is.null(exclude_dtype) && exclude_dtype) is.na(DTYPE) else TRUE) |>
  select(
    STUDYID,
    USUBJID,
    PKRFL,
    AVISIT,
    AVISITN,
    ADT,
    ADTM,
    ATPT,
    ATPTN,
    AVAL,
    any_of(starts_with("CRIT"))
  ) |>
  inner_join(adsl, by = c("USUBJID"))

lsting <- adpc |>
  mutate(
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = explicit_na(AVISIT, ""),
    COL3 = ifelse(
      !is.na(ADTM),
      paste0(
        toupper(format(as.Date(ADT), "%d%b%Y")),
        concat_sep,
        substr(as.character(ADTM), 12, 16)
      ),
      ifelse(!is.na(ADT), toupper(format(as.Date(ADT), "%d%b%Y")), "")
    ),
    # Time point is  optional
    COL4 = explicit_na(ATPT, ""),
    COL5 = explicit_na(as.character(AVAL), ""),
    # COL6: Concatenates exclusion reasons from CRIT* columns where CRIT*FL = "Y", separated by " ; "
    COL6 = apply(
      select(pick(everything()), matches("^CRIT[0-9]+FL$"), matches("^CRIT[0-9]+$")),
      1,
      function(row) {
        fl_vars <- names(row)[grepl("^CRIT[0-9]+FL$", names(row))]
        val_vars <- names(row)[grepl("^CRIT[0-9]+$", names(row))]
        vals <- sapply(fl_vars, function(fl) {
          num <- gsub("CRIT|FL", "", fl)
          val_var <- paste0("CRIT", num)
          if (isTRUE(row[[fl]] == "Y") && val_var %in% val_vars) row[[val_var]] else NA_character_
        })
        res <- paste(na.omit(stringr::str_to_sentence(vals)), collapse = " ; ")
        if (nchar(res) == 0) NA_character_ else res
      }
    ),
  ) |>
  arrange(
    COL0,
    COL1,
    AVISITN,
    ATPTN
  )

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = "Visit",
  COL3 = paste("Sample Date", "Time", sep = concat_sep),
  COL4 = "Time Point",
  COL5 = "Matrix Active Study Agent Conc. (\u00b5g/mL)",
  COL6 = "Reason for Exclusion"
)

###############################################################################
# Build listing
###############################################################################

result <- rlistings::as_listing(
  df = lsting,
  key_cols = key_cols,
  disp_cols = disp_cols,
  sort_cols = sort_cols,
  round_type = "sas"
)

###############################################################################
# Add titles and footnotes:
###############################################################################

result <- set_titles(result, tab_titles)

###############################################################################
# Output listing
###############################################################################


# [AUTO-COLWIDTH]

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
