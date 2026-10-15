library(envsetup)
library(tern)
library(dplyr)
library(tidyr)
library(rtables)
library(rlistings)
library(junco)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSIEX02"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1")
sort_cols <- c("COL0", "COL1")
disp_cols <- paste0("COL", 0:6)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")


###############################################################################
# Process data
###############################################################################

adex <- adex_jnj |>
  filter(!!rlang::sym(popfl) == "Y" & ADOSE > 0) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
    )
  )

lsting <- adex |>
  mutate(
    DAEXPDT = as.Date(DAEXPDTC, format = "%Y-%m-%d"),
    DAEXPYR = ifelse(
      stringr::str_length(sub("T.*", "", DAEXPDTC)) >= 4 &
        substr(sub("T.*", "", DAEXPDTC), 1, 4) != "----",
      substr(sub("T.*", "", DAEXPDTC), 1, 4),
      NA
    ),
    DAEXPMO = toupper(month.abb[
      as.numeric(ifelse(
        stringr::str_length(sub("T.*", "", DAEXPDTC)) >= 7 &
          substr(sub("T.*", "", DAEXPDTC), 6, 7) != "--",
        substr(sub("T.*", "", DAEXPDTC), 6, 7),
        NA
      ))
    ]),
    DAEXPDAY = ifelse(
      stringr::str_length(sub("T.*", "", DAEXPDTC)) >= 10 &
        substr(sub("T.*", "", DAEXPDTC), 9, 10) != "--",
      substr(sub("T.*", "", DAEXPDTC), 9, 10),
      NA
    ),
  ) |>
  unite(
    "DAEXPDTL",
    DAEXPDAY,
    DAEXPMO,
    DAEXPYR,
    sep = "",
    na.rm = TRUE,
    remove = FALSE
  ) |>
  mutate(
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    # Optional Column: COL2/AVISIT
    COL2 = explicit_na(stringr::str_to_sentence(AVISIT), ""),
    # Optional Column: COL3/ASTDT
    COL3 = ifelse(!is.na(ASTDT), paste0(toupper(format(ASTDT, "%d%b%Y"))), ""),
    # Optional Column: COL4/ASTDTM
    COL4 = ifelse(!is.na(ASTDTM), substr(ASTDTM, 12, 16), ""),
    COL5 = explicit_na(EXLOT, ""),
    # Optional Column: COL6/DAEXPDTC
    COL6 = explicit_na(DAEXPDTL, "")
  ) |>
  arrange(
    COL0,
    COL1,
    !is.na(ASTDT),
    ASTDT,
    !is.na(ASTDTM),
    ASTDTM,
    AVISITN,
    COL2
  )

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  # Optional Column: COL2/AVISIT
  COL2 = "Visit",
  # Optional Column: COL3/ASTDT
  # Select appropriate column header label
  # COL3 = "Date Dispensed",
  COL3 = "Date Administered",
  # Optional Column: COL4/ASTDTM
  # Select appropriate column header label
  # COL4 = "Time Dispensed",
  COL4 = "Time Administered",
  COL5 = "Batch Lot Number",
  # Optional Column: COL6/DAEXPDTC
  COL6 = "Batch Lot Expiration Date"
)

###############################################################################
# Build listing
###############################################################################

result <- rlistings::as_listing(
  df = lsting,
  key_cols = key_cols,
  sort_cols = sort_cols,
  disp_cols = disp_cols,
  round_type = "sas"
)

###############################################################################
# Add titles and footnotes
###############################################################################

result <- set_titles(result, tab_titles)

###############################################################################
# Output listing
###############################################################################


colwidth <- c(71, 63, 17, 23, 23, 65, 23)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
