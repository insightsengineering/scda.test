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

tblid <- "LSICM01"
fileid <- write_path(opath, tblid)
popfl <- "SAFFL"
trtvar <- "TRT01A"
key_cols <- c("COL0", "COL1")
sort_cols <- c("COL0", "COL1", "ASTDT")
disp_cols <- paste0("COL", 0:11)
concat_sep <- " / "
tab_titles <- list(
  title = "Dummy Title",
  subtitles = NULL,
  main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}"
)

# CM ATC levels - User input parameter to specify which ATC levels to include in the listing
atclvls <- c("CMLVL1", "CMLVL2")

# User input parameter to specify Base Preferred Term or Standardized Medication Name to include in the listing
medname_var <- c("CMDECOD") # Standardized Medication Name or/and Base Preferred Term

medname_label <- ifelse(
  length(medname_var) > 1,
  paste(c("Standardized Medication Name", "Base Preferred Term"), collapse = concat_sep),
  ifelse(medname_var == "CMBASPRF", "Base Preferred Term", "Standardized Medication Name")
)

###############################################################################
# Process data
###############################################################################

adcm <- adcm_jnj |>
  filter(
    !!rlang::sym(popfl) == "Y" &
      ((CMPRESP == "Y" & CMOCCUR == "Y") |
        (is.na(CMPRESP)))
  ) |>
  mutate(
    !!rlang::sym(trtvar) := factor(
      .data[[trtvar]],
      levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
    )
  )

lsting <- adcm |>
  # Update CQzzNAM variabes based on study requirements
  unite(
    "CQNAM",
    starts_with("CQ"),
    sep = ", ",
    na.rm = TRUE,
    remove = FALSE
  ) |>
  mutate(
    across(
      c(CMTRT, CMDOSFRQ, CMROUTE, CMINDCSP),
      ~ ifelse(. == "", "-", as.character(.))
    ),
    across(
      c(all_of(atclvls)),
      ~ case_when(
        .x == "" ~ "Uncoded",
        .default = .x
      )
    ),
    across(
      c(CMDECOD, CMBASPRF),
      ~ case_when(
        .x == "" ~ paste0("Uncoded: ", CMTRT),
        .default = .x
      )
    ),
    across(
      all_of(atclvls),
      ~ stringr::str_to_sentence(.x)
    )
  ) |>
  mutate(
    CMSTDT = as.Date(CMSTDTC, format = "%Y-%m-%d"),
    CMSTYR = ifelse(
      stringr::str_length(sub("T.*", "", CMSTDTC)) >= 4 &
        substr(sub("T.*", "", CMSTDTC), 1, 4) != "----",
      substr(sub("T.*", "", CMSTDTC), 1, 4),
      NA
    ),
    CMSTMO = month.abb[as.numeric(ifelse(
      stringr::str_length(sub("T.*", "", CMSTDTC)) >= 7 &
        substr(
          sub("T.*", "", CMSTDTC),
          6,
          7
        ) !=
          "--",
      substr(sub("T.*", "", CMSTDTC), 6, 7),
      NA
    ))],
    CMSTDAY = ifelse(
      stringr::str_length(sub("T.*", "", CMSTDTC)) >= 10 &
        substr(sub("T.*", "", CMSTDTC), 9, 10) != "--",
      substr(sub("T.*", "", CMSTDTC), 9, 10),
      NA
    ),
    CMSTDY = ifelse(CMSTDT > TRTSDT, CMSTDT - TRTSDT + 1, CMSTDT - TRTSDT),
    CMSTDYL = explicit_na(as.character(CMSTDY), ""),
    CMENDT = as.Date(CMENDTC, format = "%Y-%m-%d"),
    CMENYR = ifelse(
      stringr::str_length(sub("T.*", "", CMENDTC)) >= 4 &
        substr(sub("T.*", "", CMENDTC), 1, 4) != "----",
      substr(sub("T.*", "", CMENDTC), 1, 4),
      NA
    ),
    CMENMO = month.abb[
      as.numeric(ifelse(
        stringr::str_length(sub("T.*", "", CMENDTC)) >= 7 &
          substr(sub("T.*", "", CMENDTC), 6, 7) != "--",
        substr(sub("T.*", "", CMENDTC), 6, 7),
        NA
      ))
    ],
    CMENDAY = ifelse(
      stringr::str_length(sub("T.*", "", CMENDTC)) >= 10 &
        substr(sub("T.*", "", CMENDTC), 9, 10) != "--",
      substr(sub("T.*", "", CMENDTC), 9, 10),
      NA
    ),
    CMENDY = ifelse(CMENDT > TRTSDT, CMENDT - TRTSDT + 1, CMENDT - TRTSDT),
    CMENDYL = explicit_na(as.character(CMENDY), ""),
    PREFLL = ifelse(is.na(PREFL), NA, "P"),
    ONTRTFLL = ifelse(is.na(ONTRTFL), NA, "C"),
    FUPFLL = ifelse(is.na(FUPFL), NA, "F")
  ) |>
  unite(
    "CMSTDTL",
    CMSTDAY,
    CMSTMO,
    CMSTYR,
    sep = "",
    na.rm = TRUE,
    remove = FALSE
  ) |>
  unite(
    "CMENDTL",
    CMENDAY,
    CMENMO,
    CMENYR,
    sep = "",
    na.rm = TRUE,
    remove = FALSE
  ) |>
  unite(
    "POF",
    PREFLL,
    ONTRTFLL,
    FUPFLL,
    sep = ";",
    na.rm = TRUE,
    remove = FALSE
  ) |>
  mutate(
    STDATE = case_when(
      CMSTDTL != "" & CMSTDYL != "" ~ paste0(toupper(CMSTDTL), " (", CMSTDYL, ")"),
      CMSTDTL != "" & CMSTDYL == "" ~ paste0(toupper(CMSTDTL), ""),
      TRUE ~ ""
    ),
    ENDATE = case_when(
      CMENDTL != "" & CMENDYL != "" ~ paste0(toupper(CMENDTL), " (", CMENDYL, ")"),
      CMENDTL != "" & CMENDYL == "" ~ paste0(toupper(CMENDTL), ""),
      CMENDTL == "" & CMENRF == "AFTER" ~ "Ongoing",
      TRUE ~ ""
    ),
    DOSEU = case_when(
      (!is.na(CMDOSE)) ~ paste(CMDOSE, CMDOSU, " "),
      is.na(CMDOSE) & !is.na(CMDOSTXT) ~ paste(CMDOSTXT, CMDOSU, " "),
      is.na(CMDOSE) & is.na(CMDOSTXT) ~ ""
    ),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    # Common medication name variable based on user selection
    COL2 = paste(
      stringr::str_to_sentence(CMTRT),
      stringr::str_to_sentence(.data[[medname_var]]),
      sep = concat_sep
    ),
    COL3 = explicit_na(POF, ""),
    # Optional Column: COL4/CQzzNAM
    COL4 = explicit_na(CQNAM, ""),
    COL5 = do.call(paste, c(across(all_of(atclvls)), sep = concat_sep)),
    COL6 = explicit_na(STDATE, ""),
    COL7 = explicit_na(ENDATE, ""),
    COL8 = explicit_na(DOSEU, ""),
    COL9 = explicit_na(CMDOSFRQ, ""),
    # Optional Column: COL10/CMROUTE
    COL10 = explicit_na(stringr::str_to_sentence(CMROUTE), ""),
    COL11 = explicit_na(CMINDCSP, "")
  ) |>
  arrange(COL0, COL1, ASTDT)

atclvls_label <- sub(".*(\\d+)$", "ATC Level \\1", atclvls) |>
  paste(collapse = concat_sep)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste(
    "Medication (Verbatim)",
    medname_label,
    sep = concat_sep
  ),
  COL3 = "Medication Timing",
  # Optional Column: COL4/CQzzNAM
  COL4 = "Interest Category",
  # COL5: Based on input parameter atclvls
  COL5 = atclvls_label,
  COL6 = "Start Date of Medication (Study Day~[super a])",
  COL7 = "End Date of Medication (Study Day~[super a])",
  COL8 = "Dose (Unit)",
  COL9 = "Frequency",
  # Optional Column: COL10/CMROUTE
  COL10 = "Route",
  COL11 = "Indication: Specify"
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
# Add titles and footnotes
###############################################################################

result <- set_titles(result, tab_titles)

###############################################################################
# Output listing
###############################################################################


colwidth <- c(21, 13, 39, 20, 25, 38, 22, 22, 19, 18, 19, 29)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
