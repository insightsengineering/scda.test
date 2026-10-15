library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(rlistings)
library(junco)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSIDS05"
fileid <- write_path(opath, tblid)
popfl <- "SCRNFL"
key_cols <- c("COL1")
sort_cols <- c("COL1")
disp_cols <- paste0("COL", 1:2)
tab_titles <- list(title = "Dummy Title",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")

###############################################################################
# Process data
###############################################################################

adsl <- adsl_jnj |>
  filter(!!rlang::sym(popfl) == "Y") |>
  summarise(
    rficdt_min = toupper(format(min(RFICDT, na.rm = TRUE), "%d%b%Y")),
    lstsvdt_max = toupper(format(max(LASTCTDT, na.rm = TRUE), "%d%b%Y"))
  )

lsting <- adsl |>
  mutate(
    COL1 = rficdt_min,
    COL2 = lstsvdt_max,
  )

lsting <- var_relabel(
  lsting,
  COL1 = "First Contact in the Study~[super a]",
  COL2 = "Last Contact/Visit in the Study~[super b]"
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


colwidth <- c(20, 23)

tt_to_tlgrtf(head(result, 100), file = fileid, orientation = "landscape")
