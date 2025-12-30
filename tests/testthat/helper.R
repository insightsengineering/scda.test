
opath <- tempdir()
input <- "source_code"


attach_me <- list(opath = opath,
                  datapath = input)

pos <- 2L
attach(attach_me, name = "envsetup:paths", pos =  pos)


options(tidytlg.add_datetime = FALSE) # not to add datetime



write_test_rtf_for <- function(x, rtf_name = NULL, part_num = NULL, total_parts = NULL) {
  ipath <- test_path("source_code", x)

  source(ipath)

  if (!is.null(rtf_name)) {
    # Custom behavior - use the provided RTF name
    rtf_path <- file.path(opath, rtf_name)
  } else if (!is.null(part_num)) {
    # Part behavior - use part number to generate filename
    base_name <- gsub(".r", "", x, ignore.case = TRUE)
    if (is.null(total_parts)) {
      # Try to determine total parts by checking for existing files
      pattern <- paste0("^", base_name, "part.*of.*\\.rtf$")
      existing_files <- list.files(opath, pattern = pattern, ignore.case = TRUE)
      if (length(existing_files) > 0) {
        # Extract the total parts from the existing files
        parts_info <- regmatches(existing_files, regexpr("part\\d+of(\\d+)", existing_files))
        total_parts_matches <- regmatches(parts_info, regexpr("\\d+$", parts_info))
        if (length(total_parts_matches) > 0) {
          total_parts <- max(as.numeric(total_parts_matches))
        } else {
          total_parts <- part_num  # Default to part_num if can't determine
        }
      } else {
        total_parts <- part_num  # Default to part_num if no existing files
      }
    }
    rtf_path <- file.path(opath, paste0(base_name, "part", part_num, "of", total_parts, ".rtf"))
  } else {
    # Default behavior - use the source file name with .rtf extension
    rtf_path <- file.path(opath, gsub(".r", ".rtf", x, ignore.case = TRUE))
  }
  rtf_path
}

# Function to create test scripts for source code files
create_test_scripts <- function(source_files = NULL, force=FALSE) {
  # If no source files provided, get all R files from source_code directory
  if (is.null(source_files)) {
    source_files <- list.files(test_path("source_code"), pattern = "\\.R$", ignore.case = TRUE)
  } else if (is.character(source_files) && length(source_files) == 1) {
    # If a single string is provided, convert to a vector
    source_files <- c(source_files)
  }

  # Check which test files already exist
  existing_tests <- NULL
  if (force == FALSE) {
    existing_tests <- list.files(test_path(), pattern = "^test-.*\\.R$")
  }
  # Create test files for each source file
  created_files <- character(0)
  for (source_file in source_files) {
    # Extract base name without extension
    base_name <- tools::file_path_sans_ext(basename(source_file))

    # Create test file name
    test_file_name <- paste0("test-", base_name, ".R")

    # Skip if test file already exists
    if (test_file_name %in% existing_tests) {
      message(sprintf("Test file %s already exists. Skipping.", test_file_name))
      next
    }

    # Create test file content
    test_content <- sprintf('test_that("%s", {\n  expect_snapshot_file(write_test_rtf_for("%s"), "%s.rtf")\n})\n', 
                           base_name, source_file, base_name)

    # Write test file
    test_file_path <- test_path(test_file_name)
    writeLines(test_content, test_file_path)

    created_files <- c(created_files, test_file_name)
    message(sprintf("Created test file: %s", test_file_name))
  }

  if (length(created_files) == 0) {
    message("No new test files were created.")
  } else {
    message(sprintf("Created %d test file(s).", length(created_files)))
  }

  invisible(created_files)
}


# Source setup-jnj.R to ensure JNJ data objects are available
source("setup-jnj.R")
