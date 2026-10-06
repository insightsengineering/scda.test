# check_feature.r --- Test a feature branch against scda.test --

# Config --- user inputs --------------------------------------

if (!interactive()) {
  stop("🚨 This script needs interactive mode for readline(). Run in RStudio/Positron!")
}

branch_name <- readline("🌿 Branch name: ")
package     <- readline("📦 Package name (default: junco): ")

if (nchar(trimws(package)) == 0L) package <- "junco"

package <- tolower(trimws(package))
branch_name <- trimws(branch_name)

if (nchar(branch_name) == 0L) {
  stop("🚨 Branch name cannot be empty!")
}

# Branch --- check if exists, create or switch ----------------

all_branches <- run_git("branch", "-a")
branch_exists <- any(grepl(sprintf("/%s$|^..%s$", branch_name, branch_name), all_branches))

if (branch_exists) {
  message(sprintf("⚠️  Branch '%s' already exists!", branch_name))
  action <- tolower(trimws(readline("   [s]witch to it / [d]elete & recreate / [q]uit? ")))

  if (action == "q") {
    stop("🛑 Aborted by user.")
  } else if (action == "d") {
    message(sprintf("🗑️  Deleting '%s'...", branch_name))
    run_git("checkout", branch)
    tryCatch(run_git("branch", "-D", branch_name), error = function(e) NULL)
    tryCatch(run_git("push", "origin", "--delete", branch_name), error = function(e) NULL)
    run_git("checkout", "-b", branch_name)
    message(sprintf("🌿 Branch '%s' recreated fresh from '%s'!", branch_name, branch))
  } else {
    run_git("checkout", branch_name)
    run_git("fetch", "origin", branch_name)
    message(sprintf("🔀 Switched to existing branch '%s'.", branch_name))
  }
} else {
  run_git("checkout", "-b", branch_name)
  message(sprintf("🌿 Branch '%s' created from '%s'!", branch_name, branch))
}

# DESCRIPTION --- update Remotes entry ------------------------

desc_path <- file.path(repo_root, "DESCRIPTION")
desc_lines <- readLines(desc_path)

# Match org/package@branch but stop before comma if present
pkg_pattern <- sprintf("(\\S+/%s)@[^, ]+", package)
match_idx <- grep(pkg_pattern, desc_lines, ignore.case = TRUE)

if (length(match_idx) == 0L) {
  stop(sprintf("🚨 Package '%s' not found in DESCRIPTION Remotes!", package))
}

old_line <- desc_lines[match_idx]
new_line <- sub(
  sprintf("(\\S+/%s)@[^, ]+", package),
  sprintf("\\1@%s", branch_name),
  old_line,
  ignore.case = TRUE
)

desc_lines[match_idx] <- new_line
writeLines(desc_lines, desc_path)

message(sprintf("📝 DESCRIPTION updated:\n   Old: %s\n   New: %s", trimws(old_line), trimws(new_line)))

# Push --- commit and push ------------------------------------

run_git("add", "DESCRIPTION")

# Check if there's actually something to commit
staged <- run_git("diff", "--cached", "--name-only")

if (length(staged) > 0L && any(nchar(staged) > 0L)) {
  run_git("commit", "-m", sprintf("'feat: test %s@%s'", package, branch_name))
  run_git("push", "origin", branch_name)
  message(sprintf("🚀 Pushed! Now go run the workflow on branch '%s'", branch_name))
} else {
  message("ℹ️  DESCRIPTION unchanged — nothing to commit.")
  run_git("push", "origin", branch_name)
  message(sprintf("🚀 Branch '%s' is up to date on origin.", branch_name))
}

message("🔗 https://github.com/vikram-rawat/scda.test/actions")
message("🎉 Feature branch ready!")
