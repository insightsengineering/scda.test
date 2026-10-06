# sync_upstream.r --- Sync main with upstream via scripts ------

# Config --- edit these if remotes change ----------------------

upstream <- "upstream"
branch <- "main"
dev_branch <- "dev_scripts"
dev_pattern <- "^dev/"

# Preflight --- check dependencies ----------------------------

if (!requireNamespace("glue", quietly = TRUE)) {
  stop("🚨 glue package not installed. Run: install.packages('glue')")
}

# Helper --- run git and stop on failure ----------------------

run_git <- function(...) {
  args <- c(...)
  result <- system2("git", args, stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")
  if (!is.null(status) && status != 0L) {
    stop(sprintf(
      "🚨 git %s failed:\n%s",
      paste(args, collapse = " "),
      paste(result, collapse = "\n")
    ))
  }
  invisible(result)
}

# Setup --- paths and OS detection ----------------------------

repo_root <- trimws(run_git("rev-parse", "--show-toplevel"))
setwd(repo_root)
message(sprintf("📁 Repo root: %s", repo_root))

is_windows <- .Platform$OS.type == "windows"

# Preflight --- verify remotes exist --------------------------

remotes <- run_git("remote")
if (!upstream %in% remotes) {
  stop(sprintf(
    "🚨 Remote '%s' not found. Run:\ngit remote add %s https://github.com/insightsengineering/scda.test.git",
    upstream, upstream
  ))
}

# Preflight --- verify dev_scripts branch exists --------------

all_branches <- run_git("branch", "-a")
has_dev_branch <- any(grepl(dev_branch, all_branches, fixed = TRUE))
if (!has_dev_branch) {
  stop(sprintf(
    "🚨 Branch '%s' not found. Run:\ngit checkout -b %s && git push origin %s && git checkout main",
    dev_branch, dev_branch, dev_branch
  ))
}

# Preflight --- must be on target branch ----------------------

current_branch <- trimws(run_git("branch", "--show-current"))
if (current_branch != branch) {
  message(sprintf("🔀 Switching from '%s' to '%s'...", current_branch, branch))
  run_git("checkout", branch)
}

# Template --- read from dev_scripts branch -------------------

template_file <- if (is_windows) "sync_upstream.ps1" else "sync_upstream.sh"
template_ref <- sprintf("%s:dev/scripts/%s", dev_branch, template_file)

template_content <- tryCatch(
  run_git("show", template_ref),
  error = function(e) {
    stop(sprintf("🚨 Template not found in branch '%s': %s", dev_branch, template_ref))
  }
)

filled_script <- glue::glue(
  paste(template_content, collapse = "\n"),
  upstream = upstream,
  branch = branch,
  .open = "{{",
  .close = "}}"
)

# Execute --- write to temp, run, show output ------------------

tmp <- tempfile(fileext = if (is_windows) ".ps1" else ".sh")
on.exit(unlink(tmp), add = TRUE)
writeLines(filled_script, tmp)

if (is_windows) {
  shell_cmd <- "powershell"
  shell_args <- c("-ExecutionPolicy", "Bypass", "-File", tmp)
} else {
  Sys.chmod(tmp, "755")
  shell_cmd <- "bash"
  shell_args <- tmp
}

message(sprintf("▶️  Running: %s %s", shell_cmd, basename(tmp)))

result <- system2(shell_cmd, shell_args, stdout = TRUE, stderr = TRUE)
status <- attr(result, "status")
if (is.null(status)) status <- 0L

cat(result, sep = "\n")

if (status != 0L) {
  stop(sprintf("❌ Sync failed (exit %d). Fix the template in '%s' branch.", status, dev_branch))
}

message("✅ Sync complete! 🎉")

# Restore --- checkout dev/ from dev_scripts branch -----------

message(sprintf("📂 Restoring dev/ from '%s' branch...", dev_branch))
run_git("checkout", dev_branch, "--", "dev/")
message("📂 dev/ restored! 🎯")

# Commit --- add dev/ back to history -------------------------

run_git("add", "dev/")
run_git("commit", "-m", "'chore: preserve dev/ after upstream sync'")
message("📝 dev/ committed!")

# Housekeeping --- re-add dev/ exclusion to .Rbuildignore ------

rbuildignore_path <- file.path(repo_root, ".Rbuildignore")
rbuildignore <- readLines(rbuildignore_path)

has_dev_entry <- any(grepl(dev_pattern, rbuildignore, fixed = TRUE))

if (!has_dev_entry) {
  message("🔧 Re-adding dev/ exclusion to .Rbuildignore...")
  writeLines(c(rbuildignore, dev_pattern), rbuildignore_path)
  run_git("add", ".Rbuildignore")
  run_git("commit", "--amend", "--no-edit")
  message("📝 .Rbuildignore patched!")
}

# Push --- force push to origin --------------------------------

run_git("push", "origin", branch, "--force")
message("🚀 Pushed to origin!")

message("🎉 All done! main is synced with upstream + dev/ preserved!")
