test_that("tpkada01titer", {
  skip_if_not_installed("envsetup")

    expect_snapshot_file(write_test_rtf_for("tpkada01titer.R", rtf_name = "tpkada01titer.rtf"), "tpkada01titer.rtf")
})
