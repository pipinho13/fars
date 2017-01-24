test_that("filenames", {
    expect_that(make_filename(2013), is_a("character"))
    expect_that(make_filename(2013), is_identical_to(make_filename("2013")))
    expect_that(make_filename(2013), equals("accident_2013.csv.bz2"))
})