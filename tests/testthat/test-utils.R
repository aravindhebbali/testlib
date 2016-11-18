context('utils')

test_that('output from l matches the expected result', {

    expect_equal(l(deparse(substitute(mtcars$mpg))), "mpg")
    expect_equal(l(deparse(substitute(mpg))), "mpg")
    expect_equal(l(deparse(substitute(mtcars@mpg))), "mtcars@mpg")

})

test_that('output from row_pct matches the expected result', {

    m <- matrix(c(12, 23, 37, 21), nrow = 2)
    exptd <- matrix(c(0.245, 0.523, 0.755, 0.477), nrow = 2)
    expect_equivalent(round(row_pct(m, c(49, 44)), 3), exptd)

})


test_that('output from col_pct matches the expected result', {

    m <- matrix(c(12, 23, 37, 21), nrow = 2)
    exptd <- matrix(c(0.343, 0.657, 0.638, 0.362), nrow = 2)
    expect_equivalent(round(col_pct(m, c(35, 58)), 3), exptd)

})


test_that('output from formatter matches the expected result', {

    expect_equal(formatter(3), "            3")

})
