context('cross_table')

test_that('output from cross_table matches expected results', {

    k <- cross_table(hsb$ses, hsb$female)

    expect_equal(k$obs, 200)

    expect_equivalent(k$var2_levels, c("0", "1"))

    expect_equivalent(k$var1_levels, c("1", "2", "3"))

    expect_equivalent(k$varnames, c("ses", "female"))

    expect_equivalent(k$twowaytable[, 1], c("1", "2", "3"))

    expect_equivalent(k$twowaytable[, 2], c("15", "47", "29"))

    expect_equivalent(k$twowaytable[, 3], c("32", "48", "29"))

    expect_equivalent(k$twowaytable[, 4], c("47", "95", "58"))

    expect_equivalent(colnames(k$twowaytable), c("", "0", "1", "rowtotal"))

    expect_equivalent(k$percent_table[, 1], c(0.075, 0.235, 0.145, 0.455))

    expect_equivalent(k$percent_table[, 2], c(0.160, 0.240, 0.145, 0.545))

    expect_equivalent(k$percent_table[, 3], c(0.235, 0.475, 0.290, 1.000))

    expect_equivalent(colnames(k$percent_table), c("0", "1", "row_pct"))

    expect_equivalent(rownames(k$percent_table), c("", "", "", "col_pct"))

    expect_equivalent(k$row_percent[, 1], c(0.32, 0.49, 0.50))

    expect_equivalent(k$row_percent[, 2], c(0.68, 0.51, 0.50))

    expect_equivalent(k$row_percent[, 3], c(0.24, 0.48, 0.29))

    expect_equivalent(k$column_percent[, 1], c(0.16, 0.52, 0.32))

    expect_equivalent(k$column_percent[, 2], c(0.29, 0.44, 0.27))

    expect_equivalent(unname(k$column_totals), c(91, 109))

    expect_equivalent(unname(k$percent_column), c(0.455, 0.545))

})

test_that('cross_table returns appropriate errors', {

    expect_error(cross_table(hsb$read, hsb$write),
                 'var1 and var2 must be objects of type factor')

})