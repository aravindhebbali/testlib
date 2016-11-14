print_stats <- function(data) {

  n <- nchar(format(data$Uncorrect_SS, nsmall = 2))
  width1 <- 52 + (2 * n)
  width2 <- as.integer(width1 / 2)
  width3 <- width2 - 5
  width4 <- width2 - 2

  col1 <- max(nchar(as.character(data$Lowest_Obs)))
  col2 <- max(nchar(as.character(data$Highest_Obs)))
  col3 <- max(nchar(as.character(data$Lowest_Obs_Index)))
  col4 <- max(nchar(as.character(data$Highest_Obs_Index)))
  v <- nchar("Value")
  ol <- max(col1, col2, col3, col4, v)
  gap <- width4 - (2 * ol)

  cat(formatc("Univariate Analysis", width1), "\n\n",
      formatl("N"), formatr(data$Observations, n), formats(),
      formatl("Variance"), formatr(data$Variance, n), "\n",
      formatl("Missing"), formatr(data$Missing_Obs, n), formats(),
      formatl("Std Deviation"), formatr(data$Standard_Deviation, n), "\n",
      formatl("Mean"), formatr(data$Mean, n), formats(),
      formatl("Range"), formatr(data$Range, n), "\n",
      formatl("Median"), formatr(data$Median, n), formats(),
      formatl("Interquartile Range"), formatr(data$Interquartile_Range, n), "\n",
      formatl("Mode"), formatr(data$Mode, n), formats(),
      formatl("Uncorrected SS"), formatr(data$Uncorrect_SS, n), "\n",
      formatl("Trimmed Mean"), formatr(data$Trimmed_mean, n), formats(),
      formatl("Corrected SS"), formatr(data$Correct_SS, n), "\n",
      formatl("Skewness"), formatr(data$Skewness, n), formats(),
      formatl("Coeff Variation"), formatr(data$Coefficient_Of_Variation, n), "\n",
      formatl("Kurtosis"), formatr(data$Kurtosis, n), formats(),
      formatl("Std Error Mean"), formatr(data$Std_Error_Mean, n), "\n\n",
      formatc("Quantiles", width1), "\n\n",
      formatc("Quantile", width2), formatc("Estimate", width2), "\n\n",
      formatc("100% Max  ", width2), formatnc(data$Max, width2), "\n",
      formatc("99%       ", width2), formatnc(data$Percentile_99, width2), "\n",
      formatc("95%       ", width2), formatnc(data$Percentile_95, width2), "\n",
      formatc("90%       ", width2), formatnc(data$Percentile_90, width2), "\n",
      formatc("75% Q3    ", width2), formatnc(data$Percentile_75, width2), "\n",
      formatc("50% Median", width2), formatnc(data$Median, width2), "\n",
      formatc("25% Q1    ", width2), formatnc(data$Percentile_25, width2), "\n",
      formatc("10%       ", width2), formatnc(data$Percentile_10, width2), "\n",
      formatc("5%        ", width2), formatnc(data$Percentile_05, width2), "\n",
      formatc("1%        ", width2), formatnc(data$Percentile_01, width2), "\n",
      formatc("0% Min    ", width2), formatnc(data$Min, width2), "\n\n",
      formatc("Extreme Observations", width1), "\n\n",
      formatc("Lowest", width2), formatc("Highest", width2), "\n\n",
      formatol("Value", ol), format_gap(gap), formatol("Obs", ol), formats(),
      formatol("Value", ol), format_gap(gap), formatol("Obs", ol), "\n")
  for (i in seq_len(5)) {
      cat("",formatol(data$Lowest_Obs[i], ol), format_gap(gap), formatol(data$Lowest_Obs_Index[i], ol), formats(),
          formatol(data$Highest_Obs[i], ol), format_gap(gap), formatol(data$Highest_Obs_Index[i], ol), "\n")
  }

}


print_fcont <- function(data) {

  cat(format(paste('Variable:', data$varname), width = 77, justify = 'centre'), '\n')
      cat("|---------------------------------------------------------------------------|
|                                 Cumulative                    Cumulative  |
|     Bins      |  Frequency   |   Frequency  |   Percent    |    Percent   |
|---------------------------------------------------------------------------|")
    for (i in seq_len(data$bins)) {
        k <- i + 1
      cat("\n|", formata(data$breaks[i], 1, 5), "-", formata(data$breaks[k], 1, 5), "|",
          formata(data$frequency[i], 2, 12), "|", formata(data$cumulative[i], 2, 12), "|",
          formatas(data$percent[i], 2, 12), "|", formatas(data$cum_percent[i], 2, 12), "|")
      cat("\n|---------------------------------------------------------------------------|")
    }

}


print_ftable <- function(data) {

  nr <- nrow(data$ftable)
  nc <- ncol(data$ftable)
  cat(format(paste('Variable:', data$varname), width = 76, justify = 'centre'), '\n')
  cat("|--------------------------------------------------------------------------|
|                                Cumulative                    Cumulative  |
|    Levels    |  Frequency   |   Frequency  |   Percent    |    Percent   |
|--------------------------------------------------------------------------|\n")
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      cat("|", formatter_freq(data$ftable[i, j]))
    }
    cat("|")
    cat("\n|--------------------------------------------------------------------------|\n")
  }
  cat('\n\n')

}


print_cross <- function(data) {

    p <- length(data$var2_levels)
    q <- p + 2
    h <- p + 1
    r <- (h * 15) - 3
    f <- length(data$var1_levels)
    g <- f + 2
    h <- p + 1

    col_names <- c(data$varnames[1], data$var2_levels, "Row Total")
    col_totals <- c("Column Total", data$column_totals, data$obs)

    cat(formatter("    Cell Contents\n"),
    "|---------------|\n",
    "|", formatter("Frequency"), "|\n",
    "|", formatter("Percent"), "|\n",
    "|", formatter("Row Pct"), "|\n",
    "|", formatter("Col Pct"), "|\n",
    "|---------------|\n\n",
    "Total Observations: ", data$obs, "\n\n")
    cat("-", rep("---------------", q), sep = "")
    cat("\n")
    cat("|              |", format(data$varnames[2], width = r, justify = "centre"), "|")
    cat("\n")
    cat("-", rep("---------------", q), sep = "")
    cat("\n|")
    for (i in seq_along(col_names)) {
        cat(formatter(col_names[i]), "|")
    }
    cat("\n-", rep("---------------", q), sep = "")
    cat("\n")

    for (i in seq_len(f)) {
        cat("|")
        for (j in seq_len(q)) {
            cat(formatter(data$twowaytable[i, j]), "|")
        }
        cat("\n")
        cat("|              |")
        for (j in seq_len(p)) {
            cat(formatter(data$percent_table[i, j]), "|")
        }
        cat("              |")
        cat("\n")
        cat("|              |")
        for (j in seq_len(h)) {
            cat(formatter(data$row_percent[i, j]), "|")
        }
        cat("\n")
        cat("|              |")
        for (j in seq_len(p)) {
            cat(formatter(data$column_percent[i, j]), "|")
        }
        cat("              |")
        cat("\n-", rep("---------------", q), sep = "")
        cat("\n")
    }
    cat("|")
    for (i in seq_along(col_totals)) {
        cat(formatter(col_totals[i]), "|")
    }
    cat("\n")
    cat("|              |")
    for (i in seq_along(data$percent_column)) {
        cat(formatter(data$percent_column[i]), "|")
    }
    cat("              |")
    cat("\n-", rep("---------------", q), sep = "")
    cat("\n")

}


print_group <- function(data) {

    line <- 23
    n <- 21
    n_names <- max(nchar(data$stats[2, c(-1)]))
    n_uss <- max(nchar(data$stats[12, c(-1)]))
    w <- max(n_names, n_uss) + 2
    cola <- ncol(data$stats)
    col <- cola - 1
    ow <- 23 * cola - col
    row <- nrow(data$stats)

    cat(format(paste(data$yvar, 'by', data$xvar), width = ow, justify = 'centre'), '\n')
    cat(rep('-', ow), sep = '', '\n')
    cat('|')
    for (i in seq_len(cola)) {
        cat(format(data$stats[1, i], width = n, justify = 'right'), '|', sep = '')
    }
    cat('\n')
    cat(rep('-', ow), sep = '', '\n')
    for (i in 2:row) {
        cat('|')
        for (j in seq_len(cola)) {
            cat(format(data$stats[i, j], width = n, justify = 'right'), '|', sep = '')
        }
        cat('\n')
    }
    cat(rep('-', ow), sep = '', '\n')

}


print_screen <- function(x) {

  columns <- c('  Column Name  ', '  Data Type  ', '  Levels  ', '  Missing  ', '  Missing (%)  ')
	len_col <- as.vector(sapply(columns, nchar))
  xlev <- lapply(k$levels, paste, collapse = " ") %>%
    lapply(nchar) %>%
    unlist %>%
    max
	lengths <- list(x$Variables, x$Types, xlev, x$Missing, x$MissingPer)
	n <- length(columns)
	nlist <- list()
	for (i in seq_len(n)) {
		nlist[[i]] <- max(len_col[i], max(sapply(lengths[[i]], nchar)))
	}
	clengths <- unlist(nlist)
  clengths[3] <- max(10, xlev)
	dash <- sum(clengths) + 6
	cat(rep("-",dash), sep = "")
	cat("\n|")
	for(i in seq_len(n)) {
		cat(columns[i], "|", sep = "")
	}
	cat("\n", rep("-",dash), sep = "")
	cat("\n")
	for (i in seq_len(x$Columns)) {
		cat("|", format(x$Variables[i], width = clengths[1], justify = 'centre'), "|",
			format(x$Types[i], width = clengths[2], justify = 'centre'), "|",
			format(paste(x$levels[[i]], collapse = " "), width = clengths[3], justify = 'centre'), "|",
			format(as.character(x$Missing[i]), width = clengths[4], justify = 'centre'), "|",
			format(as.character(x$MissingPer[i]), width = clengths[5], justify = 'centre'), "|\n", sep = ""
		)
	}
	cat(rep("-",dash), sep = "")
	cat("\n\n")
	cat(' Overall Missing Values          ', x$MissingTotal, "\n", 'Percentage of Missing Values    ', x$MissingTotPer, "%\n",
		'Rows with Missing Values        ', x$MissingRows, "\n", "Columns With Missing Values     ", x$MissingCols, "\n")

}
