cross_table <- function(var1, var2) UseMethod('cross_table')

cross_table.default <- function(var1, var2) {
    
    if(!(is.factor(var1) & is.factor(var2))) {
        stop('var1 and var2 must be objects of type factor')
    }

    var_1 <- l(deparse(substitute(var1)))
    var_2 <- l(deparse(substitute(var2)))
    var_names <- c(var_1, var_2)
    x <- as.matrix(table(var1, var2))
    rownames(x) <- NULL
    n <- length(var1)
    if (is.factor(var1)) {
        row_name <- levels(var1)
    } else {
        row_name <- unique(sort(var1))
    }
    per_mat <- round(x / n, 3)
    row_pct <- apply(per_mat, 1, sum)
    col_pct <- apply(per_mat, 2, sum)
    per_mat <- cbind(per_mat, row_pct)
    per_mat <- suppressWarnings(rbind(per_mat, col_pct))
    d <- dim(per_mat)
    per_mat[d[1], d[2]] <- 1
    rowtotal <- apply(x, 1, sum)
    coltotal <- apply(x, 2, sum)
    rcent <- row_pct(x, rowtotal)
    rcent <- cbind(rcent, row_pct)
    rcent <- apply(rcent, c(1, 2), rounda)
    ccent <- col_pct(x, coltotal)
    ccent <- apply(ccent, c(1, 2), rounda)
    x <- cbind(x, rowtotal)
    x <- cbind(unname(row_name), x)
    if (is.factor(var1)) {
        col_name <- levels(var2)
    } else {
        col_name <- unique(sort(var2))
    }

    result <- list(
        obs = n,
        var2_levels = col_name,
        var1_levels = row_name,
        varnames = var_names,
        twowaytable = x,
        percent_table = per_mat,
        row_percent = rcent,
        column_percent = ccent,
        column_totals = coltotal,
        percent_column = col_pct)


    class(result) <- 'cross_table'
    return(result)
}


print.cross_table <- function(data) {

    print_cross(data)

}


barplot.cross_table <- function(data, beside = FALSE, proportional = FALSE) {
    i_data <- data$twowaytable
    nb <- ncol(i_data)
    bdata <- i_data[, c(-1, -nb)]
    ln <- length(data$variable_levels)
    bardata <- matrix(as.numeric(bdata), ncol = ln)
    cols <- nrow(bardata)
    barplot(bardata, col = rainbow(cols), beside = beside,
        main = paste(data$variable_names[1], 'by', data$variable_names[2]),
        xlab = data$variable_names[2], ylab = 'Frequency', legend.text = T)

    # proportional stacked bar plots
    if (proportional == TRUE) {
        colbar <- colSums(bardata)
        nh <- nrow(bardata)
        h <- rep(colbar, nh)
        hichka <- matrix(h, nrow = nh, byrow = T)
        propo_data  <- round((bardata / hichka) * 100, 2)
        barplot(propo_data, col = rainbow(cols),
        main = paste(data$variable_names[1], 'by', data$variable_names[2]),
        xlab = data$variable_names[2], ylab = 'Frequency', legend.text = T)
    }
}


mosaicplot.cross_table <- function(data) {
    i_data <- data$twowaytable
    nb <- ncol(i_data)
    mdata <- i_data[, c(-1, -nb)]
    ln <- length(data$variable_levels)
    modata <- matrix(as.numeric(mdata), ncol = ln)
    cols <- nrow(modata)
    mosaicplot(modata, col = rainbow(cols), xlab = data$variable_names[1],
        ylab = data$variable_names[2],
        main = paste(data$variable_names[1], 'by', data$variable_names[2]))
}
