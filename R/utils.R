rounda <- function(x) {
    round(x, 2)
}

formatter <- function(x) {
    format(as.character(x), width = 13, justify = "right")
}

row_pct <- function(mat, tot) {
    d <- dim(mat)
    rows <- d[1]
    l <- length(tot)
    result <- c()
    for (i in seq_len(rows)) {
        diva <- mat[i, ]/tot[i]
        result <- rbind(result, diva)
    }
    rownames(result) <- NULL
    return(result)
}

col_pct <- function(mat, tot) {
    d <- dim(mat)
    cols <- d[2]
    l <- length(tot)
    result <- c()
    for (i in seq_len(cols)) {
        diva <- mat[, i]/tot[i]
        result <- cbind(result, diva)
    }
    colnames(result) <- NULL
    return(result)
}

l <- function(x) {
    x <- as.character(x)
    k <- grep("\\$", x)
    if (length(k) == 1) {
        temp <- strsplit(x, "\\$")
        out <- temp[[1]][2]
    } else {
        out <- x
    }
    return(out)
}

