#Load packages
library(Gmisc)
library(Hmisc)
library(officer)

##Re-do with Kruskal
prDescGetAndValidateDefaultRef <- function(x, default_ref){
  if (missing(default_ref)){
    default_ref <- 1
  }else if (is.character(default_ref)){
    if (default_ref %in% levels(x))
      default_ref <- which(default_ref == levels(x))
    else
      stop("You have provided an invalid default reference, '",
        default_ref, "' can not be found among: ", paste(levels(x), collapse=", "))
  }else if (!default_ref %in% 1:length(levels(x)))
    stop("You have provided an invalid default reference,",
      " it is ", default_ref, " while it should be between 1 and ", length(levels(x)),
      " as this is only used for factors.")

  return(default_ref)
}

prGetStatistics <- function(x,
                            show_perc = FALSE,
                            html = TRUE,
                            digits = 1,
                            digits.nonzero = NA,
                            numbers_first = TRUE,
                            useNA = c("ifany", "no", "always"),
                            useNA.digits = digits,
                            show_all_values = FALSE,
                            continuous_fn = describeMean,
                            factor_fn = describeFactors,
                            prop_fn = factor_fn,
                            percentage_sign = TRUE)
{
  # All the describe functions have the same interface
  # so it is useful to gather all the arguments here
  describe_args <-
    list(x = x,
         html = html,
         digits = digits,
         digits.nonzero = digits.nonzero,
         number_first = numbers_first,
         useNA = useNA,
         useNA.digits = useNA.digits,
         percentage_sign = percentage_sign)

  if (is.factor(x) ||
        is.logical(x) ||
        is.character(x)){
    if ((is.factor(x) &&
           length(levels(x)) == 2) ||
          (!is.factor(x) &&
             length(unique(na.omit(x))) == 2)){
      if (show_perc){
        total_table <- fastDoCall(prop_fn, describe_args)
      }else{
        total_table <- table(x, useNA=useNA)
        names(total_table)[is.na(names(total_table))] <- "Missing"
        # Choose only the reference level
        if (show_all_values == FALSE)
          total_table <- total_table[names(total_table) %in%
                                       c(levels(as.factor(x))[1], "Missing")]
      }

    } else {
      if (show_perc)
        total_table <- fastDoCall(factor_fn, describe_args)
      else{
        total_table <- table(x, useNA=useNA) %>%
          txtInt
        names(total_table)[is.na(names(total_table))] <- "Missing"
      }
    }
  }else{
    total_table <- fastDoCall(continuous_fn, describe_args)

    # If a continuous variable has two rows then it's assumed that the second is the missing
    if (length(total_table) == 2 &&
      show_perc == FALSE)
      total_table[2] <- sum(is.na(x))
  }
  return(total_table)
}


getDescriptionStatsBy2 = function (x, by, digits = 1, html = TRUE, numbers_first = TRUE,
    statistics = FALSE, statistics.sig_lim = 10^-4, statistics.two_dec_lim = 10^-2,
    statistics.suppress_warnings = TRUE, useNA = c("ifany", "no",
        "always"), useNA.digits = digits, continuous_fn = describeMean,
    prop_fn = describeProp, factor_fn = describeFactors, show_all_values = FALSE,
    hrzl_prop = FALSE, add_total_col, total_col_show_perc = TRUE,
    use_units = FALSE, default_ref, NEJMstyle = FALSE, percentage_sign = TRUE,
    header_count, missing_value = "-", names_of_missing = NULL,
    ...)
{
    API_changes <- c(show_missing_digits = "show_missing.digits",
        show_missing = "useNA", sig.limit = "statistics.sig_lim",
        two_dec.limit = "statistics.two_dec_lim")
    dots <- list(...)
    fenv <- environment()
    for (i in 1:length(API_changes)) {
        old_name <- names(API_changes)[i]
        new_name <- API_changes[i]
        if (old_name %in% names(dots)) {
            if (class(fenv[[new_name]]) == "name") {
                fenv[[new_name]] <- dots[[old_name]]
                dots[[old_name]] <- NULL
                warning("Deprecated: '", old_name, "'", " argument is now '",
                  new_name, "'", " as of ver. 1.0")
            }
            else {
                stop("You have set both the old parameter name: '",
                  old_name, "'", " and the new parameter name: '",
                  new_name, "'.")
            }
        }
    }
    useNA <- match.arg(useNA)
    if (!is.function(statistics)) {
        if (is.list(statistics) || (statistics != FALSE)) {
            if (is.list(statistics)) {
                types <- c("continuous", "proportion", "factor")
                if (any(!names(statistics) %in% types))
                  stop("If you want to provide custom functions for generating statistics",
                    " you must either provide a function or a list with the elements:",
                    " '", paste(types, collapse = "', '"), "'")
                if (is.numeric(x) && length(unique(x)) != 2) {
                  statistics <- statistics[["continuous"]]
                }
                else if (length(unique(x)) == 2) {
                  if ("proportion" %in% names(statistics)) {
                    statistics <- statistics[["proportion"]]
                  }
                  else {
                    statistics <- statistics[["factor"]]
                  }
                }
                else {
                  statistics <- statistics[["factor"]]
                }
                if (is.character(statistics))
                  statistics <- get(statistics)
            }
            if (!is.function(statistics)) {
                if (length(unique(x)) == 2) {
                  statistics <- getPvalFisher
                }
                else if (is.numeric(x)) {
                  if (length(unique(by)) == 2)
                    statistics <- getPvalWilcox
                  else statistics <- getPvalKruskal
                }
                else {
                  statistics <- getPvalFisher
                }
            }
        }
    }
    if (is.function(statistics)) {
        if (statistics.suppress_warnings) {
            pval <- suppressWarnings(statistics(x = x, by = by))
        }
        else {
            pval <- statistics(x = x, by = by)
        }
    }
    if (missing(add_total_col) && hrzl_prop) {
        add_total_col = TRUE
    }
    if (is.null(x))
        stop("You haven't provided an x-value to do the statistics by.",
            " This error is most frequently caused by referencing an old",
            " variable name that doesn't exist anymore")
    if (is.null(by))
        stop("You haven't provided a by-value to do the statistics by.",
            " This error is most frequently caused by referencing an old",
            " variable name that doesn't exist anymore")
    if (label(x) == "")
        name <- paste0(deparse(substitute(x)), collapse = "")
    else name <- label(x)
    if (is.logical(x)) {
        x <- factor(x, levels = c(TRUE, FALSE))
    }
    else if (is.character(x) && any(table(x, by) == 0)) {
        x <- factor(x)
    }
    if (any(is.na(by))) {
        warning(sprintf("Your 'by' variable has %d missing values",
            sum(is.na(by))), "\n   The corresponding 'x' and 'by' variables are automatically removed")
        x <- x[!is.na(by)]
        if (inherits(x, "factor")) {
            x <- factor(x)
        }
        by <- by[!is.na(by)]
        if (inherits(by, "factor")) {
            by <- factor(by)
        }
    }
    if (useNA == "ifany" && any(is.na(x)))
        useNA <- "always"
    if (show_all_values & deparse(substitute(prop_fn)) == "describeProp")
        prop_fn <- describeFactors
    addEmptyValuesToMakeListCompatibleWithMatrix <- function(t) {
        for (n in names(t)) {
            if (is.matrix(t[[n]])) {
                tmp_names <- rownames(t[[n]])
                t[[n]] <- as.vector(t[[n]])
                names(t[[n]]) <- tmp_names
            }
        }
        all_row_names <- c()
        for (n in names(t)) {
            all_row_names <- union(all_row_names, names(t[[n]]))
        }
        if (is.null(all_row_names))
            return(t)
        if (any(is.na(all_row_names)))
            all_row_names <- append(all_row_names[is.na(all_row_names) ==
                FALSE], NA)
        ret <- list()
        for (n in names(t)) {
            ret[[n]] <- rep(missing_value, times = length(all_row_names))
            names(ret[[n]]) <- all_row_names
            for (nn in all_row_names) {
                if (nn %in% names(t[[n]])) {
                  if (is.na(nn)) {
                    ret[[n]][is.na(names(ret[[n]]))] <- t[[n]][is.na(names(t[[n]]))]
                  }
                  else {
                    ret[[n]][nn] <- t[[n]][nn]
                  }
                }
            }
        }
        return(ret)
    }
    if (is.numeric(x)) {
        if (hrzl_prop)
            t <- by(x, by, FUN = continuous_fn, html = html,
                digits = digits, number_first = numbers_first,
                useNA = useNA, useNA.digits = useNA.digits, horizontal_proportions = table(is.na(x),
                  useNA = useNA), percentage_sign = percentage_sign)
        else t <- by(x, by, FUN = continuous_fn, html = html,
            digits = digits, number_first = numbers_first, useNA = useNA,
            useNA.digits = useNA.digits, percentage_sign = percentage_sign)
        missing_t <- sapply(t, is.null)
        if (any(missing_t)) {
            substitute_t <- rep(missing_value, length(t[!missing_t][[1]]))
            names(substitute_t) <- names(t[!missing_t][[1]])
            for (i in seq_along(t[missing_t])) {
                t[missing_t][[i]] <- substitute_t
            }
        }
        if (all(unlist(sapply(t, is.na))) & !is.null(names_of_missing)) {
            substitute_t <- rep(missing_value, length(names_of_missing))
            names(substitute_t) <- names_of_missing
            substitute_list <- vector("list", length = length(t))
            names(substitute_list) <- names(t)
            for (i in seq_along(substitute_list)) {
                substitute_list[[i]] <- substitute_t
            }
            t <- substitute_list
        }
        if (length(t[[1]]) != 1) {
            fn_name <- deparse(substitute(continuous_fn))
            if (fn_name == "describeMean")
                names(t[[1]][1]) = "Mean"
            else if (fn_name == "describeMedian")
                names(t[[1]][1]) = "Median"
            else names(t[[1]][1]) = fn_name
        }
    }
    else if ((!is.factor(x) && length(unique(na.omit(x))) ==
        2) || (is.factor(x) && length(levels(x)) == 2) && hrzl_prop ==
        FALSE) {
        default_ref <- prDescGetAndValidateDefaultRef(x, default_ref)
        t <- by(x, by, FUN = prop_fn, html = html, digits = digits,
            number_first = numbers_first, useNA = useNA, useNA.digits = useNA.digits,
            default_ref = default_ref, percentage_sign = percentage_sign)
        missing_t <- sapply(t, is.null)
        if (any(missing_t)) {
            substitute_t <- rep(missing_value, length(t[!missing_t][[1]]))
            names(substitute_t) <- names(t[!missing_t][[1]])
            for (i in seq_along(t[missing_t])) {
                t[missing_t][[i]] <- substitute_t
            }
        }
        if (all(unlist(sapply(t, is.na))) & !is.null(names_of_missing)) {
            substitute_t <- rep(missing_value, length(names_of_missing))
            names(substitute_t) <- names_of_missing
            substitute_list <- vector("list", length = length(t))
            names(substitute_list) <- names(t)
            for (i in seq_along(substitute_list)) {
                substitute_list[[i]] <- substitute_t
            }
            t <- substitute_list
        }
        if (unique(sapply(t, length)) == 1)
            name <- sprintf("%s %s", capitalize(levels(x)[default_ref]),
                tolower(name))
        if (NEJMstyle) {
            percent_sign <- ifelse(html, "%", "\\%")
            if (numbers_first)
                name <- sprintf("%s - no (%s)", name, percent_sign)
            else name <- sprintf("%s - %s (no)", name, percent_sign)
        }
        if (length(t[[1]]) == 1) {
            names(t[[1]][1]) <- name
        }
    }
    else {
        if (hrzl_prop) {
            t <- by(x, by, FUN = factor_fn, html = html, digits = digits,
                number_first = numbers_first, useNA = useNA,
                useNA.digits = useNA.digits, horizontal_proportions = table(x,
                  useNA = useNA), percentage_sign = percentage_sign)
        }
        else {
            t <- by(x, by, FUN = factor_fn, html = html, digits = digits,
                number_first = numbers_first, useNA = useNA,
                useNA.digits = useNA.digits, percentage_sign = percentage_sign)
        }
        missing_t <- sapply(t, is.null)
        if (any(missing_t)) {
            substitute_t <- rep(missing_value, length(t[!missing_t][[1]]))
            names(substitute_t) <- names(t[!missing_t][[1]])
            for (i in seq_along(t[missing_t])) {
                t[missing_t][[i]] <- substitute_t
            }
        }
        if (all(unlist(sapply(t, is.na))) & !is.null(names_of_missing)) {
            substitute_t <- rep(missing_value, length(names_of_missing))
            names(substitute_t) <- names_of_missing
            substitute_list <- vector("list", length = length(t))
            names(substitute_list) <- names(t)
            for (i in seq_along(substitute_list)) {
                substitute_list[[i]] <- substitute_t
            }
            t <- substitute_list
        }
    }
    t <- addEmptyValuesToMakeListCompatibleWithMatrix(t)
    results <- matrix(unlist(t), ncol = length(t))
    getHeader <- function(tbl_cnt, header_count, html) {
        if (missing(header_count) || identical(header_count,
            FALSE)) {
            return(names(tbl_cnt))
        }
        if (is.character(header_count)) {
            if (!grepl("%s", header_count, fixed = TRUE))
                stop("Your header_count must accept a string character",
                  " or it will fail to add the count, i.e. use the ",
                  " format: 'Text before %s text after'")
            cnt_str <- sprintf(header_count, txtInt(tbl_cnt))
        }
        else {
            cnt_str <- paste("No.", txtInt(tbl_cnt))
        }
        return(mapply(txtMergeLines, names(tbl_cnt), cnt_str,
            html = html))
    }
    cn <- getHeader(table(by), header_count, html)
    if (class(t[[1]]) == "matrix")
        rownames(results) <- rownames(t[[1]])
    else rownames(results) <- names(t[[1]])
    if (is.null(rownames(results)) && nrow(results) == 1)
        rownames(results) <- name
    if (!missing(add_total_col) && add_total_col != FALSE) {
        total_table <- prGetStatistics(x[is.na(by) == FALSE],
            numbers_first = numbers_first, show_perc = total_col_show_perc,
            show_all_values = show_all_values, useNA = useNA,
            useNA.digits = useNA.digits, html = html, digits = digits,
            continuous_fn = continuous_fn, factor_fn = factor_fn,
            prop_fn = prop_fn, percentage_sign = percentage_sign)
        if (!is.matrix(total_table)) {
            total_table <- matrix(total_table, ncol = 1, dimnames = list(names(total_table)))
        }
        if (nrow(total_table) != nrow(results)) {
            stop("There is a discrepancy in the number of rows in the total table",
                " and the by results: ", nrow(total_table), " total vs ",
                nrow(results), " results", "\n Rows total:",
                paste(rownames(total_table), collapse = ", "),
                "\n Rows results:", paste(rownames(results),
                  collapse = ", "))
        }
        cn_tot <- getHeader(c(Total = length(x[is.na(by) == FALSE])),
            header_count, html)
        if (add_total_col != "last") {
            results <- cbind(total_table, results)
            cn <- c(cn_tot, cn)
        }
        else {
            results <- cbind(results, total_table)
            cn <- c(cn, cn_tot)
        }
    }
    if (isTRUE(use_units)) {
        if (units(x) != "") {
            unitcol <- rep(sprintf("%s", units(x)), times = NROW(results))
            unitcol[rownames(results) == "Missing"] <- ""
        }
        else {
            unitcol <- rep("", times = NROW(results))
        }
        if (length(unitcol) != nrow(results)) {
            stop("There is an discrepancy in the number of rows in the units",
                " and the by results: ", length(unitcol), " units vs ",
                nrow(results), " results", "\n Units:", paste(unitcol,
                  collapse = ", "), "\n Rows results:", paste(rownames(results),
                  collapse = ", "))
        }
        results <- cbind(results, unitcol)
        cn <- c(cn, "units")
    }
    else if (use_units == "name") {
        if (units(x) != "") {
            name <- sprintf("%s (%s)", name, units(x))
        }
    }
    if (is.function(statistics)) {
        if (is.numeric(pval) && pval <= 1 && pval >= 0) {
            pval <- txtPval(pval, lim.sig = statistics.sig_lim,
                lim.2dec = statistics.two_dec_lim, html = html)
            results <- cbind(results, c(pval, rep("", nrow(results) -
                1)))
            cn <- c(cn, "P-value")
        }
        else if (is.character(pval) && !is.null(attr(pval, "colname"))) {
            results <- cbind(results, c(pval, rep("", nrow(results) -
                1)))
            cn <- c(cn, attr(pval, "colname"))
        }
        else {
            stop("Your statistics function should either return a numerical value from 0 to 1 or a character with the attribute 'colname'")
        }
    }
    colnames(results) <- cn
    label(results) <- name
    return(results)
}

#Function to compute N=
describeN = function (x, html = TRUE, digits = 1, number_first = TRUE, useNA = c("ifany", "no", "always"), useNA.digits = digits, percentage_sign = TRUE, plusmin_str, language = "en", ...)
{
    dot_args <- list(...)
    if ("show_missing_digits" %in% names(dot_args)) {
        useNA.digits <- dot_args$show_missing_digits
        dot_args$show_missing_digits <- NULL
        warning("Deprecated: show_missing_digits argument is now useNA.digits as of ver. 1.0")
    }
    if ("show_missing" %in% names(dot_args)) {
        if (missing(useNA)) {
            useNA <- convertShowMissing(dot_args$show_missing)
        }
        dot_args$show_missing <- NULL
        warning("Deprecated: show_missing argument is now useNA as of ver. 1.0")
    }
    useNA <- match.arg(useNA)
    if (missing(plusmin_str))
        if (html)
            plusmin_str <- "&plusmn;"
        else plusmin_str <- "\\pm"
    ret <- paste("N=", length(x[!is.na(x)]), sep="")
    if (html == FALSE)
        ret <- sprintf("$%s$", ret)
    if (useNA %in% c("ifany", "always") & sum(is.na(x)) > 0) {
        ret <- rbind(ret, descGetMissing(x = x, html = html,
            number_first = number_first, percentage_sign = percentage_sign,
            language = language, useNA.digits = useNA.digits,
            dot_args = dot_args))
        rownames(ret) <- c("Mean (SEM)", "Missing")
    }
    else if (useNA == "always") {
        if (percentage_sign == TRUE)
            percentage_sign <- ifelse(html, "%", "\\%")
        else if (is.character(percentage_sign) == FALSE)
            percentage_sign = ""
        empty <- sprintf(ifelse(number_first, "0 (0%s)", "0%s (0)"),
            percentage_sign)
        ret <- rbind(ret, rep(empty, times = NCOL(ret)))
        rownames(ret) <- c("Mean (SEM)", "Missing")
    }
    else {
        names(ret) <- "Mean (SEM)"
    }
    return(ret)
}



#Function to compute mean (SEM)
describeSEM = function (x, html = TRUE, digits = 1, number_first = TRUE, useNA = c("ifany", "no", "always"), useNA.digits = digits, percentage_sign = TRUE, plusmin_str, language = "en", ...)
{
    dot_args <- list(...)
    if ("show_missing_digits" %in% names(dot_args)) {
        useNA.digits <- dot_args$show_missing_digits
        dot_args$show_missing_digits <- NULL
        warning("Deprecated: show_missing_digits argument is now useNA.digits as of ver. 1.0")
    }
    if ("show_missing" %in% names(dot_args)) {
        if (missing(useNA)) {
            useNA <- convertShowMissing(dot_args$show_missing)
        }
        dot_args$show_missing <- NULL
        warning("Deprecated: show_missing argument is now useNA as of ver. 1.0")
    }
    useNA <- match.arg(useNA)
    if (missing(plusmin_str))
        if (html)
            plusmin_str <- "&plusmn;"
        else plusmin_str <- "\\pm"
    ret <- c(sprintf(sprintf("%%.%df (%s%%3.%df)", digits, plusmin_str,
        digits), mean(x, na.rm = T), sd(x, na.rm = T)/sqrt(length(x[!is.na(x)]))))
    if (html == FALSE)
        ret <- sprintf("$%s$", ret)
    if (useNA %in% c("ifany", "always") & sum(is.na(x)) > 0) {
        ret <- rbind(ret, descGetMissing(x = x, html = html,
            number_first = number_first, percentage_sign = percentage_sign,
            language = language, useNA.digits = useNA.digits,
            dot_args = dot_args))
        rownames(ret) <- c("Mean (SEM)", "Missing")
    }
    else if (useNA == "always") {
        if (percentage_sign == TRUE)
            percentage_sign <- ifelse(html, "%", "\\%")
        else if (is.character(percentage_sign) == FALSE)
            percentage_sign = ""
        empty <- sprintf(ifelse(number_first, "0 (0%s)", "0%s (0)"),
            percentage_sign)
        ret <- rbind(ret, rep(empty, times = NCOL(ret)))
        rownames(ret) <- c("Mean (SEM)", "Missing")
    }
    else {
        names(ret) <- "Mean (SEM)"
    }
    return(ret)
}

################################
#FUNCTION TO AUTOMATE TABLE 1S"#
################################

#data is data frame
#var.names is list of data frame variables you wish to include as rows in the table
#ind.cat is 0/1 vector of same length as var.names equal to 0 if the variable is continuous and 1 if categorical
#group.name is the categorical variable you wish to display in the columns
#cfn is how you summarize continuous variables; describeMean if mean (sd) desired; describeMedian if median (IQR) desired
#miss is equal to "no" if no missing values to be displayed; "always" if always display (even if 0); "ifany" only display if any missing values
#NOTE: set miss="always" if you want both levels displayed for binary variables
#pval is equal to TRUE if want p-values; FALSE if not
#tot is equal to "last" if you want a column for totals and FALSE if not
#digit is the number of digits you want displayed, e.g., 1 if 12.1, 2 if 12.12, etc.
#NOTE: I recommend prefixing categories of categorical variables with alphabetized letters to ensure ordering in the table, e.g., for race, code it as "a.Black", "b.White", etc.

mktab = function(data, var.names, ind.cat, group.name, cfn, miss, pval, tot, digit, kruskal=FALSE) {
    n = names(data)
    cols = as.character(data[, which(n==group.name)])
    r = table(data[, which(n==group.name)])
    if (tot=="last") {r = matrix(c(r, sum(r)), 1, length(r)+1)} #update r - this is why I was having trouble!!
    j = c(apply(r, 1, function(x) paste("n=",x,sep="")))
    for (i in 1:length(var.names)) {
        if (ind.cat[i]==0) {
            outc = data[, which(n==var.names[i])]
            #label(outc) = var.names[i]
            if (kruskal==FALSE) {
                dd2 = getDescriptionStatsBy(outc, cols, html=TRUE, useNA=miss, statistics=pval, add_total_col=tot, continuous_fn=cfn, digits=digit)}
            else if (kruskal==TRUE) {
                    dd2 = getDescriptionStatsBy2(outc, cols, html=TRUE, useNA=miss, statistics=pval, add_total_col=tot, continuous_fn=cfn, digits=digit)
            }
            rownames(dd2)[1] = var.names[i]
        }
        else if (ind.cat[i]==1) {
            outc = data[, which(n==var.names[i])]
            #label(outc) = var.names[i]
            dd1 = getDescriptionStatsBy(factor(outc), cols, html=TRUE, useNA=miss, statistics=pval, add_total_col=tot, digits=digit)
            dd2 = rbind(rep("", dim(dd1)[2]), dd1)
            rownames(dd2)[1] = var.names[i]
        }
      j = rbind(j, dd2)
    }

    #Clean up a few things
    k = t(apply(j, 1, function(x) gsub("&plusmn;", "\\±", x)))
    k2 = t(apply(k, 1, function(x) gsub("&lt; ", "<", x)))
    rownames(k2)[rownames(k2)=="j"] = ""
    rownames(k2) = lapply(rownames(k2),function(x) gsub("a[.]", " ", x))
    rownames(k2) = lapply(rownames(k2),function(x) gsub("b[.]", " ", x))
    rownames(k2) = lapply(rownames(k2),function(x) gsub("c[.]", " ", x))
    rownames(k2) = lapply(rownames(k2),function(x) gsub("d[.]", " ", x))
    rownames(k2) = lapply(rownames(k2),function(x) gsub("e[.]", " ", x))
    rownames(k2) = lapply(rownames(k2),function(x) gsub("f[.]", " ", x))
    colnames(k2) = lapply(colnames(k2),function(x) gsub("a[.]", " ", x))
    colnames(k2) = lapply(colnames(k2),function(x) gsub("b[.]", " ", x))
    colnames(k2) = lapply(colnames(k2),function(x) gsub("c[.]", " ", x))
    colnames(k2) = lapply(colnames(k2),function(x) gsub("d[.]", " ", x))
    colnames(k2) = lapply(colnames(k2),function(x) gsub("e[.]", " ", x))
    colnames(k2) = lapply(colnames(k2),function(x) gsub("f[.]", " ", x))
    #Remove uninformative missing values if missing="ifany"
    if (miss=="always") {n0=apply(k2, 1, function(x) sum(x=="0 (0%)" | x=="0 (0.0%)" | x==""))
    if (pval==TRUE) {r=matrix(c(r,""), 1, length(r)+1)}
    rmv = which(rownames(k2)=="Missing" & n0==length(r))
    if (length(rmv)>0) {k2 = k2[-as.numeric(rmv), ]}}
    #Remove "n=" below P-value if pval=TRUE
    if (pval==TRUE) {k2[1, colnames(k2)=="P-value"] = ""}
    k2
}

#tab2 = mktab(data=d4, var.names=c("M1DEM_MOTHERS_AGE"), ind.cat=c(0), group.name="FPI1", cfn=describeMedian, miss="always", pval=TRUE, tot="last", digit=1)


#############################
#Function for two-way tables#
#############################

#data is data frame
#rowvar is the row variable to summarize
#colvar is the column variable to summarize
#disp.var is the display variable
#ind.bin is whether or not the display variable is binary (TRUE if yes, FALSE if no)
#digit is the number of digits to display in the statistics
mktab2 = function(data, rowvar, colvar, disp.var, ind.bin, tot=FALSE, digit) {
    row = names(table(data[, names(data)==rowvar]))
    col = names(table(data[, names(data)==colvar]))
    mat = matrix(NA, nrow=length(row), ncol=length(col))
    rownames(mat) = row
    colnames(mat) = col
    disp = as.numeric(data[, names(data)==disp.var]) #variable in table
    rtot = rep(NA, length(row)) #row totals
    ctot = rep(NA, length(col)) #column totals
    for (i in 1:length(row)) {
    for (j in 1:length(col)) {
        v1 = rownames(mat)[i]
        v2 = colnames(mat)[j]
        if (ind.bin==TRUE) {
            mat[i,j] = paste(sum(disp[which(data[, names(data)==rowvar]==v1 & data[, names(data)==colvar]==v2)], na.rm=TRUE) , paste(length(disp[which(!is.na(disp) & data[, names(data)==rowvar]==v1 & data[, names(data)==colvar]==v2)]), paste(paste("(", round(100*mean(disp[which(data[, names(data)==rowvar]==v1 & data[, names(data)==colvar]==v2)], na.rm=TRUE), digit), sep=""), ")", sep="%"), sep=" "), sep="/")
            rtot[i] = paste(sum(disp[which(data[, names(data)==rowvar]==v1)], na.rm=TRUE) , paste(length(disp[which(!is.na(disp) & data[, names(data)==rowvar]==v1)]), paste(paste("(", round(100*mean(disp[which(data[, names(data)==rowvar]==v1)], na.rm=TRUE), digit), sep=""), ")", sep="%"), sep=" "), sep="/")
            ctot[j] = paste(sum(disp[which(data[, names(data)==colvar]==v2)], na.rm=TRUE) , paste(length(disp[which(!is.na(disp) & data[, names(data)==colvar]==v2)]), paste(paste("(", round(100*mean(disp[which(data[, names(data)==colvar]==v2)], na.rm=TRUE), digit), sep=""), ")", sep="%"), sep=" "), sep="/")
        }
        else if (ind.bin==FALSE) {
            mat[i,j] = paste(round(mean(disp[which(data[, names(data)==rowvar]==v1 & data[, names(data)==colvar]==v2)], na.rm=TRUE), digit), paste("(", paste(round(sd(disp[which(data[, names(data)==rowvar]==v1 & data[, names(data)==colvar]==v2)], na.rm=TRUE), digit), ")", sep=""), sep="±"), sep=" ")
            rtot[i] = paste(round(mean(disp[which(data[, names(data)==rowvar]==v1)], na.rm=TRUE), digit), paste("(", paste(round(sd(disp[which(data[, names(data)==rowvar]==v1)], na.rm=TRUE), digit), ")", sep=""), sep="±"), sep=" ")
            ctot[j] = paste(round(mean(disp[which(data[, names(data)==colvar]==v2)], na.rm=TRUE), digit), paste("(", paste(round(sd(disp[which(data[, names(data)==colvar]==v2)], na.rm=TRUE), digit), ")", sep=""), sep="±"), sep=" ")

    }
    }
}
    if (tot==TRUE) {
        mat = cbind(mat, rtot)
        if (ind.bin==TRUE) {
            ovtot = paste(sum(disp, na.rm=TRUE) , paste(length(disp), paste(paste("(", round(100*mean(disp, na.rm=TRUE), digit), sep=""), ")", sep="%"), sep=" "), sep="/")
            }
        else if (ind.bin==FALSE) {
            ovtot = paste(round(mean(disp, na.rm=TRUE), digit), paste("(", paste(round(sd(disp, na.rm=TRUE), digit), ")", sep=""), sep="±"), sep=" ")
            }
        ctot = c(ctot, ovtot)
        mat = rbind(mat, ctot)
        }
mat
}

##########################################
#Function to automate Word table creation#
##########################################
##Simpler word.tab function using officer
word.tab = function(tab, dest, help=FALSE) {
tab = data.frame(tab)
if (help==TRUE) {
    tab = cbind(rownames(tab), tab)
    colnames(tab)[1] = " "
    colnames(tab)[colnames(tab)=="P.value"] = "P-value"
}
NarrativeDoc = read_docx()
NarrativeDoc = body_add_table(NarrativeDoc, tab)
print(NarrativeDoc, target=dest)
}


#mktab(data=mtcars, var.names=c("mpg"), ind.cat=c(0), group.name="gear", cfn=describeMean, miss="always", pval=TRUE, tot=FALSE, kruskal=TRUE, digit=1)

#mktab(data=mtcars, var.names=c("mpg"), ind.cat=c(0), group.name="gear", cfn=describeMean, miss="always", pval=TRUE, tot=FALSE, kruskal=FALSE, digit=1)


#kruskal.test(mtcars$mpg, mtcars$gear)
#anova(lm(mpg ~ factor(gear), data=mtcars)) #make sure gear is a factor variable

#word.tab(tab=tt, dest="/Users/jrigdon/Box sync/Rigdon/Useful Functions/Joe3.docx", help=TRUE)

#out = anova(lm(drat ~ vs, data=mtcars))
#out[["Pr(>F)"]][[1]]









