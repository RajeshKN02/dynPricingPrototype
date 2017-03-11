#'
#' Appends an "BETWEEN" condition. If lowerBound is not null, the condition that
#' column is between lowerBound and upperBound is appended to conditions.
#'
#' @param conditions existing conditions to which new condition is appended
#' @param column name of the column
#' @param lowerBound accepted start value
#' @param upperBound accepted end value
#' @param quote If TRUE the values are quoted by a single quote (required for
#' character values).
#' @param expression if not null, this expression is applied to each value by
#' replacing "%" with the value. E.g. if expression="to_char(%)", a value "4711"
#' appears as "to_char(4711)" in the condition.
#'
#' @return either conditions or conditions + new condition
#'
appendBetweenCondition <- function(conditions, column, lowerBound, upperBound, quote=FALSE, expression=NULL) {
    if (is.null(lowerBound)) {
        return(conditions)
    }

    if (quote) {
        lowerBound <- quoteSql(lowerBound)
        upperBound <- quoteSql(upperBound)
    }
    lowerBound <- applyExpression(lowerBound, expression)
    upperBound <- applyExpression(upperBound, expression)

    cond <- paste(column, "between", lowerBound, "and", upperBound)
    append(conditions, cond)
}

#'
#' Appends condition for a date.
#'
#' @param conditions existing conditions to which new condition is appended
#' @param column name of the column
#' @param date the date as class "date"
#'
#' @return either conditions or conditions + new condition
#'
appendDateCondition <- function(conditions, column, date) {
    if (is.null(date)) {
        return(conditions)
    }

    cond <- paste(column, "=", paste("to_date(", quoteSql(date), ", 'YYYY-MM-DD')") )
    append(conditions, cond)
}

#'
#' Appends an "IN" condition. If values is not null, the condition that
#' column is in the given values is appended to conditions.
#'
#' @param conditions existing conditions to which new condition is appended
#' @param column name of the column
#' @param values (vector) accepted values
#' @param quote If TRUE the values are quoted by a single quote (required for
#' character values).
#' @param expression if not null, this expression is applied to each value by
#' replacing "%" with the value. E.g. if expression="to_char(%)", a value "4711"
#' appears as "to_char(4711)" in the condition.
#'
#' @return either conditions or conditions + new condition
#'
appendInCondition <- function(conditions, column, values, quote=FALSE, expression=NULL) {
    if (is.null(values)) {
        return(conditions)
    }

    if (quote) {
        values <- quoteSql(values)
    }
    values <- applyExpression(values, expression)

    cond <- paste(column, "in (", paste(values, collapse=","),")" )
    append(conditions, cond)
}

#'
#' Appends an "INSTR" condition, i.e. searches for a string in the column. <br>
#' If the search string is empty, nothing is added.
#'
#' @param conditions existing conditions to which new condition is appended
#' @param column name of the column
#' @param searchString string we look for in the column
#'
#' @return either conditions or conditions with new condition
#'
appendInstrCondition <- function(conditions, column, searchString) {
    if (searchString == "") {
        return(conditions)
    }

    cond <- paste("instr(", column, ",", searchString, ") > 0")
    append(conditions, cond)
}

#'
#' Joins several conditions to SQL WHERE clause.
#'
#' @param conditions vector
#' @param operator joining the conditions
#'
#' @return "where c1 [operator] c2 ..." or empty string
#'
buildWhere <- function(conditions, operator="and") {
    if (length(conditions)==0){
        return ("")
    }
    operator <- paste0(" ",operator, " ")
    paste0("where ", paste(conditions, collapse=operator))
}

#'
#' Quotes a string in SQL style, i.e. using single quotes
#'
#' @param string to quote
#'
#' @return quoted string
#'
quoteSql <- function(string) {
    # escape single quotes in the string
    string <- gsub("'", "''", string)
    # surround with single quotes
    paste0 ("'", string, "'")
}

#'
#' Replaces every element in values by substituted expression. A "%" in expression
#' is replaced by the actual element.
#'
#' Example: applyExpression(c(1, 2), "to_char(%)" yields c("to_char(1)", "to_char(2)")
#'
#' @param values vector
#' @param expression containing one or more percent characters. If NULL, values is returned
#'
#' @return vector of the same size as values
#'
applyExpression <- function(values, expression){
    if (is.null(expression)){
        return (values)
    }
    sapply(values, function(x) gsub("%", x, expression))
}

#'
#' Function that constructs a temporary table with one column (columnName) and the given
#' dates as the row values.
#'
#' @param dates a list with the dates
#' @param columnName a string with the name for the result column
#'
#' @return a charactor with the temporary table
selectDatesFromDual <- function(dates, columnName) {
    result <- ""
    newline <- "\n"
    for (index in 1:length(dates)) {
        if (index > 1) {
            append(result, paste("UNION ALL", newline))
        }
        date <- dates[index]
        cond <- paste("SELECT", paste("to_date(", quoteSql(date), ", 'YYYY-MM-DD')"), "AS", columnName, newline, "FROM dual", newline )
        result <- paste(result, cond)
    }

    return(result)
}