/**
 * Detects whether an R script explicitly defines column types when reading data.
 *
 * Specifically, it looks for common column type parameters:
 * - `col_types` (readr/tidyverse)
 * - `col.type` (some custom conventions)
 * - `colClasses` (base R)
 *
 * @param content - Raw R code or text to analyze.
 * @returns `true` if any explicit column type parameter is found, otherwise `false`.
 */

const columnDefinitionRegex = /(^|[^#]*)(col_types|col.type|colClasses)/
export const hasExplicitColumnDefinitions = (content: string) => {
  return columnDefinitionRegex.test(content)
}
