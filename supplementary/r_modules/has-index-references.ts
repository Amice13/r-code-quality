/**
 * Detects whether an R script contains index-based data access patterns.
 *
 * The function scans the content line-by-line and looks for:
 * - Indexing via brackets (e.g. `x[1]`, `df[1, 2]`, `x[c(1,2)]`)
 * - Nested or named access (e.g. `df$col[1]`, including quoted names)
 * - `select()` calls with numeric indices (e.g. `select(df, 1:3)` or `select(df, c(1,2))`)
 *
 * @param content - Raw R script or text to analyze.
 * @returns `true` if any index-based access pattern is detected, otherwise `false`.
 */

import { validVariableRegex } from './constants.ts'

const quotedVariables = '(?<quote>[\'"`])[^\\\\]+\\k<quote>'
const completeVariable = `${validVariableRegex}(?:\\$?${quotedVariables})?`
const indexPattern = '(?:\\d|c\\([\\d\\s,:]+\\))'
const selectRegex = 'select\\s*\\((?:\\s*-?c\\s*\\()?\\d.*'
const findIndicesRegexp = new RegExp(`(?:${completeVariable}\\s*\\[+(?:\\d+\\s*\\]|${indexPattern}|[^\\]]*?,\\s*${indexPattern}\\s*\\])|${selectRegex})`, 'g')

export const hasIndexReferences = (content: string) => {
  const lines = content.split(/[\r\n]+/g)  
  const matches = lines.map(line => {
    line = line.replace(/#.*/g, '')
    return line.match(findIndicesRegexp)
  }).filter(Boolean)
  return matches.length > 0
}
