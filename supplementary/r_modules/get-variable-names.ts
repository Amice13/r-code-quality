import { validVariableRegex, stripStringsAndComments } from './constants.ts'

/**
 * Extracts variable names from an R script by detecting assignment expressions.
 *
 * Supports:
 * - Left-hand assignments: `x <- ...`, `x = ...`, `x %<>% ...`
 * - Right-hand assignments: `... -> x`
 *
 * Returns a normalized structure for each detected variable:
 * - `name`: variable name
 * - `type`: `'lha'` (left-hand assignment) or `'rha'` (right-hand assignment)
 *
 * @param content - Raw R script or text to analyze.
 * @returns Array of detected variable assignments with their types.
 */

const lhaRegex = `(?:^\\s*|;\\s*)(?<name>${validVariableRegex})\\s*(?:<-|=|%<>%)`
const rhaRegex = `->\\s*(?<name_rha>${validVariableRegex})(?=\\s*(?:[\n\r]|$))`
const variableNameRegex = new RegExp(`${lhaRegex}|${rhaRegex}`, 'g')

export const getVariableNames = (content: string) => {
  const lines = content.split(/[\n\r]+/g)
  let parenthesesBalance = 0
  const variables: { name: string; type: 'lha' | 'rha'; line: number }[] = []

  lines.forEach((line, idx) => {
    const cleanLine = stripStringsAndComments(line)
    if (parenthesesBalance === 0) {
      for (const match of cleanLine.matchAll(variableNameRegex)) {
        const groups = match.groups
        if (!groups) continue
        variables.push({
          name: groups.name ?? groups.name_rha!,
          type: groups.name !== undefined ? 'lha' : 'rha',
          line: idx + 1 // lines are 1-based
        })
      }
    }
    for (const char of cleanLine) {
      if (char === '(' || char === '[') parenthesesBalance++
      if (char === ')' || char === ']') parenthesesBalance--
    }
  })

  return variables
}