import { validVariableRegex, stripStringsAndComments } from './constants.ts'
import { getVariableNames } from './index.ts'
import { rBaseFunctions } from './r-base-functions.ts'

const attachRegex = /^[^#]*\battach\(/
const rhaRegex = new RegExp(`^[^#]*->\\s*(?<name_rha>${validVariableRegex})(?=\\s*(?:[\n\r]|$))`)

const infixOperators = '(?:[=+-]|<-|->|\\|>|%>%|%<%|%<>%|\\||\\|\\||&&|&|<|>)'
const infixExceptions = '(?::::|::|:)'

const noInfixSpacesRegex = new RegExp(`[a-z\\d]${infixOperators}|${infixOperators}[a-z\\d.]`)
const wrongInfixSpacesRegex = new RegExp(`\\s+${infixExceptions}(?!\\s?=)|${infixExceptions}\\s+`, 'g')

const keywordsWithSpace = [
  'if',
  'for',
  'while',
  'repeat',
  'switch',
  'function',
  'return'
]

interface Issue {
  name: string
  line: number
}

export const getFormattingIssues = (content: string) => {

  const issues: Issue[] = []
  let n = 0

  const lines = content.split(/[\n\r]+/g)
  const variables = getVariableNames(content)

  // Avoid using names of existing functions and variables - http://adv-r.had.co.nz/Style.html
  for (const variable of variables) {
    if (rBaseFunctions.includes(variable.name)) {
      issues.push({ name: 'Existing function is re-assigned', line: variable.line })
    }
  }

  for (let line of lines) {
    n++

    if (line.length > 80) issues.push({ name: 'Line length is greater than 80', line: n })

    // Skip comments
    if (line.match(/^\s*#/)) continue

    // Replace comments
    line = stripStringsAndComments(line)

    // Don't use attach() - https://google.github.io/styleguide/Rguide.html
    const usesAttach = attachRegex.test(line)
    if (usesAttach) issues.push({ name: 'Attach is used', line: n })

    // Right-hand assignment - https://google.github.io/styleguide/Rguide.html - ->
    const usesRHA = rhaRegex.test(line)
    if (usesRHA) issues.push({ name: 'Right-hand assignment is used', line: n })

    // No spaces around all infix operators - http://adv-r.had.co.nz/Style.html
    const hasInfixWithoutSpaces = noInfixSpacesRegex.test(line)
    if (hasInfixWithoutSpaces) issues.push({ name: 'Has no spaces around infix operators', line: n })

    // Spaces around exceptional infix operators - http://adv-r.had.co.nz/Style.html
    const hasWrongInfixSpacesRegex = wrongInfixSpacesRegex.test(line)
    if (hasWrongInfixSpacesRegex) issues.push({ name: 'Has spaces around infix operators', line: n })

  }

  // Place a space before left parentheses, except in a function call - http://adv-r.had.co.nz/Style.html
  // Do not place spaces around code in parentheses or square brackets - http://adv-r.had.co.nz/Style.html
  // An opening curly brace should never go on its own line and should always be followed by a new line. - http://adv-r.had.co.nz/Style.html
  // Strive to limit your code to 80 characters per line. - http://adv-r.had.co.nz/Style.html
  // When indenting your code, use two spaces. Never use tabs or mix tabs and spaces. -- http://adv-r.had.co.nz/Style.html
  // Use <-, not =, for assignment - http://adv-r.had.co.nz/Style.html
  // File names should be machine readable - https://style.tidyverse.org/files.html
  // Use commented lines of - and = to break up your file into easily readable chunks - http://adv-r.had.co.nz/Style.html
  // Variable and function names should use only lowercase letters, numbers, and _. - https://style.tidyverse.org/syntax.html
  // If you find yourself attempting to cram data into variable names (e.g. model_2018, model_2019, model_2020), consider using a list or data frame instead. - https://style.tidyverse.org/syntax.html
  // The embracing operator, { }, should always have inner spaces to help emphasise its special behaviour: - https://style.tidyverse.org/syntax.html
  // Single-sided formulas when the right-hand side is a single identifier. - https://style.tidyverse.org/syntax.html
  // When used in tidy evaluation !! (bang-bang) and !!! (bang-bang-bang) (because they have precedence equivalent to unary -/+). - https://style.tidyverse.org/syntax.html
  // The help operator - https://style.tidyverse.org/syntax.html ???
  // Extra spaces - https://style.tidyverse.org/syntax.html
  // Vertical space - https://style.tidyverse.org/syntax.html
  // The body of a loop must be a braced expression. - https://style.tidyverse.org/syntax.html
  // message <- if (x > 10) { "big" } else { "small" } - https://style.tidyverse.org/syntax.html
  // Semicolons are never recommended. In particular, don’t put ; at the end of a line, and don’t use ; to put multiple commands on one line. - https://style.tidyverse.org/syntax.html
  // Use ", not ', for quoting text. The only exception is when the text already contains double quotes and no single quotes. - https://style.tidyverse.org/syntax.html
  // Prefer TRUE and FALSE over T and F. - https://style.tidyverse.org/syntax.html
  // Each line of a comment should begin with the comment symbol and a single space: # - https://style.tidyverse.org/syntax.html
  // Only use return() for early returns. Otherwise, rely on R to return the result of the last evaluated expression. - https://style.tidyverse.org/functions.html
  // |> should always have a space before it, and should usually be followed by a new line. After the first step, each line should be indented by two spaces.  - https://style.tidyverse.org/pipes.html
  // Short pipes - https://style.tidyverse.org/pipes.html
  // Variable name and assignment on separate lines, Variable name and assignment on the same line: - https://style.tidyverse.org/pipes.html
  // magrittr - https://style.tidyverse.org/pipes.html
  // #' @describeIn something-cool Get the mean - https://style.tidyverse.org/package-files.html
  // #' Combine values into a vector or list - https://style.tidyverse.org/documentation.html

  // variable names - datasets vs vars // snake case, camel case, etc
  return issues
}

