import { stripStringsAndComments } from './constants.ts'
import { getSpacingIssues } from './lint-rules/get-spacing-issues.ts'
import { getInfixWithoutSpacesAround } from './lint-rules/get-infix-without-spaces-around.ts'
import { getColonWithSpacesAround } from './lint-rules/get-colon-with-spaces-around.ts'
import { getReassignedVariables } from './lint-rules/get-reassigned-variables.ts'
import { getBadComments } from './lint-rules/get-bad-comments.ts'
import { hasAttach } from './lint-rules/has-attach.ts'
import { hasRha } from './lint-rules/has-rha.ts'
import { hasHelp } from './lint-rules/has-help.ts'
import { getBadVerticalSpacing } from './lint-rules/get-bad-vertical-spacing.ts'
import { getBadEmbracing } from './lint-rules/get-bad-embracing.ts'
import { getLoopsWithoutBraces } from './lint-rules/get-loops-without-braces.ts'
import { getSemicolons } from './lint-rules/get-semicolons.ts'
import { getTf } from './lint-rules/get-tf.ts'
import { getBadQuotes } from './lint-rules/get-bad-quotes.ts'
import { getPipesWithoutSpaces } from './lint-rules/get-pipes-without-spaces.ts'
import { getBracedIfElse } from './lint-rules/get-braced-if-else.ts'
import { getFormulasWithRhs } from './lint-rules/get-formulas-with-rhs.ts'

// Use <-, not =, for assignment - http://adv-r.had.co.nz/Style.html
// File names should be machine readable - https://style.tidyverse.org/files.html
// Variable and function names should use only lowercase letters, numbers, and _. - https://style.tidyverse.org/syntax.html
// If you find yourself attempting to cram data into variable names (e.g. model_2018, model_2019, model_2020), consider using a list or data frame instead. - https://style.tidyverse.org/syntax.html
// magrittr - https://style.tidyverse.org/pipes.html
// variable names - datasets vs vars // snake case, camel case, etc

interface Issue {
  name: string
  line?: number
}

export const getFormattingIssues = (content: string) => {

  const issues: Issue[] = []
  let n = 0

  const reassingedVariables = getReassignedVariables(content)
  issues.push(...reassingedVariables)
  const spacingIssues = getSpacingIssues(content)
  issues.push(...spacingIssues)
  const infixIssues = getInfixWithoutSpacesAround(content)
  issues.push(...infixIssues)
  const colonIssues = getColonWithSpacesAround(content)
  issues.push(...colonIssues)
  const badVerticalSpacing = getBadVerticalSpacing(content)
  issues.push(...badVerticalSpacing)
  const semicolons = getSemicolons(content)
  issues.push(...semicolons)
  const badQuotes = getBadQuotes(content)
  issues.push(...badQuotes)

  const withoutComments = stripStringsAndComments(content)

  // When indenting your code, use two spaces. Never use tabs or mix tabs and spaces. -- http://adv-r.had.co.nz/Style.html
  if (/^\t+/.test(withoutComments) && /^ +/.test(withoutComments)) {
    issues.push({ name: 'Mix of tabs and spaces for indentation' })
  }

  const lines = content.split(/\r?\n/g)
  for (let line of lines) {
    n++

    // Strive to limit your code to 80 characters per line. - http://adv-r.had.co.nz/Style.html
    if (line.length > 80) issues.push({ name: 'Line length is greater than 80', line: n })

    // Skip comments
    if (line.match(/^\s*#/)) continue

    // Replace comments
    line = stripStringsAndComments(line)

    const attach = hasAttach(line)
    if (attach !== null) issues.push({ ...attach, line: n })

    const rha = hasRha(line)
    if (rha !== null) issues.push({ ...rha, line: n })

    const badComments = getBadComments(line)
    if (badComments !== null) issues.push({ ...badComments, line: n })

    const help = hasHelp(line)
    if (help !== null) issues.push({ ...help, line: n })

    const badEmbracing = getBadEmbracing(line)
    issues.push(...badEmbracing.map(el => ({ ...el, line: n })))

    const loopWithoutBraces = getLoopsWithoutBraces(line)
    if (loopWithoutBraces !== null) issues.push({ ...loopWithoutBraces, line: n })

    const tfs = getTf(line)
    issues.push(...tfs.map(el => ({ ...el, line: n })))

    const pipesWithoutSpaces = getPipesWithoutSpaces(line)
    issues.push(...pipesWithoutSpaces.map(el => ({ ...el, line: n })))

    const bracedIfElse = getBracedIfElse(line)
    if (bracedIfElse !== null) issues.push({ ...bracedIfElse, line: n })

    const formulasWithRhs = getFormulasWithRhs(line)
    if (formulasWithRhs.length > 0) console.log(...formulasWithRhs.map(el => ({ ...el, line: n })))

  }

  // Single-sided formulas when the right-hand side is a single identifier. - https://style.tidyverse.org/syntax.html
  return issues
}

