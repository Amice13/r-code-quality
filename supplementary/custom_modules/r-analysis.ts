const validVariableRegex = '(?:\\.[a-z\\d_]+|[a-z][a-z\\d_]+)'

/**
 * Analyzes a text block line-by-line and produces basic structural metrics.
 *
 * @param content - Raw text content to analyze.
 * @returns An object containing line statistics:
 * - `numberOfLines`: total number of lines in the input
 * - `numberOfComments`: lines beginning with a comment marker
 * - `numberOfFormattedComments`: comment lines containing a secondary `#`
 *   pattern used for visual formatting or section separators
 * - `numberOfEmptyLines`: lines containing only whitespace
 */

export const analyzeLines = (content: string) => {
  const lines = content.split(/[\n\r]/g)
  const commentLines = lines.filter(line => /^[\s\t]*#/.test(line))
  const commentFormattingLines = commentLines.filter(line => /#[\s\t]*#/.test(line))
  const emptyLines = lines.filter(line => /^[\s\t]*$/gm.test(line))
  
  return {
    numberOfLines: lines.length,
    numberOfComments: commentLines.length,
    numberOfFormattedComments: commentFormattingLines.length,
    numberOfEmptyLines: emptyLines.length
  }
}

/**
 * Detects usage of pipe operators within a text block.
 *
 * @param content - Raw text content to analyze.
 * @returns An object containing boolean flags:
 * - `hasMagrittrPipes`: true if magrittr pipe (`%>%`) operator is found
 * - `hasNativePipes`: true if native pipe (`|>`) is found
 * - `hasCompoundAssignmentPipes`: true if compound assignemnt pipe (`%<>%`) is found 
 */

const magrittrPipeRegex = /%>%>/
const compoundAssignmentRegex = /%<>%/
const nativePipeRegex = /\|>/
export const analyzePipes = (content: string) => {
  const hasMagrittrPipes = magrittrPipeRegex.test(content)
  const hasNativePipes = nativePipeRegex.test(content)
  const hasCompoundAssignmentPipes = compoundAssignmentRegex.test(content)
  return {
    hasMagrittrPipes,
    hasNativePipes,
    hasCompoundAssignmentPipes
  }
}

/**
 * Extracts function names from a text block by scanning characters
 * and detecting names immediately followed by an opening parenthesis `(`.
 *
 * @param content - Raw text content to analyze.
 * @param startPosition - Optional offset to begin parsing from.
 * @returns Array of detected function names in order of appearance.
 */

export const getFunctionNames = (content: string, startPosition = 0) => {
  const functionNames: string[] = []
  content = content.slice(startPosition)

  let currentName = ''
  let inString: string | null = null
  let escaped = false
  let inComment = false

  for (const char of content) {
    if (inComment) {
      if (char === '\n') inComment = false
      continue
    }

    if (escaped) {
      escaped = false
      continue
    }

    if (char === '\\') {
      escaped = true
      continue
    }

    if (inString) {
      if (char === inString) inString = null
      continue
    } else if (['\'', '"', '`'].includes(char)) {
      inString = char
      continue
    }

    if (char === '#') {
      inComment = true
      continue
    }

    // Function name detected when '(' appears outside strings/comments
    if (char === '(') {
      if (currentName) functionNames.push(currentName)
      currentName = ''
      continue
    }

    // Only accumulate valid function name characters
    currentName = /[a-zA-Z0-9._:]/.test(char) ? currentName + char : ''
    if (/^[\d:]/.test(currentName)) currentName = ''
    if (/\.\d/.test(currentName)) currentName = ''
    if (/[^:]:[^:]$/.test(currentName)) currentName = ''
  }

  return functionNames
}

const fileFormats = [
  'arff',
  'csv',
  'csv2',
  'dat',
  'DAT',
  'dbf',
  'delim',
  'delim2',
  'dta',
  'dta13',
  'epiinfo',
  'https',
  'mtp',
  'octave',
  'R',
  'r',
  'rda',
  'Rda',
  'RData',
  'Rdata',
  'Rds',
  'rds',
  'S',
  'sav',
  'spss',
  'ssd',
  'systat',
  'table',
  'text',
  'tsv',
  'txt',
  'TXT',
  'xp',
  'xport',
  'XPT'
]

/**
 * Extracts file-reading calls from a text block and derives metadata about referenced file paths.
 *
 * For each detected call, it attempts to extract:
 * - function name (`fn`)
 * - file format (`format`)
 * - filename/path argument (`filename`)
 * - quote type used (`quote`)
 *
 * The function also applies heuristic checks on extracted paths:
 * - `pathIsWinValid`: filters out clearly invalid Windows path characters,
 *   while allowing placeholder-like paths (e.g. starting with `...`)
 * - `pathIsOnline`: detects URLs (http/ftp)
 * - `pathIsGlobal`: identifies absolute/global paths (`~`, drive letters, `/`)
 * - `pathOutsideScriptFolder`: detects relative paths navigating upward (`../`)
 *
 * @param content - Raw R code or text to analyze.
 * @returns Array of objects describing detected file-reading operations and path characteristics.
 */

const readFunctionsRegex = `(?<=^|[^#]*?)(?<fn>readLines|gsheet2tbl|read_excel|read(?:\.file)?[._](?<format>${fileFormats.join('|')}))`
const filenameRegex = '\\s*\\((?:(?:\\s*file\\s*=\\s*)?(?<quote>[\'"`])(?<filename>(?:\\\\.|(?!\\k<quote>).)*)\\k<quote>)?'
const fileReaderRegex = new RegExp(`${readFunctionsRegex}${filenameRegex}`, 'gmi')
const pathIsWinValid = (p: string) => !/[<>:"|?*]/.test(p) || /^\.\.\./.test(p)
const pathIsOnline = (p: string) => /^(http|ftp)/.test(p)
const pathIsGlobal = (p: string) => /^(~|[A-Z]:|\/)/.test(p)
const pathOutsideScriptFolder = (p: string) => /^\.\.[\/\\]/.test(p)

export const getFiles = (content: string) => {
  const matches = [...content.matchAll(fileReaderRegex)]
  const results = matches.map(match => {
    const result: Record<string, string | undefined | boolean> = {
      fullMatch: match[0],
      fn: match.groups?.fn,
      format: match.groups?.format,
      filename: match.groups?.filename,
      quote: match.groups?.quote
    }
    if (result.fn === 'read_excel') result.format = 'xlsx'
    if (result.fn === 'readLines' && typeof result.filename === 'string') {
      result.format = result.filename?.split('.').at(-1)
    }
    if (result.fn === 'gsheet2tbl' && typeof result.filename === 'string') {
      result.format = 'Google sheet'
    }
    if (result.filename !== undefined && typeof result.filename !== 'boolean') {
      result.pathIsWinValid = pathIsWinValid(result.filename)
      result.pathIsOnline = pathIsOnline(result.filename)
      result.pathIsGlobal = pathIsGlobal(result.filename)
      result.pathOutsideScriptFolder = pathOutsideScriptFolder(result.filename)
    }
    return result
  })
  return results
}

/**
 * Checks whether the provided content contains interactive file input methods.
 *
 * Specifically detects usage of:
 * - `file.choose()` — opens a file selection dialog
 * - `read.clipboard()` — reads data directly from the system clipboard
 *
 * @param content - Raw R code or text to analyze.
 * @returns `true` if any interactive method is detected, otherwise `false`.
 */

const interactiveRegex = /(?:file\.choose|read\.clipboard)\(/g
export const hasInteractiveMethods = (content: string) => {
  return interactiveRegex.test(content)
}

/**
 * Scans the input text for potential hardcoded API keys, tokens, or secrets.
 *
 * The detection is based on a heuristic regex that looks for:
 * - security-related keywords (e.g. `token`, `api`, `key`, `auth`, `secret`, etc.)
 * - followed by a quoted string containing a long, key-like value
 *
 * A key-like value is defined as:
 * - at least 22 characters long
 * - consisting of lowercase letters, digits, dots, or hyphens
 *
 * @param content - Raw text or code to analyze.
 * @returns Array of matched substrings that may contain API keys, or an empty array if none found.
 */

const apiKeyRegex = /(token|api|key|auth|access|aws|ssh|rsa\b|pgp\b|secret|pass)[^'"\n]*('|")(?<key>[a-z0-9.-]{22,})('|")/ig
export const getApiKeys = (content: string) => {
  return apiKeyRegex.test(content)
}

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
export const hasEpxlicitColumnDefinition = (content: string) => {
  return columnDefinitionRegex.test(content)
}

/**
 * Extracts R package installation calls from a script and parses the installed package names.
 *
 * This function detects calls to common R installation functions:
 * - Base R: `install.packages()`
 * - devtools: `devtools::install_github()`
 * - renv: `renv::install()`
 *
 * For each call, it captures either:
 * - **Literal package names** (e.g., `"dplyr"` in `install.packages("dplyr")`), including multiple packages specified via `c(...)`
 * - **Variables** (e.g., `pkgs` in `install(pkgs)`), when the package list is stored in a variable
 *
 *
 * @param content - Raw R script or code to analyze.
 * @returns An array of package installation objects, one per installed package or variable reference.
 */

const installFunctions = [
  'install\\.packages',
  '(?:devtools::)?install_github',
  '(?:renv::)?install'
]
const packageNamesRegex = '(?:(?:c\\()?(?<packages>(?:[\\s,]*(?<quote>[\'"]).*?\\k<quote>)+)|(?<variable>[_.a-z]+))'
const installPackagesRegexp = new RegExp(`(?<=\\b(?<fn>${installFunctions.join('|')})\\s*\\()${packageNamesRegex}`, 'ig')

export const getInstalledPackages = (content: string) => {
  const packages = [...content.matchAll(installPackagesRegexp)]
  const results = packages.map(match => {
    const type = match.groups?.fn === 'install.packages'
      ? 'base'
      : (match.groups?.fn ?? '').includes('github')
        ? 'devtools' : 'renv'
    if (match.groups?.variable !== undefined) {
      return {
        type,
        definedByVariable: true,
        variable: match.groups.variable
      }
    }
    const packages = (match.groups?.packages ?? '').split(',').map(el => el.replace(/['"\s\r\n]/g, ''))
    return packages.map(name => ({
      type, name
    }))
  })
  return results.flat()
}

/**
 * Extracts R package loading calls from a script and identifies loaded libraries.
 *
 * It parses the arguments passed to these functions and extracts:
 * - the library name (quoted or unquoted)
 * - the loading method (`base` or `pacman`)
 *
 * @param content - Raw R script or text to analyze.
 * @returns Array of objects describing loaded packages:
 * - `library`: package name
 * - `type`: `'base' | 'pacman'`
 */

const loadFunctions = [
  'library',
  '(?:pacman::)?p_load'
]
const libraryNamesRegex = `(?<quote>[\'"])?(?<library>${validVariableRegex}?)(?:\\k<quote>|,|\\))?`
const loadedPackagesRegexp = new RegExp(`(?<=\\b(?<fn>${loadFunctions.join('|')})\\s*\\()${libraryNamesRegex}`, 'ig')
export const getLoadedPackages = (content: string) => {
  const packages = [...content.matchAll(loadedPackagesRegexp)]
  return packages.map(m => ({
    library: m.groups?.library,
    type: m.groups?.fn === 'library' ? 'base' : 'pacman'
  }))
}

const lhaRegex = `(?:^\\s*|;\\s*)(?<name>${validVariableRegex})\\s*(?:<-|=|%<>%)`
const rhaRegex = `->\\s*(?<name_rha>${validVariableRegex})(?=\\s*(?:[\n\r]|$))`
const variableNameRegex = new RegExp(`${lhaRegex}|${rhaRegex}`, 'gm')
console.log(variableNameRegex)

export const getVariableNames = (content: string) => {
  const variables = [...content.matchAll(variableNameRegex)]
  console.log(variables.map(v => v.groups))
}

// // Get list of variables
// const varNames = content.match(/(?<=^\s*|\n\s*)(?:\.[a-z\d_]|[a-z])[a-z\d_]*(?=[\s\t]*<-|=)/ig) ?? []

// // Get all working directories
// const workingDirectories = content.match(/(?<=setwd\(')[^']*?(?=,|'[\)])|(?<=setwd\(")[^"]+?(?=,|"[\)])|(?<=setwd\()[^"']*?(?=,|[\)])/g)


//   // Project uses session info
//   const hasSessionInfo = content.match(/sessionInfo/)

//   // Project uses user defined functions
//   const hasUdf = content.match(/(?<=(?:=|<-)\s*)function\s*\(.*/g)

//   // Project uses citations for the paper
//   const hasCitations = content.match(/citation\(.*/g)

//   // Project defines themes
//   const hasTheme = content.match(/theme\(.*/g)

  
//   // References by indices
//   const indexRefs = content.match(/\[(?:c\()?\d+[\d,\s ]+(?:\))?\]/g)

const poorValidation = (content: string) => {

  // Don’t use attach() - https://google.github.io/styleguide/Rguide.html
  const usesAttach = /^[^#]*attach\(/.test(content)

  // Right-hand assignment - https://google.github.io/styleguide/Rguide.html - ->
  const usesRightHandAssignment = /\b->\b/.test(content)

  // avoid using names of existing functions and variables - http://adv-r.had.co.nz/Style.html
  // spaces around all infix operators - http://adv-r.had.co.nz/Style.html
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

}

const checkFiles = (files: string[]) => {
  // Names in sequence - http://adv-r.had.co.nz/Style.html
  // Meaningful names - http://adv-r.had.co.nz/Style.html
  // File names should be machine readable - https://style.tidyverse.org/files.html
  // yyyy-mm-dd (ISO8601) - https://style.tidyverse.org/files.html
  // using “final” or similar words in file names - https://style.tidyverse.org/files.html

  // Bad filenames
  // (n)$, less than 4chars, replication|base|code|init|r_script|functions|libraries|replication|simulations|setup  // fig|fg|ui|table|tab|chapter|plot|shiny|chart|appendix

}

const randomPlaceForLibraries = (content: string) => {
  // // libraries in functions - { }

}

const saveMethods = (content: string) => {
  // // How often charts are saved with tools - ggsave
  // // writecsv, saveRDS - temporary files
  // // df1 <- df1 [, 39:65]
  // dev.off()
  // save(out, file="Estimates_Lognormal")\

}

const usingIndices = (content: string) => {
  // // How anything is refered by index? [\d]

}

const testing = (content: string) => {
  // stopifnot, assert

}

const getDefaultFolder = (content: string) => {
  // args = commandArgs()
  // scriptName = args[substr(args,1,7) == '--file=']

  // if (length(scriptName) == 0) {
  //   scriptName <- rstudioapi::getSourceEditorContext()$path
  // } else {
  //   scriptName <- substr(scriptName, 8, nchar(scriptName))
  // }

}

const getTormatters = (content: string) => {
  // knitr kableExtra xtable
}