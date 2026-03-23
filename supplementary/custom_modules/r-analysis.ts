export const analyzeLines = (content: string) => {
  // Get number of lines
  const lines = content.split(/[\n\r]/g)

  // Get number of lines with comments
  const commentLines = lines.filter(line => /^[\s\t]*#/.test(line))

  // Get number of lines with formatted comments
  const commentFormattingLines = commentLines.filter(line => /#[\s\t]*#/.test(line))

  // Get number of empty lines
  const emptyLines = lines.filter(line => /^[\s\t]*$/gm.test(line))
  
  return {
    numberOfLines: lines.length,
    numberOfComments: commentLines.length,
    numberOfFormattedComments: commentFormattingLines.length,
    numberOfEmptyLines: emptyLines.length
  }
}

const magrittrPipeRegex = /%>%|\|>|\|/g
const nativePipeRegex = /\|>/g
export const analyzePipes = (content: string) => {
  const hasMagrittrPipes = magrittrPipeRegex.test(content)
  const hasNativePipes = nativePipeRegex.test(content)

  return {
    hasMagrittrPipes,
    hasNativePipes
  }
}

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
const fileReaderRegex = new RegExp(`readLines|read_excel|read(?:\.file)?[._](?<format>${fileFormats.join('|')})`)

const getFiles = (content: string) => {

}

const getExternalSourcesRegex = /(?<=gsheet2tbl\()(.*?)(?=\))/
const getExternalSources = (content: string) => {
  return content.match(getExternalSourcesRegex)
}

const interactiveRegex = /(?:file\.choose|read\.clipboard)\(/g
export const hasInteractiveMethods = (content: string) => {
  return interactiveRegex.test(content)
}

const apiKeyRegex = /(token|api|key|auth|access|aws|ssh|rsa\b|pgp\b|secret|pass)[^'"\n]*('|")(?<key>[a-z0-9.-]{22,})('|")/ig
export const getApiKeys = (content: string) => {
  const matches = content.match(apiKeyRegex)
  return matches ?? []
}

const hasColumnDefinition = (content: string) => {

}

const installPackagesRegexp = /(?<=\binstall\.packages|\b(?:renv::)?install\()/

const getInstalledPackages = (content: string) => {
  // Get list of packages
  const packages = content.match(/(?<=install\.packages|renv::install\().*?(?=[,\)])/g)

}

export const getLoadedPackages = (content: string) => {
  const libraries = content.match(/(?<=\b(?:library|p_load)\().*?(?=\))/g)
  return libraries ?? []
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
