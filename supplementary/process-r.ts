import fs from 'fs'
import path from 'path'

const SOURCE_FOLDER = path.join('..', 'data', 'scripts')

const projects = fs.readdirSync(SOURCE_FOLDER, { recursive: true })
  .filter((file) => typeof file === 'string' && /\.r$/i.test(file))

for (const file of projects) {
  if (typeof file !== 'string') continue
  const content = fs.readFileSync(path.join(SOURCE_FOLDER, file)).toString()
  const [project, filename] = file.split('/')
  if (project === undefined || filename === undefined) continue 

  // Get file size
  const fileStats: Partial<FileStats> = { project, filename }
  fileStats.size = content.length

  // Get number of lines
  const lines = content.split(/[\n\r]/g)
  fileStats.lines = lines.length

  // Get number of lines with comments
  const commentLines = lines.filter(line => /^[\s\t]*#/.test(line))
  fileStats.commentLines = commentLines.length

  // Get number of lines with formatted comments
  const commentFormattingLines = commentLines.filter(line => /#[\s\t]*#/.test(line))
  fileStats.commentFormattingLines = commentFormattingLines.length

  // Get number of empty lines
  const emptyLinesLength = lines.filter(line => /^[\s\t]*$/gm.test(line)).length
  fileStats.emptyLines = emptyLinesLength

  // Check pipes are used
  const pipes = content.match(/%>%|\|>|\|/g)

  // Get list of variables
  const varNames = content.match(/(?<=^\s*|\n\s*)(?:\.[a-z\d_]|[a-z])[a-z\d_]*(?=[\s\t]*<-|=)/ig) ?? []

  // Get all working directories
  const workingDirectories = content.match(/(?<=setwd\(')[^']*?(?=,|'[\)])|(?<=setwd\(")[^"]+?(?=,|"[\)])|(?<=setwd\()[^"']*?(?=,|[\)])/g)

  // Get API credentials

  // Get list of packages
  const packages = content.match(/(?<=install\.packages\().*?(?=[,\)])/g)

  // Get list of libraries
  const libraries = content.match(/(?<=library\().*?(?=[,\)])/g)

  // Project uses session info
  const hasSessionInfo = content.match(/sessionInfo/)

  // Project uses user defined functions
  const hasUdf = content.match(/(?<=(?:=|<-)\s*)function\s*\(.*/g)

  // Project uses citations for the paper
  const hasCitations = content.match(/citation\(.*/g)

  // Project defines themes
  const hasTheme = content.match(/theme\(.*/g)

  // References by indices
  const indexRefs = content.match(/\[(?:c\()?\d+[\d,\s ]+(?:\))?\]/g)
  console.log(filename)

  // Don’t use attach() - https://google.github.io/styleguide/Rguide.html

  // Right-hand assignment - https://google.github.io/styleguide/Rguide.html - ->

  // Use explicit returns - https://google.github.io/styleguide/Rguide.html

  // Qualifying namespaces - https://google.github.io/styleguide/Rguide.html

  // Meaningful names - http://adv-r.had.co.nz/Style.html

  // Names in sequence - http://adv-r.had.co.nz/Style.html

  // Variable and function names should be lowercase - http://adv-r.had.co.nz/Style.html

  // avoid using names of existing functions and variables - http://adv-r.had.co.nz/Style.html

  // spaces around all infix operators - http://adv-r.had.co.nz/Style.html

  // Place a space before left parentheses, except in a function call - http://adv-r.had.co.nz/Style.html

  // Do not place spaces around code in parentheses or square brackets - http://adv-r.had.co.nz/Style.html

  // An opening curly brace should never go on its own line and should always be followed by a new line. - http://adv-r.had.co.nz/Style.html

  // Strive to limit your code to 80 characters per line. - http://adv-r.had.co.nz/Style.html

  // When indenting your code, use two spaces. Never use tabs or mix tabs and spaces. -- http://adv-r.had.co.nz/Style.html

  // Use <-, not =, for assignment - http://adv-r.had.co.nz/Style.html

  // Use commented lines of - and = to break up your file into easily readable chunks - http://adv-r.had.co.nz/Style.html

  // File names should be machine readable - https://style.tidyverse.org/files.html

  // yyyy-mm-dd (ISO8601) - https://style.tidyverse.org/files.html

  // using “final” or similar words in file names - https://style.tidyverse.org/files.html

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

  





  // Bad filenames
  // (n)$, less than 4chars, replication|base|code|init|r_script|functions|libraries|replication|simulations|setup  // fig|fg|ui|table|tab|chapter|plot|shiny|chart|appendix
  // helper2?

  // console.log(packages)
}

//     const tablesAndVariables = content.match(/(?<![a-z\d\.])(?:\.[a-z]|[a-z])[a-z\d_]+\$(?:\.[a-z]|[a-z])[a-z\d_]/g)
//     const goodGrepl = content.match(/\[grepl.*/g)
//     const hasReport = content.match(/report_.*?\(.*/g)

//     // console.log(tablesAndVariables)
//     return {
//       project,
//       name,
//       linesLength,
//       commentsLength,
//       multiCommentsLength,
//       emptyLinesLength,
//       // content,
//       length
//     }
//   })

// // console.log(projects)


// // <- vs = and mix of them

// // Check different syntaxes - pipes | , %>% , ~ , dplyr, data.table, tidytable


// // Check the length
// // Check the number of files

// // libraries in functions - { }

// // `dplyr::filter` и `stats::filter`

// // How often charts are saved with tools - ggsave

// // How anything is refered by index? [\d]

// // variable names - datasets vs vars // snake case, camel case, etc

// // How often collon is used dplyr::fn()

// // How source is used?

// // Creation of custom functions()

// // Proportion of comments

// // writecsv, saveRDS - temporary files

// // stopifnot, assert

// // Where library is executed?

// // List popular libraries

// // Set default params or not and if there any comments before?

// // dev.off()


// // prediction.ideology.5 <- results.ideology.5[4]$stats$`Expected Values: E(Y|X)`\r\n'

// // df1 <- df1 [, 39:65]


// // read.csv, read.xlsx... etc, read_sav

// // save(out, file="Estimates_Lognormal")\

// // install.packages
// // p_load


// // saveRDS

// // setwd
// // getwd
// // read.csv
// // read.table
// // read.rds

// // Check comments




// /*

// args = commandArgs()
// scriptName = args[substr(args,1,7) == '--file=']

// if (length(scriptName) == 0) {
//   scriptName <- rstudioapi::getSourceEditorContext()$path
// } else {
//   scriptName <- substr(scriptName, 8, nchar(scriptName))
// }

// pathName = substr(
//   scriptName, 
//   1, 
//   nchar(scriptName) - nchar(strsplit(scriptName, '.*[/|\\]')[[1]][2])
// )

// setwd(pathName)
// parent_path <- getwd()

// */

