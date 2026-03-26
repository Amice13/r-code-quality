import { pathIsWinValid } from './constants.ts'
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
  'json',
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

const readFunctionsRegex = `(?<=^|[^#]*?)(?<fn>readLines|gsheet2tbl|read_excel|read(?:\.file)?[._](?<format>${fileFormats.join('|')}))`
const filenameRegex = '\\s*\\((?:(?:\\s*file\\s*=\\s*)?(?<quote>[\'"`])(?<filename>(?:\\\\.|(?!\\k<quote>).)*)\\k<quote>)?'
const fileReaderRegex = new RegExp(`${readFunctionsRegex}${filenameRegex}`, 'gmi')
const pathIsOnline = (p: string) => /^(http|ftp)/.test(p)
const pathIsGlobal = (p: string) => /^(~|[A-Z]:|\/)/.test(p)
const pathOutsideScriptFolder = (p: string) => /^\.\.[\/\\]/.test(p)

export const getDataSources = (content: string) => {
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
