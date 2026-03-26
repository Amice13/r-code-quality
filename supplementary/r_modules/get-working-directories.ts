import { validVariableRegex } from './constants.ts'
import { pathIsWinValid } from './constants.ts'

/**
 * Extracts working directory definitions (`setwd()`) from an R script
 * and classifies the type of path used.
 *
 * For each detected call, it returns metadata describing the path:
 * - `path`: extracted string path (if available)
 * - `isEmpty`: true if the path is empty or omitted
 * - `isHere`: true if using a `here()`-style call
 * - `isRStudioApi`: true if using RStudio API constructs
 * - `isInteractive`: true if using interactive directory selection
 * - `isCall`: true if the path appears to be a placeholder or prompt-like string
 * - `isVar`: true if the path is provided via a variable
 * - `isValid`: true if the path passes a basic Windows path validity check
 *
 * @param content - Raw R script or text to analyze.
 * @returns Array of objects describing detected working directory configurations.
 */

const wdRegex = '\\bsetwd\\s*\\(\\s*'
const realPathRegex = '(?<quote>[\'"`])(?<path>(?:\\\\.|(?!\\k<quote>).)*)\\k<quote>'
const interactivePathRegex = '(?<interactive>choose\\.dir)'
const hereRegex = '(?<here>.*here\\()'
const rstudioapiRegex = '(?<rstudioApi>dirname.*rstudioapi)'
const emptyRegex = '(?<empty>(?<q>[\'"`])\\k<q>\\s*\\)|\\))'
const isVarRegex = `(?<var>${validVariableRegex})`
const pathOptions = [
  realPathRegex,
  interactivePathRegex,
  hereRegex,
  rstudioapiRegex,
  emptyRegex,
  isVarRegex
]
const callRegex = /^[^\/~\\:]*(?:folder|path|your|enter|personal|<|\[|\{)/i

const pathRegexp = new RegExp(`${wdRegex}(?:${pathOptions.join('|')})`, 'gi')
export const getWorkingDirectories = (content: string) => {
  const paths = [...content.matchAll(pathRegexp)]
  return paths.map(m => ({
    path: m.groups?.path,
    isEmpty: typeof m.groups?.empty === 'string',
    isHere: typeof m.groups?.here === 'string',
    isRStudioApi: typeof m.groups?.rstudioApi === 'string',
    isInteractive: typeof m.groups?.interactive === 'string',
    isCall: callRegex.test(m.groups?.path ?? ''),
    isVar: typeof m.groups?.var === 'string',
    isValid: Boolean(m.groups?.path) && pathIsWinValid(m.groups?.path ?? '')
  }))
}
