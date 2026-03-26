import { validVariableRegex, stripStringsAndComments } from './constants.ts'

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
 * - `line`: number of line where the library is loaded
 */

const getLineIndexMap = (content: string) => {
  const lines = content.split(/[\r\n]+/)
  const offsets: number[] = []
  let pos = 0

  for (const line of lines) {
    offsets.push(pos)
    pos += line.length + 1
  }

  return { lines, offsets }
}

const getLineNumber = (index: number, offsets: number[]) => {
  for (let i = offsets.length - 1; i >= 0; i--) {
    if (index >= (offsets[i] ?? 0)) return i
  }
  return 0
}

const loadFunctions = [
  'library',
  '(?:pacman::)?p_load'
]
const libraryNamesRegex = `(?<quote>[\'"])?(?<library>${validVariableRegex})(?:\\k<quote>|,|\\))?`
const loadedPackagesRegexp = new RegExp(`(?<=\\b(?<fn>${loadFunctions.join('|')})\\s*\\(\\s*)${libraryNamesRegex}`, 'ig')
export const getLoadedPackages = (content: string) => {
  const { offsets } = getLineIndexMap(content)
  const packages = [...content.matchAll(loadedPackagesRegexp)]
  return packages.map(m => {
    const lineIndex = getLineNumber(m.index ?? 0, offsets)
    return {
      library: m.groups?.library,
      type: m.groups?.fn === 'library' ? 'base' : 'pacman',
      line: lineIndex + 1
    }
  })
}
