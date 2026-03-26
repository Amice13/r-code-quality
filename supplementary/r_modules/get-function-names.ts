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
