// Semicolons are never recommended. In particular, don’t put ; at the end of a line, and don’t use ; to put multiple commands on one line. - https://style.tidyverse.org/syntax.html

export const getSemicolons = (content: string) => {
  let inString: string | null = null
  let escaped = false
  let inComment = false
  let line = 1
  let column = 0
  const issues = []
  for (const char of content) {
    column++

    if (char === '\n') {
      line++
      column = 0
      inComment = false
      continue
    }

    if (inComment) continue

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

    if (char === ';') {
      issues.push({
        name: 'Semicolon is used',
        line,
        column
      })
    }
  }
  return issues
}
