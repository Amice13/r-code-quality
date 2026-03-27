// Use ", not ', for quoting text. The only exception is when the text already contains double quotes and no single quotes.
// https://style.tidyverse.org/syntax.html

type Issue = {
  line: number
  column: number
  name: 'Single quotes used instead of double quotes'
}

export const getBadQuotes = (content: string): Issue[] => {
  const lines = content.split(/\r?\n/)
  const issues: Issue[] = []

  lines.forEach((line, lineIdx) => {
    let inString: string | null = null
    let stringStart = -1
    let stringContent = ''
    let escaped = false

    for (let i = 0; i < line.length; i++) {
      const char = line[i]

      if (escaped) {
        escaped = false
        stringContent += char
        continue
      }

      if (char === '\\') {
        escaped = true
        stringContent += char
        continue
      }

      if (char === '#' && !inString) break

      if (!inString) {
        if (char === '"' || char === "'") {
          inString = char
          stringStart = i
          stringContent = ''
        }
        continue
      }

      if (char === inString) {
        if (inString === "'") {
          const hasDoubleQuotes = stringContent.includes('"')
          const hasSingleQuotes = stringContent.includes("\\'")
          if (!hasDoubleQuotes || hasSingleQuotes) {
            issues.push({
              line: lineIdx + 1,
              column: stringStart + 1,
              name: 'Single quotes used instead of double quotes'
            })
          }
        }
        inString = null
        stringStart = -1
        stringContent = ''
        continue
      }

      stringContent += char
    }
  })

  return issues
}