// The help operator - https://style.tidyverse.org/syntax.html
// Should be removed from scripts completely

const helpRegexp = /^\?|[.a-z\d_]\s*\?\s*[.a-z\d_]/
export const hasHelp = (line: string) => {
  const match = line.match(helpRegexp)
  if (match === null) return null
  return {
    name: 'Help function is not removed',
    column: match.index
  }
}
