// The body of a loop must be a braced expression. - https://style.tidyverse.org/syntax.html

const loopRegexp = /\b(?:for|while|repeat)\b\s*\(.*?[^{](?:$|\s*#)/
export const getLoopsWithoutBraces = (line: string) => {
  const match = line.trim().match(loopRegexp)
  if (match === null) return null
  return {
    name: 'The body of a loop is not a braced expression or body at the same line',
    column: match.index
  }
}
