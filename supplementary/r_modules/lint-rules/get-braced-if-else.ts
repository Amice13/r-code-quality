// A single line if statement must never contain braced expressions - https://style.tidyverse.org/syntax.html

const statementRegexp = /\bif\b.*?\)\{..*\belse/
export const getBracedIfElse = (line: string) => {
  const match = statementRegexp.test(line)
  if (!match) return null
  return {
    name: 'A single line statement contain braced expressions'
  }
}
