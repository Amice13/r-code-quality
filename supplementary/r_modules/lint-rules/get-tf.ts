// Prefer TRUE and FALSE over T and F. - https://style.tidyverse.org/syntax.html

const tfRegexp = /\b(T|F)\b/g
export const getTf = (line: string) => {
  const tfs = [...line.matchAll(tfRegexp)]
  return tfs.map(match => ({
    name: 'TRUE or FALSE is stored as T or F',
    column: match.index
  }))
}
