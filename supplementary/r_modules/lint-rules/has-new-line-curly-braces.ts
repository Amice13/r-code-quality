// An opening curly brace should never go on its own line and should always be followed by a new line. - http://adv-r.had.co.nz/Style.html

const curlyBraceRegexp = /(?<=^\s*)\{/
export const hasNewLineCurlyBraces = (line: string) => {
  const match = line.match(curlyBraceRegexp)
  if (match === null) return null
  return {
    name: 'Opening curly brace on a new line'
  }
}
