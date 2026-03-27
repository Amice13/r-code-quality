// The embracing operator, { }, should always have inner spaces to help emphasise its special behaviour: - https://style.tidyverse.org/syntax.html

const embracingRegexp = /\{\{(?! )|(?<! )\}\}/g
export const getBadEmbracing = (line: string) => {
  const match = [...line.matchAll(embracingRegexp)]
  return match.map(el => ({
    name: 'No inner spaces for embracing',
    column: el.index
  }))
}
