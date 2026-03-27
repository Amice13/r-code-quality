// Vertical space - https://style.tidyverse.org/syntax.html

const badSpacingRepexp = /(?:\r?\n){3,}|\{(?:\r?\n){2,}/g
export const getBadVerticalSpacing = (content: string) => {
  const matches = [...content.matchAll(badSpacingRepexp)]
  return matches.map(match => ({
    name: 'Bad vertical spacing',
    line: (content.slice(0, match.index).match(/\n/g)?.length ?? 1) + 2
  }))
}
