// |> should always have a space before it, and should usually be followed by a new line. After the first step, each line should be indented by two spaces.  - https://style.tidyverse.org/pipes.html

const pipeRegexp = /[^ ]\|>|\|>(?!\s*$)/g

export const getPipesWithoutSpaces = (line: string) => {
  const matches = [...line.matchAll(pipeRegexp)]
  return matches.map(match => ({
    name: '|> does not have a space before it or not followed by a new line',
    column: match.index
  }))
}
