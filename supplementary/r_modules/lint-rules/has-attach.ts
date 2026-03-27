// Don't use attach() - https://google.github.io/styleguide/Rguide.html

const attachRegex = /^[^#]*\battach\(/

export const hasAttach = (line: string) => {
  const match = line.match(attachRegex)
  if (match === null) return null
  return {
    name: 'Has attach keyword',
    column: match.index
  }
} 
