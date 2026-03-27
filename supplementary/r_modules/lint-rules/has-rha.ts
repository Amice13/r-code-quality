import { validVariableRegex } from '../constants.ts'

// Right-hand assignment - https://google.github.io/styleguide/Rguide.html - ->

const rhaRegex = new RegExp(`(?<=^[^#]*)->\\s*(?<name_rha>${validVariableRegex})(?=\\s*(?:[\n\r]|$))`)

export const hasRha = (line: string) => {
  const match = line.match(rhaRegex)
  if (match === null) return null
  return {
    name: 'Right hand assignment is used',
    column: match.index
  }
}
