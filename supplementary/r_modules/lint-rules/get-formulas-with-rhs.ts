import { stripStringsAndComments, validVariableRegex } from '../constants.ts'

const rhsRegexp = new RegExp(`(?<=${validVariableRegex})(~ *| *~)${validVariableRegex}|(?<!${validVariableRegex}|=|,) +~${validVariableRegex}`, 'g')
export const getFormulasWithRhs = (line: string) => {
  const matches = [...stripStringsAndComments(line).matchAll(rhsRegexp)]
  return matches.map(match => ({
    name: 'Fuck',
    column: match.index
  }))
}
