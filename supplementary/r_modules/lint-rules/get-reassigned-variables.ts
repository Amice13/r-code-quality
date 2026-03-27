import { rBaseFunctions } from '../r-base-functions.ts'
import { getVariableNames } from '../index.ts'

// Avoid using names of existing functions and variables - http://adv-r.had.co.nz/Style.html

export const getReassignedVariables = (content: string) => {
  const issues = []
  const variables = getVariableNames(content)
  for (const variable of variables) {
    if (rBaseFunctions.includes(variable.name)) {
      issues.push({ name: 'Existing function is re-assigned', line: variable.line })
    }
  }
  return issues
}
