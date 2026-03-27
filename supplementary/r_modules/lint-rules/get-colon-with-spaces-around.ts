import { stripStringsAndComments } from '../constants.ts'

// Spaces around exceptional infix operators - http://adv-r.had.co.nz/Style.html
// When used in tidy evaluation !! (bang-bang) and !!! (bang-bang-bang)
// (because they have precedence equivalent to unary -/+). - https://style.tidyverse.org/syntax.html

const colonOperators = [':::', '::', ':', '!!!', '!!']

type ColonIssue = {
  line: number
  column: number
  name: 'Unexpected space before operator :' | 'Unexpected space after operator :'
}

const isSequenceColon = (line: string, opStart: number): boolean => {
  const prevChar = line[opStart - 1] ?? ''
  const nextChar = line[opStart + 1] ?? ''
  return /\w/.test(prevChar) && /\w/.test(nextChar)
}

export const getColonWithSpacesAround = (content: string): ColonIssue[] => {
  const lines = content.split(/\r?\n/)
  const issues: ColonIssue[] = []

  const matchOperatorAt = (line: string, index: number): string | null => {
    for (const op of colonOperators) {
      if (line.startsWith(op, index)) return op
    }
    return null
  }

  lines.forEach((line, lineIdx) => {
    const cleanLine = stripStringsAndComments(line)

    for (let i = 0; i < cleanLine.length; i++) {
      const op = matchOperatorAt(cleanLine, i)
      if (!op) continue

      const opStart = i
      const opEnd = i + op.length - 1

      if (op === ':' && isSequenceColon(cleanLine, opStart)) {
        i += op.length - 1
        continue
      }

      const prevChar = cleanLine[opStart - 1]
      const nextChar = cleanLine[opEnd + 1]
      const isAtStart = opStart === 0
      const isAtEnd = opEnd === cleanLine.length - 1

      if (!isAtStart && prevChar === ' ') {
        issues.push({
          line: lineIdx + 1,
          column: opStart + 1,
          name: 'Unexpected space before operator :'
        })
      }

      if (!isAtEnd && nextChar === ' ') {
        issues.push({
          line: lineIdx + 1,
          column: opEnd + 1,
          name: 'Unexpected space after operator :'
        })
      }

      i += op.length - 1
    }
  })

  return issues
}
