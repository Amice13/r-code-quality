import { stripStringsAndComments } from '../constants.ts'

// No spaces around all infix operators - http://adv-r.had.co.nz/Style.html

const infixOperators = [
  '%<>%',
  '%>%',
  '%<%',
  '<-',
  '->',
  '==',
  '!=',
  '>=',
  '=~',
  '<=',
  '||',
  '&&',
  '|>',
  '|',
  '&',
  '=',
  '+',
  '-',
  '<',
  '>'
]

type Issue = {
  line: number
  column: number
  name: 'Missing space before operator' | 'Missing space after operator'
}

export const getInfixWithoutSpacesAround = (content: string): Issue[] => {
  const lines = content.split(/\r?\n/)
  const issues: Issue[] = []

  const matchOperatorAt = (line: string, index: number): string | null => {
    for (const op of infixOperators) {
      if (line.startsWith(op, index)) return op
    }
    return null
  }

  const isUnaryMinus = (line: string, opStart: number): boolean => {
    let j = opStart - 1
    while (j >= 0 && line[j] === ' ') j--
    if (j < 0) return true
    return /[=+\-*/<>|&%,([{~]/.test(line[j] ?? '')
  }

  lines.forEach((line, lineIdx) => {
    const cleanLine = stripStringsAndComments(line)

    for (let i = 0; i < cleanLine.length; i++) {
      const op = matchOperatorAt(cleanLine, i)
      if (!op) continue

      const opStart = i
      const opEnd = i + op.length - 1

      if (op === '-' && isUnaryMinus(cleanLine, opStart)) {
        i += op.length - 1
        continue
      }

      const prevChar = cleanLine[opStart - 1]
      const nextChar = cleanLine[opEnd + 1]
      const isAtStart = opStart === 0
      const isAtEnd = opEnd === cleanLine.length - 1

      if (!isAtStart && prevChar !== ' ') {
        issues.push({
          line: lineIdx + 1,
          column: opStart + 1,
          name: 'Missing space before operator'
        })
      }

      if (!isAtEnd && nextChar !== ' ') {
        issues.push({
          line: lineIdx + 1,
          column: opEnd + 1,
          name: 'Missing space after operator'
        })
      }

      i += op.length - 1
    }
  })

  return issues
}
