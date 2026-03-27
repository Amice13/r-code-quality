import { stripStringsAndComments } from '../constants.ts'

// Place a space before left parentheses, except in a function call - http://adv-r.had.co.nz/Style.html
// Do not place spaces around code in parentheses or square brackets - http://adv-r.had.co.nz/Style.html

const keywordSet = new Set([
  'if',
  'for',
  'while',
  'repeat',
  'switch',
  'function'
])

type Issue = {
  line: number
  column: number
  name:
    | 'Unexpected space before ('
    | 'Missing space before ('
    | 'Missing space after )'
    | 'Unexpected space after ('
    | 'Unexpected space before )'
    | 'Unexpected space before ['
    | 'Unexpected space after ['
    | 'Unexpected space before ]'
    | 'Missing space after ,'
    | 'Unexpected space before ,'
}

export const getSpacingIssues = (content: string): Issue[] => {
  const lines = content.split(/\r?\n/)
  const issues: Issue[] = []

  lines.forEach((line, lineIdx) => {
    const cleanLine = stripStringsAndComments(line)
    let currentToken = ''
    let lastToken = ''
    let lastTokenEnd = -1

    const push = (column: number, name: Issue['name']) =>
      issues.push({ line: lineIdx + 1, column, name })

    for (let i = 0; i < cleanLine.length; i++) {
      const char = cleanLine[i]

      // Track current word token
      if (/[a-zA-Z0-9_.]/.test(char)) {
        currentToken += char
        continue
      }
      if (currentToken) {
        lastToken = currentToken
        lastTokenEnd = i - 1
        currentToken = ''
      }

      // ── ( ──────────────────────────────────────────────────────────────
      if (char === '(') {
        const isKeyword = keywordSet.has(lastToken)
        const isFunctionCall = lastToken !== '' && lastTokenEnd === i - 1

        // Space before ( in a function call (non-keyword)
        if (isFunctionCall && !isKeyword) {
          // no space expected — but there can't be one since lastTokenEnd === i - 1
          // This branch is only reachable without a space, so nothing to flag here
        }

        // Keyword: check space count between keyword and (
        if (isKeyword) {
          let spaceCount = 0
          for (let j = lastTokenEnd + 1; j < i; j++) {
            if (cleanLine[j] === ' ') spaceCount++
          }
          if (spaceCount === 0) {
            push(i + 1, 'Missing space before (')
          }
          // Multiple spaces before keyword ( — only flag if not an alignment context
          // (alignment exception applies to = and <-, not keyword parens)
          else if (spaceCount > 1) {
            push(i + 1, 'Unexpected space before (')
          }
        }

        // Space after (
        if (cleanLine[i + 1] === ' ') {
          push(i + 2, 'Unexpected space after (')
        }

        lastToken = ''
        continue
      }

      // ── ) ──────────────────────────────────────────────────────────────
      if (char === ')') {
        // Space before )
        if (cleanLine[i - 1] === ' ') {
          push(i + 1, 'Unexpected space before )')
        }

        // Space after )
        let j = i + 1
        let spaceCount = 0
        while (j < cleanLine.length && cleanLine[j] === ' ') {
          spaceCount++
          j++
        }
        const nextChar = cleanLine[j]
        if (nextChar && !/[)$,;:\r\n\]]/.test(nextChar)) {
          if (spaceCount === 0) {
            push(i + 1, 'Missing space after )')
          }
        }

        lastToken = ''
        continue
      }

      // ── [ ──────────────────────────────────────────────────────────────
      if (char === '[') {
        // Space before [ in indexing (preceded immediately by a token or ])
        if (cleanLine[i - 1] === ' ' && lastTokenEnd === i - 2) {
          push(i + 1, 'Unexpected space before [')
        }

        // Space after [
        if (cleanLine[i + 1] === ' ') {
          push(i + 2, 'Unexpected space after [')
        }

        lastToken = ''
        continue
      }

      // ── ] ──────────────────────────────────────────────────────────────
      if (char === ']') {
        // Space before ] (but allow `diamonds[5, ]` — space after comma is ok)
        const prevChar = cleanLine[i - 1]
        if (prevChar === ' ') {
          // Walk back to find what's before the space(s)
          let j = i - 1
          while (j >= 0 && cleanLine[j] === ' ') j--
          if (cleanLine[j] !== ',') {
            push(i + 1, 'Unexpected space before ]')
          }
        }

        lastToken = ''
        continue
      }

      // ── , ──────────────────────────────────────────────────────────────
      if (char === ',') {
        // Space before comma
        if (cleanLine[i - 1] === ' ') {
          push(i + 1, 'Unexpected space before ,')
        }

        // Missing space after comma — but ignore trailing comma (next non-space is ] or ))
        const next = cleanLine[i + 1]
        if (next !== undefined && next !== ' ' && next !== '\n' && next !== '\r') {
          // Allow `diamonds[5, ]` style — if next non-space is ] or ) that's fine
          if (!/[\])]/.test(next)) {
            push(i + 2, 'Missing space after ,')
          }
        }

        lastToken = ''
        continue
      }

      // ── space — check for `plot (x)` ───────────────────────────────────
      if (char === ' ') {
        // If the very next non-space char is ( and the preceding token is NOT a keyword,
        // it means something like `plot (x)` — space before ( in a function call
        if (lastToken !== '' && !keywordSet.has(lastToken)) {
          let j = i
          while (j < cleanLine.length && cleanLine[j] === ' ') j++
          if (cleanLine[j] === '(' && lastTokenEnd === i - 1) {
            push(j + 1, 'Unexpected space before (')
          }
        }
      }
    }
  })

  return issues
}
