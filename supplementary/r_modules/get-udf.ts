import { stripStringsAndComments } from './constants.ts'
import { getVariableNames } from './get-variable-names.ts'

const isWhitespace = (ch: string) => /\s/.test(ch)

const skipWhitespace = (str: string, i: number) => {
  while (i < str.length && isWhitespace(str[i] ?? '')) i++
  return i
}

const extractParenBlock = (str: string, i: number) => {
  if (str[i] !== '(') return null

  let depth = 0
  let start = i

  while (i < str.length) {
    if (str[i] === '(') depth++
    else if (str[i] === ')') {
      depth--
      if (depth === 0) {
        return {
          content: str.slice(start + 1, i),
          end: i + 1
        }
      }
    }
    i++
  }

  return null
}

const extractBraceBlock = (str: string, i: number) => {
  if (str[i] !== '{') return null

  let depth = 0
  let start = i

  while (i < str.length) {
    if (str[i] === '{') depth++
    else if (str[i] === '}') {
      depth--
      if (depth === 0) {
        return {
          content: str.slice(start + 1, i),
          end: i + 1
        }
      }
    }
    i++
  }

  return null
}

const extractIdentifierForward = (str: string, i: number) => {
  i = skipWhitespace(str, i)

  let start = i
  while (i < str.length && /[A-Za-z0-9._]/.test(str[i] ?? '')) i++

  const name = str.slice(start, i)
  return name || null
}

const collectFunctions = (content: string) => {
  const results = []
  const len = content.length

  for (let i = 0; i < len; i++) {
    // detect "function"
    if (content.slice(i, i + 8) !== 'function') continue

    let j = i + 8
    j = skipWhitespace(content, j)

    // params
    const paramsBlock = extractParenBlock(content, j)
    if (!paramsBlock) continue

    j = skipWhitespace(content, paramsBlock.end)

    // body
    const bodyBlock = extractBraceBlock(content, j)
    if (!bodyBlock) continue

    const params = paramsBlock.content.trim()
    const body = bodyBlock.content.trim()

    const before = content.slice(0, i)
    let name = null

    const assignMatch = before.match(/([A-Za-z0-9._]+)\s*(<-|=)\s*$/)
    if (assignMatch) {
      name = assignMatch[1]
    }

    if (!name) {
      let k = skipWhitespace(content, bodyBlock.end)

      if (content.slice(k, k + 2) === '->') {
        name = extractIdentifierForward(content, k + 2)
      }
    }

    if (name) {
      results.push({
        name,
        params,
        body
      })
    }

    i = bodyBlock.end
  }

  return results
}

const parseParams = (input: string) => {
  const params: Array<{ name: string, hasDefault: boolean }> = []
  let i = 0
  let current = ''

  let depthParen = 0
  let depthBracket = 0
  let depthBrace = 0
  let inString = false
  let stringChar = null

  const pushParam = (str: string) => {
    const trimmed = str.trim()
    if (!trimmed) return

    const eqIndex = trimmed.indexOf('=')

    if (eqIndex === -1) {
      params.push({
        name: trimmed,
        hasDefault: false
      })
    } else {
      const name = trimmed.slice(0, eqIndex).trim()

      params.push({
        name,
        hasDefault: true
      })
    }
  }

  while (i < input.length) {
    const ch = input[i]

    // ---- strings ----
    if (inString) {
      current += ch

      if (ch === stringChar && input[i - 1] !== '\\') {
        inString = false
        stringChar = null
      }

      i++
      continue
    }

    if (ch === '"' || ch === "'") {
      inString = true
      stringChar = ch
      current += ch
      i++
      continue
    }

    // ---- comments (NEW) ----
    if (ch === '#') {
      // skip until end of line
      while (i < input.length && input[i] !== '\n') i++
      continue
    }

    // ---- nesting ----
    if (ch === '(') depthParen++
    else if (ch === ')') depthParen--
    else if (ch === '[') depthBracket++
    else if (ch === ']') depthBracket--
    else if (ch === '{') depthBrace++
    else if (ch === '}') depthBrace--

    // ---- split ----
    if (
      ch === ',' &&
      depthParen === 0 &&
      depthBracket === 0 &&
      depthBrace === 0
    ) {
      pushParam(current)
      current = ''
      i++
      continue
    }

    current += ch
    i++
  }

  pushParam(current)

  return params
}

const alternativesRegexp = /\b(if|else if|for|while|repeat|&&|\|\||switch|tryCatch)\b/g

const estimateCyclomaticComplexity = (content: string) => {
  return content.match(alternativesRegexp)?.length ?? 0
}

const getMaxBlockDepth = (input: string) => {
  let depth = 0
  let maxDepth = 0

  let i = 0
  let inString = false
  let stringChar = null

  while (i < input.length) {
    const ch = input[i]

    if (inString) {
      if (ch === stringChar && input[i - 1] !== '\\') {
        inString = false
        stringChar = null
      }
      i++
      continue
    }

    if (ch === '"' || ch === '\'') {
      inString = true
      stringChar = ch
      i++
      continue
    }

    if (ch === '#') {
      while (i < input.length && input[i] !== '\n') i++
      continue
    }

    if (ch === '{') {
      depth++
      if (depth > maxDepth) maxDepth = depth
    } else if (ch === '}') {
      depth--
    }

    i++
  }

  return maxDepth
}

const hasReturnRegexp = /return\s*\(.*\)\s*$/
export const getUdf = (content: string) => {
  const functions = collectFunctions(content)
  for (const fn of functions) {
    const params = parseParams(fn.params)
    const withoutComments = stripStringsAndComments(fn.body)
    const cyclomaticComplexity = estimateCyclomaticComplexity(withoutComments)
    const pipes = withoutComments.match(/\b(?:%>%|%<>%|\|>)\b/g)
    const lines = fn.body.split(/[\n\r]+/g)
    const hasReturn = hasReturnRegexp.test(lines.at(-1) ?? '')
    const maxDepth = getMaxBlockDepth(fn.body)
    const variables = getVariableNames(fn.body)
    const numberOfLines = lines.length
    const numberOfVariables = variables.length
    const numberOfPipes = pipes?.length
  }
}
