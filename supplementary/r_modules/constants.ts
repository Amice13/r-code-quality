export const validVariableRegex = '(?:\\.[A-z\\d_]+|[A-z][A-z\\d_.]*)'
export const pathIsWinValid = (p: string) => !/[<>"|?*]/.test(p) && !/\.\.\./.test(p)

export const stripStringsAndComments = (line: string) => {
  let result = ''
  let inString: string | null = null
  let escaped = false

  for (const char of line) {
    if (escaped) {
      escaped = false
      continue
    }
    if (char === '\\') {
      escaped = true
      continue
    }
    if (inString) {
      if (char === inString) inString = null
      continue
    }
    if (char === '"' || char === "'" || char === '`') {
      inString = char
      continue
    }
    if (char === '#') break
    result += char
  }

  return result
}
