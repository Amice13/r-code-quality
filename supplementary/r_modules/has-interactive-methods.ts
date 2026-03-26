/**
 * Checks whether the provided content contains interactive file input methods.
 *
 * @param content - Raw R code or text to analyze.
 * @returns `true` if any interactive method is detected, otherwise `false`.
 */

const interactiveRegex = /(?:file\.choose|choose\.dir|tk_choose\.dir|read\.clipboard|getwd)\s*\(/g
export const hasInteractiveMethods = (content: string) => {
  return interactiveRegex.test(content)
}
