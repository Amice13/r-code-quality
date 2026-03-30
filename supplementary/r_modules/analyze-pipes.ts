/**
 * Detects usage of pipe operators within a text block.
 *
 * @param content - Raw text content to analyze.
 * @returns An object containing boolean flags:
 * - `hasMagrittrPipes`: true if magrittr pipe (`%>%`) operator is found
 * - `hasNativePipes`: true if native pipe (`|>`) is found
 * - `hasCompoundAssignmentPipes`: true if compound assignemnt pipe (`%<>%`) is found 
 */

const magrittrPipeRegex = /%>%/
const compoundAssignmentRegex = /%<>%/
const nativePipeRegex = /\|>/
export const analyzePipes = (content: string) => {
  const hasMagrittrPipes = magrittrPipeRegex.test(content)
  const hasNativePipes = nativePipeRegex.test(content)
  const hasCompoundAssignmentPipes = compoundAssignmentRegex.test(content)
  return {
    hasMagrittrPipes,
    hasNativePipes,
    hasCompoundAssignmentPipes
  }
}
