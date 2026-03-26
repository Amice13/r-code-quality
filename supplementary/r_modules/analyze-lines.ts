/**
 * Analyzes a text block line-by-line and produces basic structural metrics.
 *
 * @param content - Raw text content to analyze.
 * @returns An object containing line statistics:
 * - `numberOfLines`: total number of lines in the input
 * - `numberOfComments`: lines beginning with a comment marker
 * - `numberOfFormattedComments`: comment lines containing a secondary `#`
 *   pattern used for visual formatting or section separators
 * - `numberOfEmptyLines`: lines containing only whitespace
 */

export const analyzeLines = (content: string) => {
  const lines = content.split(/[\n\r]/g)
  const commentLines = lines.filter(line => /^[\s\t]*#/.test(line))
  const commentFormattingLines = commentLines.filter(line => /#[\s\t]*#/.test(line))
  const emptyLines = lines.filter(line => /^[\s\t]*$/gm.test(line))
  
  return {
    numberOfLines: lines.length,
    numberOfComments: commentLines.length,
    numberOfFormattedComments: commentFormattingLines.length,
    numberOfEmptyLines: emptyLines.length
  }
}
