declare global {
  interface FileStats {
    project: string
    filename: string
    size: number
    lines: number
    commentLines: number
    commentFormattingLines: number
    emptyLines: number
  }
}

export {}
