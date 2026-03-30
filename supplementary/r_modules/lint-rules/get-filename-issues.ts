// File names should be machine readable - https://style.tidyverse.org/files.html

const validPattern = /[a-z\d_-]+\.R/
const datePattern = /\d{2}[.-]\d{2}[.-]\d{4}|\d{4}[.-]\d{2}[.-]\d{2}|[^.-]\d{4}$/
const sequencePattern = /^\d{1,3}/

export const getFilenameIssues = (files: string[]) => {
  return files.map(path => {
    const parts = path.split(/\\\//g)
    if (parts.length === 1) {
      return {
        file: path,
        isValid: validPattern.test(path),
        hasDate: datePattern.test(path),
        hasSequence: sequencePattern.test(path)
      }
    }
    const file = parts.at(-1)
    const folder = parts.slice(0, parts.length - 1).join('/')
    return {
      file,
      isValid: validPattern.test(file ?? ''),
      hasDate: datePattern.test(file ?? ''),
      hasSequence: sequencePattern.test(file ?? ''),
      folder
    }
  })
}
