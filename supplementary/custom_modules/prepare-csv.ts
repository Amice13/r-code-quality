import fs from 'fs'
import { stringify } from 'csv-stringify'

const datasetColumns = [
  'name',
  'url',
  'global_id',
  'published_at',
  'publisher',
  'name_of_dataverse',
  'subjects',
  'citation',
  'versionState',
  'authors'
]

export const createCsvStream = (filename: string) => {
  const fileExists = fs.existsSync(filename)
  const isEmpty = !fileExists || fs.statSync(filename).size === 0

  const stream = fs.createWriteStream(filename, { flags: 'a' })

  const stringifier = stringify({
    header: isEmpty,
    columns: datasetColumns
  })

  stringifier.pipe(stream)

  return stringifier
}
