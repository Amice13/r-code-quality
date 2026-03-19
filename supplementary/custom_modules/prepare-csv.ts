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
  const datasetsWritableStream = fs.createWriteStream(filename)
  const datasetStringifier = stringify({ header: true, columns: datasetColumns })
  datasetStringifier.pipe(datasetsWritableStream)
  return datasetStringifier
}
