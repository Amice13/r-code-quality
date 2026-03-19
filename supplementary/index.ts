import fs from 'fs'
import path from 'path'
import {
  getPage,
  getFileset,
  downloadFile
} from './custom_modules/harvard-downloader.ts'
import { createCsvStream } from './custom_modules/prepare-csv.ts'

// Place to store metadata and scripts
const OUTPUT_FOLDER = path.join('..', 'data')
const OUTPUT_FILE = path.join(OUTPUT_FOLDER, 'datasets.csv')

fs.mkdirSync(OUTPUT_FOLDER, { recursive: true })

const csvStream = createCsvStream(OUTPUT_FILE)

// Old

const fileExists = fs.existsSync('./.tmp')
const downloaded = fileExists ? fs.readFileSync('./.tmp').toString().split(/[\r\n]+/g).filter(Boolean) : []
console.log(downloaded)
let page = 1
let nextPageExists = true

while (nextPageExists) {
  const datasets = await getPage(page)
  if (datasets === null) break
  const { items } = datasets
  for (const dataset of items) {
    const csvData = structuredClone(dataset) as unknown as Record<string, string | number>
    if (dataset.fileCount === undefined || dataset.fileCount < 1) continue
    console.log(downloaded.includes(dataset.global_id), dataset.global_id)
    if (downloaded.includes(dataset.global_id)) continue
    const fileSet = await getFileset(dataset.global_id)
    fs.appendFileSync('./.tmp', dataset.global_id + '\n')
    if (fileSet?.latestVersion?.files === undefined) continue
    const files = fileSet.latestVersion.files
    const filesWithR = files.filter(file => file?.dataFile?.contentType === 'type/x-r-syntax')
    if (filesWithR.length === 0) continue
    const projectFolder = dataset.global_id.replace(/[^a-z\d-]/ig, '_')
    const folder = path.join(OUTPUT_FOLDER, 'scripts', projectFolder)
    if (fs.existsSync(folder)) continue
    fs.mkdirSync(folder, { recursive: true })

    csvData.subjects = dataset.subjects.join(';')
    csvData.authors = dataset.authors.join(';')
    csvStream.write(dataset)

    for (const file of filesWithR) {
      const filename = file.dataFile.filename
      const fileId = file.dataFile.id
      downloadFile(String(fileId), path.join(folder, filename))
    }
  }
  nextPageExists = datasets.count_in_response > 0
  page = page + datasets.count_in_response
}

csvStream.end()
