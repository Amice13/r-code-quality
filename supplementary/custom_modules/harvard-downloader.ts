import fs from 'fs'

const DEBUG = true

// Delay is necessary to not abuse API. Set it from 1000 to 5000
const DELAY = 1000

// Setup custom fetcher
const TOKEN = process.env.TOKEN
const BASE_URL = 'https://dataverse.harvard.edu/api'
const customFetch = async (relativeURL: string) => {
  const url = `${BASE_URL}${relativeURL}`
  const headers = {
    'X-Dataverse-key': TOKEN
  }
  return await fetch(url, { headers })
}

// Do not set greater than 1000
const PER_PAGE = 1000

// Search script params
const SEARCH_PARAMS = {
  q: '.R',
  per_page: String(PER_PAGE),
  types: 'file',
  fileTypeGroupFacet: 'Code'
}

export const getPage = async (page = 1): Promise<SearchResponse['data']> => {
  if (DEBUG) console.log(`Page ${page} is being fetched`)
  await new Promise(resolve => setTimeout(resolve, DELAY))
  const params = new URLSearchParams({ ...SEARCH_PARAMS, page: String(page) })
  const response = await customFetch(`/search?${params.toString()}`)
  const json = await response.json() as SearchResponse
  return json.data
}

export const getFileset = async (global_id: string): Promise<Metadata | null> => {
  if (DEBUG) console.log(`Fileset ${global_id} is being fetched`)
  await new Promise(resolve => setTimeout(resolve, DELAY))
  try {
    const response = await customFetch(`/datasets/:persistentId?persistentId=${global_id}`)
    const json =  await response.json() as MetadataResponse
    return json.data
  } catch (_) {
    console.log(`Fileset ${global_id} is not found`)
    return null
  }
}

export const downloadFile = async (id: string, filename: string): Promise<void> => {
  if (DEBUG) console.log(`File ${filename} is being downloaded`)
  await new Promise(resolve => setTimeout(resolve, 1000))
  try {
    const response = await customFetch(`/access/datafile/${id}`)
    const buffer = Buffer.from(await response.arrayBuffer())
    fs.writeFileSync(filename, buffer)    
  } catch (_) {
    console.log(_)
    console.log(`${filename} has not been downloaded`)
  }
}
