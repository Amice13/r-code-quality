import fs from 'fs'

const DEBUG = true

// Delay is necessary to not abuse API. Set it from 1000 to 5000
const DELAY = 1000
const MAX_DELAY = 5000

// Setup custom fetcher
const TOKEN = process.env.TOKEN
const BASE_URL = 'https://dataverse.harvard.edu/api'
let limit = 0

const customFetch = async (relativeURL: string): Promise<null | Response> => {
  if (limit) {
    if (limit > MAX_DELAY) {
      limit = 1000
      return null
    }
    await new Promise(resolve => setTimeout(resolve, limit))
  }
  const url = `${BASE_URL}${relativeURL}`
  const headers = {
    'X-Dataverse-key': TOKEN
  }

  const response = await fetch(url, { headers }).catch(err => {
    if (DEBUG) console.log(`Delay is ${limit}`)
    console.log(err)
    return null
  })
  if (response === null || response.status === 401 || response.status === 403) {
    if (limit) limit *= 2
    if (!limit) limit = DELAY
    if (DEBUG) console.log(`Response - ${String(response?.status)}. Delay is ${String(limit)}`)
    return customFetch(relativeURL)
  }
  limit = 1000
  return response
}

// Do not set greater than 1000
const PER_PAGE = 100

// Search script params
const SEARCH_PARAMS = {
  q: '.R',
  per_page: String(PER_PAGE),
  types: 'file',
  fileTypeGroupFacet: 'Code'
}

export const getPage = async (start = 1): Promise<SearchResponse['data'] | null> => {
  if (DEBUG) console.log(`Page ${start} is being fetched`)
  const params = new URLSearchParams({ ...SEARCH_PARAMS, start: String(start) })
  const response = await customFetch(`/search?${params.toString()}`)
  if (!response) return null
  const json = await response.json() as SearchResponse
  return json.data
}

export const getFileset = async (global_id: string): Promise<Metadata | null> => {
  if (DEBUG) console.log(`Fileset ${global_id} is being fetched`)
  try {
    const response = await customFetch(`/datasets/:persistentId?persistentId=${global_id}`)
    if (!response) return null
    const json =  await response?.json() as MetadataResponse

    return json.data
  } catch (err) {
    console.log(err)
    console.log(`Fileset ${global_id} is not found`)
    return null
  }
}

export const downloadFile = async (id: string, filename: string): Promise<void> => {
  if (DEBUG) console.log(`File ${filename} is being downloaded`)
  await new Promise(resolve => setTimeout(resolve, 1000))
  try {
    const response = await customFetch(`/access/datafile/${id}`)
    if (!response) return
    const buffer = Buffer.from(await response.arrayBuffer())
    fs.writeFileSync(filename, buffer)    
  } catch (err) {
    console.log(err)
    console.log(`${filename} has not been downloaded`)
  }
}
