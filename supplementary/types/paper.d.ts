declare global {
  interface Paper {
    name: string
    type: string
    url: string
    identifier?: string
    global_id: string
    description: string
    published_at: string
    publisher: string
    citationHtml: string
    identifier_of_dataverse: string
    name_of_dataverse: string
    citation: string
    publicationStatuses: string[]
    storageIdentifier: string
    subjects: string[]
    fileCount: number
    versionId: number
    versionState: string
    majorVersion: number
    minorVersion: number
    createdAt: string
    updatedAt: string
    contacts: {
      name: string
      affiliation: string
    }
    relatedMaterial?: string[]
    publications: Array<{
      citation: string
      url: string
    }>
    geographicCoverage?: Array<{
      country?: string
      other?: string
    }>
    authors: string[]
  }
}

export {}