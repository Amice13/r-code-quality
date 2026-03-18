interface File {
  description: string
  label: string
  restricted: boolean
  version: number
  datasetVersionId: number
  categories: string[]
  dataFile: {
    id: number
    persistentId: string
    pidURL: string
    filename: string
    contentType: string
    friendlyType: string
    filesize: number
    description: string
    categories: string[]
    storageIdentifier: string
    rootDataFileId: number
    md5: string
    checksum: {
      type: number
      value: string
    },
    tabularData: boolean
    creationDate: string
    publicationDate: string
    lastUpdateTime: string
    fileAccessRequest: boolean
  }
}

interface License {
  name: string
  uri: string
  iconUri: string
  rightsIdentifier: string
  rightsIdentifierScheme: string
  schemeUri: string
  languageCode: string
}

interface Version {
  id: number
  datasetId: number
  datasetPersistentId: string
  datasetType: string
  storageIdentifier: string
  versionNumber: number
  internalVersionNumber: number
  versionMinorNumber: number
  versionState: string
  latestVersionPublishingState: string
  deaccessionNote: string
  deaccessionLink: string
  distributionDate: string
  productionDate: string
  UNF: string
  lastUpdateTime: string
  releaseTime: string
  createTime: string
  alternativePersistentId: string
  publicationDate: string
  citationDate: string
  license: License
  fileAccessRequest: boolean
  metadataBlocks: unknown
  files: File[]
}

declare global {
  interface Metadata {
    id: number
    identifier: string
    persistentUrl: string
    protocol: string
    authority: string
    separator: string
    publisher: string
    publicationDate: string
    storageIdentifier: string
    datasetType: string
    latestVersion: Version
  }

  interface MetadataResponse {
    status: string
    data: Metadata
  }
}

export {}
