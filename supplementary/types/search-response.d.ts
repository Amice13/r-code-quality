declare global {
  interface SearchResponse {
    status: String
    data: {
      q: string,
      total_count: number,
      start: number,
      items: Paper[]
      count_in_response: number
    }
  }
}

export {}
