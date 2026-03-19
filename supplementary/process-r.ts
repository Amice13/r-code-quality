import fs from 'fs'
import path from 'path'
import {
  analyzeLines,
  analyzePipes,
  hasInteractiveMethods, 
  getLoadedPackages,
  getApiKeys
} from './custom_modules/r-analysis.ts'

const SOURCE_FOLDER = path.join('..', 'data', 'scripts')

const projects = fs.readdirSync(SOURCE_FOLDER, { recursive: true })
  .filter((file) => typeof file === 'string' && /\.r$/i.test(file))

for (const file of projects) {
  if (typeof file !== 'string') continue
  const content = fs.readFileSync(path.join(SOURCE_FOLDER, file)).toString()
  const [project, filename] = file.split('/')
  if (project === undefined || filename === undefined) continue 

  // Get file size
  const fileStats: Partial<FileStats> = { project, filename }
  fileStats.size = content.length

  const linesData = analyzeLines(content)
  const pipesData = analyzePipes(content)
  const libraries = getLoadedPackages(content)  
  const yep = hasInteractiveMethods(content)
  const apiKeys = getApiKeys(content)

  console.log(content.match(/->/g))


  // Use explicit returns - https://google.github.io/styleguide/Rguide.html

  // Qualifying namespaces - https://google.github.io/styleguide/Rguide.html

  // Variable and function names should be lowercase - http://adv-r.had.co.nz/Style.html
}

//     const tablesAndVariables = content.match(/(?<![a-z\d\.])(?:\.[a-z]|[a-z])[a-z\d_]+\$(?:\.[a-z]|[a-z])[a-z\d_]/g)
//     const goodGrepl = content.match(/\[grepl.*/g)
//     const hasReport = content.match(/report_.*?\(.*/g)

//     // console.log(tablesAndVariables)
//     return {
//       project,
//       name,
//       linesLength,
//       commentsLength,
//       multiCommentsLength,
//       emptyLinesLength,
//       // content,
//       length
//     }
//   })

// // console.log(projects)


// // prediction.ideology.5 <- results.ideology.5[4]$stats$`Expected Values: E(Y|X)`\r\n'
