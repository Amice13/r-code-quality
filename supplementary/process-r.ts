import fs from 'fs'
import path from 'path'
import { getFormattingIssues } from './r_modules/index.ts'

const SOURCE_FOLDER = path.join('..', 'data', 'scripts')

const projects = fs.readdirSync(SOURCE_FOLDER, { recursive: true })
  .filter((file) => typeof file === 'string' && /\.r$/i.test(file))

// console.log(projects)
for (const file of projects) {
  if (typeof file !== 'string') continue
  const content = fs.readFileSync(path.join(SOURCE_FOLDER, file)).toString()
  const [project, filename] = file.split('/')
  if (project === undefined || filename === undefined) continue 

  // Get file size
  const fileStats: Partial<FileStats> = { project, filename }
  fileStats.size = content.length

  const issues = getFormattingIssues(content)
  // console.log(issues)
  console.log(file)
  // const linesData = analyzeLines(content)
  // const pipesData = analyzePipes(content)
  // const libraries = getLoadedPackages(content)  
  // const dependsOnInteractiveMethods = hasInteractiveMethods(content)
  // const apiKeys = getApiKeys(content)
  // const functionNames = getFunctionNames(content)
  // const files = getFiles(content)
  // const explicitColumnDefinition = hasEpxlicitColumnDefinition(content)
  // const installedPackages = getInstalledPackages(content)
  // const installedPackages = getLoadedPackages(content)
  // const installedPackages = getVariableNames(content)
  // console.log(installedPackages, file)
  // console.log(installedPackages)
  // console.log(files, project, file)
  // console.log(dependsOnInteractiveMethods)
  // console.log(functionNames, file)
  // const lines = content.split(/[\n\r]+/g)
  // const wd = getWorkingDirectories(content)
  // console.log(wd, file)
  // for (const line of lines) {
  //   if (line.match(/^[^#]*->(?!>)(?:[^"')]*(?:#|$))/g)) console.log(line.split(/^[^#]*->/g).slice(-1), line)
  // }

  // Use explicit returns - https://google.github.io/styleguide/Rguide.html

  // Qualifying namespaces - https://google.github.io/styleguide/Rguide.html

  // Variable and function names should be lowercase - http://adv-r.had.co.nz/Style.html
}
