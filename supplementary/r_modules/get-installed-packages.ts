/**
 * Extracts R package installation calls from a script and parses the installed package names.
 *
 * This function detects calls to common R installation functions:
 * - Base R: `install.packages()`
 * - devtools: `devtools::install_github()`
 * - renv: `renv::install()`
 *
 * For each call, it captures either:
 * - **Literal package names** (e.g., `"dplyr"` in `install.packages("dplyr")`), including multiple packages specified via `c(...)`
 * - **Variables** (e.g., `pkgs` in `install(pkgs)`), when the package list is stored in a variable
 *
 *
 * @param content - Raw R script or code to analyze.
 * @returns An array of package installation objects, one per installed package or variable reference.
 */

const installFunctions = [
  'install\\.packages',
  '(?:devtools::)?install_github',
  '(?:renv::)?install'
]
const packageNamesRegex = '(?:(?:c\\()?(?<packages>(?:[\\s,]*(?<quote>[\'"]).*?\\k<quote>)+)|(?<variable>[_.a-z]+))'
const installPackagesRegexp = new RegExp(`(?<=\\b(?<fn>${installFunctions.join('|')})\\s*\\()${packageNamesRegex}`, 'ig')

export const getInstalledPackages = (content: string) => {
  const packages = [...content.matchAll(installPackagesRegexp)]
  const results = packages.map(match => {
    const type = match.groups?.fn === 'install.packages'
      ? 'base'
      : (match.groups?.fn ?? '').includes('github')
        ? 'devtools' : 'renv'
    if (match.groups?.variable !== undefined) {
      return {
        type,
        definedByVariable: true,
        variable: match.groups.variable
      }
    }
    const packages = (match.groups?.packages ?? '').split(',').map(el => el.replace(/['"\s\r\n]/g, ''))
    return packages.map(name => ({
      type, name
    }))
  })
  return results.flat()
}
