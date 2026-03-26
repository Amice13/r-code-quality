const testCommands = [
  'stopifnot',
  'assert',
  'test_that',
  'expect_equal',
  'expect_snapshot',
  'expect_equal',
  'expect_no_error',
  'local_reproducible_output',
  'verify_output',
  'expect_known_output',
  'expect_known_value',
  'expect_known_hash'
]

const testRegexp = new RegExp(`(?:${testCommands.join('|')})\\s*\\(`)
export const hasTests = (content: string) => {
  return testRegexp.test(content)
}
