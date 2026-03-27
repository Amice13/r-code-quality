// Each line of a comment should begin with the comment symbol and a single space: #. - http://adv-r.had.co.nz/Style.html

const badCommentRegexp = /^# /
export const getBadComments = (line: string) => {
  if (!badCommentRegexp.test(line)) return null
  return {
    name: 'Comment does not start with the comment symbol and a single space'
  }
}
