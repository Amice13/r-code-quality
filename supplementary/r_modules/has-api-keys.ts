/**
 * Scans the input text for potential hardcoded API keys, tokens, or secrets.
 *
 * The detection is based on a heuristic regex that looks for:
 * - security-related keywords (e.g. `token`, `api`, `key`, `auth`, `secret`, etc.)
 * - followed by a quoted string containing a long, key-like value
 *
 * A key-like value is defined as:
 * - at least 22 characters long
 * - consisting of lowercase letters, digits, dots, or hyphens
 *
 * @param content - Raw text or code to analyze.
 * @returns Array of matched substrings that may contain API keys, or an empty array if none found.
 */

const apiKeyRegex = /(token|api|key|auth|access|aws|ssh|rsa\b|pgp\b|secret|pass)[^'"\n]*('|")(?<key>[a-z0-9.-]{22,})('|")/ig
export const hasApiKeys = (content: string) => {
  return apiKeyRegex.test(content)
}
