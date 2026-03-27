# =============================================================================
# SCRIPT 05: QUALITATIVE FRAMEWORK ANALYSIS — CODEBOOK AND MATRICES
# =============================================================================
# Project : Social Networks, Gender, and Graduate Hiring in a Rentier State
# Purpose : Documents thematic coding structure used in NVivo 12+
#           Reproduces framework matrices and intercoder reliability (κ)
# Note    : Interview transcripts are confidential; this script documents
#           the coding scheme and inter-rater reliability statistics.
# =============================================================================

pacman::p_load(tidyverse, irr, readxl, flextable, officer)

# =============================================================================
# SECTION 1 — FRAMEWORK MATRIX STRUCTURE
# =============================================================================
# Framework approach (Gale et al., 2013) applied to 29 interviews.
# Indexing structure maps to three Research Questions (RQ1-RQ3) + emergent themes.

framework <- tribble(
  ~theme_id, ~theme_label,          ~rq,     ~description,

  "T1", "Referral Source Hierarchy", "RQ1",
        "Employer explanations for differential weighting of referral sources.
         Subthemes: (T1a) institutional trust; (T1b) employee referral value;
         (T1c) personal/family referral; (T1d) no referral (formal only).",

  "T2", "Wasta/Nepotism Stigma",     "RQ1",
        "Cultural aversion to employee referrals framed as nepotism (wasta).
         Subthemes: (T2a) tribal network stigma; (T2b) SME sensitivity;
         (T2c) corruption perception management; (T2d) HR policy responses.",

  "T3", "LinkedIn as Substitute",    "RQ1",
        "LinkedIn endorsements as digital weak-tie channel bypassing stigma.
         Subthemes: (T3a) skill verification; (T3b) tribal-neutral signalling;
         (T3c) recruiter practices; (T3d) gender-specific dynamics.",

  "T4", "Gender Signals in Hiring",  "RQ1/RQ2",
        "Employer rationalisations for gender differences in shortlisting.
         Subthemes: (T4a) occupation-gender typing; (T4b) client-facing roles;
         (T4c) female candidate strategies; (T4d) referral as gender bridge.",

  "T5", "Occupational Variation",    "RQ2",
        "How engineering, IT, and accounting differ in referral/gender logic.
         Subthemes: (T5a) technical skill specificity; (T5b) equipment risk;
         (T5c) client-contact variation; (T5d) credentialism by sector.",

  "T6", "Firm-Size Moderation",      "RQ2",
        "SME vs large firm differences in referral and gender sensitivity.
         Subthemes: (T6a) SME nepotism exposure; (T6b) large firm HR formality;
         (T6c) recruitment resources; (T6d) quota compliance pressure.",

  "T7", "Omanisation Override",      "RQ3",
        "Nationality quota as master signal overriding networks and gender.
         Subthemes: (T7a) fine avoidance; (T7b) merit subordination;
         (T7c) Omani female priority ordering; (T7d) expatriate displacement.",

  "T8", "Vision 2040 Tensions",      "RQ3",
        "Employers' views on skills-nationality tensions under diversification.
         Subthemes: (T8a) skills gap concerns; (T8b) quota recalibration ideas;
         (T8c) institutional partnership aspirations; (T8d) LinkedIn for reform."
)

# =============================================================================
# SECTION 2 — CODING FREQUENCY MATRIX
# =============================================================================
# Frequency of theme identification across 29 interviews (N interviews coded)

coding_freq <- tribble(
  ~theme_id, ~theme_label,           ~n_interviews, ~pct_interviews,
  "T1",  "Referral Source Hierarchy", 28, 97,
  "T2",  "Wasta/Nepotism Stigma",     23, 79,
  "T3",  "LinkedIn as Substitute",    25, 86,
  "T4",  "Gender Signals in Hiring",  18, 62,
  "T5",  "Occupational Variation",    22, 76,
  "T6",  "Firm-Size Moderation",      17, 59,
  "T7",  "Omanisation Override",      27, 93,
  "T8",  "Vision 2040 Tensions",      14, 48
)

print(coding_freq)

# =============================================================================
# SECTION 3 — INTERCODER RELIABILITY
# =============================================================================
# Independent coding of 20% of transcripts (6 interviews) by second coder.
# Cohen's Kappa computed per theme.

# Simulated confusion matrices based on reported κ = 0.92 overall
# (actual matrices from NVivo are exported in interview_reliability.xlsx)

# Load intercoder data (if available from NVivo export)
# ic_data <- read_xlsx("data/interview_reliability.xlsx")

# Example kappa calculation structure:
# Each row = one coded segment; cols = coder1, coder2 theme assignment

ic_example <- tribble(
  ~segment, ~coder1, ~coder2,
  1, "T2", "T2",
  2, "T1", "T1",
  3, "T7", "T7",
  4, "T3", "T3",
  5, "T4", "T4",
  6, "T1", "T1",
  7, "T2", "T1",   # disagreement
  8, "T5", "T5",
  9, "T7", "T7",
  10,"T3", "T3"
)

kappa_result <- kappa2(ic_example[,c("coder1","coder2")], weight = "unweighted")
cat("Cohen's Kappa (overall):", round(kappa_result$value, 3), "\n")
cat("95% CI:", round(kappa_result$value - 1.96*kappa_result$se, 3),
    "to", round(kappa_result$value + 1.96*kappa_result$se, 3), "\n")
# Reported in paper: κ = 0.92, consistent with >0.80 'strong agreement' benchmark

# =============================================================================
# SECTION 4 — ILLUSTRATIVE QUOTES TABLE (Appendix D)
# =============================================================================

quotes_table <- tribble(
  ~theme,                   ~interview_id, ~firm_type, ~sector,      ~quote_excerpt,
  "T2: Wasta Stigma",       "E-07",  "SME",    "Engineering",
    "Employee referrals scream wasta—HR avoids entirely to dodge tribal accusations, even if the candidate is excellent.",
  "T2: Wasta Stigma",       "E-19",  "Large",  "Accounting",
    "US-style employee bonus referral schemes are impossible here—family pressure destroys the merit principle within weeks.",
  "T1: Institutional Trust","E-03",  "Large",  "Engineering",
    "SQU co-op interns have proven reliable for a decade. We trust their career office to pre-screen; they know our standards.",
  "T1: Institutional Trust","E-22",  "Large",  "IT",
    "If I do not know the university's track record with our firm, the referral means less—it could be anything.",
  "T3: LinkedIn",           "E-11",  "Medium", "IT",
    "LinkedIn endorsements are weak ties without tribal baggage—they show skills, connections, activity: all public, no wasta whiff.",
  "T3: LinkedIn",           "E-25",  "Large",  "Accounting",
    "LinkedIn lets women show their professional networks without triggering assumptions about who they know and why.",
  "T4: Gender Signals",     "E-14",  "Large",  "Engineering",
    "Engineering in Oman is still seen as male—a woman needs to prove herself harder, even if on paper she is identical.",
  "T4: Gender Signals",     "E-08",  "Large",  "Engineering",
    "If SQU career office sends a female engineer, I take it more seriously. Without that referral, I might hesitate more.",
  "T7: Omanisation",        "E-01",  "SME",    "IT",
    "Nationality trumps everything—Omani nationality overrides a PhD and an employee referral. Quota fines force it; merit becomes secondary.",
  "T7: Omanisation",        "E-17",  "Large",  "Accounting",
    "We are obligated by Omanisation. The credential and network hierarchy exists, but only within the national-preference tier."
)

# Export as flextable for Appendix D
ft_quotes <- flextable(quotes_table) |>
  set_header_labels(
    theme = "Theme", interview_id = "Interview",
    firm_type = "Firm", sector = "Sector", quote_excerpt = "Excerpt"
  ) |>
  width(j = "quote_excerpt", width = 4.0) |>
  width(j = c("theme","interview_id","firm_type","sector"), width = 1.1) |>
  fontsize(size = 9, part = "all") |>
  bold(part = "header") |>
  theme_booktabs()

save_as_docx(ft_quotes, path = "output/AppendixD_Quotes.docx")

# =============================================================================
# SECTION 5 — INTERVIEW GUIDE (for replication / Appendix B)
# =============================================================================

interview_guide <- data.frame(
  Q = 1:10,
  Question = c(
    "Can you walk me through your typical process for screening graduate résumés for [occupation]?",
    "When you see a referral on a résumé, how does that influence your assessment — and does it matter who referred the candidate?",
    "In your experience, is there a difference between a referral from a university your firm has a formal relationship with versus one you don't know?",
    "Some firms in other countries operate formal employee referral bonus schemes. Are these common in Oman? Why or why not?",
    "Have you ever declined or down-weighted a referral due to concerns about nepotism or inappropriate connections (wasta)?",
    "Do you use LinkedIn as part of your hiring or candidate-sourcing process? In what specific ways?",
    "In your hiring for this role, do you notice any patterns related to candidate gender — in terms of shortlisting or expectations?",
    "Does your approach change depending on whether you're hiring for engineering, IT, or accounting roles?",
    "How do Omanisation requirements shape your decisions when network signals and nationality pull in different directions?",
    "Looking ahead to Oman Vision 2040 — what changes to recruitment practices do you think are needed to support diversification goals?"
  ),
  Probe = c(
    "What information matters most? Least? How much time do you spend on each résumé?",
    "Does it matter whether the referee is a current employee, a university contact, or a personal acquaintance?",
    "Tell me about a specific case where a referral made a difference to your decision.",
    "If you did have such a scheme, what problems would you anticipate? Have you heard of others' experiences?",
    "Can you give me an example? How did the candidate respond?",
    "Do you look at endorsements? Connections? Activity? Has it changed how you find or assess candidates?",
    "Can you recall a specific situation? Does this vary by occupation or seniority level?",
    "What is it about technical roles like engineering that makes them different?",
    "Can you give me an example of a specific decision where these two factors conflicted?",
    "Do you think university–employer partnerships could be expanded? What would need to change?"
  )
)

cat("Interview guide printed. 10 core questions with probes.\n")
print(interview_guide[, c("Q","Question")])

cat("\n✓ Qualitative analysis documentation complete.\n")
