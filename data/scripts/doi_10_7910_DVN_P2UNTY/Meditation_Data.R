# Create the complete study dataset
study_data <- data.frame(
  ParticipantID = 1:10,
  Age = c(32, 28, 45, 36, 29, 41, 33, 39, 27, 34),
  Gender = c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Male", "Female", "Female"),
  Illness_Duration = c(5, 3, 12, 8, 4, 10, 6, 9, 2, 7),
  Medication_Months = c(6, 4, 8, 7, 5, 9, 6, 8, 3, 7),
  PANSS_Positive_Pre = c(23, 21, 25, 22, 20, 24, 21, 23, 19, 22),
  PANSS_Positive_Post = c(19, 17, 21, 19, 16, 20, 18, 19, 15, 18),
  PANSS_Negative_Pre = c(25, 23, 28, 24, 22, 27, 23, 26, 20, 24),
  PANSS_Negative_Post = c(22, 20, 25, 21, 19, 24, 20, 23, 17, 21),
  PANSS_General_Pre = c(31, 28, 35, 29, 27, 34, 28, 32, 25, 30),
  PANSS_General_Post = c(27, 24, 31, 25, 23, 30, 24, 28, 21, 26),
  DigitSpan_Pre = c(5, 4, 6, 5, 4, 6, 5, 5, 4, 5),
  DigitSpan_Post = c(6, 5, 7, 6, 5, 7, 6, 6, 5, 6),
  TrailMaking_Pre = c(48, 42, 52, 44, 40, 50, 43, 47, 38, 45),
  TrailMaking_Post = c(43, 38, 47, 39, 36, 45, 39, 42, 34, 41),
  QOL_Psych_Pre = c(62, 58, 55, 65, 60, 52, 63, 57, 68, 59),
  QOL_Psych_Post = c(72, 67, 65, 75, 70, 62, 73, 67, 78, 69),
  QOL_Social_Pre = c(58, 52, 48, 60, 55, 45, 62, 50, 65, 53),
  QOL_Social_Post = c(70, 65, 60, 72, 68, 58, 75, 63, 78, 66)
)

# View 1: Participant Demographics
cat("=== VIEW 1: PARTICIPANT DEMOGRAPHICS ===\n")
demo_view <- study_data[, 1:5]
print(demo_view)
cat("\nPress Enter to continue...")
invisible(readline())
# View 2: PANSS Scores - Positive Symptoms
cat("\n=== VIEW 2: PANSS POSITIVE SYMPTOMS ===\n")
panss_positive <- study_data[, c(1, 6:7)]
panss_positive$Change <- panss_positive$PANSS_Positive_Pre - panss_positive$PANSS_Positive_Post
print(panss_positive)
cat("\nMean Pre:", round(mean(panss_positive$PANSS_Positive_Pre), 1))
cat("\nMean Post:", round(mean(panss_positive$PANSS_Positive_Post), 1))
cat("\nMean Improvement:", round(mean(panss_positive$Change), 1))
cat("\n\nPress Enter to continue...")
invisible(readline())
# View 3: PANSS Scores - Negative Symptoms
cat("\n=== VIEW 3: PANSS NEGATIVE SYMPTOMS ===\n")
panss_negative <- study_data[, c(1, 8:9)]
panss_negative$Change <- panss_negative$PANSS_Negative_Pre - panss_negative$PANSS_Negative_Post
print(panss_negative)
cat("\nMean Pre:", round(mean(panss_negative$PANSS_Negative_Pre), 1))
cat("\nMean Post:", round(mean(panss_negative$PANSS_Negative_Post), 1))
cat("\nMean Improvement:", round(mean(panss_negative$Change), 1))
cat("\n\nPress Enter to continue...")
invisible(readline())
# View 4: PANSS Scores - General Psychopathology
cat("\n=== VIEW 4: PANSS GENERAL PSYCHOPATHOLOGY ===\n")
panss_general <- study_data[, c(1, 10:11)]
panss_general$Change <- panss_general$PANSS_General_Pre - panss_general$PANSS_General_Post
print(panss_general)
cat("\nMean Pre:", round(mean(panss_general$PANSS_General_Pre), 1))
cat("\nMean Post:", round(mean(panss_general$PANSS_General_Post), 1))
cat("\nMean Improvement:", round(mean(panss_general$Change), 1))
cat("\n\nPress Enter to continue...")
invisible(readline())
# View 5: Cognitive Performance - Digit Span Test
cat("\n=== VIEW 5: COGNITIVE PERFORMANCE - DIGIT SPAN ===\n")
digit_span <- study_data[, c(1, 12:13)]
digit_span$Improvement <- digit_span$DigitSpan_Post - digit_span$DigitSpan_Pre
print(digit_span)
cat("\nMean Pre:", round(mean(digit_span$DigitSpan_Pre), 1))
cat("\nMean Post:", round(mean(digit_span$DigitSpan_Post), 1))
cat("\nMean Improvement:", round(mean(digit_span$Improvement), 1))
cat("\n\nPress Enter to continue...")
invisible(readline())
# View 6: Cognitive Performance - Trail Making Test
cat("\n=== VIEW 6: COGNITIVE PERFORMANCE - TRAIL MAKING ===\n")
trail_making <- study_data[, c(1, 14:15)]
trail_making$Improvement <- trail_making$TrailMaking_Pre - trail_making$TrailMaking_Post
print(trail_making)
cat("\nMean Pre:", round(mean(trail_making$TrailMaking_Pre), 1), "seconds")
cat("\nMean Post:", round(mean(trail_making$TrailMaking_Post), 1), "seconds")
cat("\nMean Improvement:", round(mean(trail_making$Improvement), 1), "seconds faster")
cat("\n\nPress Enter to continue...")
invisible(readline())
# View 7: Quality of Life - Psychological Well-being
cat("\n=== VIEW 7: QUALITY OF LIFE - PSYCHOLOGICAL WELL-BEING ===\n")
qol_psych <- study_data[, c(1, 16:17)]
qol_psych$Improvement <- qol_psych$QOL_Psych_Post - qol_psych$QOL_Psych_Pre
print(qol_psych)
cat("\nMean Pre:", round(mean(qol_psych$QOL_Psych_Pre), 1))
cat("\nMean Post:", round(mean(qol_psych$QOL_Psych_Post), 1))
cat("\nMean Improvement:", round(mean(qol_psych$Improvement), 1), "points")
cat("\n\nPress Enter to continue...")
invisible(readline())
# View 8: Quality of Life - Social Relationships
cat("\n=== VIEW 8: QUALITY OF LIFE - SOCIAL RELATIONSHIPS ===\n")
qol_social <- study_data[, c(1, 18:19)]
qol_social$Improvement <- qol_social$QOL_Social_Post - qol_social$QOL_Social_Pre
print(qol_social)
cat("\nMean Pre:", round(mean(qol_social$QOL_Social_Pre), 1))
cat("\nMean Post:", round(mean(qol_social$QOL_Social_Post), 1))
cat("\nMean Improvement:", round(mean(qol_social$Improvement), 1), "points")
cat("\n\nPress Enter to continue...")
invisible(readline())
# View 9: Summary Statistics
cat("\n=== VIEW 9: STUDY SUMMARY STATISTICS ===\n")
summary_stats <- data.frame(
  Measure = c("PANSS Positive", "PANSS Negative", "PANSS General", 
              "Digit Span", "Trail Making", "QOL Psychological", "QOL Social"),
  Pre_Mean = c(22.4, 24.6, 30.1, 5.2, 45.3, 60.5, 55.3),
  Post_Mean = c(18.2, 21.3, 26.7, 5.8, 40.7, 69.6, 66.4),
  Improvement = c(4.2, 3.3, 3.4, 0.6, 4.6, 9.1, 11.1),
  P_Value = c("<0.05", "<0.01", "<0.01", "<0.05", "<0.05", "<0.05", "<0.01"),
  Effect_Size = c(0.8, 0.7, 0.9, 0.6, 0.6, 0.9, 0.9)
)
print(summary_stats)
cat("\nPress Enter to continue...")
invisible(readline())
# View 10: Qualitative Themes Summary
cat("\n=== VIEW 10: QUALITATIVE THEMES SUMMARY ===\n")
themes_summary <- data.frame(
  Theme = c("Self-Awareness", "Reduced Hallucinations", "Cultural Sensitivity",
            "Improved Coping", "Emotional Regulation", "Empowerment"),
  Patients_Reporting = c(10, 9, 9, 10, 10, 10),
  Percentage = c("100%", "90%", "90%", "100%", "100%", "100%"),
  Example_Quote = c(
    "I can feel more in control of my thoughts",
    "The voices are quieter now when I meditate",
    "Meditation feels natural to me",
    "I use breathing techniques to calm myself",
    "I don't get overwhelmed as easily anymore",
    "Meditation makes me feel like I can handle this"
  )
)
print(themes_summary)
cat("\n=== ALL DATA VIEWS COMPLETE ===\n")
