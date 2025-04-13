# Sample Project: Analyzing and Visualizing Student Scores in R

# Load necessary libraries
install.packages("reshape2")
install.packages("ggplot2")

library(reshape2)
library(ggplot2)


# Step 1: Create a dataset
student_scores <- data.frame(
  student_name = c("Alice", "Bob", "Charlie", "David", "Eve", "Frank", "Grace", "Hannah", "Ian", "Jack"),
  Math = c(85, 90, 78, 92, 88, 76, 95, 89, 84, 91),
  Science = c(80, 85, 82, 88, 90, 78, 87, 92, 81, 86),
  English = c(78, 82, 85, 80, 87, 90, 88, 84, 83, 86)
)

# Display the dataset
print(student_scores)

# Step 2: Perform basic statistical analysis
summary_math <- summary(student_scores$Math)
summary_science <- summary(student_scores$Science)
summary_english <- summary(student_scores$English)

# Calculate mean, median, and standard deviation for each subject
stats <- data.frame(
  Subject = c("Math", "Science", "English"),
  Mean = c(mean(student_scores$Math), mean(student_scores$Science), mean(student_scores$English)),
  Median = c(median(student_scores$Math), median(student_scores$Science), median(student_scores$English)),
  SD = c(sd(student_scores$Math), sd(student_scores$Science), sd(student_scores$English))
)
print(stats)

# Step 3: Visualize the data
# Bar plot for average scores
avg_scores <- colMeans(student_scores[,-1]) # Exclude the student_name column
avg_scores_df <- data.frame(Subject = names(avg_scores), Average_Score = avg_scores)

ggplot(avg_scores_df, aes(x = Subject, y = Average_Score, fill = Subject)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Scores by Subject", x = "Subject", y = "Average Score") +
  theme_minimal()

# Histogram for Math scores
ggplot(student_scores, aes(x = Math)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Math Scores", x = "Math Scores", y = "Frequency") +
  theme_minimal()

# Box plot for all subjects
student_scores_long <- reshape2::melt(student_scores, id.vars = "student_name", variable.name = "Subject", value.name = "Score")

ggplot(student_scores_long, aes(x = Subject, y = Score, fill = Subject)) +
  geom_boxplot() +
  labs(title = "Box Plot of Scores by Subject", x = "Subject", y = "Scores") +
  theme_minimal()

# Step 4: Identify highest and lowest scorers in each subject
highest_scorers <- apply(student_scores[,-1], 2, function(x) student_scores$student_name[which.max(x)])
lowest_scorers <- apply(student_scores[,-1], 2, function(x) student_scores$student_name[which.min(x)])

highest_lowest <- data.frame(
  Subject = names(highest_scorers),
  Highest_Scorer = highest_scorers,
  Lowest_Scorer = lowest_scorers
)
print(highest_lowest)

# Step 5: Compare performance trends across subjects
ggplot(student_scores_long, aes(x = student_name, y = Score, color = Subject, group = Subject)) +
  geom_line() +
  geom_point() +
  labs(title = "Performance Trends Across Subjects", x = "Student Name", y = "Score") +
  theme_minimal()

