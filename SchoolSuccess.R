install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
install.packages("reshape2")
library(ggplot2)
library(dplyr)
library(readr)
library(reshape2)

data <- read.csv('Desktop/Data101/schoolsuccess.csv', sep = ";", header = TRUE)
head(data)
summary(data)

colnames(data) <- make.names(colnames(data))

clean_data <- na.omit(data)
clean_data <- distinct(clean_data)
clean_data
clean_data$Target <- as.factor(clean_data$Target)
summary(clean_data)  
str(clean_data) 
clean_data



# <--------------------------------------------------------------------->

# heatmap of correlation between numeric columns
numeric_columns <- clean_data %>%
  select(where(is.numeric))

cor_matrix <- cor(numeric_columns, use = "complete.obs")
heatmap_data <- as.data.frame(as.table(cor_matrix))


#Main Heatmap of All Variables
ggplot(heatmap_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  labs(title = "Correlation Heatmap Across All Variables", x = "Variables", y = "Variables", fill = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid.major = element_line(color = "gray90", size = 0.2),  
    panel.grid.minor = element_line(color = "gray90", size = 0.2)  
  ) 

ggsave("Correlation_Heatmap1.png", width = 20, height = 20, dpi = 300) #window wasn't big enough

# <--------------------------------------------------------------------->
#Grades (Concentrated)

# Plot 1: Bargraph
ggplot(clean_data, aes(x = Admission.grade)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Admission Grades", x = "Grades", y = "Number of Students") +
  theme_minimal()

# Plot 2a: Boxplot of Curricular Units Grades by Target (1)
ggplot(clean_data, aes(x = Target, y = Curricular.units.1st.sem..grade., fill = Target)) +
  geom_boxplot() +
  labs(title = "Curricular Units 1st Semester Grades by Target", x = "Target", y = "1st Semester Grade") +
  theme_minimal()

# Plot 2b: Boxplot of Curricular Units Grades by Target (2)
ggplot(clean_data, aes(x = Target, y = Curricular.units.2nd.sem..grade., fill = Target)) +
  geom_boxplot() +
  labs(title = "Curricular Units 2nd Semester Grades by Target", x = "Target", y = "2nd Semester Grade") +
  theme_minimal()

# Plot 3: Scatter Plot of First vs Second Semester Grades
ggplot(clean_data, aes(x = Curricular.units.1st.sem..grade., y = Curricular.units.2nd.sem..grade., color = Target)) +
  geom_point(alpha = 0.7) +
  labs(title = "1st vs 2nd Semester Grades", x = "1st Semester Grade", y = "2nd Semester Grade") +
  theme_minimal()


# <--------------------------------------------------------------------->
# Academic Factors

important_vars <- clean_data %>%
  select(Admission.grade, Curricular.units.1st.sem..credited., 
         Curricular.units.1st.sem..enrolled., Curricular.units.1st.sem..evaluations., 
         Curricular.units.1st.sem..approved., Curricular.units.1st.sem..grade., 
         Curricular.units.1st.sem..without.evaluations., Curricular.units.2nd.sem..credited., 
         Curricular.units.2nd.sem..enrolled., Curricular.units.2nd.sem..evaluations., 
         Curricular.units.2nd.sem..approved., Curricular.units.2nd.sem..grade., 
         Curricular.units.2nd.sem..without.evaluations., Previous.qualification, 
         Previous.qualification..grade., Daytime.evening.attendance., 
         Educational.special.needs, Debtor, Target) %>%
  select(where(is.numeric))

cor_matrix1 <- cor(important_vars, use = "complete.obs")

heatmap_data2 <- as.data.frame(as.table(cor_matrix1))

ggplot(heatmap_data2, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  labs(
    title = "Correlation Heatmap of Academic Factors",
    x = "Variables",
    y = "Variables",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# <--------------------------------------------------------------------->
# Socio-economic Factors
#important variables based on context (grades, target var, others?)
important_vars1 <- clean_data %>%
  select(Mother.s.qualification, Mother.s.occupation, Father.s.qualification, 
         Father.s.occupation, Unemployment.rate, Inflation.rate, GDP,
         Tuition.fees.up.to.date, Scholarship.holder,Target) %>%
  select(where(is.numeric))

# correlation matrix for the selected variables
cor_matrix2 <- cor(important_vars1, use = "complete.obs")

# convert to better suit ggplot
heatmap_data3 <- as.data.frame(as.table(cor_matrix2))

# Plot heatmap
ggplot(heatmap_data3, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  labs(
    title = "Correlation Heatmap of Socio-economic Factors",
    x = "Variables",
    y = "Variables",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# <--------------------------------------------------------------------->
# Demographic Factors

#important variables based on context (grades, target var, others?)
important_vars2 <- clean_data %>%
  select(Gender, International, Displaced, Nacionality, Age.at.enrollment , 
         Marital.status, Target) %>%
  select(where(is.numeric))

cor_matrix2 <- cor(important_vars2, use = "complete.obs")

heatmap_data3 <- as.data.frame(as.table(cor_matrix2))

ggplot(heatmap_data3, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  labs(
    title = "Correlation Heatmap of Demographic Factors",
    x = "Variables",
    y = "Variables",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# <--------------------------------------------------------------------->
# summary of the counts and calculated percentages
data_summary <- clean_data %>%
  group_by(Target) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

ggplot(data_summary, aes(x = "", y = count, fill = Target)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Target Variable") +
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +
  theme_void() +
  scale_fill_manual(values = c("Graduated" = "yellow", "Dropout" = "red", "Enrolled" = "blue"))

# <--------------------------------------------------------------------->

# Plots focused on Dropout rates

tuition_vs_dropout <- data %>%
  group_by(Tuition.fees.up.to.date, Target) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

ggplot(tuition_vs_dropout, aes(x = Target, y = percentage, fill = factor(Tuition.fees.up.to.date))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = paste0(percentage, "%")), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3.5
  ) +
  labs(
    title = "Relationship Between Tuition Payment and Dropout Rate",
    x = "Dropout Status (Target)",
    y = "Percentage",
    fill = "Tuition Fees Up to Date"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("maroon", "dark blue"))

#-----------------------------------------------------

ggplot(clean_data, aes(x = Age.at.enrollment)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Age at enrollment Distribution", x = "Age", y = "Number of Students") +
  theme_minimal()
#---------------------


#Sample data
set.seed(123)
data <- data.frame(
  Age = c(rnorm(500, mean = 20, sd = 1.5), rnorm(200, mean = 25, sd = 5)),
  Dropout = factor(c(rep(0, 500), rep(1, 200)))
)

ggplot(data, aes(x = Age, fill = Dropout)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribution of Age at Enrollment by Dropout Status",
    x = "Age at Enrollment",
    y = "Probability Density",
    fill = "Dropout"
  ) +
  theme_minimal()


#<---------------------------------------------------------------------->
marital_pie_data <- clean_data %>%
  filter(Target == "Dropout") %>%
  count(Marital.status)

marital_pie_data$Marital.status <- factor(marital_pie_data$Marital.status,
                                          levels = c(1, 2, 3, 4, 5, 6),
                                          labels = c("Single", "Married", "Widower", 
                                                     "Divorced", "Facto Union", 
                                                     "Legally Separated"))

pie(marital_pie_data$n, 
    labels = marital_pie_data$Marital.status, 
    main = "Marital Status vs Dropout Rate")

#<---------------------------------------------------------------------->

#academic_variables <- "Admission.grade", 
                    #    "Curricular.units.1st.sem..grade."
                    #    "Curricular.units.2nd.sem..grade."

grouped_data <- clean_data %>%
  filter(Target %in% c("Enrolled", "Dropout"))

t_test_result <- t.test(
  Admission.grade ~ Target, 
  data = grouped_data,
  var.equal = TRUE
)

print(t_test_result)

t_test_result_semester1 <- t.test(
  Curricular.units.1st.sem..grade. ~ Target,  
  data = grouped_data,
  var.equal = TRUE  
)

print(t_test_result_semester1)

t_test_result_semester2 <- t.test(
  Curricular.units.2nd.sem..grade. ~ Target,
  data = grouped_data,
  var.equal = TRUE  
)

print(t_test_result_semester2)

#<--------------------------------------------------------------------->
#economic_variables <- "Tuition.fees.up.to.date", 
                        "Mother.s.occupation"
                        "Father.s.occupation" 

tuition <- t.test(
  Tuition.fees.up.to.date ~ Target,  
  data = grouped_data,
  var.equal = TRUE  
)

print(tuition)

moccup <- t.test(
  Mother.s.occupation ~ Target, 
  data = grouped_data,
  var.equal = TRUE  
)

print(moccup)

foccup <- t.test(
  Father.s.occupation ~ Target,  
  data = grouped_data,
  var.equal = TRUE 
)

print(foccup)

#<--------------------------------------------------------------------->
#demographic_variables <- "Age.at.enrollment"
                         #  "Marital.status"
                           
age <- t.test(
  Age.at.enrollment ~ Target, 
  data = grouped_data,
  var.equal = TRUE  
)

print(age)

marital_status <- t.test(
  Marital.status ~ Target,  
  data = grouped_data,
  var.equal = TRUE  
)

print(marital_status)

#<--------------------------------------------------------------------->






