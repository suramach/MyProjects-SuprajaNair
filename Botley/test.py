import pandas as pd
from collections import defaultdict

# Load Excel files
bdf = pd.read_excel("/Users/sr1tech/Downloads/Datasets/PN/Botley/Beginning-of-Semester Survey (Responses).xlsx") 
edf = pd.read_excel("/Users/sr1tech/Downloads/Datasets/PN/Botley/End-of-Semester Survey (Responses).xlsx") 

# Preview the first few rows    
print("Beginning Survey (bdf):")
print(bdf.head())

print("\nEnding Survey (edf):")
print(edf.head())

# Step 1: Compare q1 responses in beginning and end surveys
q1_comparison = pd.DataFrame({
    'Begin_q1': bdf['q1'],
    'End_q1': edf['q1']
})

# Show only rows where responses are equal
q1_equal = q1_comparison[q1_comparison['Begin_q1'] == q1_comparison['End_q1']]

# Print total matches and preview
print(f"\nStep 1: Number of matching Q1 responses: {len(q1_equal)} out of {len(q1_comparison)}")
print(q1_equal.head())

# Step 2: Count responses in Q4 from beginning survey
q5_counts = bdf['q4'].value_counts(dropna=False)

print("\nStep 2: Q4 Response Counts (BDF)")
print(q5_counts)

# Step 3: Group related roles in Q7 from beginning survey
q6_series = bdf['q6'].dropna().str.lower()  # lowercase for easy matching

# Count Pre-K roles (includes variations like "pre-k", "prek", "pre k")
pre_k_count = q6_series.str.contains(r'\bpre[- ]?k\b', regex=True).sum()

# Count Preschool roles
preschool_count = q6_series.str.contains('preschool').sum()

# Count all other unique roles (not matching Pre-K or Preschool)
other_roles = q6_series[~q6_series.str.contains(r'\bpre[- ]?k\b', regex=True)]
other_roles = other_roles[~other_roles.str.contains('preschool')]
other_counts = other_roles.value_counts()

# Print counts
print("\nStep 3: Grouped Q6 Response Counts (BDF)")
print(f"Pre-K related roles: {pre_k_count}")
print(f"Preschool related roles: {preschool_count}")
print("\nOther Roles:")
print(other_counts)



# Step 4: Count 'yes' in Q7 from beginning survey
yes_count_q7 = bdf['q7'].str.contains('yes', case=False, na=False).sum()

print(f"\nStep 4: Number of 'yes' responses in Q7: {yes_count_q7}")

# Step 5: Count responses in Q8 from beginning survey
q8_counts = bdf['q8'].value_counts(dropna=False)
print("\nStep 5: Q8 Response Counts (BDF)")
print(q8_counts)

# Step 6: Count responses in Q9 from beginning survey
q9_counts = bdf['q9'].value_counts(dropna=False)
print("\nStep 6: Q9 Response Counts (BDF)")
print(q9_counts)


# Step 7: Compare q10_bdf and q4_edf (words added in EDF that weren’t in BDF)
bdf_edf = pd.merge(bdf, edf, on='q1', suffixes=('_bdf', '_edf'))

# Function to find added subjects
def find_added_subjects(row):
    bdf_subjects = {s.strip().lower() for s in str(row['q10_bdf']).split(',') if s.strip()}
    edf_subjects = {s.strip().lower() for s in str(row['q4_edf']).split(',') if s.strip()}
    added = edf_subjects - bdf_subjects
    return ', '.join(sorted(subject.title() for subject in added)) if added else None
print("\nStep 7:")
# Apply and print
bdf_edf['Subjects Added in EDF'] = bdf_edf.apply(find_added_subjects, axis=1)
added_subjects = bdf_edf[['q1', 'Subjects Added in EDF']].dropna()
print(added_subjects)


# Step 8:
# Convert q11 (BDF) and q5 (EDF) to numeric
bdf_edf['q11_bdf'] = pd.to_numeric(bdf_edf['q11_bdf'], errors='coerce')
bdf_edf['q5_edf'] = pd.to_numeric(bdf_edf['q5_edf'], errors='coerce')

# ✅ Now filter and display only the ones that increased
increased_q5 = bdf_edf[bdf_edf['q5_edf'] > bdf_edf['q11_bdf']]
print(f"\n✅ Step 8: {len(increased_q5)} users had increased values in Q5 (EDF) vs Q11 (BDF):")
print(increased_q5[['q1', 'q11_bdf', 'q5_edf']])

# ✅ Now filter and display only the ones that decreased
decreased_q5 = bdf_edf[bdf_edf['q5_edf'] < bdf_edf['q11_bdf']]
print(f"\n✅ Step 8: {len(decreased_q5)} users had decreased values in Q5 (EDF) vs Q11 (BDF):")
print(decreased_q5[['q1', 'q11_bdf', 'q5_edf']])

#Step 9:
# Convert q12 and q6 to numeric
bdf_edf['q12_bdf'] = pd.to_numeric(bdf_edf['q12_bdf'], errors='coerce')
bdf_edf['q6_edf'] = pd.to_numeric(bdf_edf['q6_edf'], errors='coerce')

# ✅ Filter and display participants where Q6 increased over Q12
increased_q6 = bdf_edf[bdf_edf['q6_edf'] > bdf_edf['q12_bdf']]
print(f"\n✅ Step 9: {len(increased_q6)} users had increased values in Q6 (EDF) vs Q12 (BDF):")
print(increased_q6[['q1', 'q12_bdf', 'q6_edf']])

# ✅ Filter and display participants where Q6 decreased over Q12
decreased_q6 = bdf_edf[bdf_edf['q6_edf'] < bdf_edf['q12_bdf']]
print(f"\n✅ Step 9: {len(decreased_q6)} users had increased values in Q6 (EDF) vs Q12 (BDF):")
print(decreased_q6[['q1', 'q12_bdf', 'q6_edf']])

# Step 10: List and count q13 responses from BDF
print("\n✅ Step 10: Q13 Response Counts (BDF):")
print(bdf['q13'].value_counts(dropna=False))

# Step 11: List and count q7 responses from EDF
print("\n✅ Step 11: Q7 Response Counts (EDF):")
print(edf['q7'].value_counts(dropna=False))

# Step 12: List and count q8 to q25 responses from EDF
print("\n✅ Step 12: Q8 to Q25 Response Counts (EDF):")
for i in range(8, 26):
    col = f'q{i}'
    if col in edf.columns:
        print(f"\n{col} Value Counts:")
        print(edf[col].value_counts(dropna=False))

#Step 13: 
selected_cols = ['q27', 'q29', 'q31', 'q33', 'q35', 'q37', 'q42']
print("\n✅ Step 13: Selected Reflection Questions (EDF):")
for col in selected_cols:
    if col in edf.columns:
        print(f"\n{col} Value Counts:")
        print(edf[col].value_counts(dropna=False))


