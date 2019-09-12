# acct-am
Accountability Files

Repository for files used to produce Accountability Files

Below is a brief summary of the purpose of each file in the top level of 2019:
- **absenteeism**: Pulls attendance data and creates absenteeism files
- **absenteeism_match**: My code to produce school, district, and state level files that will be compared with output of other coder
- **act_substitution**: Substituting ACT when appropriate (Junior didn't take a Math EOC)
- **cdf_to_student_level**: Going from the CDF (raw results from vendor) and applying business rules to get to an overall student level achievement files
- **district_accountability**: Applying business rules for District Accountability (Found in section 5.1 of the [protocol](https://www.tn.gov/content/dam/tn/education/accountability/Accountability_Protocol_2019.pdf))
- **district_assessment**: Producing the district level assessment file. This can be found in the State Assessment section of [Data Downloads](https://www.tn.gov/education/data/data-downloads.html)
- **elpa**: Applies business rules for English Language Proficiency Assessment. Outputs student, school, district, and state level ELPA Files
- **enrollment**: Enrollment file to identify students with multiple enrollments in the absenteeism file
- **grad_rate**: school, district, and state level graduation rate files
- **grade_2**: Creating a student level file for grade 2 assessments
- **grade_2_assessment**: District and school level file for grade 2
