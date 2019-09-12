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
- **press_release**: Creating plots for press release of 2019 assessment results
- **ready_grad**: Creating the school, district, and state level Ready Graduate files
- **reward_schools_with_core**: Creating list of Reward schools with the CORE region included
- **school_accountability**: Producing school accountability with business rules described in Section 6 of the [protocol](https://www.tn.gov/content/dam/tn/education/accountability/Accountability_Protocol_2019.pdf)
- **school_assessment**: School level assessment file. This can be seen in the State Assessment section of [Data Downloads](https://www.tn.gov/education/data/data-downloads.html)
- **school_designations**: Applying business rules to assign designations to the appropriate schools. Designations can be found in the [2019 School Accountability website] (https://www.tn.gov/content/tn/education/data/accountability/2019-school-accountability.html)
- **school_information_pull**: Querying the school directory database to identify closed, alternate, new, or SPED schools
- **state_assessment**: Proudcing the state level assessment file which can be seen in the State Assessment section of [Data Downloads](https://www.tn.gov/education/data/data-downloads.html)
- **zip_split_files**: Code to zip together the split files in case it was needed
