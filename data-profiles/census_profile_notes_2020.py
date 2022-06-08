# Notes about symbols in the data
symbol_notes = ['Explanation of Symbols:',
				'1. An "**" entry in the margin of error column indicates that either no sample observations or too few sample observations were available to compute a standard error and thus the margin of error. A statistical test is not appropriate.',
				'2. An "-" entry in the estimate column indicates that either no sample observations or too few sample observations were available to compute an estimate, or a ratio of medians cannot be calculated because one or both of the median estimates falls in the lowest interval or upper interval of an open-ended distribution, '+
				'or the margin of error associated with a median was larger than the median itself.',
				'3. An "-" following a median estimate means the median falls in the lowest interval of an open-ended distribution.',
				'4. An "+" following a median estimate means the median falls in the upper interval of an open-ended distribution.',
				'5. An "***" entry in the margin of error column indicates that the median falls in the lowest interval or upper interval of an open-ended distribution. A statistical test is not appropriate.',
				'6. An "*****" entry in the margin of error column indicates that the estimate is controlled. A statistical test for sampling variability is not appropriate.',
				'7. An "N" entry in the estimate and margin of error columns indicates that data for this geographic area cannot be displayed because the number of sample cases is too small.',
				'8. An "(X)" means that the estimate is not applicable or not available.',
				'']

# Notes generic to all profiles			
all_profile_notes = ['Notes applicable to all Census Data Profiles:',
                     'Although the American Community Survey (ACS) produces population, demographic and housing unit estimates, it is the Census Bureaus Population Estimates Program that produces and disseminates the official estimates of the population for the nation, states, counties, cities, and towns and estimates of housing units for states and counties.',
                     '',
                     'Data are based on a sample and are subject to sampling variability. The degree of uncertainty for an estimate arising from sampling variability is represented through the use of a margin of error. The value shown here is the 90 percent margin of error. '+
                     'The margin of error can be interpreted roughly as providing a 90 percent probability that the interval defined by the estimate minus the margin of error and the estimate plus the margin of error (the lower and upper confidence bounds) contains the true value. In addition to '+
                     'sampling variability, the ACS estimates are subject to nonsampling error (for a discussion of nonsampling variability, see Accuracy of the Data). The effect of nonsampling error is not represented in these tables.',
                     '',
                     'While the 2018 American Community Survey (ACS) data generally reflect the July 2015 Office of Management and Budget (OMB) delineations of metropolitan and micropolitan statistical areas, in certain instances the names, codes, and boundaries of the principal cities shown in ACS tables may differ from the OMB delineations due to differences in the effective dates of the geographic entities.',
                     '',
                     'Estimates of urban and rural population, housing units, and characteristics reflect boundaries of urban areas defined based on Census 2010 data. As a result, data for urban and rural areas from the ACS do not necessarily reflect the results of ongoing urbanization.',
                     '',
					 'Supporting documentation on code lists, subject definitions, data accuracy, and statistical testing can be found on the American Community Survey website in the Technical Documentation section.',
					 '',
					 'Sample size and data quality measures (including coverage rates, allocation rates, and response rates) can be found on the American Community Survey website in the Methodology section.',
					 '']

# Notes specific to Data Profile 2	
dp02_notes = ['Notes for DP02: SELECTED SOCIAL CHARACTERISTICS IN THE UNITED STATES',
              'Ancestry listed in this table refers to the total number of people who responded with a particular ancestry; for example, the '+
              'estimate given for Russian represents the number of people who listed Russian as either their first or second ancestry. This table lists only '+
              'the largest ancestry groups; see the Detailed Tables for more categories. Race and Hispanic origin groups are not included in this table because '+
              'official data for those groups come from the Race and Hispanic origin questions rather than the ancestry question (see Demographic Table).',
              '',
              'Data for year of entry of the native population reflect the year of entry into the U.S. by people who were born in Puerto Rico, U.S. Island Areas or '+
			  'born outside the U.S. to a U.S. citizen parent and who subsequently moved to the U.S.',
              '',
              'Methodological changes to citizenship edits may have affected citizenship data for those born in American Samoa. Users should be '+
              'aware of these changes when using 2018 data or multi-year data containing data from 2018. For more information, see:  '+
              'American Samoa Citizenship User Note.',
              '',
              'The Census Bureau introduced a new set of disability questions in the 2008 ACS questionnaire. Accordingly, comparisons of disability data '+
              'from 2008 or later with data from prior years are not recommended. For more information on these questions and their evaluation in the 2006 ACS Content Test, '+
              'see the Evaluation Report Covering Disability.',
              '',
              'Data about computer and Internet use were collected by asking respondents to select "Yes" or "No" to each type of computer '+
              'and each type of Internet subscription. Therefore, respondents were able to select more than one type of computer and more '+
              'than one type of Internet subscription.',
              '',
              'The category "with a broadband Internet subscription" refers to those who said "Yes" to at least one of the following types of Internet subscriptions: '+
              'Broadband such as cable, fiber optic, or DSL; a cellular data plan; satellite; or a fixed wireless subscription.',              
              '',
			  'An Internet "subscription" refers to a type of service that someone pays for to access the Internet such as a cellular data plan, broadband such as cable, '+
			  'fiber optic or DSL, or other type of service. This will normally refer to a service that someone is billed for directly for Internet alone or sometimes '+
			  'as part of a bundle.',
			  '',
              '"With a computer" includes those who said "Yes" to at least one of the following types of computers: Desktop or laptop; smartphone; tablet or other portable '+
			  'wireless computer; or some other type of computer.',
			  '',
			  'Caution should be used when comparing data for computer and Internet use before and after 2016. Changes in 2016 to the questions involving the wording as well '+
			  'as the response options resulted in changed response patterns in the data. Most noticeable are increases in overall computer ownership or use, the total of '+
			  'Internet subscriptions, satellite subscriptions, and cellular data plans for a smartphone or other mobile device. For more detailed information about these '+
			  'changes, see the 2016 American Community Survey Content Test Report for Computer and Internet Use located at '+
			  'https://www.census.gov/programs-surveys/acs/methodology/content-test.htm or the user note regarding changes in the 2016 questions located at '+
			  'https://www.census.gov/programs-surveys/acs/technical-documentation/user-notes.html .',
			  '']

# Notes specific to Data Profile 3	
dp03_notes = ['Notes for DP03: SELECTED ECONOMIC CHARACTERISTICS',
              'Employment and unemployment estimates may vary from the official labor force data released by the Bureau of Labor Statistics because of '+
              'differences in survey design and data collection. For guidance on differences in employment and unemployment estimates from different sources go '+
              'to Labor Force Guidance.', 
              '',
              'Workers include members of the Armed Forces and civilians who were at work last week.',
              '',
              'Industry codes are 4-digit codes and are based on the North American Industry Classification System 2012. The Industry categories adhere to the '+
              'guidelines issued in Clarification Memorandum No. 2, "NAICS Alternate Aggregation Structure for Use By U.S. Statistical Agencies," issued by the '+
              'Office of Management and Budget.',
              '',
              'Occupation codes are 4-digit codes and are based on Standard Occupational Classification 2018.',
              '',
              'Logical coverage edits applying a rules-based assignment of Medicaid, Medicare and military health coverage were added as of 2009 -- please see '+
              'https://www.census.gov/library/working-papers/2010/demo/coverage_edits_final.html for more details. The 2008 data table in American FactFinder '+
              'does not incorporate these edits. Therefore, the estimates that appear in these tables are not comparable to the estimates in the 2009 and later '+
              'tables. Select geographies of 2008 data comparable to the 2009 and later tables are available at https://www.census.gov/data/tables/timeseries/'+
              'acs/1-year-re-run-health-insurance.html. The health insurance coverage category names were modified in 2010. See '+
              'https://www.census.gov/topics/health/health-insurance/about/glossary.html#par_textimage_18 for a list of the insurance type definitions.',              
              '',
			  'Beginning in 2017, selected variable categories were updated, including age-categories, income-to-poverty ratio (IPR) categories, and the age universe '+
			  'for certain employment and education variables. See user note entitled "Health Insurance Table Updates" for further details.',
			  '']

# Notes specific to Data Profile 4	
dp04_notes = ['Notes for DP04: SELECTED HOUSING CHARACTERISTICS',
              'Households not paying cash rent are excluded from the calculation of median gross rent.', 
              '',
              'Telephone service data are not available for certain geographic areas due to problems with data collection of this question that occurred in 2015 and '+
              '2016. Both ACS 1-year and ACS 5-year files were affected. It may take several years in the ACS 5-year files until the estimates are available for the '+
              'geographic areas affected.',
              '']

# Notes specific to Data Profile 5
dp05_notes = ['Notes for DP05: ACS DEMOGRAPHIC AND HOUSING ESTIMATES',
              'For more information on understanding race and Hispanic origin data, please see the Census 2010 Brief entitled, Overview of Race and Hispanic Origin: 2010, issued March 2011. (pdf format)', 
              '']


   
