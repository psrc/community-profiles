# Notes about symbols in the data
symbol_notes = ['Explanation of Symbols:',
                '1. An "**" entry in the margin of error column indicates that either no sample observations or too few sample observations were available to compute a standard error and thus the margin of error. A statistical test is not appropriate.',
                '2. An "-" entry in the estimate column indicates that either no sample observations or too few sample observations were available to compute an estimate, or a ratio of medians cannot be calculated because one or both of the median estimates falls in the lowest interval or upper interval of an open-ended distribution.',
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
                     'While the 2012 American Community Survey (ACS) data generally reflect the December 2009 Office of Management and Budget (OMB) definitions of metropolitan and micropolitan statistical areas; in certain instances the names, codes, and boundaries of the principal cities shown in ACS tables may differ from the OMB definitions due to differences in the effective dates of the geographic entities.',
                     '',
                     'Estimates of urban and rural population, housing units, and characteristics reflect boundaries of urban areas defined based on Census 2010 data. As a result, data for urban and rural areas from the ACS do not necessarily reflect the results of ongoing urbanization.',
                     '',
					 'Supporting documentation on code lists, subject definitions, data accuracy, and statistical testing can be found on the American Community Survey website in the Data and Documentation section.',
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
              'Data for year of entry of the native population reflect the year of entry into the U.S. by people who were born in Puerto Rico, U.S. Island Areas or born outside the U.S. to a U.S. citizen parent and who subsequently moved to the U.S.',
              '',
              'Fertility data are not available for certain geographic areas due to problems with data collection. See Errata Note #92 for details.',
              '',
              'The Census Bureau introduced a new set of disability questions in the 2008 ACS questionnaire. Accordingly, comparisons of disability data '+
              'from 2008 or later with data from prior years are not recommended. For more information on these questions and their evaluation in the 2006 ACS Content Test, '+
              'see the Evaluation Report Covering Disability.',
			'']

# Notes specific to Data Profile 3	
dp03_notes = ['Notes for DP03: SELECTED ECONOMIC CHARACTERISTICS',
              'Employment and unemployment estimates may vary from the official labor force data released by the Bureau of Labor Statistics because of '+
              'differences in survey design and data collection. For guidance on differences in employment and unemployment estimates from different sources go '+
              'to Labor Force Guidance.', 
              '',
              'Workers include members of the Armed Forces and civilians who were at work last week.',
              '',
              'Industry codes are 4-digit codes and are based on the North American Industry Classification System 2007. The Industry categories adhere to the '+
              'guidelines issued in Clarification Memorandum No. 2, "NAICS Alternate Aggregation Structure for Use By U.S. Statistical Agencies," issued by the '+
              'Office of Management and Budget.',
              '',
              'Occupation codes are 4-digit codes and are based on Standard Occupational Classification 2010.',
              '',
			  'The health insurance coverage category names were modified in 2010. See ACS Health Insurance Definitions for a list of the insurance type definitions.',
			  '',
			  'The Census Bureau introduced an improved sequence of labor force questions in the 2008 ACS questionnaire. Accordingly, we recommend using caution when '+
			  'making labor force data comparisons from 2008 or later with data from prior years. For more information on these questions and their evaluation in the 2006 ACS '+
			  'Content Test, see the "Evaluation Report Covering Employment Status" at http://www.census.gov/acs/www/Downloads/methodology/content_test/P6a_Employment_Status.pdf, '+
			  'and the "Evaluation Report Covering Weeks Worked" at http://www.census.gov/acs/www/Downloads/methodology/content_test/P6b_Weeks_Worked_Final_Report.pdf. Additional '+
			  'information can also be found at http://www.census.gov/people/laborforce/.',			  
              '']

# Notes specific to Data Profile 4	
dp04_notes = ['Notes for DP04: SELECTED HOUSING CHARACTERISTICS',
              'The median gross rent excludes no cash renters.',
			  '',
			  'Telephone service data are not available for certain geographic areas due to problems with data collection. See Errata Note #93 for details.',
			  '',
			  'Median calculations for base table sourcing VAL, MHC, SMOC, and TAX should exclude zero values.',
			  '',
			  'In prior years, the universe included all renter-occupied units. It is now restricted to include only those units where GRAPI is computed, that is, gross rent and household '+
			  'Income are valid values.',
			  '',
			  'In prior years, the universe included all owner-occupied units without a mortgage. It is now restricted to include only those units where SMOCAPI is computed, that is, '+
			  'SMOC and household income are valid values.',
			  '',
			  'In prior years, the universe included all owner-occupied units with a mortgage. It is now restricted to include only those units where SMOCAPI is computed, that is, '+
			  'SMOC and household income are valid values.',
              '']

# Notes specific to Data Profile 5
dp05_notes = ['Notes for DP05: ACS DEMOGRAPHIC AND HOUSING ESTIMATES',
              'For more information on understanding race and Hispanic origin data, please see the Census 2010 Brief entitled, Overview of Race and Hispanic Origin: 2010, issued March 2011.',
			  '',
			  'The ACS questions on Hispanic origin and race were revised in 2008 to make them consistent with the Census 2010 question wording. Any changes in estimates for 2008 and beyond may '+
			  'be due to demographic changes, as well as factors including questionnaire changes, differences in ACS population controls, and methodological differences in the population estimates, '+
			  'and therefore should be used with caution. For a summary of questionnaire changes see http://www.census.gov/acs/www/methodology/questionnaire_changes/. For more information about changes '+
			  'in the estimates see http://www.census.gov/population/hispanic/files/acs08researchnote.pdf.',
              '']

