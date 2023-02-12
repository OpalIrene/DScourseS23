-- Load the CSV file into a table called 'florida_insurance_sample'
.mode csv
.import florida_insurance_sample.csv florida_insurance

-- (b) Print out the first 10 rows of the data set
SELECT *
  FROM florida_insurance
LIMIT 10;

-- (c) List which counties are in the sample (i.e. list unique values of the county variable)
SELECT DISTINCT county
FROM florida_insurance;

-- (d) Compute the average property appreciation from 2011 to 2012 (i.e. compute the mean of tiv_2012 - tiv_2011)
SELECT AVG(tiv_2012 - tiv_2011) AS avg_appreciation
FROM florida_insurance;

-- (e) Create a frequency table of the construction variable to see what fraction of buildings are made out of wood or some other material
SELECT construction, COUNT(*), COUNT(*) / (SELECT COUNT(*) FROM florida_insurance) * 100 AS percentage
FROM florida_insurance
GROUP BY construction;
