-- Continents with long COVID symptoms with statistically significant p values in ascending order
SELECT Continent, Symptom, `P value` 
FROM Continents c 
WHERE `P value` < 0.05
ORDER BY `P value`;



-- INNER JOIN tables: Countries and Risk factors
-- Find out distinctive clinical symptoms/signs associated with female gender as one of the risk factors in certain countries
SELECT DISTINCT(`Associated with`), `Risk factor`, `Country`
FROM `Risk factors` rf 
INNER JOIN Countries c 
on c.Study = rf.Study 
WHERE `Risk factor` = 'Female sex';


-- Find out distinctive clinical symptoms/signs associated with male gender as one of the risk factors in certain countries
SELECT DISTINCT(`Associated with`), `Risk factor`, `Country`
FROM `Risk factors` rf 
INNER JOIN Countries c 
on c.Study = rf.Study
WHERE `Risk factor` LIKE 'Male%';


-- Find out the associated clinical symptoms/signs with age as one of the risk factors for long COVID in certain countries
SELECT `Category`, `Risk factor`, `Associated with`, Country 
FROM Countries c 
INNER JOIN `Risk factors` rf 
on c.Study = rf.Study 
WHERE Category = 'Age';


-- Find out the associated clinical symptoms/signs with co-morbidities as one of the risk factors for long COVID in certain countries
SELECT `Category`, `Risk factor`, `Associated with`, Country 
FROM Countries c 
INNER JOIN `Risk factors` rf 
on c.Study = rf.Study 
WHERE Category = 'Comorbidities';



-- Change column name Subgroup to Hosp_status in table Hospitalisation
ALTER TABLE Hospitalisation 
RENAME COLUMN Subgroup TO Hosp_status;

-- Check the column name has changed as above
SELECT * FROM Hospitalisation h;



-- All long COVID symptoms, proportions, hospitalisation status and relevant p values
SELECT Symptom, `Proportion% (95% CIs)`, Hosp_status, `P value` 
FROM Hospitalisation h 
WHERE `P value` < 0.05
ORDER BY `P value`;



-- Focus on long COVID symptoms associated with hospitalised population along with relevant proportions and p values 
SELECT Symptom,`Proportion% (95% CIs)`, Hosp_status, `P value`
FROM Hospitalisation h 
WHERE Hosp_status = 'Hospitalised'
ORDER BY `Proportion% (95% CIs)` DESC;



-- CREATE view to store long COVID hospitalisation data for later use e.g. visualisation
CREATE VIEW long_covid_hospitalisation AS
SELECT Symptom,`Proportion% (95% CIs)`, Hosp_status, `P value`
FROM Hospitalisation h
WHERE `P value` < 0.05;



-- Symptoms associated with hospitalised cases from saved view
SELECT Hosp_status, Symptom, `P value`
FROM long_covid_hospitalisation  
WHERE Hosp_status = 'Hospitalised'
ORDER BY `P value`;



-- Symptoms associated with non-hospitalised cases from saved view
SELECT Hosp_status, Symptom, `P value` 
FROM long_covid_hospitalisation 
WHERE Hosp_status = 'Non-hospitalised'
ORDER BY `P value`;



-- Symptoms associated with both hospitalised and non-hospitalised (mixed) cases from saved view
SELECT Hosp_status, Symptom, `P value` 
FROM long_covid_hospitalisation 
WHERE Hosp_status = 'Mixed'
ORDER BY `P value`;



-- Create another view to store dataset on long covid risk factors & associated symptoms/signs for later use
CREATE VIEW long_covid_risk_factors AS
SELECT Country, `Population size`, `Age (years)`, `Risk factor`, `Associated with`, `COVID-19 confirmation method`, `Follow-up time (days)` 
FROM Countries c, `Risk factors` rf
WHERE c.Study = rf.Study;



-- Check out long COVID risk factors with associated symptoms/signs for different countries and popluation sizes from the saved view
SELECT DISTINCT(Country), `Population size`, `Risk factor`, `Associated with`
FROM long_covid_risk_factors
ORDER BY `Population size` DESC;