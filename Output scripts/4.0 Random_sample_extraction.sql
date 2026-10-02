-- a script to extract data from the 1 million random sample to use as a baseline
DROP TABLE IF EXISTS freya.random_sample;
CREATE TABLE freya.random_sample
SELECT cl.e_patid, cl.eventdate, cl.medcode
FROM 18_299_Lyratzopoulos_e2.cprd_clinical cl
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_random_sample rs on rs.e_patid = cl.e_patid
WHERE cl.eventdate >= "2005-01-01" AND cl.eventdate <= "2017-12-31";

-- changing to not bring in the medcodes at this stage
DROP TABLE IF EXISTS freya.random_sample_1_v2;
CREATE TABLE freya.random_sample_1_v2
SELECT v0.e_patid,v0.eventdate, pr.lcd, pr.uts, pt.deathdate, pt.yob, pt.mob, pt.crd, pt.gender, pt.tod
FROM freya.random_sample v0
LEFT JOIN 18_299_Lyratzopoulos_e2.cprd_linkage_eligibility_gold eg ON v0.e_patid = eg.e_patid
LEFT JOIN 18_299_Lyratzopoulos_e2.cprd_practice pr on eg.e_pracid = pr.e_pracid
LEFT JOIN 18_299_Lyratzopoulos_e2.cprd_patient pt on pt.e_patid = v0.e_patid;

DROP TABLE IF EXISTS freya.random_sample_1_v2_dist;
CREATE TABLE freya.random_sample_1_v2_dist
SELECT DISTINCT *
FROM freya.random_sample_1_v2;

-- check the patients in the patient table and left join onto cprd clinical to check which patients don't have a record, would be good to count

DROP TABLE IF EXISTS freya.random_sample_2_v2;
CREATE TABLE freya.random_sample_2_v2
SELECT v2.*, crt.diagnosisdatebest, crt.site_icd10_o2, crt.stage_best, cs.cancer_site_desc, cs.cancer_group_desc
FROM freya.random_sample_1_v2 v2
LEFT JOIN 18_299_Lyratzopoulos_e2.cancer_registration_tumour crt on crt.e_patid = v2.e_patid
LEFT JOIN lookup_tables.lookup_cancersite cs on cs.icd10_4dig = crt.site_icd10_o2;

DROP TABLE IF EXISTS freya.random_sample_2_v2_dist;
CREATE TABLE freya.random_sample_2_v2_dist
SELECT DISTINCT *
FROM freya.random_sample_2_v2;

-- GOING TO R script 4.1
