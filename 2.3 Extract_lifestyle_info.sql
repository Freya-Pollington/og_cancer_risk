-- creating a table for alcohol and bmi as they require disease codes, but need a 20 year look back

-- first creating a table of all patients with weight and height measurements in cprd
DROP TABLE IF EXISTS freya.height_weight_ent;
CREATE TABLE freya.height_weight_ent
SELECT cal.data1,et.description,cal.adid
FROM 18_299_Lyratzopoulos_e2.cprd_additional cal 
INNER JOIN lookup_tables.lookup_entity et on et.enttype=cal.enttype
AND (cal.enttype = 13 OR cal.enttype = 14);

CREATE INDEX adid ON freya.height_weight_ent(adid);

-- CREATE INDEX e_patid ON freya.cprd_clinical_30_yr(e_patid);
CREATE INDEX e_patid ON freya.cohortv1_8(e_patid);

CREATE TABLE freya.cprd_clinical_symptom_cohort
SELECT cl.e_patid,cl.eventdate,cl.adid
FROM freya.cohortv1_8 v1
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_clinical cl on cl.e_patid = v1.e_patid
AND cl.eventdate >= makedate(1987, 1) AND cl.eventdate <= makedate(2017, 365);

CREATE INDEX adid ON freya.cprd_clinical_symptom_cohort(adid);

DROP TABLE IF EXISTS freya.height_weight_symp;
CREATE TABLE freya.height_weight_symp
SELECT cl.e_patid, cl.eventdate, et.data1, et.description
FROM freya.cprd_clinical_symptom_cohort cl
INNER JOIN freya.height_weight_ent et on et.adid = cl.adid
; 

CREATE INDEX e_patid ON freya.random_sample_elig(e_patid);

CREATE TABLE freya.cprd_clinical_rand_cohort
SELECT cl.e_patid,cl.eventdate,cl.adid
FROM freya.random_sample_elig v1
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_clinical cl on cl.e_patid = v1.e_patid
AND cl.eventdate >= makedate(1987, 1) AND cl.eventdate <= makedate(2017, 365);

CREATE INDEX adid ON freya.cprd_clinical_rand_cohort(adid);

DROP TABLE IF EXISTS freya.height_weight_rand;
CREATE TABLE freya.height_weight_rand
SELECT cl.e_patid, cl.eventdate, et.data1, et.description
FROM freya.cprd_clinical_rand_cohort cl
INNER JOIN freya.height_weight_ent et on et.adid = cl.adid
; 

DROP TABLE IF EXISTS freya.obesity;
CREATE TABLE freya.obesity
SELECT cl.e_patid, cl.eventdate, cl.medcode, cl.d_cprdclin_key,cal.disease_name, cal.disease_number
FROM 18_299_Lyratzopoulos_e2.cprd_clinical cl
INNER JOIN lookup_tables.lookup_cal_cprd cal on cl.medcode = cal.medcode
WHERE cl.eventdate >= makedate(1987, 1) AND cl.eventdate <= makedate(2017, 365)
AND cal.disease_number = 159 -- obesity
; 

-- ----------------------------------------------------------------------------------

DROP TABLE IF EXISTS freya.alc_problems;
CREATE TABLE freya.alc_problems
SELECT cl.e_patid, cl.eventdate, cl.medcode, cl.d_cprdclin_key,cal.disease_name, cal.disease_number
FROM 18_299_Lyratzopoulos_e2.cprd_clinical cl
INNER JOIN lookup_tables.lookup_cal_cprd cal on cl.medcode = cal.medcode
WHERE cl.eventdate >= makedate(1987, 1) AND cl.eventdate <= makedate(2017, 365)
AND cal.disease_number = 9 -- alc_problems
; 
-- ----------------------------------
DROP TABLE IF EXISTS freya.alcohol_cprd_clin;
CREATE TABLE freya.alcohol_cprd_clin
SELECT cl.e_patid, cl.eventdate, cl.adid
FROM 18_299_Lyratzopoulos_e2.cprd_clinical cl
WHERE cl.eventdate >= makedate(1997, 1) AND cl.eventdate <= makedate(2017, 365)
; 

DROP TABLE IF EXISTS freya.alcohol_add;
CREATE TABLE freya.alcohol_add
SELECT cal.e_patid, cal.data1, cal.adid
FROM 18_299_Lyratzopoulos_e2.cprd_additional cal
WHERE (cal.enttype = 5)
; 

create index adid on freya.alcohol_cprd_clin(adid);
create index adid on freya.alcohol_add(adid);

create index e_patid on freya.alcohol_add(e_patid);
create index e_patid on freya.alcohol_cprd_clin(e_patid);

DROP TABLE IF EXISTS freya.alcohol_join;
CREATE TABLE freya.alcohol_join
SELECT cl.e_patid, cl.eventdate, ad.data1
FROM freya.alcohol_add ad
INNER JOIN freya.alcohol_cprd_clin cl on cl.adid = ad.adid AND cl.e_patid = ad.e_patid
;

DROP TABLE IF EXISTS freya.alcohol_join_dis;
CREATE TABLE freya.alcohol_join_dis
SELECT DISTINCT *
FROM freya.alcohol_join;

-- Go to R script 2.3.1