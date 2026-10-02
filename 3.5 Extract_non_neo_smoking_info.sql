#Extracting smoking information for the non-neoplastic cohorts
CREATE INDEX e_patid on freya.disease_index_all_pot_elig (e_patid);

DROP TABLE IF EXISTS freya.smoking_cohort_non_neo_all;

CREATE TABLE freya.smoking_cohort_non_neo_all(
SELECT b.e_patid, a.eventdate AS smok_date, m.smokingcat, m.`Clinical term`
FROM freya.disease_index_all_pot_elig b
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_clinical a ON a.e_patid=b.e_patid
INNER JOIN freya.smoking_codelist m ON a.medcode=m.medcode
);
 
DROP TABLE IF EXISTS freya.smoking_cohort_gord_extra;

CREATE TABLE freya.smoking_cohort_gord_extra2(
SELECT b.e_patid, a.eventdate AS smok_date, m.smokingcat, m.`Clinical term`
FROM freya.disease_index_gord_all_flags b
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_clinical a ON a.e_patid=b.e_patid
INNER JOIN freya.smoking_codelist m ON a.medcode=m.medcode
);

DROP TABLE IF EXISTS freya.smoking_cohort_non_neo_all_dist;

CREATE TABLE freya.smoking_cohort_non_neo_all_dist
SELECT DISTINCT *
FROM freya.smoking_cohort_non_neo_all
;

-- go to R script 3.5.1