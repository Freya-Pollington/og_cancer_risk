-- Extract 20 years of smoking history
DROP TABLE IF EXISTS freya.smoking_pot_elig;

CREATE INDEX e_patid on freya.cohortv1_1_1(e_patid);

CREATE TABLE freya.smoking_pot_elig(
SELECT b.e_patid, a.eventdate AS smok_date, m.smokingcat
FROM freya.cohortv1_1_1 b
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_clinical a ON a.e_patid=b.e_patid
INNER JOIN freya.smoking_codelist m ON a.medcode=m.medcodeaaa
WHERE a.eventdate <= '2017-12-31' AND a.eventdate >= '1992-01-01'
);

DROP TABLE IF EXISTS freya.smoking_pot_elig_dist;

CREATE TABLE freya.smoking_pot_elig_dist
SELECT DISTINCT *
FROM freya.smoking_pot_elig
;

-- Go to R script 2.2