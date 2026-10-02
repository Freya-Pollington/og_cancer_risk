-- using the table of main symptoms with vague ones joined in before splitting them out
-- finding the history of icd 10 codes for each patient

DROP TABLE IF EXISTS freya.elixhauser_codes_non_neo_all;

CREATE TABLE freya.elixhauser_codes_non_neo_all
SELECT a.e_patid, a.disease_name,a.eventdate,b.icd
FROM freya.disease_index_all_pot_elig a 
LEFT JOIN 18_299_Lyratzopoulos_e2.hes_apc_diagnosis_hosp b on b.e_patid = a.e_patid
WHERE b.discharged >= date_sub(a.eventdate,interval 20 year)
AND b.discharged <= a.eventdate
;

-- using the table of main symptoms with vague ones joined in before splitting them out
-- finding the history of icd 10 codes for each patient
DROP TABLE IF EXISTS freya.elixhauser_codes_non_neo_hes;

CREATE TABLE freya.elixhauser_codes_non_neo_hes
SELECT a.e_patid, a.disease_name, a.eventdate,b.icd
FROM freya.disease_index_hes_pot_elig_flag a 
LEFT JOIN 18_299_Lyratzopoulos_e2.hes_apc_diagnosis_hosp b on b.e_patid = a.e_patid
WHERE b.discharged >= date_sub(a.eventdate,interval 20 year)
AND b.discharged <= a.eventdate
AND a.pot_elig = 1
;

-- Union the two dataframes
DROP TABLE IF EXISTS freya.elixhauser_codes_non_neo_all;

CREATE TABLE freya.elixhauser_codes_non_neo_all
SELECT * FROM freya.elixhauser_codes_non_neo_hes
UNION
SELECT * FROM freya.elixhauser_codes_non_neo_cprd
;
-- go to R script 3.6.1