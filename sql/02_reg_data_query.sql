SELECT
  fr.financial_year,
  fr.extrication,
  fr.n_casualties,
  fr.sex,
  fr.age_band,
  s.casualty_severity,
  s.number_of_stat19_reported_casualties,
  ROUND(CAST(fr.n_casualties AS numeric) / NULLIF(s.number_of_stat19_reported_casualties, 0), 4) AS extrication_rate
FROM fire_rescue_extrication_casualties AS fr
LEFT JOIN stats19_by_financial_year AS s
  ON fr.financial_year = s.financial_year
WHERE fr.extrication IS NOT NULL
  AND fr.sex IS NOT NULL
  AND fr.age_band IS NOT NULL
