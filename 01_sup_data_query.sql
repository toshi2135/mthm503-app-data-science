SELECT
  cas.accident_index,
  cas.casualty_severity,
  cas.sex_of_casualty,
  cas.age_of_casualty,
  cas.casualty_home_area_type,
  cas.pedestrian_movement,
  cas.casualty_type,

  acc.weather_conditions,
  acc.light_conditions,
  acc.road_surface_conditions,
  acc.pedestrian_crossing_physical_facilities,
  acc.carriageway_hazards,
  acc.urban_or_rural_area,
  acc.obs_date,
  EXTRACT(HOUR FROM acc.obs_date) AS hour_of_day,
  EXTRACT(DOW FROM acc.obs_date) AS day_of_week,
  CASE WHEN EXTRACT(DOW FROM acc.obs_date) IN (0,6) THEN 1 ELSE 0 END AS is_weekend,
  acc.road_type,
  acc.speed_limit_mph,
  acc.junction_detail,

  veh.sex_of_driver,
  veh.age_of_driver,
  veh.vehicle_type,
  veh.vehicle_manoeuvre,
  veh.age_of_vehicle,
  veh.journey_purpose_of_driver,
  veh.driver_home_area_type

FROM stats19_casualties cas

LEFT JOIN stats19_accidents acc
  ON cas.accident_index = acc.accident_index

LEFT JOIN stats19_vehicles veh
  ON cas.accident_index = veh.accident_index
     AND cas.vehicle_reference = veh.vehicle_reference

WHERE cas.casualty_class = 'Pedestrian'
