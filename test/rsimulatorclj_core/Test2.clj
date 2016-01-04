(fn [map]
  (update-in map [:simulatorResponse] assoc :response (str "<xml>" (.trim (:response (:simulatorResponse map))) "</xml>")))
