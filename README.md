# geo-yoots

Tools for working with geometry things

Functions are implemented using spherical trigonometry: https://en.wikipedia.org/wiki/Spherical_trigonometry

## Structures

### Sphere

Supported features
1. Point to point distance
2. Point to polyline distance
3. Point to circle distance
4. Point to polygon distance

Points are `latitude` and `longitude` pairs. Input/Output units are in kilometers.

#### Point to point

```clojure
;; p1     -> [12.12345 53.54321] => [lat lon]
;; return -> [distance ...]

(geo.sphere.distance/to-point p1 p2)
```

#### Point to polyline

```clojure
;; pts               -> [[lat lon] ...]
;; polyline-vertices -> [[lat lon] [lat lon] ... ]
;; return            -> [distance ...]

(geo.sphere.distance/to-polyline pts polyline-vertices)
```

#### Point to cirlce

```clojure
;; pts    -> [[lat lon] ...]
;; center -> [lat lon]
;; radius -> cirlce radius
;; return -> [distance ...]

(geo.sphere.distance/to-circle pts center radius)
```

#### Point to polygon

```clojure
;; pts               -> [[lat lon] ...]
;; polyline-vertices -> [[lat lon] [lat lon] ... ]
;; return            -> [distance ...]

(geo.sphere.distance/to-polygon pts polygon-vertices)
```

#### Distance Boolean Functions

Polyline, circle and polygon structures support distance predicates - i.e.
Are input points within `distance` to geometry.

```clojure
(def limit 1.75) ;; kilometers
(geo.sphere.distance/within-distance-to-polyline? limit pts polyline)
```


## License

Copyright © 2020 Navil Charles

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
