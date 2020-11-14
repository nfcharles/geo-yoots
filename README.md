# geo-yoots

Tools for working with geometry things on Earth.

Distance Functions are implemented using spherical trigonometry and vector maths.
 - https://en.wikipedia.org/wiki/Spherical_trigonometry
 - https://mathworld.wolfram.com/SphericalCoordinates.html


Area Functions use Shoelace algorithm
 - https://en.wikipedia.org/wiki/Shoelace_formula
 - https://en.wikipedia.org/wiki/Rotation_matrix


## Features

### Distance Functions

Supported objects
1. Point to *point* distance
2. Point to *polyline* distance
3. Point to *circle* distance
4. Point to *polygon* distance

Points are (`latitude`, `longitude`) pairs. Units are in kilometers.

#### Point to point

```clojure
;; p1     -> [12.12345 53.54321] => [lat lon]
;; return -> distance

(geo.sphere.distance/to-point p1 p2)
```

#### Point to polyline

```clojure
;; pt                -> [lat lon]
;; polyline-vertices -> [[lat lon] [lat lon] ... ]
;; return            -> distance

(geo.sphere.distance/to-polyline pt polyline-vertices)
```

#### Point to cirlce

```clojure
;; pt     -> [lat lon]
;; center -> [lat lon]
;; radius -> radius
;; return -> distance

(geo.sphere.distance/to-circle pts center radius)
```

#### Point to polygon

```clojure
;; pt               -> [lat lon]
;; polygon-vertices -> [[lat lon] [lat lon] ... ]
;; return           -> distance

(geo.sphere.distance.signed/to-polygon pt polygon-vertices)
```

#### Distance Boolean Functions

Polyline, circle and polygon structures support distance predicates - i.e.
Are input points within `distance` to geometry.

```clojure
(def limit 1.75) ;; kilometers
(geo.sphere.distance/within-distance-to-polyline? limit pts polyline)
```

### Inclusion Tests

#### Point in polygon

*Convex and Non-Convex Simple Polygons*

```clojure
(geo.sphere.impl.inclusion/point-in-polygon? pt vertices)
```

### Area Functions

Calculate polygon areas using the `sphere.area` package.

```clojure
(def poly1 [[ 1.000000  0.000000]
            [ 0.000000  1.000000]
            [-1.000000  0.000000]
            [ 0.000000 -1.000000]])

(def poly2 [[-15.290669  179.159774]
            [-11.469978  179.626362]
            [ -9.559677 -177.255858]
            [-13.678565 -174.857297]
            [-17.228967 -177.598549]])

(geo.sphere.area/polgyon poly1) ;; 24740.08 KM
(geo.sphere.area/polgyon poly2) ;; 338457.92 KM
```

## License

Copyright © 2020 Navil Charles

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
