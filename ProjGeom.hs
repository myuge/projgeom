module ProjGeom

data Element a = Space|Plane a |Line a|Point a|Nothing

Space :: uniqe and constant

Plane a ::

Line a ::

Point a ::

Nothing :: uniqe and constant


dimension :: Element -> Integer
dimension e = case e of
  Space -> 3
  Plane -> 2
  Line  -> 1
  Point -> 0
  Main.Nothing -> -1

meet :: Element a -> Element a -> Element a
meet _ Nothing = Nothing
meet Space (Element a) = Element a
meet (Plane a) (Plane b) = Line c
  where c = f1 a b
meet (Plane a) (Line b)
  | included (Line b) (Plane a)  = Line b
  | otherwise = Point c
  where c = f2 a b
meet (Plane a) (Point b)
  | included (Point b) (Plane a) = Point b
  | otherwise = Nothing
meet (Line a) (Line b)
  | included (Line b) (Line a)  = Line a
  | inaplane (Line a) (Line b)  = Nothing
  | otherwise = Point c
  where c = f3 a b
meet (Line a) (Point b)
  | included (Point b) (Line a) = Point b
  | otherwise = Nothing
meet (Point a) (Point b)
  | included (Point b) (Point a) = Point b
  | otherwise = Nothing
meet (Element a) (Element b) = (Element b) (Element a)

join :: Element a -> Element a -> Element a
join (Element a) Nothing = Element a
join Space (Element a) = Space
join (Plane a) (Plane b)
  | included (Plane b) (Plane a) = Plane a
  | otherwise = Space
join (Plane a) (Line b)
  | included (Line b) (Plane a)  = Plane a
  | otherwise = Space
join (Line a) (Line b)
  | included (Line b) (Line a)  = Line a
  | inaplane (Line a) (Line b)  = Plane c
  where c = g1 a b
  | otherwise = Space
join (Line a) (Point b)
  | included (Point b) (Line a) = Line a
  | otherwise = Plane c
  where c = g2 a b
join (Point a) (Point b)
  | included (Point b) (Point a) = Point a
  | otherwise = Line c
  where c = g3 a b
join (Element a) (Element b) = (Element b) (Element a)



--euclidize :: Element -> TODO

-- representation of Elements by homogenious coordinates
Point (x, y, z, w)
Line
Plane
