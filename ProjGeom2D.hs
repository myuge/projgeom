module ProjGeom2D
where

type HomogeneousCoordinate = (Double,Double,Double)

type LineCoefficient = (Double,Double,Double)

data Element = Plane | Line LineCoefficient | Point HomogeneousCoordinate | Nothing

data Line = Line LineCoefficient

data Point = Point HomogeneousCoordinate

dimension :: Element a -> Integer
dimension e = case e of
  Plane -> 2
  Line _  -> 1
  Point _ -> 0
  ProjGeom2D.Nothing -> -1

meet :: Element -> Element -> Element
meet _ ProjGeom2D.Nothing = ProjGeom2D.Nothing
meet Plane e = e
meet (Line a) (Line b)
  | included (Line b) (Line a)  = Line a
  | otherwise = incident (Line b) (Line a)
meet (Line a) (Point b)
  | included (Point b) (Line a) = Point b
  | otherwise = ProjGeom2D.Nothing
meet (Point a) (Point b)
  | included (Point b) (Point a) = Point a
  | otherwise = ProjGeom2D.Nothing
meet e1 e2 = meet e2 e1

join :: Element -> Element -> Element
join e ProjGeom2D.Nothing = e
join Plane _ = Plane
join (Line a) (Line b)
  | included (Line b) (Line a)  = Line a
  | otherwise  = Plane
join (Line a) (Point b)
  | included (Point b) (Line a) = Line a
  | otherwise = Plane
join (Point a) (Point b)
  | included (Point b) (Point a) = Point a
  | otherwise = aLine (Point a) (Point b)
join e1 e2 = join e2 e1

precision :: Double
precision = 0.0001

included :: Element -> Element -> Bool
included _ Plane = True
included Plane _ = False
included Nothing _ = True
include _ Nothig = False
included (Line a) (Line b) = ((Line a) ProjGeom2D.== (Line b))
included (Line _) (Point _) = False
included (Point (x,y,z)) (Line (a,b,c)) = abs(a*x + b*y + c*z) < precision*(sqrt(a^2+b^2+c^2)*sqrt(x^2+y^2+z^2))
included (Point (x1,y1,z1)) (Point (x2,y2,z2)) = False --todo: implement

(==) (Line (a1,b1,c1)) (Line (a2,b2,c2)) =
  abs(a1*b2-a1*b1) < precision*(abs(a1)*abs(b2)+abs(a2)*abs(b1))/2.0
  && abs(c1*a2-c2*a1) < precision*(abs(c1)*abs(a2)+ abs(c2)*abs(a1))/2.0

-- a symmetricity between point and line.

incident :: Line -> Line -> Point
incident (Line (a1,b1,c1)) (Line (a2,b2,c2)) = Point ((b1*c2-b2*c1),(c1*a2-c2*a1),(a1*b2-b1*a2))

aLine :: Point -> Point -> Line
aLine (Point (x1,y1,z1)) (Point (x2,y2,z2)) = Line ((y1*z2-y2*z1),(z1*x2-z2*x1),(x1*y2-x2*y1))


-- perspectivity

-- projectivity
