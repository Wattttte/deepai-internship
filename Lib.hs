module Lib where

------------------------
--    V E C T O R S   --
------------------------

--Defining Vectors
data Vector = 
    Vector Float Float Float Float deriving Eq

--Show Instance for Vectors
instance Show Vector where
    show (Vector a b c d) = show a ++ "\n" ++ (show b) ++ "\n" ++ (show c) ++ "\n" ++ (show d)

toFloat :: Integer -> Float
toFloat x = fromInteger x :: Float

--Num instance for Vectors
instance Num Vector where
    (+) (Vector a b c d) (Vector x y z w) = Vector (a+x) (b+y) (c+z) (d+w)
    (-) (Vector a b c d) (Vector x y z w) = Vector (a-x) (b-y) (c-z) (d-w)
    (*) (Vector a b c d) (Vector x y z w) = Vector (a*x) (b*y) (c*z) (d*w)
    abs (Vector a b c d) = Vector (abs a) (abs b) (abs c) (abs d)
    signum (Vector a b c d) = Vector (signum a) (signum b) (signum c) (signum d)
    negate (Vector a b c d) = Vector (negate a) (negate b) (negate c) (negate d)
    fromInteger a = Vector (toFloat a) (toFloat a) (toFloat a) (toFloat a)

--Extra functions for Vectors
magnitude :: Vector -> Float
magnitude (Vector a b c d) = sqrt (a^2 + b^2 + c^2 + d^2)

normalize :: Vector -> Vector
normalize (Vector a b c d) = (Vector (a/(magnitude v)) (b/(magnitude v)) (c/(magnitude v)) (d/(magnitude v))) where v = (Vector a b c d)

dotProduct :: Vector -> Vector -> Float
dotProduct (Vector a b c d) (Vector x y z w) = (a*x) + (b*y) + (c*z) + (d*w)


------------------------
--   M A T R I C E S  --
------------------------

--Defining a 4x4 Matrix
data Matrix4x4 =
    Matrix4x4 Vector Vector Vector Vector deriving Eq

--Show instance for 4x4 Matrices
instance Show Matrix4x4 where
    show (Matrix4x4 (Vector a1 b1 c1 d1) (Vector a2 b2 c2 d2) (Vector a3 b3 c3 d3) (Vector a4 b4 c4 d4)) = (show a1) ++ " " ++ (show b1) ++ " " ++ (show c1) ++ " " ++ (show d1) ++ "\n" ++ (show a2) ++ " " ++ (show b2) ++ " " ++ (show c2) ++ " " ++ (show d2) ++ "\n" ++ (show a3) ++ " " ++ (show b3) ++ " " ++ (show c3) ++ " " ++ (show d3) ++ "\n" ++ (show a4) ++ " " ++ (show b4) ++ " " ++ (show c4) ++ " " ++ (show d4)

{-- WIP
instance Num Matrix4x4 where
    (+) (Matrix4x4 a b c d) (Matrix4x4 x y z w) = Matrix4x4 (a+x) (b+y) (c+z) (d+w)
    (-) (Matrix4x4 a b c d) (Matrix4x4 x y z w) = Matrix4x4 (a-x) (b-y) (c-z) (d-w)
    (*) (Matrix4x4 (Vector a1 b1 c1 d1) (Vector a2 b2 c2 d2) (Vector a3 b3 c3 d3) (Vector a4 b4 c4 d4)) (Matrix4x4 (Vector x1 y1 z1 w1) (Vector x2 y2 z2 w2) (Vector x3 y3 z3 w3) (Vector x4 y4 z4 w4)) = 
    abs (Matrix4x4 a b c d) = Matrix4x4 (abs a) (abs b) (abs c) (abs d)
    signum (Matrix4x4 a b c d) = Matrix4x4 (signum a) (signum b) (signum c) (signum d)
    negate (Matrix4x4 a b c d) = Matrix4x4 (negate a) (negate b) (negate c) (negate d)
    fromInteger a = Matrix4x4 (fromInteger a :: Vector) (fromInteger a :: Vector) (fromInteger a :: Vector) (fromInteger a :: Vector)
--}

--Defining a 3x3 Matrix
data Matrix3x3 =
    Matrix3x3 Float Float Float Float Float Float Float Float Float deriving Eq

--Show instance for 3x3 Matrices
instance Show Matrix3x3 where
    show (Matrix3x3 a b c d e f g h i) = show a ++ " " ++ (show b) ++ " " ++ (show c) ++ "\n" ++ (show d) ++ " " ++ (show e) ++ " " ++  (show f) ++ "\n" ++ (show g) ++ " " ++ (show h) ++ " " ++ (show i)

--Rotation Matrices
rotateX :: Float -> Matrix3x3
rotateX a = (Matrix3x3 1 0 0 0 (cos a) (sin(360-a)) 0 (sin a) (cos a))

rotateY :: Float -> Matrix3x3
rotateY a = (Matrix3x3 (cos a) 0 (sin a) 0 1 0 (sin(360-a)) 0 (cos a))

rotateZ :: Float -> Matrix3x3
rotateZ a = (Matrix3x3 (cos a) (sin(360-a)) 0 (sin a) (cos a) 0 0 0 1)

------------------------
--    C O L O R S     --
------------------------

--Defining color
data Color =
    Color Float Float Float deriving Eq

toInt :: Float -> Int
toInt = round

--Num instance for Color
instance Num Color where
    (+) (Color a b c) (Color x y z) = Color (a+x) (b+y) (c+z)
    (-) (Color a b c) (Color x y z) = Color (a-x) (b-y) (c-z)
    (*) (Color a b c) (Color x y z) = Color (a*x) (b*y) (c*z)
    abs (Color a b c) = Color (abs a) (abs b) (abs c)
    signum (Color a b c) = Color (signum a) (signum b) (signum c)
    negate (Color a b c) = Color (negate a) (negate b) (negate c)
    fromInteger a = Color (toFloat a) (toFloat a) (toFloat a)

--Expand and Clamp function to restrain color values
expandAndClampHelp :: Float -> Float
expandAndClampHelp a 
    | (a > 255) = 255
    | (a < 0) = 0
    | otherwise = toFloat (round a)

expandAndClamp :: Color -> Color
expandAndClamp (Color a b c) = (Color (expandAndClampHelp a) (expandAndClampHelp b) (expandAndClampHelp c))

instance Show Color where
    show (Color a b c) = show (toInt (expandAndClampHelp a)) ++ " " ++ show (toInt (expandAndClampHelp b)) ++ " " ++ show (toInt (expandAndClampHelp c))

--Setting up Colors
red = (Color 255 0 0)
orange = (Color 255 127 0)
yellow = (Color 255 255 0)
green = (Color 80 220 35)
blue = (Color 10 80 215)
purple = (Color 90 10 215)
black = (Color 0 0 0)
white = (Color 255 255 255)

--Formatting colors so they can be printed in a ppm file
pxl :: Color -> String
pxl a = show (a) ++ " "