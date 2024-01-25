type Ray       = Float
type Triangle  = Float


-- intro

--------------------------------------------------------------

nearest :: Ray -> [Triangle] -> Float
nearest ray = fold min 1e30 . map (intersect ray)

--------------------------------------------------------------

data Primitive = Triangle _ | Sphere _

nearest :: Ray -> [Primitive] -> Float
nearest ray = fold min 1e30 . map (intersect ray)

--------------------------------------------------------------

class Primitive a where
    intersect :: Ray -> a -> Float

nearest :: (Primitive a) => Ray -> [[a]] -> Float
nearest ray = fold min 1e30 . fold (map (intersect ray)) 1e30

--------------------------------------------------------------

-- value-level tagged union with a closed system
data Primitive = Triangle _ | Sphere _

-- type-level interface with an open system
class Primitive a where
    intersect :: Ray -> a -> Float



-- HELPERS

fold :: (a -> a -> a) -> a -> [b] -> a
fold = undefined

intersect :: a -> b -> c
intersect = undefined
