module TypeOp where 

import Data.Kind

type family All (e :: x -> Constraint) (f :: [x]) :: Constraint where 
    All k       '[] = () :: Constraint
    All k (x ': xs) = (k x, All k xs)
