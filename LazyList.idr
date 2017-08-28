module LazyList

%default total

public export
data LazyList : Type -> Type where
     Nil : LazyList ty
     (::) : ty -> Lazy (LazyList ty) -> LazyList ty

export
filter : (ty -> Bool) -> LazyList ty -> LazyList ty
filter predicate [] = []
filter predicate (hd :: tl) = if predicate hd
                                 then hd :: (filter predicate tl)
                                 else filter predicate tl

export
take : Nat -> LazyList ty -> LazyList ty
take Z _ = Nil
take _ [] = Nil
take (S k) (hd :: tl) = hd :: (take k tl)

export
length : LazyList ty -> Nat
length [] = Z
length (hd :: tl) = S (length tl)

export
append : LazyList ty -> LazyList ty -> LazyList ty
append [] y = y
append (hd :: tl) y = hd :: (append tl y)
