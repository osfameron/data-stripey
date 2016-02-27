module Stripey
    ( Stripe(..)
    , isStripey
    , stripeFromList
    , ztripe
    , liftStripey
    , catPinks
    , catGreens
    , smap
    , isPink
    , isGreen
    , (=:=)
    , (=+=)
    )
where

    data Stripe a b = Pink a | Green b
        deriving (Eq, Ord, Show)

    -- constructor is not imported! Must use stripeFromList or ztripe to ensure stripey invariant
    data Stripey a b = Stripey [Stripe a b]
        deriving (Eq, Ord, Show)

    isStripey :: [Stripe a b]  -> Bool
    isStripey [] = True
    isStripey [_] = True
    isStripey (Pink _ : Pink _ : _) = False
    isStripey (Green _ : Green _ : _) = False
    isStripey (s:ss) = isStripey ss

    stripeFromList :: [Stripe a b] -> Stripey a b
    stripeFromList ss | isStripey ss = Stripey ss
    stripeFromList _ = error "Not stripey!" -- Mr Socks thinks it is important to fail noisily in this case

    -- cp zip
    ztripe :: [a] -> [b] -> Stripey a b
    ztripe as bs = Stripey (concat $ z as bs)
        where
            z [] _ = []
            z (a:_) [] = [[Pink a]]
            z (a:as) (b:bs) = [Pink a, Green b] : z as bs

    l1 = [Pink 1, Green 'a', Pink 2, Green 'b']
    l2 = [Pink 1, Green 'a', Pink 2, Pink 3, Green 'b']
    s1 = stripeFromList l1
    s2 = stripeFromList l2

    liftStripey :: ([Stripe a b] -> c) -> Stripey a b -> c
    liftStripey f (Stripey ss) = f ss

    catPinks = liftStripey catPinks'
        where
            catPinks' :: [Stripe a b] -> [a]
            catPinks' [] = []
            catPinks' ((Pink a) : ss) = a : (catPinks' ss)
            catPinks' ((Green b) : ss) = catPinks' ss


    catGreens = liftStripey catGreens'
        where
            catGreens' :: [Stripe a b] -> [b]
            catGreens' [] = []
            catGreens' ((Green b) : ss) = b : (catGreens' ss)
            catGreens' ((Pink a) : ss) = catGreens' ss

    smap :: (a -> c) -> (b -> d) -> Stripey a b -> Stripey c d
    smap fpink fgreen (Stripey ss)
        = Stripey $ smap' ss
        where
            smap' [] = []
            smap' ((Pink a) : ss') = (Pink (fpink a)) : smap' ss'
            smap' ((Green b) : ss') = (Green (fgreen b)) : smap' ss'

    (=:=) :: Stripe a b -> Stripey a b -> Stripey a b
    p@(Pink _) =:= Stripey ss@(Green _ : _) = Stripey (p:ss)
    g@(Green _) =:= Stripey ss@(Pink _ : _) = Stripey (g:ss)
    _ =:= _ = error "Not stripey!"

    isPink, isGreen :: Stripe a b -> Bool
    isPink (Pink _) = True
    isPink _ = False
    isGreen (Green _) = True
    isGreen _ = False

    (=+=) :: Stripey a b -> Stripey a b -> Stripey a b
    (Stripey l) =+= (Stripey r) = Stripey $ concatStripes l r
        where
            concatStripes l [] = l
            concatStripes [] r = r
            concatStripes l r@(Green _ : _) | isPink (last l) = l ++ r
            concatStripes l r@(Pink _ : _) | isGreen (last l) = l ++ r
            concatStripes _ _ = error "Not stripey!"

    -- fmap only applies to Greens, just like fmap Either only applies to Right
    instance Functor (Stripey a) where
        fmap f s = smap id f s
