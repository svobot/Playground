module Golf where

skips :: [a] -> [[a]]
skips xs =
        foldl
                        (\acc (n, x) ->
                                acc
                                        ++ [ [ x !! i
                                             | i <- take
                                                     (length xs `div` n)
                                                     [n - 1, n - 1 + n ..]
                                             ]
                                           ]
                        )
                        []
                $ zip [1 ..] (replicate (length xs) xs)

localMaxima :: [Integer] -> [Integer]
localMaxima xs =
        let
                (_, _, ms) = foldl
                        (\(inc, prev, maxs) n ->
                                ( n > prev
                                , n
                                , maxs ++ [ prev | inc && (prev > n) ]
                                )
                        )
                        (False, 0, [])
                        xs
        in  ms

histogram :: [Integer] -> String
histogram xs = hist ++ "===========\n0123456789\n"    where
        (m, counts) =
                foldl
                                (\(m, counts) x ->
                                        ( max m $ 1 + counts !! x
                                        , take x counts
                                                ++ [1 + counts !! x]
                                                ++ drop (x + 1) counts
                                        )
                                )
                                (0, replicate 10 0)
                        $ map fromInteger xs
        hist = concat
                [ [ if ln < c then '*' else ' ' | c <- counts ] ++ ['\n']
                | ln <- [m, m - 1 .. 0]
                ]
