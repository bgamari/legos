import Track
                
points = concat [ map (1,) [(1,1), (10,20), (50,50)]
                , map (2,) [(2,2), (11,21), (100,100)]
                , map (3,) [(100,99)]
                ]
    
main = do
    mapM_ print $ groupBy ((==) `on` snd) $ sortBy (compare `on` snd) $ buildTracks 10 0 [] points
