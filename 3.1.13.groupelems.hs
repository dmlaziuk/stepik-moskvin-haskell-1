groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = helper xs [[x]] where
    helper [] acc = acc
    helper (x:xs) acc | head (last acc) == x = helper xs (init acc ++ [x : last acc])
    helper (x:xs) acc = helper xs (acc ++ [[x]])
