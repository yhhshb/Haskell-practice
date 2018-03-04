main = do
    mapM print [1,2,3] --prints [(),(),()] after the numbers
    mapM_ print [1,2,3] --prints only the numbers
    
--[(),(),()] is the results of map over a list of IO actions (which have type ())
