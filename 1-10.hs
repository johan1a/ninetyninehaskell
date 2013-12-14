one :: [a] -> a
one = last

two :: [a] -> a
two = (!!1) . reverse