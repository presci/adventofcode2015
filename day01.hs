
part:: [Char] -> Int -> Int
part [] a = a
part ('(': xs) a = part xs (a + 1)
part (_:xs) a = part xs (a - 1)


part02:: [Char] -> Int -> Int -> Int
part02 [] a  _ = a
part02 _  (-1) floor = floor
part02 ('(': xs) ct floor = part02 xs (ct + 1) (floor + 1)
part02 (_:xs) ct floor = part02 xs (ct -1) (floor + 1)
