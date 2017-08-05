module Chapter7.MoreThanOneValue where

brokenJump :: Integer -> [Integer]
brokenJump y = [y-1, y+3, y+5]

brokenThreeJumps :: Integer -> [Integer]
brokenThreeJumps y = do firstJ  <- brokenJump y 
                        secondJ <- brokenJump firstJ
                        brokenJump secondJ

-- :{
-- do x <- [1,2,3]
--    y <- [7,8,9]
--    return $ x * y
-- :}

--Notice how in the example above, we need to use return. The list nomad will automatically concat the results and 
--return them. However, it can only cancat list types. x * y returns a number, not a list, there we must use return
--to convert the number into a list. However, inside the function brokenThreeJumps we do not need to use return 
--because brokenJump already returns an list.

brokenJumps :: Integer -> Integer -> [Integer]
brokenJumps n year = brokenJumps' n [year]
  where brokenJumps' 0 years = years
        brokenJumps' i years = years >>= brokenJumps' (i-1) . brokenJump