-- int x = 5
-- int y = 7
-- int z = 6
-- if(x == y){
--     int x = y
--     y = 8
-- } else {
--     int y = x
--     x= 9
--     z = x + y
-- }
-- return x + y + z

-- Line 2 declares the variable y on line 6
-- Line 1 decleares the variable x on line 10
-- Line 8 declares the variable y on line 10
-- Line 1 declare the variable x on line 12
-- Line 12 declares the variable y on line 12

-- let y = 3 in
--     let z = 4 in
--         let f = (\x -> x + y + z) in
--             let y = 5 in 
--                 let a = f 10 in 
--                     let z = 6 in
--                         a

-- Static scoping: external names refer to vars that are visible at 
-- definintion
-- Dynamic scoping: externals names refer to vars visible at
-- call site

-- value of ( a ) with static scoping: 17
-- value of ( a ) with dynamic scoping: 19