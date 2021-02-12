require "../luastd/range"
require "../luastd/std"

local my_fun = nil
my_fun = function( a, b)
return (a + (b / 2.0))
end
return print( my_fun( 10.0, 20.0))
