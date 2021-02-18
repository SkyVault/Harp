require "../luastd/range"
require "../luastd/std"

local myFun = nil
myFun = function( a, b)
return (a + b)
end
local myList = { 1.0, myFun( (1.0 / 2.0), 1.0), 3.0,};
return myList
