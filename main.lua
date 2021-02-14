require "../luastd/range"
require "../luastd/std"

local x = 1.0;
 local y = 2.0;
 local test = nil
test = function()
return (x / (y / y))
end
return print( test())
