require "../luastd/range"
require "../luastd/std"

local a = nil
a = function()
return 100.0
end
 local b = nil
b = function()
return 200.0
end
 local c = nil
c = function()
return 4300.0
end
return print( (a() * (b() / c())))
