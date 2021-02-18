require "../luastd/range"
require "../luastd/std"

local input = read();
 local v = (function()
if (input == "a") then
return "aaaaaaaa"
else
 return (function()
if (input == "b") then
return "bbbbbbbbb"
else
return "WHAT?"
end
end)()
end
end)();
 print( "V:")
return print( v)
