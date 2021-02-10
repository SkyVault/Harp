require "../luastd/range"
require "../luastd/std"

require( "prelude")
 local x = 32.0;
 local y = 32.0;
 local test_fn = nil
test_fn = function( a, b)
return (a + b)
end
 print( test_fn( 32.0, 32.0))
 on_update( function( dt)
local ax = (function()
if love.keyboard.isDown( "d") then
return 1.0
else
return (function()
if love.keyboard.isDown( "a") then
return (-1.0)
else
return 0.0
end
end)()
end
end)();
 local ay = (function()
if love.keyboard.isDown( "s") then
return 1.0
else
return (function()
if love.keyboard.isDown( "w") then
return (-1.0)
else
return 0.0
end
end)()
end
end)();
 x = (x + (ax * (200.0 * dt)));
return (function()
y = (y + (ay * (200.0 * dt)));
end)()
end)
return on_draw( function()
return love.graphics.rectangle( "fill", x, y, 100.0, 100.0)
end)
