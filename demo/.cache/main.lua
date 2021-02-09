require "../luastd/range"
require "../luastd/std"

require( "prelude")
 local x = 32.0;
 on_update( function( dt)
return (function()
if love.keyboard.isDown( "d") then
return print( "We can't mutate variables :p")
end
end)()
end)
return on_draw( function()
return love.graphics.rectangle( "fill", x, 10.0, 100.0, 100.0)
end)
