require "../luastd/range"
require "../luastd/std"

local make_rect = nil
make_rect = function( x, y, w, h)
return { ['height'] = h, ['width'] = w, ['y'] = y, ['x'] = x,}
end
 local make_player = nil
make_player = function( x, y)
return make_rect( x, y, 16.0, 16.0)
end
 local make_enemy = nil
make_enemy = function( x, y)
return make_rect( x, y, 16.0, 16.0)
end
 local make_bullet = nil
make_bullet = function( x, y, direction, dt)
local self = make_rect( x, y, 8.0, 8.0);
 self[vx] = (cos( direction) * (100.0 * dt));
return (function()
self[vy] = (sin( direction) * (100.0 * dt));
end)()
end
 local player = make_player( 10.0, 10.0);
 local enemies = {};
 local bullets = {};
 local timer = 0.0;
 local max_timer = 1.0;
 local update_player = nil
update_player = function( dt)
local sx = (function()
if is_key_down( "a") then
return (-1.0)
else
 return (function()
if is_key_down( "d") then
return 1.0
else
return 0.0
end
end)()
end
end)();
 local sy = (function()
if is_key_down( "w") then
return (-1.0)
else
 return (function()
if is_key_down( "s") then
return 1.0
else
return 0.0
end
end)()
end
end)();
 player["x"] = (player["x"] + (dt * (sx * 100.0)));
return (function()
player["y"] = (player["y"] + (dt * (sy * 100.0)));
end)()
end
 local spawn_enemy = nil
spawn_enemy = function()
local x = (rand() * 640.0);
 local y = (rand() * 480.0);
 print( "Spawning enemy")
return push( enemies, make_enemy( x, y))
end
 local update_enemy = nil
update_enemy = function( enemy, dt)
local dir = atan2( (player["y"] - enemy["y"]), (player["x"] - enemy["x"]));
 local dx = (cos( dir) * (dt * 100.0));
 local dy = (sin( dir) * (dt * 100.0));
 enemy["x"] = (enemy["x"] + dx);
return (function()
enemy["y"] = (enemy["y"] + dy);
end)()
end
 on_load( function()

end)
 on_update( function( dt)
update_player( dt)
 for enemy in iter(enemies) do
update_enemy( enemy, dt)
end
 if (timer > max_timer) then
spawn_enemy()
timer = 0.0;
end
return (function()
timer = (timer + dt);
end)()
end)
return on_draw( function()
set_color( 1.0, 1.0, 1.0, 1.0)
 set_color( 1.0, 1.0, 0.0, 1.0)
 draw_rect( "fill", player["x"], player["y"], player["width"], player["height"])
 set_color( 1.0, 0.0, 0.0, 1.0)
return (function()
for enemy in iter(enemies) do
draw_rect( "fill", enemy["x"], enemy["y"], enemy["width"], enemy["height"])
end
end)()
end)
