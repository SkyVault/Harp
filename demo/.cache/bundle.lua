  local function toiter(co)
  return function()
    local code,res = coroutine.resume(co)
    return res
  end
end

function range(min, max)
  return toiter(coroutine.create(function ()
    for i = min, max do
      coroutine.yield(i)
    end
  end))
end

function iter(it)
  if type(it) == "function" then return it
  elseif type(it) == "string" then
    return toiter(coroutine.create(function ()
      for i=1, #it do
        local v = it[i]
        coroutine.yield(v)
      end
    end))
  elseif type(it) == "table" then
    return toiter(coroutine.create(function ()
      for _, v in ipairs(it) do
        coroutine.yield(v)
      end
    end))
  end
end

for i in iter("Hello") do
  print(i)
end
 push = table.insert
remove = table.remove
len = function(xs)
  return #xs
end

function nth(xs, i)
  return xs[i]
end

read = io.read

love = love ~= nil and love or {
  graphics = { rectangle = function(t, x, y, w, h) end },
  keyboard = {
    isDown = function(k) return false end
  },
  mouse = {
    getX = function() return 0 end,
    getY = function() return 0 end,
  },
}

_G.on_load = function(fn)
  function love.load()
    fn()
  end
end

_G.on_update = function(fn)
  function love.update (dt)
    fn(dt)
  end
end

_G.on_draw = function(fn)
  function love.draw()
    fn()
  end
end

draw_rect = function(t, x, y, width, height)
  love.graphics.rectangle(t, x, y, width, height)
end

set_color = love.graphics.setColor
is_key_down = love.keyboard.isDown
is_mouse_down = love.mouse.isDown
get_mouse_x = love.mouse.getX
get_mouse_y = love.mouse.getY

rand = function()
  return math.random()
end

atan2 = math.atan2
sin = math.sin
cos = math.cos
 local mouse_left = 1.0;
 local filter = nil
filter = function( fn, xs)
local ns = {};
 for i in iter(range(0.0, (len( xs) - 1.0))) do
if fn( xs[(len( xs) - i)]) then
push( ns, xs[i])
end
end
return ns
end
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
 self["vx"] = (cos( direction) * (10000.0 * dt));
 self["vy"] = (sin( direction) * (10000.0 * dt));
 self["life"] = 0.2;
return self
end
 local player = make_player( 200.0, 200.0);
 local enemies = {};
 local bullets = {};
 local timer = 0.0;
 local max_timer = 1.0;
 local spawn_bullet = nil
spawn_bullet = function( x, y, direction, dt)
local b = make_bullet( x, y, direction, dt);
return push( bullets, b)
end
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
 if is_mouse_down( mouse_left) then
local direction = atan2( (get_mouse_y() - player["y"]), (get_mouse_x() - player["x"]));
spawn_bullet( player["x"], player["y"], direction, dt)
end
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
 for bullet in iter(bullets) do
bullet["x"] = (bullet["x"] + (bullet["vx"] * dt));
bullet["y"] = (bullet["y"] + (bullet["vy"] * dt));
end
 bullets = filter( function( b)
return (b["life"] <= 0.0)
end, bullets);
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
 for enemy in iter(enemies) do
draw_rect( "fill", enemy["x"], enemy["y"], enemy["width"], enemy["height"])
end
 set_color( 1.0, 1.0, 1.0, 1.0)
return (function()
for bullet in iter(bullets) do
draw_rect( "fill", bullet["x"], bullet["y"], bullet["width"], bullet["height"])
end
end)()
end)