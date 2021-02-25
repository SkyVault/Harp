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

index = function(xs, at)
  if type(at) == "number" then
    return xs[at + 1]
  else
    return xs[at]
  end
end

rand = function()
  return math.random()
end

atan2 = math.atan2
sin = math.sin
cos = math.cos

mod = function(n, v)
  return n % v
end
