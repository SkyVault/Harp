print("HERE")
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
