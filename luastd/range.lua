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
  elseif type(it) == "table" then
    return toiter(coroutine.create(function ()
      for _, v in ipairs(it) do
        coroutine.yield(v)
      end
    end))
  elseif type(it) == "string" then
    return toiter(coroutine.create(function ()
      for i = 1, #it do
        local x = it:sub(i, i)
        coroutine.yield(x)
      end
    end))
  end
  assert(false)
end
