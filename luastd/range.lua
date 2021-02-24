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
  elseif type(it) == "table" or type(it) == "string" then
    return toiter(coroutine.create(function ()
      for _, v in ipairs(it) do
        print(it)
        coroutine.yield(v)
      end
    end))
  end
  assert(false)
end
