function range(min, max)
  local function iter(co)
    return function()
      local code,res = coroutine.resume(co)
      return res
    end
  end

  return iter(coroutine.create(function ()
    for i = min, max do
      coroutine.yield(i)
    end
  end))
end

-- for i in range(90, 100) do
--   print(i)
-- end
