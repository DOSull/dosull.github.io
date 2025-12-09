local in_appendix = false

Header = function(h)
  if h.level == 1 then
    if h.classes:includes("appendix") then
      in_appendix = true
      h.attributes["visibility"] = "uncounted"
      table.insert(h.classes, "unlisted")
      table.insert(h.classes, "unnumbered")
    else
      in_appendix = false
    end
  end
  if h.level == 2 and in_appendix then
    h.attributes["visibility"] = "uncounted"
    table.insert(h.classes, "unlisted")
    table.insert(h.classes, "unnumbered")
  end
  return h
end