String::startsWith = (substring) ->
  regex = new RegExp("^#{substring}")
  regex.test(@)
