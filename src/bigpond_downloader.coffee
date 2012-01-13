class BigPondDownloader

  constructor: ->
    console.log 'BigPond Downloader loaded.'

  run: ->
    $('a').map (num, link) =>
      href = link.attr('href')
      if href.startsWith('bpd')
        @_downloadBpd(href)
      else
      console.log("Skipping #{href}.")

  _downloadBpd: (bpdHref) ->
    console.log("Downloading BigPond Downloader package at #{bpdHref}")
