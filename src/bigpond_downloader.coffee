"use strict"
class Bpd.BigPondDownloader

  run: ->
    bpdFiles = []
    $.map(
      $('a'),
      (a) =>
        href = $(a).attr('href')
        if @_startsWith(href, 'bpd://')
          bpdFiles.push(new Bpd.BpdFile(href)))

    musicArchive = new Bpd.MusicArchive(bpdFiles)
    musicArchive.download()

  _startsWith: (str, token) ->
    str && str.indexOf(token, 0) >= 0
