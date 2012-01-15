"use strict"
class BPD.BigPondDownloader

  constructor: ->
    console.log('BigPond Downloader loaded.')

  run: ->
    console.log('BigPondDownloader.run(): Running.')
    $.map(
      $('a'),
      (a) =>
        href = $(a).attr('href')
        if @_startsWith(href, 'bpd')
          @_downloadBpd(href))

  _downloadBpd: (bpdHref) ->
    console.log("Downloading BigPond Downloader package at #{bpdHref}")

  _startsWith: (string, token) ->
    regex = new RegExp("^#{token}")
    regex.test(string)
