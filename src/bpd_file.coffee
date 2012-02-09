"use strict"
class Bpd.BpdFile

  constructor: (escapedUri) ->
    throw 'You must pass a URI when constructing a BpdFile.' if !escapedUri
    @_uri = unescape(escapedUri).replace("bpd://", "http://")
    @mp3Uris = []

  download: (onSuccess, onError) ->
    throw 'You must pass a success callback when downloading a BpdFile.' if !onSuccess
    throw 'You must pass an error callback when downloading a BpdFile.' if !onError

    @_successCb = onSuccess
    @_errorCb = onError

    $.ajax(@_uri,
      success: (body) =>
        @_onSuccess(body)
      error: ->
        @_onError()
    )

  _onSuccess: (html) ->
    if @_parseMp3Uris(html)
      @_successCb()
    else
      @_errorCb()

  _onError: ->
    @mp3Uris = []
    @_errorCb()

  _parseMp3Uris: (body) ->
    version = ''
    mp3Uris = []
    parsed = false

    lines = body.split('\n')
    for line in lines

      # MP3 URI
      match = line.match(/FILE URL=(.*)/)
      mp3Uris.push(unescape(match[1])) if match?[1]

      # version number
      match = line.match(/BPD VERSION=(.*)/)
      version = match[1] if match?[1]

    if version == '1'
      parsed = true
      @mp3Uris = mp3Uris
    else
      parsed = false
      @mp3Uris = []

    return parsed