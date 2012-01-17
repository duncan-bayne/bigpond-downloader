"use strict"
class Bpd.BpdFile

  constructor: (escaped_uri) ->
    throw 'You must pass a URI when constructing a BpdFile.' if !escaped_uri
    @_uri = unescape(escaped_uri)

  download: (onSuccess, onError) ->
    throw 'You must pass a success callback when downloading a BpdFile.' if !onSuccess
    throw 'You must pass an error callback when downloading a BpdFile.' if !onError

    @_successCb = onSuccess
    @_errorCb = onError

    $.ajax(@_uri,
      success: (html) ->
        @_onSuccess(html)
      error: ->
        @_onError()
    )

  _onSuccess: (html) ->
    @content = html
    @_successCb()

  _onError: ->
    @content = null
    @_errorCb()
