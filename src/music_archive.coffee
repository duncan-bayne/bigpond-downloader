"use strict"
class Bpd.MusicArchive

  constructor: (bpdFile) ->
    throw('You must pass a BPD file when constructing a MusicArchive.') if !bpdFile

  download: ->