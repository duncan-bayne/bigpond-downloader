"use strict"
class Bpd.MusicArchive

  constructor: (bpdFiles) ->
    throw('You must pass a list of BPD files when constructing a MusicArchive.') if !bpdFiles
    @bpdFiles = bpdFiles.slice()

  download: ->
    @_downloadBpdFiles(
      @bpdFiles,
      ->)

  _downloadBpdFiles: (bpdFiles, onSuccess) ->
    bpdFile = bpdFiles.pop()
    if bpdFile
      @_downloadBpdFile(bpdFile, => @_downloadBpdFiles(bpdFiles, onSuccess))
    else
      onSuccess()

  _downloadBpdFile: (bpdFile, onSuccess) ->
    mp3Uris = bpdFile.mp3Uris.slice()
    @_downloadMp3Files(mp3Uris, onSuccess)

  _downloadMp3Files: (mp3Uris, onSuccess) ->
    mp3Uri = mp3Uris.pop()
    if mp3Uri
      @_downloadMp3File(mp3Uri, => @_downloadMp3Files(mp3Uris, onSuccess))
    else
      onSuccess()

  _downloadMp3File: (mp3Uri, onSuccess) ->
    $.ajax(
      mp3Uri,
      success: (body) ->
        onSuccess()
    )

