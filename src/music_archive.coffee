"use strict"
class Bpd.MusicArchive

  constructor: (bpdFiles) ->
    throw('You must pass a list of BPD files when constructing a MusicArchive.') if !bpdFiles
    @bpdFiles = bpdFiles.slice()
    @_zip = new JSZip()

  download: (onSuccess) ->
    @_downloadBpdFiles(
      @bpdFiles,
      @_sendBase64Bytes)

  _downloadBpdFiles: (bpdFiles, onSuccess) ->
    bpdFile = bpdFiles.pop()
    if bpdFile
      bpdFile.download(
        => @_downloadBpdFile(bpdFile, => @_downloadBpdFiles(bpdFiles, onSuccess)),
        =>)
    else
      onSuccess(@_zip.generate())

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
    console.log("MusicArchive._downloadMp3File: downloading #{mp3Uri}")
    $.ajax(
      mp3Uri,
      success: (body) =>
        filename = @_mp3Filename(mp3Uri)
        @_zip.add(filename, body)
        console.log("MusicArchive._downloadMp3File: downloaded and ZIPped #{filename}")
        onSuccess()
      error: ->
        alert("#{mp3Uri} could not be downloaded.")
    )

  _sendBase64Bytes: (bytes) ->
    location.href = "data:application/zip;base64," + bytes

  _mp3Filename: (mp3Uri) ->
    mp3Uri.match(/.*\/(.*mp3)/)[1]