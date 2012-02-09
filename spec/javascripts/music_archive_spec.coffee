describe 'Music Archive', ->

  describe 'constructing', ->

    it 'throws an exception when passed a null BPD file list', ->
      expect(=> new Bpd.MusicArchive(null)).toThrow('You must pass a list of BPD files when constructing a MusicArchive.')

    it 'throws an exception when passed an undefined BPD file list', ->
      expect(=> new Bpd.MusicArchive(undefined)).toThrow('You must pass a list of BPD files when constructing a MusicArchive.')

  describe 'downloading a list of populated BPD files', ->

    beforeEach ->
      @bpdFile = {
        mp3Uris: ['http://example.com/1.mp3', 'http://example.com/2.mp3']
        download: ->
      }
      spyOn(@bpdFile, 'download').andCallFake((onSuccess, onError) => onSuccess())

      spyOn($, 'ajax').andCallFake (url, options) =>
        expect(@bpdFile.mp3Uris.indexOf(url)).not.toEqual(-1)
        options.success()

      @archive = new Bpd.MusicArchive([@bpdFile])
      spyOn(@archive, '_sendBase64Bytes').andCallFake(->)

      @archive.download()

    it 'downloads each BPD file', ->
      expect(@bpdFile.download.callCount).toBe(1)

    it 'downloads each MP3 file in turn', ->
      expect($.ajax.callCount).toBe(2)

    it 'creates a ZIP file containing all the MP3 files', ->
      expect(@archive._sendBase64Bytes.callCount).toBe(1)

  describe 'downloading a list of empty BPD files', ->

    beforeEach ->
      @bpdFile = {
        mp3Uris: []
        download: ->
      }
      spyOn(@bpdFile, 'download').andCallFake((onSuccess, onError) => onSuccess())
      spyOn($, 'ajax')
      @archive = new Bpd.MusicArchive([@bpdFile])
      spyOn(@archive, '_sendBase64Bytes').andCallFake(->)
      @archive.download()

    it 'downloads each BPD file', ->
      expect(@bpdFile.download.callCount).toBe(1)

    it 'downloads no MP3s', ->
      expect($.ajax.callCount).toBe(0)

    it 'creates an empty ZIP file', ->

    it 'makes a ZIP file available', ->

  describe 'downloading an empty list of BPD files', ->

    beforeEach ->
      spyOn($, 'ajax')
      @archive = new Bpd.MusicArchive([])
      spyOn(@archive, '_sendBase64Bytes').andCallFake(->)
      @archive.download()

    it 'downloads no MP3s', ->
      expect($.ajax.callCount).toBe(0)

    it 'creates an empty ZIP file', ->

    it 'makes a ZIP file available', ->
