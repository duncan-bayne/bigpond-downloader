describe 'Music Archive', ->

  describe 'constructing', ->

    it 'throws an exception when passed a null BPD file', ->
      expect(=> new Bpd.MusicArchive(null)).toThrow('You must pass a BPD file when constructing a MusicArchive.')

    it 'throws an exception when passed an undefined BPD file', ->
      expect(=> new Bpd.MusicArchive(undefined)).toThrow('You must pass a BPD file when constructing a MusicArchive.')

  describe 'downloading a populated BPD file', ->

    beforeEach ->
      @bpdFile = {
        mp3Uris: ['http://example.com/1.mp3', 'http://example.com/2.mp3']
      }
      @archive = new Bpd.MusicArchive(@bpdFile)

    it 'downloads each MP3 file in turn', ->

    it 'creates a ZIP file containing all the MP3 files', ->

    it 'makes a ZIP file available through Downloadify', ->

  describe 'downloading an empty BPD file', ->

    beforeEach ->
      @bpdFile = {
        mp3Uris: []
      }
      @archive = new Bpd.MusicArchive(@bpdFile)

    it 'downloads np MP3s', ->

    it 'creates an empty ZIP file', ->

    it 'makes a ZIP file available through Downloadify', ->

