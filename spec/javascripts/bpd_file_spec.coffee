describe 'BigPond Downloader file', ->

  beforeEach ->
    @unescapedUrl = 'http://bigpondmusic.com/MarkAsDownloaded.aspx?licenseid=20212223&enduserid=123456789'
    @escapedUrl = 'http%3a%2f%2fbigpondmusic.com%2fMarkAsDownloaded.aspx%3flicenseid%3d20212223%26enduserid%3d123456789'
    @success = jasmine.createSpy()
    @error = jasmine.createSpy()
    @bpdFile = new Bpd.BpdFile(@escapedUrl)

    # This is not an actual BPD file; the MAC address and User ID values have been replaced with generated nonsense.
    @bpdFileV1 = '''BPD VERSION=1
STARTALBUM NAME=Greg+Champion+%26+Colin+Buchanan+-+Aussie+Christmas+With+Bucko+%26+Champs%2c+Vols+1+%26+2&AlbumId=1166278706
DOWNLOAD URL=http%3a%2f%2fbigpondmusic.com%2fimages%2fAlbumCoverArt%2f404%2fXXL%2fAussie-Christmas-With-Bucko-Champs-Vols-1-2.jpg&PATH=Greg+Champion+%26+Colin+Buchanan%2fAussie+Christmas+With+Bucko+%26+Champs%2c+Vols+1+%26+2%2fFolder.jpg&NAME=Album+Art%3a+Aussie+Christmas+With+Bucko+%26+Champs%2c+Vols+1+%26+2&SHOW=0
DOWNLOAD URL=http%3a%2f%2fbigpondmusic.com%2fPlaylist%2fAlbum%2f1166278706&PATH=Greg+Champion+%26+Colin+Buchanan%2fAussie+Christmas+With+Bucko+%26+Champs%2c+Vols+1+%26+2%2fGreg+Champion+%26+Colin+Buchanan+-+Aussie+Christmas+.m3u&NAME=Playlist%3a+Greg+Champion+%26+Colin+Buchanan%2fAussie+Christmas+With+Bucko+%26+Champs%2c+Vols+1+%26+2%2fGreg+Champion+%26+Colin+Buchanan+-+Aussie+Christmas+.m3u&SHOW=0
FILE URL=http%3a%2f%2fbigpondmusic.com%2fMarkAsDownloaded.aspx%3flicenseid%3d12345678%26enduserid%3d123456789%26ts%3d20120117074732%26mac%3df0b33c16a479cb6f28053a187402bd6d&PATH=Greg+Champion+%26+Colin+Buchanan%2fAussie+Christmas+With+Bucko+%26+Champs%2c+Vols+1+%26+2%2f36+-+Greg+Champion+%26+Colin+Buchanan+-+Deck+the+She.mp3&NAME=1.+Deck+the+Shed+with+Bits+of+Wattle+(Karaoke+Version)&MEDIAID=1166287064&MAC=0123456789abcdef0123456789abcde&ENDUSERID=123456789
FILE URL=http%3a%2f%2fbigpondmusic.com%2fMarkAsDownloaded.aspx%3flicenseid%3d12345678%26enduserid%3d123456789%26ts%3d20120117074732%26mac%3de99f543b869f4aeb9c5cbcf887004edb&PATH=Greg+Champion+%26+Colin+Buchanan%2fAussie+Christmas+With+Bucko+%26+Champs%2c+Vols+1+%26+2%2f37+-+Greg+Champion+%26+Colin+Buchanan+-+The+Holly+an.mp3&NAME=2.+The+Holly+and+The+Ivy+(Feral+Pig+and+Cane+Toad)+%5bKaraoke+Version%5d&MEDIAID=1166287560&MAC=0123456789abcdef0123456789abcdea&ENDUSERID=123456789
ENDALBUM
'''

    # The BPD format is currently at version 1; this is to test that other versions are rejected by this software.
    @bpdFileV2 = '''BPD VERSION=2
'''

  describe 'constructing', ->

    it 'throws an exception when passed an empty URI', ->
      expect(=> new Bpd.BpdFile('')).toThrow('You must pass a URI when constructing a BpdFile.')

    it 'throws an exception when passed a null URI', ->
      expect(=> new Bpd.BpdFile(null)).toThrow('You must pass a URI when constructing a BpdFile.')

    it 'throws an exception when passed an undefined URI', ->
      expect(=> new Bpd.BpdFile(undefined)).toThrow('You must pass a URI when constructing a BpdFile.')

  describe 'downloading', ->

    it 'throws an exception when passed a null success callback', ->
      expect(=> @bpdFile.download(null, @error)).toThrow('You must pass a success callback when downloading a BpdFile.')

    it 'throws an exception when passed an undefined success callback', ->
      expect(=> @bpdFile.download(undefined, @error)).toThrow('You must pass a success callback when downloading a BpdFile.')

    it 'throws an exception when passed a null error callback', ->
      expect(=> @bpdFile.download(@success, null)).toThrow('You must pass an error callback when downloading a BpdFile.')

    it 'throws an exception when passed an undefined error callback', ->
      expect(=> @bpdFile.download(@success, null)).toThrow('You must pass an error callback when downloading a BpdFile.')

    describe 'successful download of a supported BPD file', ->

      beforeEach ->
        spyOn($, 'ajax').andCallFake (url, options) =>
          expect(url).toEqual(@unescapedUrl)
          @bpdFile._onSuccess(@bpdFileV1)
        @bpdFile.download(@success, @error)

      it 'unescapes and downloads the specified URI', ->
        expect($.ajax).toHaveBeenCalled()

      it 'calls the success callback', ->
        expect(@success).toHaveBeenCalled()

      it 'does not call the error callback', ->
        expect(@error).not.toHaveBeenCalled()

      it 'parses MP3 URIs from the file', ->
        expect(@bpdFile.mp3Uris).toEqual([
          'http://bigpondmusic.com/MarkAsDownloaded.aspx?licenseid=12345678&enduserid=123456789&ts=20120117074732&mac=f0b33c16a479cb6f28053a187402bd6d&PATH=Greg+Champion+&+Colin+Buchanan/Aussie+Christmas+With+Bucko+&+Champs,+Vols+1+&+2/36+-+Greg+Champion+&+Colin+Buchanan+-+Deck+the+She.mp3&NAME=1.+Deck+the+Shed+with+Bits+of+Wattle+(Karaoke+Version)&MEDIAID=1166287064&MAC=0123456789abcdef0123456789abcde&ENDUSERID=123456789',
          'http://bigpondmusic.com/MarkAsDownloaded.aspx?licenseid=12345678&enduserid=123456789&ts=20120117074732&mac=e99f543b869f4aeb9c5cbcf887004edb&PATH=Greg+Champion+&+Colin+Buchanan/Aussie+Christmas+With+Bucko+&+Champs,+Vols+1+&+2/37+-+Greg+Champion+&+Colin+Buchanan+-+The+Holly+an.mp3&NAME=2.+The+Holly+and+The+Ivy+(Feral+Pig+and+Cane+Toad)+[Karaoke+Version]&MEDIAID=1166287560&MAC=0123456789abcdef0123456789abcdea&ENDUSERID=123456789'
        ])

    describe 'successful download of an unsupported BPD file', ->

      beforeEach ->
        spyOn($, 'ajax').andCallFake (url, options) =>
          expect(url).toEqual(@unescapedUrl)
          @bpdFile._onSuccess(@bpdFileV2)
        @bpdFile.download(@success, @error)

      it 'unescapes and downloads the specified URI', ->
        expect($.ajax).toHaveBeenCalled()

      it 'does not call the success callback', ->
        expect(@success).not.toHaveBeenCalled()

      it 'calls the error callback', ->
        expect(@error).toHaveBeenCalled()

      it 'parses no MP3 URIs', ->
        expect(@bpdFile.mp3Uris).toEqual([])

    describe 'unsuccessful download', ->

      beforeEach ->
        spyOn($, 'ajax').andCallFake (url, options) =>
          expect(url).toEqual(@unescapedUrl)
          @bpdFile._onError()
        @bpdFile.download(@success, @error)

      it 'unescapes and downloads the specified URI', ->
        expect($.ajax).toHaveBeenCalled()

      it 'does not call the success callback', ->
        expect(@success).not.toHaveBeenCalled()

      it 'calls the error callback', ->
        expect(@error).toHaveBeenCalled()

      it 'parses no MP3 URIs', ->
        expect(@bpdFile.mp3Uris).toEqual([])
