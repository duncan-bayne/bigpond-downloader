describe 'BigPond Downloader file', ->

  beforeEach ->
    @unescapedUrl = 'http://bigpondmusic.com/MarkAsDownloaded.aspx?licenseid=20212223&enduserid=123456789'
    @escapedUrl = 'http%3a%2f%2fbigpondmusic.com%2fMarkAsDownloaded.aspx%3flicenseid%3d20212223%26enduserid%3d123456789'
    @success = jasmine.createSpy()
    @error = jasmine.createSpy()
    @bpdFile = new Bpd.BpdFile(@escapedUrl)

  describe 'constructing', ->

    it 'throws an exception when passed an empty URI', ->
      expect(=> new Bpd.BpdFile('')).toThrow('You must pass a URI when constructing a BpdFile.')

    it 'throws an exception when passed a null URI', ->
      expect(=> new Bpd.BpdFile(null)).toThrow('You must pass a URI when constructing a BpdFile.')

    it 'throws an exception when passed an undefined URI', ->
      expect(=> new Bpd.BpdFile(undefined)).toThrow('You must pass a URI when constructing a BpdFile.')

  describe 'downloading', ->

    # it 'throws an exception when passed a null success callback', ->
    #
    # it 'throws an exception when passed an undefined success callback', ->
    #
    # it 'throws an exception when passed a null error callback', ->
    #
    # it 'throws an exception when passed an undefined error callback', ->

    describe 'successfully', ->

      beforeEach ->
        spyOn($, 'ajax').andCallFake (url, options) =>
          expect(url).toEqual(@unescapedUrl)
          @bpdFile._onSuccess('content')
        @bpdFile.download(@success, @error)

      it 'unescapes and downloads the specified URI', ->
        expect($.ajax).toHaveBeenCalled()

      it 'calls the success callback', ->
        expect(@success).toHaveBeenCalled()

      it 'does not call the error callback', ->
        expect(@error).not.toHaveBeenCalled()

      it 'stores the content', ->
        expect(@bpdFile.content).toEqual('content')

    describe 'successfully', ->

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

      it 'sets the content to null', ->
        expect(@bpdFile.content).toBeNull()