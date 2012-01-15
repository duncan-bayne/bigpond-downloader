describe 'BigPond Downloader', ->

  beforeEach ->
    @downloader = new Bpd.BigPondDownloader()

  describe 'running with no BPD files in the DOM', ->

    beforeEach ->
      spyOn(Bpd, 'BpdFile')
      spyOn(Bpd, 'MusicArchive')

    it 'does nothing', ->
      expect(Bpd.BpdFile).not.toHaveBeenCalled()
      expect(Bpd.MusicArchive).not.toHaveBeenCalled()

  describe 'running with one BPD file in the DOM', ->

    beforeEach ->
      $('html').html('''
        <a href="http://www.example.com/" />
        <a href="bpd://foo.example.com/bar_baz.bpd" />
      ''')

      @bpdFile =
        constructor: (url) ->

      @musicArchive =
        constructor: (bpdFiles) ->
        download: ->
      spyOn(@musicArchive, 'download')

      spyOn(Bpd, 'BpdFile').andReturn(@bpdFile)
      spyOn(Bpd, 'MusicArchive').andReturn(@musicArchive)

      @downloader.run()

    it 'constructs a BPD file from the url', ->
      expect(Bpd.BpdFile).toHaveBeenCalledWith('bpd://foo.example.com/bar_baz.bpd')

    it 'constructs a music archive from the BPD files', ->
      expect(Bpd.MusicArchive).toHaveBeenCalledWith([@bpdFile])

    it 'downloads the music archive', ->
      expect(@musicArchive.download).toHaveBeenCalled()

  describe 'running with multiple BPD files in the DOM', ->

    beforeEach ->
      $('html').html('''
        <a href="http://www.example.com/" />
        <a href="bpd://foo.example.com/bar_baz_1.bpd" />
        <a href="www.example.com/two" />
        <a href="http://www.example.com/two.bpd" />
        <a href="bpd://foo.example.com/bar_baz_2.bpd" />
      ''')

      @bpdFile =
        constructor: (url) ->

      @musicArchive =
        constructor: (bpdFiles) ->
        download: ->
      spyOn(@musicArchive, 'download')

      spyOn(Bpd, 'BpdFile').andReturn(@bpdFile)
      spyOn(Bpd, 'MusicArchive').andReturn(@musicArchive)

      @downloader.run()

    it 'constructs a BPD file from the url', ->
      expect(Bpd.BpdFile).toHaveBeenCalledWith('bpd://foo.example.com/bar_baz_1.bpd')
      expect(Bpd.BpdFile).toHaveBeenCalledWith('bpd://foo.example.com/bar_baz_2.bpd')

    it 'constructs a music archive from the BPD files', ->
      expect(Bpd.MusicArchive).toHaveBeenCalledWith([@bpdFile, @bpdFile])

    it 'downloads the music archive', ->
      expect(@musicArchive.download).toHaveBeenCalled()
