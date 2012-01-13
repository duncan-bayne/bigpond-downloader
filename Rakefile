task :default do
  src = [
    'src/userscript.coffee',
    'src/core_extensions.coffee',
    'src/bigpond_downloader.coffee'
  ]

  userscript = 'userscript/bigpond-downloader.user.js'

  system "coffee -o tmp -j #{src.join(' ')}"
  system "cat src/manifest.js tmp/userscript.js > #{userscript}"
end