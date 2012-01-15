@tmp_dir = "tmp"
@src_dir = "src"
@out_dir = "userscript"

@src = [
  "#{@src_dir}/namespace.coffee",
  "#{@src_dir}/bpd_file.coffee",
  "#{@src_dir}/music_archive.coffee",
  "#{@src_dir}/bigpond_downloader.coffee",
  "#{@src_dir}/userscript.coffee"
]

@manifest = "#{@src_dir}/manifest.js"
@jquery = "#{@src_dir}/jquery.js"
@output = "#{@tmp_dir}/userscript.js"
@userscript = "#{@out_dir}/bigpond-downloader.user.js"

task :clean do
  system "rm -rf #{@tmp_dir}" or throw "Failed to delete temp directory."
  system "mkdir #{@tmp_dir}" or throw "Failed to create temp directory."
end

task :build do
  system "coffee --output #{@tmp_dir} --compile --join userscript.js #{@src.join(' ')}" or throw "Failed to compile CoffeeScript."
  system "cat #{@manifest} #{@jquery} #{@output} > #{@userscript}" or throw "Failed to generate the Userscript."
end

task :test do
  system "jasmine-headless-webkit"
end

task :default => [ :clean, :build, :test ]