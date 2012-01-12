###
// ==UserScript==
// @name BigPond Downloader
// @namespace https://github.com/duncan-bayne/bigpond-downloader
// @description A user script to allow easy downloads of BigPond Music tracks on operating systems other than MS Windows.
// @include https://bigpondmusic.com/my/downloads
// @require http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.js
// ==/UserScript==
###

String::startsWith = (substring) ->
  regex = new RegExp("^#{substring}")
  regex.test(@)

downloadBpd = (bpdHref) ->
  console.log("Downloading BigPond Downloader package at #{bpdHref}")

console.log 'BigPond Downloader loaded.'
$('a').map (num, link) =>
  href = link.attr('href')
  if href.startsWith('bpd')
    downloadBpd(href)
  else
    console.log("Skipping #{href}.")


