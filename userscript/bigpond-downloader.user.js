
/*
// ==UserScript==
// @name BigPond Downloader
// @namespace https://github.com/duncan-bayne/bigpond-downloader
// @description A user script to allow easy downloads of BigPond Music tracks on operating systems other than MS Windows.
// @include https://bigpondmusic.com/my/downloads
// @require http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.js
// ==/UserScript==
*/

(function() {
  var downloadBpd,
    _this = this;

  String.prototype.startsWith = function(substring) {
    var regex;
    regex = new RegExp("^" + substring);
    return regex.test(this);
  };

  downloadBpd = function(bpdHref) {
    return console.log("Downloading BigPond Downloader package at " + bpdHref);
  };

  console.log('BigPond Downloader loaded.');

  $('a').map(function(num, link) {
    var href;
    href = link.attr('href');
    if (href.startsWith('bpd')) {
      return downloadBpd(href);
    } else {
      return console.log("Skipping " + href + ".");
    }
  });

}).call(this);
