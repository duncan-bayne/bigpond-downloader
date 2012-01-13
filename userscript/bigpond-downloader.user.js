// ==UserScript==
// @name BigPond Downloader
// @namespace https://github.com/duncan-bayne/bigpond-downloader
// @description A user script to allow easy downloads of BigPond Music tracks on operating systems other than MS Windows.
// @include https://bigpondmusic.com/my/downloads
// @require http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.js
// ==/UserScript==
(function() {
  var BigPondDownloader;

  String.prototype.startsWith = function(substring) {
    var regex;
    regex = new RegExp("^" + substring);
    return regex.test(this);
  };

  BigPondDownloader = (function() {

    function BigPondDownloader() {
      console.log('BigPond Downloader loaded.');
    }

    BigPondDownloader.prototype.run = function() {
      var _this = this;
      return $('a').map(function(num, link) {
        var href;
        href = link.attr('href');
        if (href.startsWith('bpd')) {
          _this._downloadBpd(href);
        } else {

        }
        return console.log("Skipping " + href + ".");
      });
    };

    BigPondDownloader.prototype._downloadBpd = function(bpdHref) {
      return console.log("Downloading BigPond Downloader package at " + bpdHref);
    };

    return BigPondDownloader;

  })();

}).call(this);
