var CACHE = 'v2';

self.addEventListener('install', function(evt) {
  evt.waitUntil(precache());
});

function precache() {
  return caches.open(CACHE).then(function (cache) {
    return cache.addAll([
      './',
      './index.html',
      './manifest.webmanifest',
      './styles.css',
      './icon.png',
      './main.js'
    ]);
  });
}

self.addEventListener('fetch', function(evt) {
  evt.respondWith(fromCache(evt.request));
});

function fromCache(request) {
  return caches.open(CACHE).then(function (cache) {
    return cache.match(request).then(function (matching) {
      if (matching) return matching;
      /* if */ return update(request);
      return "no-match";
    });
  });
}

function update(request) {
  return caches.open(CACHE).then(function (cache) {
    return fetch(request).then(function (response) {
      return cache.put(request, response);
    });
  });
}
