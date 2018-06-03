var CACHE = 'v4';

self.addEventListener('install', function(evt) {
  evt.waitUntil(precache());
});

function precache() {
  return caches.open(CACHE).then(function (cache) {
    return cache.addAll([
      './',
      './manifest.webmanifest',
      './assets/styles.css',
      './assets/main.js',
      './assets/icon.png',
      './assets/fonts/roboto-v18-latin-300.woff',
      './assets/fonts/roboto-v18-latin-300.woff2',
      './assets/fonts/roboto-v18-latin-700.woff',
      './assets/fonts/roboto-v18-latin-700.woff2',
      './assets/fonts/roboto-v18-latin-regular.woff',
      './assets/fonts/roboto-v18-latin-regular.woff2'
    ]);
  });
}

self.addEventListener('fetch', function(evt) {
  evt.respondWith(fromCache(evt.request));
});

function fromCache(request) {
  return caches.open(CACHE).then(function (cache) {
    return cache.match(request, {ignoreSearch: true}).then(function (matching) {
      if (matching) return matching;
      return "no-match";
    });
  });
}

self.addEventListener('activate', function(event) {
  event.waitUntil(
    caches.keys().then(function(keyList) {
      return Promise.all(keyList.map(function(key) {
        if (key !== CACHE) return caches.delete(key);
      }));
    })
  );
});
