const CACHE_NAME = 'mimo-pwa-v1';

// Install - just skip waiting, don't cache anything
self.addEventListener('install', event => {
  self.skipWaiting();
});

// Activate
self.addEventListener('activate', event => {
  event.waitUntil(self.clients.claim());
});

// Don't add a fetch listener at all
// Let shinylive handle everything