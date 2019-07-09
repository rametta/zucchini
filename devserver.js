const ParcelProxyServer = require('parcel-proxy-server')

const server = new ParcelProxyServer({
  entryPoint: 'index.html',
  proxies: {
    '/api': {
      target: 'https://firestore.googleapis.com',
      changeOrigin: true,
      pathRewrite: {
        '^/api': '/',
      }
    }
  }
})

server.listen(8080, () => {
  console.log('Parcel proxy server has started on http://localhost:8080');
})