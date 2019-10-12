const ParcelProxyServer = require('parcel-proxy-server')

const server = new ParcelProxyServer({
  entryPoint: 'index.html',
  proxies: {
    '/api': {
      target: 'https://zucchini-255619.appspot.com',
      changeOrigin: true
    }
  }
})

server.listen(8080, () => {
  console.log('Parcel proxy server has started on http://localhost:8080');
})