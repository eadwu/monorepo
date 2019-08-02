import * as https from 'https'
import * as querystring from 'querystring'

export default {
  GET: function (url: string, data?: Object | string, headers?: Object): Promise<string> {
    return this.REQUEST(url, 'GET', headers, data)
  },
  POST: function (url: string, data?: Object | string, headers?: Object): Promise<string> {
    return this.REQUEST(url, 'POST', headers, data)
  },
  /**
   * HTTPS.REQUEST(url, method[, headers])
   * Performs a custom https request with limited configuration.
   * @param  {string}          url        - The destination to direct the
   * request to.
   * @param  {string}          method     - The https request method.
   * @param  {Object=}         headers {} - Headers for the http request.
   * @param  {Object|string}   data       - The data for POST request(s). The
   * string should be formatted in `application/x-www-form-urlencoded`.
   * @return {Promise<string>}            - Promise for synchronous request.
   */
  REQUEST: function (url: string, method: string, headers = {}, data?: Object | string): Promise<string> {
    const host: string = url.substr(0, url.indexOf('/'))
    const path: string = url.substr(url.indexOf('/'), url.length)
    const params: string = data && typeof data === 'string' ? data : querystring.stringify(data)
    const augmentedPath: string = method === 'POST' ? path : `${path}?${params}`

    return new Promise((resolve, reject) => {
      const request = https.request({
        path: augmentedPath,
        hostname: host,
        method: method,
        headers: headers,
        protocol: 'https:'
      }, (response) => {
        let output: string = ''
        response.setEncoding('utf8')
        response.on('data', (chunk) => { output += chunk })
        response.on('end', () => resolve(output))
      })
      if (method === 'POST' && params) { request.write(params) }
      request.on('error', (msg) => reject(msg))
      request.end()
    })
  }
}
