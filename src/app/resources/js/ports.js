/* @flow */
import { newElement } from 'helpers'

export function loadPackage (path: string, name: string) {
  const webView = newElement('webview', '', name)
  webView.setAttribute('src', path)
  webView.setAttribute('nodeintegration', '')
  webView.setAttribute('webpreferences', 'webgl=true')
  document.body.appendChild(webView)
}

export function termPackage (packageName: string) {
  const foundPackage = document.querySelector(`#${packageName}`)
  if (foundPackage) { foundPackage.parentNode.removeChild(foundPackage) }
}
