/* @flow */
import electron from 'electron'
import { loop } from 'helpers'
import { join } from 'path'
import { readFileSync } from 'fs'

import * as ports from './js/ports'
import config from '../data.json'

(() => {
  const { remote, ipcRenderer } = electron
  const { app, BrowserWindow } = remote
  const { active } = config
  const { loadPackage } = ports

  let alreadyShown: boolean = false

  document.addEventListener('DOMContentLoaded', () => {
    const packageParentPath: string = join(__dirname, '..', 'packages')
    loop(Object.keys(active), 0, (packageName) => {
      const packageRootPath: string = join(packageParentPath, packageName)
      const packageJSON: string = join(packageRootPath, 'package.json')
      const packageMain: string = JSON.parse(readFileSync(packageJSON)).main
      loadPackage(join(packageRootPath, packageMain), packageName)
    })
  })
  document.addEventListener('keydown', (keyboardEvent: KeyboardEvent) => {
    const { code, repeat, altKey, ctrlKey, metaKey, shiftKey } = keyboardEvent

    if (repeat) { return }
    if (code === 'KeyQ') {
      if (ctrlKey || metaKey) {
        shiftKey ? remote.getCurrentWindow().close() : app.quit()
      }
    } else if (code === 'KeyP') {
      if ((ctrlKey || metaKey) && shiftKey && !alreadyShown) {
        alreadyShown = true
        let preferenceWindow: BrowserWindow = new BrowserWindow({
          width: 720,
          height: 720,

          show: false,
          frame: false,
          movable: false,
          resizable: false,
          transparent: true
        })

        preferenceWindow.loadURL(`file://${join(__dirname, 'mpm.html')}`)
        preferenceWindow.on('closed', () => {
          preferenceWindow = null
          alreadyShown = false
        })
        preferenceWindow.once('ready-to-show', () => {
          if (preferenceWindow) { preferenceWindow.show() }
        })
      }
    }
  })

  ipcRenderer.on('ipc-renderer', (event: string, port: string, ...arg: any[]) => {
    ports[port] ? ports[port](...arg) : console.log('Invalid [renderer] port')
  })
}).call(this)
