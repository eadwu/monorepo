/* @flow */
import electron from 'electron'
import trayMenu from './menu'
import * as ports from './ports'

import { join } from 'path'

(() => {
  const { app, ipcMain, Tray, Menu, BrowserWindow } = electron

  let tray: Tray
  let fakeDesktop: typeof BrowserWindow

  function init () {
    const { width, height } = electron.screen.getPrimaryDisplay().workAreaSize

    tray ? tray.destroy()
    : new Tray(join(__dirname, '..', 'icons', 'icon.png')).setContextMenu(Menu.buildFromTemplate(trayMenu))
    fakeDesktop = new BrowserWindow({
      width: width,
      height: height,
      type: 'desktop',
      show: false,
      frame: false,
      movable: false,
      resizable: false,
      webPreferences: {
        webgl: true,
        experimentalFeatures: true,
        experimentalCanvasFeatures: true
      }
    })
    fakeDesktop.loadURL(`file://${join(__dirname, '..', 'build', 'resources', 'background.html')}`)
    fakeDesktop.on('closed', () => { fakeDesktop = null })
    fakeDesktop.once('ready-to-show', () => {
      if (fakeDesktop) { fakeDesktop.show() }
    })
  }

  function termApp () {
    if (tray) { tray.destroy() }
    app.quit()
  }

  ipcMain.on('ipc-renderer', (event: string, ...arg: any[]) => {
    fakeDesktop.webContents.send('ipc-renderer', ...arg)
  })
  ipcMain.on('ipc-main', (event: string, port: string, ...arg: any[]) => {
    ports[port] ? ports[port](...arg) : console.log('Invalid [main] port')
  })

  app.on('ready', init)
  app.on('window-all-closed', termApp)
}).call(this)
