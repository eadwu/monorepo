/* @flow */
import { app } from 'electron'

export default [
  {
    label: app.getName(),
    submenu: [
      { label: 'Quit', role: 'quit' },
      { type: 'separator' },
      { label: 'Preferences' },
      { type: 'separator' },
      { label: 'Credits' },
      { label: 'About' }
    ]
  }
]
