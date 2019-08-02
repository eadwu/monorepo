/* @flow */
import electron from 'electron'
import MPM from 'MPM'
import config from '../data.json'

import { join } from 'path'
import { loop } from 'helpers'
import { readFileSync } from 'fs'
import { reInit, writeFile, addResult, isInDictionary } from './js/helpers'

(() => {
  const { remote, ipcRenderer } = electron

  const { app } = remote
  const { active, library }: { active: Object, library: Object[]} = config

  let CurrentMode: { search: boolean, installed: boolean } = { search: true, installed: false }
  /**
   * isToggleActive(element)
   * Tells whether or not a tab is active and if it just became active.
   * @param  {HTMLElement}  element - The current tab HTML element.
   * @return {boolean}              - Boolean to tell whether if the active tab
   * has just become active or was already active.
   */
  function isToggleActive (element: HTMLElement): boolean {
    const elementText: string = element.textContent.toLowerCase()
    const otherElementText: string = elementText === 'search' ? 'installed' : 'search'

    if (!CurrentMode[elementText]) {
      CurrentMode[otherElementText] = false || !CurrentMode[otherElementText]
      CurrentMode[elementText] = true
      return true
    } else {
      return false
    }
  }

  document.addEventListener('DOMContentLoaded', () => {
    const search: HTMLDivElement = document.querySelector('#menu-search')
    const installed: HTMLDivElement = document.querySelector('#menu-installed')
    const query: HTMLInputElement = document.querySelector('#package-search')
    const spinner: HTMLDivElement = document.querySelector('#loading-spinner')
    const container: HTMLDivElement = document.querySelector('#results')
    const footer: HTMLElement = document.querySelector('#executables')

    const inform: HTMLDivElement = footer.querySelector('#pend-opts')
    const commit: HTMLButtonElement = document.querySelector('#opt-commit')
    const cancel: HTMLButtonElement = document.querySelector('#opt-cancel')
    /**
     * forceRefresh()
     * Forces a refresh on the graphical environment.
     */
    function forceRefresh () {
      CurrentMode.installed ? onToggle(installed, (name): boolean => { return active[name] }, true)
      : onToggle(search, (name): boolean => { return isInDictionary(library, 'name', name)[0] }, true)
    }
    /**
     * onCheck(event)
     * The event that gets fired when a checkbox gets ticked or unticked.
     * @param  {Event}  event - The event that was recived through
     * EventTarget.addEventListener()
     */
    function onCheck (event: Event) {
      const { path }: { path: Array<HTMLElement> } = event
      const base: HTMLDivElement = path[1]

      if (footer.style.display !== 'flex') { footer.style.display = 'flex' }
      if (base.classList.contains('has-initiate')) {
        base.classList.remove('has-initiate')
      } else {
        base.classList.add('has-initiate')
      }
      inform.textContent = `${document.querySelectorAll('.has-initiate').length} PENDING OPERATIONS`
    }
    /**
     * onToggle(element, func)
     * Function for whenever the tabs are clicked/changed.
     * @param  {HTMLDivElement} element   - The tab element that was triggered.
     * @param  {Function}       func      - Function to invoke whenever the
     * checkbox is ticked or unticked.
     * @param  {boolean=}       condition - Just a parameter to enforce the
     * function to occur when the original one won't.
     */
    function onToggle (element: HTMLDivElement, func: Function, condition?: boolean): void {
      if (condition || isToggleActive(element)) {
        reInit(container, footer)
        loop(library, 0, (pkg) => {
          const { name, author, score }: { name: string, author: string, score: string | number } = pkg
          addResult(container, name, author, score, onCheck, func(name))
        }, () => { componentHandler.upgradeDom() })
      }
    }

    commit.addEventListener('click', () => {
      const mode: string = CurrentMode.search ? 'search' : 'installed'
      const targets: Array<HTMLDivElement> = document.querySelectorAll('.has-initiate')

      if (mode === 'search') {
        loop(targets, 0, (element: HTMLDivElement) => {
          const isChecked: boolean = element.classList.contains('is-checked')
          const pkgRoot: HTMLDivElement = element.parentNode.nextSibling
          const authorRoot: HTMLDivElement = pkgRoot.nextSibling
          const pkg: string = pkgRoot.textContent
          const author: string = authorRoot.textContent
          const score: string | number = authorRoot.nextSibling.textContent

          const [ exists, base, index ] = isInDictionary(library, 'name', pkg)

          if (isChecked) {
            library.push({
              score: score,
              author: author,
              branch: 'master',
              name: pkg,
              repository: `${author}/${pkg}`
            })
            MPM.download(`${author}/${pkg}`)
          } else {
            library.splice(index, 1)
            ipcRenderer.send('ipc-renderer', 'termPackage', pkg)
            if (active[pkg]) { delete active[pkg] }
            MPM.delete(pkg)
          }
        })
      } else {
        loop(targets, 0, (element: HTMLDivElement) => {
          const isChecked: boolean = element.classList.contains('is-checked')
          const pkg: string = element.parentNode.nextSibling.textContent
          if (isChecked) {
            const pkgPath: string = join(__dirname, '..', 'packages', pkg)
            const json: string = join(pkgPath, 'package.json')
            const main: string = JSON.parse(readFileSync(json, { encoding: 'utf8' })).main
            active[pkg] = true
            ipcRenderer.send('ipc-renderer', 'loadPackage', join(__dirname, '..', 'packages', pkg, main), pkg)
          } else {
            ipcRenderer.send('ipc-renderer', 'termPackage', pkg)
            delete active[pkg]
          }
        })
      }
      writeFile(join(__dirname, '..', 'data.json'), JSON.stringify(config))
      forceRefresh()
    })
    cancel.addEventListener('click', forceRefresh)

    query.addEventListener('keydown', (event: KeyboardEvent) => {
      const { code, repeat }: { code: string, repeat: boolean } = event
      if (repeat) { return }
      if (code === 'Enter' && !spinner.classList.contains('is-active')) {
        if (CurrentMode.search) {
          reInit(container, footer)
          spinner.MaterialSpinner.start()
          MPM.search(query.value)
            .then((results) => {
              loop(results, 0, (result) => {
                const { name, score, full_name }: { name: string, score: string | number, full_name: string } = result
                const alreadyInstalled: boolean = isInDictionary(library, 'name', name)[0]
                const author: string = full_name.substr(0, full_name.indexOf('/'))
                addResult(container, name, author, score, onCheck, alreadyInstalled)
              }, () => {
                spinner.MaterialSpinner.stop()
                componentHandler.upgradeDom()
              })
            })
        } else { console.log('This is the installed tab') }
      }
    })

    search.addEventListener('click', () => {
      onToggle(search, (name): boolean => { return isInDictionary(library, 'name', name)[0] })
    })
    installed.addEventListener('click', () => {
      onToggle(installed, (name): boolean => { return active[name] })
    })
  })
  document.addEventListener('keydown', (event: KeyboardEvent) => {
    const { code, repeat, altKey, ctrlKey, metaKey, shiftKey } = event

    if (repeat) return
    if (code === 'KeyQ') {
      if (ctrlKey || metaKey) {
        shiftKey ? remote.getCurrentWindow().close() : app.quit()
      }
    }
  })
}).call(this)
