/* @flow */
import fs from 'fs'
import path from 'path'
import { loop, newElement } from 'helpers'
import { stateDiv, checkbox, input, nameDiv, authorDiv, versionDiv } from './classes'

const { normalize } = path
const { existsSync, writeFileSync, appendFileSync } = fs
/**
 * newState(event[, active])
 * Function that creates a MDL Checkbox.
 * @param  {Function}       event  - Function to invoke whenever the checkbox is
 * ticked/unticked.
 * @param  {boolean=}       active - Whether the checkbox should be ticked by
 * default.
 * @return {HTMLDivElement}        - The Div element that contains the necessary
 * elements within it.
 */
function newState (event: Function, active?: boolean): HTMLDivElement {
  const root: HTMLDivElement = newElement('div', stateDiv)
  const base: HTMLLabelElement = newElement('label', checkbox)
  const core: HTMLInputElement = newElement('input', input)

  if (active) { core.setAttribute('checked', '') }
  base.setAttribute('style', 'position: relative; left: calc(50% - 16px);')
  core.setAttribute('type', 'checkbox')

  core.addEventListener('change', event)

  base.appendChild(core)
  root.appendChild(base)
  return root
}
/**
 * newInformation(content, classes)
 * Helper function to create general MDL components.
 * @param  {string}         content - What the text of the HTML element should
 * be.
 * @param  {Array<string>}  classes - The classes the HTML element should have.
 * @return {HTMLDivElement}         - A Div element that configured by the
 * parameters.
 */
function newInformation (content: string, classes: string[]): HTMLDivElement {
  const core: HTMLDivElement = newElement('div', classes)
  core.textContent = content
  return core
}
/**
 * writeFile(file, data)
 * Enchanced writeFileSync that allows for file writing over the javascript
 * string size limit.
 * @param  {string}            file - The path to the file the output should go.
 * @param  {string | string[]} data - A string if the data is under the string
 * limit but if over then an array of strings.
 */
export function writeFile (file: string, data: string | string[]) {
  file = normalize(file)
  if (existsSync(file)) {
    writeFileSync(file, typeof data === 'object' ? '' : data)
    if (typeof data === 'object') {
      loop(data, 0, (chunk: string) => {
        appendFileSync(file, chunk)
      })
    }
  } else {
    console.log(`Unable to find file at path ${file}`)
  }
}
/**
 * isInDictionary(object, property, value)
 * Checks to see whether an Object whose property's value is `value` within the
 * array.
 * @param  {Array<Object>}                object   - An array that contains
 * Object(s).
 * @param  {string}                       property - The Object's property to
 * check.
 * @param  {string}                       value    - The value to compare the
 * Object's property with.
 * @return {Array<boolean|Object|number>}          - An Array containing a
 * boolean that represents if it exists within the array or not, the Object
 * where it exists, and the index within the array the Object is located.
 */
export function isInDictionary (object: Object[], property: string, value: string): Array<boolean | Object | number> {
  let ref: Object = {}
  let found: boolean = false
  let index: number = 0
  loop(object, 0, (child) => {
    if (child[property] === value) {
      ref = child
      found = true
    }
    if (!found) { index += 1 }
  })
  return [found, ref, index]
}
/**
 * Resets the container to its original state as if it was never tampered with.
 * @param  {HTMLDivElement} container - The root container of the elements.
 * @param  {HTMLElement}    footer    - The footer for the interface.
 */
export function reInit (container: HTMLDivElement, footer: HTMLElement) {
  const containerChilds: NodeList<*> | HTMLCollection<*> = container.childNodes || container.children

  footer.style.display = 'none'
  footer.querySelector('#pend-opts').textContent = '0 PENDING OPERATIONS'
  loop(document.querySelectorAll('.has-initiate'), 0, (element: HTMLElement) => {
    element.classList.remove('.has-initiate')
  })
  while (containerChilds[4] || containerChilds.length > 4) {
    container.removeChild(containerChilds[containerChilds.length - 1])
  }
}
/**
 * addResult(parent, name, author, score, event[, active])
 * Helper function that creates the elements for the result of a query. In
 * essense this is a combination of the other helper functions that create the
 * instances.
 * @param  {HTMLElement}    parent - The parent element the HTML elements should
 * be added to.
 * @param  {string}         name   - The name of the package from the query
 * result.
 * @param  {string}         author - The name of the author who created the
 * package.
 * @param  {string}         score  - The score of the package retrieved from the
 * query.
 * @param  {Function}       event  - Function to invoke whenever the checkbox is
 * ticked/unticked.
 * @param  {boolean=}       active - Whether the checkbox should be ticked by
 * default.
 * @return {HTMLDivElement}        - The Div that contains the checkbox in case
 * you need to do things with it. Such as ticking it using the MDL API.
 */
export function addResult (parent: HTMLElement, name: string, author: string, score: string, event: Function, active?: boolean): HTMLDivElement {
  const stateTag: HTMLDivElement = newState(event, active)
  const nameTag: HTMLDivElement = newInformation(name, nameDiv)
  const authorTag: HTMLDivElement = newInformation(author, authorDiv)
  const scoreTag: HTMLDivElement = newInformation(score, versionDiv)

  parent.appendChild(stateTag)
  parent.appendChild(nameTag)
  parent.appendChild(authorTag)
  parent.appendChild(scoreTag)
  return stateTag
}
