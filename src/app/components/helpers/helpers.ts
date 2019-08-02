import { join } from 'path'
import { statSync, readdirSync } from 'fs'
/**
 * promisifiedLoop(object, index, func[, callback])
 * Synchronous loop for asynchronous functions within the loop achieved with
 * promises which preserves the scope of the original function.
 * @param {any[]}     object   - The array in which the function will loop.
 * @param {number}    index    - The index to begin the loop on.
 * @param {Function}  func     - Function to execute on every item on the array.
 * The parameter is the item at object[index].
 * @param {Function=} callback - Callback for the loop that executes when
 * finished. The parameter is the array or the parameter object.
 */
export function promisifiedLoop (object: any[], index: number, func: Function, callback?: Function): void {
  new Promise (function (resolve, reject) {
    resolve(func.call(this, object[index]))
  }.bind(this)).then(function () {
    index < object.length - 1 ? promisifiedLoop.call(this, object, ++index, func)
    : callback ? callback(object): () => {}
  }.call(this))
}
/**
 * loop(object, index, func[, callback])
 * Default numerical loop with better syntax in my opinion. For parameter
 * description, refer to promisifiedLoop()
 */
 export function loop (object: any[], index: number, func: Function, callback?: Function): void {
   for (let i = index, end = object.length; i < end; i++) func(object[i])
   if (callback) callback(object)
 }
/**
 * newElement(tag, classes[, id])
 * Helper function to create a new element to be added to the HTML data.
 * @param  {string}        tag     - The tag name of the HTML element to create.
 * @param  {Array<string>} classes - The class(es) the HTML element should have.
 * Basically edits the class attribute.
 * @param  {string=}       id      - If an id is passed, the HTML element's id
 * attribute is `id`.
 * @return {HTMLElement}           - The HTML element with the configuration of
 * the parameters.
 */
export function newElement (tag: string, classes: string | string[], id?: string): HTMLElement {
  const newHTMLElement: HTMLElement = document.createElement(tag)
  newHTMLElement.setAttribute('class', typeof (classes) === 'string' ? classes : classes.join(' '))
  if (id) newHTMLElement.setAttribute('id', id)
  return newHTMLElement
}
/**
 * generateBuffer(webGL, type, data[, contentSize[, dataType]])
 * Function to generate a buffer with the specified options.
 * @param  {WebGLRenderingContext} webGL           - The RenderingContext that
 * was returned from `HTMLCanvasElement.getContext`.
 * @param  {GLenum}                type            - A GLenum that specifies the
 * kind of buffer it should be.
 * @param  {string[]}              data            - An array of strings that
 * contain the data to put into the buffer.
 * @param  {number}                [contentSize=3] - A number that specifies the
 * about of values a single value consumes. Defaults to 3.
 * @param  {any=}                  dataType        - The array type the buffer
 * should be
 * @return {WebGLBuffer}                           - The buffer created with the
 * options given in the arguments.
 */
export function generateBuffer (webGL: WebGLRenderingContext, type: GLenum, data: string[], contentSize = 3, dataType?: any): WebGLBuffer {
  const buffer: WebGLBuffer = webGL.createBuffer()
  webGL.bindBuffer(type, buffer)
  webGL.bufferData(type, new (dataType || Float32Array)(data), webGL.STATIC_DRAW)
  buffer.contentSize = contentSize
  buffer.contentLength = data.length / contentSize
  webGL.bindBuffer(type, null)
  return buffer
}
/**
 * getCommitFromURL(commitURL)
 * Extracts the commit from the given URL. To be used with FGit.currentCommit()
 * or within a function that supports this extraction.
 * @param  {string} commitURL - The URL to extract the commit from.
 * @return {string}           - The commit from the given URL.
 */
export function getCommitFromURL (commitURL: string): string {
  const reversedURL: string = commitURL.split('').reverse().join()
  const extractedCommit: string = reversedURL.substr(0, reversedURL.indexOf('/'))
  const commit: string = extractedCommit.split('').reverse().join()
  return commit
}
/**
 * readDir(root, invoke)
 * An enchanced version or fs.readDir which calls a function(invoke) on all
 * files, even those within its subdirectories.
 * @param  {string}   root             - The directory to extract files from.
 * @param  {Function} invoke           - Function to be executed on every file.
 * The parameter is the file's path.
 * @param  {string[]} [directories=[]] - Array to store directory paths.
 * Defaults to [].
 * @return {string[]}                  - Array that contains the subdirectories.
 */
export function readDir (root: string, invoke: Function, directories = []): string[] {
  const rootContents = readdirSync(root)
  if (rootContents.length > 0) {
    promisifiedLoop(rootContents, 0, (object) => {
      const filePath = join(root, object)
      if (statSync(filePath).isDirectory) {
        directories.unshift(filePath)
        readDir(filePath, invoke, directories)
      } else invoke(filePath)
    })
  }
  return directories
}
