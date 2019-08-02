import * as fs from 'fs'
import { loop } from 'helpers'

const read: Function = fs.readFileSync
const exists: Function = fs.existsSync

let objFuncs: OBJFormats = {'v': undefined, 'vn': undefined, 'vt': undefined, 'f': undefined}
let indice: number = 0
/**
 * objFuncs.v(data, array)
 * Parsing instructions for the `v` element for OBJ files.
 * @param {string[]} data  - The data to be added to the array.
 * @param {OBJData}  - The root array to push the contents to.
 */
objFuncs['v'] = function(data: string[], array: OBJData): void {
  array.verts.push(data)
}
objFuncs['vn'] = function(data: string[], array: OBJData): void {
  array.norms.push(data)
}
objFuncs['vt'] = function(data: string[], array: OBJData): void {
  array.texs.push(data)
}
objFuncs['f'] = function(data: string[], array: OBJData): void {
  loop.call(this, data, 0, function(currentElement) {
    let siftContents: string[] = currentElement.split(/\/+/) // Split a/b to [a, b] and c//d to [c, d]
    let siftDefined: number = array.indices[currentElement] // Check if cached value exists
    let hasTexture: boolean = array.texs.length > 0 && siftContents.length == 3 // Checks to see if textures need be defined. Format is vert[, texture], normal

    if (siftDefined && siftDefined !== undefined) {
      this.indices.push(array.indices[currentElement]) // If already done get cached value and use it
    } else {
      let vertArr: string[] = array.verts[+siftContents[0] - 1] // -1 index correction
      let texArr: string[] = array.texs[+siftContents[1] - 1]
      let normArr: string[] = array.norms[+siftContents[2] - 1] || array.norms[+siftContents[1] - 1]

      this.verts.push(+vertArr[0], +vertArr[1], +vertArr[2])
      this.norms.push(+normArr[0], +normArr[1], +normArr[2])
      if (hasTexture) {
        this.texs.push(+texArr[0], +texArr[1])
      }
      array.indices[currentElement] = indice
      this.indices.push(indice)
      ++indice
    }
  })
}
/**
 * obj(filePath)
 * Function to parse an OBJ file.
 * @param {string} filePath - The path to the OBJ file.
 */
function obj (filePath: string): void {
  if (exists(filePath)) {
    let temp: OBJData = {verts: [], norms: [], texs: [], indices: {}}
    let fileContents: string = read(filePath, "utf8")
    let siftedFile: string[] = fileContents.split(/\n/g) // OBJ files has one definition a line

    loop.call(this, siftedFile, 0, function(currentLine) {
      if (currentLine && currentLine !== undefined) {
        let lineElements: string[] = currentLine.split(/\s+/g) // Split "v 1 1 1" to [v, 1, 1, 1].
        let lineDefinition: string = (lineElements.splice(0, 1) || []).toString()

        if (lineDefinition && objFuncs[lineDefinition]) {
          objFuncs[lineDefinition].call(this, lineElements, temp)
        }
      }
    })
  }
}
/**
 * json(filePath[, format])
 * Function for extremely limited JSON parsing support.
 * @param {string}  filePath - The path to the JSON file.
 * @param {Object=} format   - Optional argument which specifies the format of
 * the JSON file if it isn't the one(s) provided.
 */
function json (filePath: string, format?: Object): void {
  if (exists(filePath)) {
    let fileContents: string = read(filePath, 'utf8')
    let parsedContents = JSON.parse(fileContents)

    this.verts = parsedContents.vertices || parsedContents.format[0]
    this.norms = parsedContents.normals || parsedContents.format[1]
    this.texs = parsedContents.textures || parsedContents.format[2]
    this.indices = parsedContents.indices || parsedContents.format[3]
  }
}

export default {
  obj: obj,
  json: json
}
