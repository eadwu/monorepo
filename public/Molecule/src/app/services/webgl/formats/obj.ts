import { generateBuffer } from 'helpers'
import * as formats from './parse-format'

export default class OBJ {
  verts: string[] = []
  norms: string[] = []
  texs: string[] = []
  indices: string[] = []
  buffers: WebGLBuffer[] = []

  constructor (filePath = '', fileType = 'obj') {
    if (formats[fileType]) {
      formats[fileType].call(this, filePath)
    } else {
      console.log('Invalid filetype')
    }
  }
  /**
   * OBJ.initBuffers(webGL)
   * Creates the buffers for the OBJ mesh.
   * @param {WebGLRenderingContext} webGL - The WebGLRenderingContext interface
   * for OpenGL ES 2.0 for the HTML <canvas> element.
   */
  initBuffers (webGL: WebGLRenderingContext): void {
    this.buffers.push(
      generateBuffer(webGL, webGL.ARRAY_BUFFER, this.verts),
      generateBuffer(webGL, webGL.ARRAY_BUFFER, this.norms),
      generateBuffer(webGL, webGL.ARRAY_BUFFER, this.texs, 2),
      generateBuffer(webGL, webGL.ELEMENT_ARRAY_BUFFER, this.indices, 1, Uint16Array)
    )
  }
  /**
   * OBJ.deleteBuffers(webGL)
   * Deletes the buffers created by OBJ.initBuffers().
   * @param {WebGLRenderingContext} webGL - The WebGLRenderingContext interface
   * for OpenGL ES 2.0 for the HTML <canvas> element.
   */
  deleteBuffers (webGL: WebGLRenderingContext): void {
    for (let buffer = 0; buffer < this.buffers.length; buffer++) {
      webGL.deleteBuffer(this.buffers[buffer])
    }
  }
}
