type WebGLContext = WebGLRenderingContext | CanvasRenderingContext2D
type ShaderEntry = [number, string]
type ShaderLayout = { [type: number]: string }
type UniformEntryNormal = [string, [string, number, number]]
type UniformEntryException = [string, [string]]
type UniformEntry = UniformEntryNormal | UniformEntryException
type UniformLayoutNormal = [string, number, number]
type UniformLayoutException = [string, number]
type UniformLayout = { [method: string]: UniformLayoutNormal | UniformLayoutException }

import OBJ from './formats/obj'
import VIDEO from './formats/video'
import AUDIO from './formats/audio'

import { loop, newElement } from 'helpers'

export default class WebGLHandler {
  webgl?: WebGLContext
  canvas: HTMLCanvasElement

  OBJ = OBJ
  VIDEO = VIDEO
  AUDIO = AUDIO
  /**
   * new WebGLHandler([, canvas[, contextType]])
   * WebGLHandler constructor function.
   * @param  {HTMLCanvasElement=} canvas         - A HTMLCanvasElement where the
   * WebGL should do stuff in.
   * @param  {string=}            contextType '' - The contextType to use in
   * HTMLCanvasElement.getContext().
   * @return {WebGLHandler}                      - A WebGLHandler object.
   */
  constructor (canvas?: HTMLCanvasElement, contextType?: string) {
    if (canvas) { this.canvas = canvas } else {
      const newCanvas: HTMLElement = newElement('canvas', '')
      if (newCanvas instanceof HTMLCanvasElement) {
        this.canvas = newCanvas
        document.body.appendChild(newCanvas)
      } else console.error(`HTMLCanvasElement This type is incompatible with ${newCanvas.constructor.name}`)
    }
    this.webgl = this.canvas.getContext(contextType || 'webgl2')
    if (!this.webgl) console.error('WebGL is incompatible with the current version of Chromium')
  }
  /**
   * WebGLHandler.setUniforms(webgl, webglProgram, layout)
   * Helper function to set the values of uniform values given a UniformLayout.
   * @param  {WebGLRenderingContext} webgl        - The WebGLRenderingContext
   * obtained from HTMLCanvasElement.getContext().
   * @param  {WebGLProgram}          webglProgram - The WebGLProgram constructed
   * from the vertex and/or fragment shaders.
   * @param  {UniformLayout}         layout       - A dictionary that the method
   * of the WebGLRenderingContext to invoke as its key/index and in which its
   * value is an Array that contains the arguments to invoke the function with.
   */
  setUniforms (webgl: WebGLRenderingContext, webglProgram: WebGLProgram, layout: UniformLayout) {
    loop(Object.entries(layout), 0, (entry: UniformEntry) => {
      const [method, data] = entry
      const [uniform, ...arg] = data
      const uniformLocation: WebGLUniformLocation = webgl.getUniformLocation(webglProgram, uniform)
      const param = [uniformLocation].concat(arg)
      if (webgl[method]) { webgl[method].apply(webgl, param) }
    })
  }
  /**
   * WebGLHandler.constructProgram(webgl, layout)
   * Constructs a WebGLProgram given the ShaderLayout in the parameters.
   * @param  {WebGLRenderingContext} webgl  - The WebGLRenderingContext obtained
   * from HTMLCanvasElement.getContext().
   * @param  {ShaderLayout}          layout - A dictionary that contains the
   * type of the shader for gl.createShader() as its key/index and then the
   * source code of the shader as its value.
   * @return {WebGLProgram|void}            - If constructed successfully, the
   * WebGLProgram object will be returned.
   */
  constructProgram (webgl: WebGLRenderingContext, layout: ShaderLayout): WebGLProgram | void {
    const newProgram: WebGLProgram = webgl.createProgram()
    loop(Object.entries(layout), 0, (entry: ShaderEntry) => {
      const [type, data] = entry
      const newShader: WebGLShader = webgl.createShader(type)
      webgl.shaderSource(newShader, data)
      webgl.compileShader(newShader)
      if (webgl.getShaderParameter(newShader, webgl.COMPILE_STATUS)) {
        webgl.attachShader(newProgram, newShader)
      } else webgl.deleteShader(newShader)
    })
    webgl.linkProgram(newProgram)
    if (!webgl.getProgramParameter(newProgram, webgl.LINK_STATUS)) {
      webgl.deleteProgram(newProgram)
    } else return newProgram
  }
}
