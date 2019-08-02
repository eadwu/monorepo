type VBufferEntry = [string, [GLenum, any, GLenum]]
type VBufferLayout = { [attrib: string]: [GLenum, any, GLenum] }

import { loop, newElement } from 'helpers'

export default class VIDEO {
  fps: number
  video: HTMLVideoElement
  buffers: { [key: string]: WebGLBuffer }
  /**
   * VIDEO constructor function.
   * @param  {string=}           source '' - A string defining the path to the source video element.
   * @param  {number=}           fps    30 - How much times the video should be rendered per second.
   * @param  {HTMLVideoElement=} video     - A HTMLVideoElement for the class to do its stuff in.
   * @return {VIDEO}                       - A VIDEO object.
   */
  constructor (source?: string, fps?: number, video?: HTMLVideoElement) {
    this.fps = fps || 30
    this.buffers = {}
    if (video) { this.video = video } else {
      const newVideo: HTMLElement = newElement('video', '')
      newVideo.setAttribute('loop', '')
      newVideo.setAttribute('preload', 'auto')
      newVideo.setAttribute('crossorigin', 'anonymous')
      newVideo.setAttribute('src', source)
      if (newVideo instanceof HTMLVideoElement) {
        this.video = newVideo
        document.body.appendChild(newVideo)
      } else console.error(`HTMLVideoElement This type is incompatible with ${newVideo.constructor.name}`)
    }
    if (!this.video) console.error(`Value expected, got null`)
  }
  /**
   * VIDEO.initBuffers(webgl, program, layout)
   * Creates WebGLBuffer(s) which is stored in this.buffers.
   * @param  {WebGLRenderingContext} webgl   - The WebGLRenderingContext
   * obtained from HTMLCanvasElement.getContext().
   * @param  {WebGLProgram}          program - The WebGLProgram constructed from
   * the vertex and/or fragment shaders.
   * @param  {VBufferLayout}         layout  - A dictionary in which the
   * key/index is the attrib name and its value is an Array which contains the
   * arguments to pass to gl.bufferData().
   */
  initBuffers (webgl: WebGLRenderingContext, program: WebGLProgram, layout: VBufferLayout) {
    const _this = this
    loop(Object.entries(layout), 0, (entry: VBufferEntry) => {
      const [attrib, data] = entry
      const [type, src, usage] = data
      const attribLocation = webgl.getAttribLocation(program, attrib)
      const newBuffer = _this.buffers[attrib] = webgl.createBuffer()
      webgl.bindBuffer(type, newBuffer)
      if (src && usage) webgl.bufferData(type, src, usage)
      webgl.enableVertexAttribArray(attribLocation)
      webgl.vertexAttribPointer(attribLocation, 2, webgl.FLOAT, false, 0, 0)
    })
  }
  /**
   * VIDEO.updateTexture(webgl, source[, texture])
   * Helper function that updates the current texture to the correct frame.
   * @param  {WebGLRenderingContext} webgl   - The WebGLRenderingContext
   * obtained from HTMLCanvasElement.getContext().
   * @param  {HTMLVideoElement}      source  - The HTMLVideoElement that
   * contains the video.
   * @param  {WebGLTexture=}         texture - A WebGLTexture to set the texture
   * on if it is already created.
   */
  updateTexture (webgl: WebGLRenderingContext, source: HTMLVideoElement, texture?: WebGLTexture) {
    const targetTexture: WebGLTexture = texture || webgl.createTexture()
    webgl.bindTexture(webgl.TEXTURE_2D, targetTexture)
    webgl.pixelStorei(webgl.UNPACK_FLIP_Y_WEBGL, true)
    webgl.texImage2D(webgl.TEXTURE_2D, 0, webgl.RGBA, webgl.RGBA, webgl.UNSIGNED_BYTE, source)
    webgl.generateMipmap(webgl.TEXTURE_2D)
  }
}
