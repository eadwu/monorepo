import HTTPS from 'HTTPS'
import { loop, newElement } from 'helpers'

const root: HTMLBodyElement = document.querySelector('body')

export default class AUDIO { // TODO: Expand for audio manipulation using HTML Audio API
  audioContext: AudioContext
  audioElement: HTMLAudioElement
  nodes: Array<AudioNode>
  /**
   * AUDIO constructor function.
   * new AUDIO(path, type[, attr])
   * @param  {string}         path - The path to the audio file.
   * @param  {string}         type - The type of format the audio file is in.
   * @param  {Array<string>=} attr - A list of attributes to put on the HTML
   * audio element.
   */
  constructor (path: string, type: string, attr?: Array<string>) {
    this.audioContext = new AudioContext()
    const audioEle: HTMLAudioElement = newElement('audio', '')
    const audioSource: HTMLSourceElement = newElement('source', '')
    if (attr) {
      loop(attr, 0, (attribute) => {
        audioEle.setAttribute(attribute, '')
      })
    }
    audioSource.setAttribute('src', path)
    audioSource.setAttribute('type', type)
  }
  /**
   * Plays the audio.
   */
  exec () {
    this.audioElement.play()
  }
}
/*
function init() {
  // Fix up prefixing
  window.AudioContext = window.AudioContext || window.webkitAudioContext;
  context = new AudioContext();

  bufferLoader = new BufferLoader(
    context,
    [
      '../sounds/hyper-reality/br-jam-loop.wav',
      '../sounds/hyper-reality/laughter.wav',
    ],
    finishedLoading
    );

  bufferLoader.load();
}

function finishedLoading(bufferList) {
  // Create two sources and play them both together.
  var source1 = context.createBufferSource();
  var source2 = context.createBufferSource();
  source1.buffer = bufferList[0];
  source2.buffer = bufferList[1];

  source1.connect(context.destination);
  source2.connect(context.destination);
  source1.start(0);
  source2.start(0);
} */
