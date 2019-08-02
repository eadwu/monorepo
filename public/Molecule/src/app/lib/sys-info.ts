import * as os from 'os'

let tick: number = 0

function updateSystemStats () {
  return {
    // CPU
    cpus: os.cpus(),
    cpuArchitecture: os.arch(),
    // Memory
    freeMemory: os.freemem(),
    totalMemory: os.totalmem(),
    // Network
    networkInterfaces: os.networkInterfaces(),
    // Misc
    uptime: os.uptime(),
    user: os.userInfo({encoding: 'username'}),

    platform: os.platform(),
    release: os.release(),
    osType: os.type(),

    homeDirectory: os.homedir(),
    hostName: os.hostname()
  }
}

export default class SystemStats {
  links: {}
  caches: {}
  updateSystemStats: Function

  constructor () {
    this.updateSystemStats = updateSystemStats
    requestAnimationFrame(this.onUpdate)
  }

  ln (source: string, target: string): void {
    if (!this.links[target]) {
      this.links[target] = source
    }
  }

  unlink (target: string): void {
    if (this.links[target]) {
      delete this.links[target]
    }
  }

  getStat (key: string): any {
    return this.caches[key] || this.caches[this.links[key]]
  }

  private onUpdate (timestamp: number): void {
    if (timestamp > tick + 1000) {
      this.caches = this.updateSystemStats()
    }
    tick = timestamp
    requestAnimationFrame(this.onUpdate)
  }
}
