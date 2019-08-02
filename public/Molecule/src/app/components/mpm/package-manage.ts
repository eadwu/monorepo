import FGit from './github'

export default {
  /**
   * MPM.search(keywords)
   * FGit.queryGithub() in MPM. Refer to FGit.queryGithub() for more
   * information.
   */
  search: FGit.queryGithub,
  /**
   * MPM.delete(name)
   * FGit.deleteRepo() in MPM. Refer to FGit.deleteRepo() for more
   * information.
   */
  delete: FGit.deleteRepo,
  /**
   * MPM.update(package_name)
   * Updates/Refreshes the repository with name `package_name`.
   * @param {string} package_name - The package name formatted as :user/:repo.
   */
  update: function (package_name: string): void {
    const name: string = package_name.substr(package_name.indexOf('/') + 1)

    FGit.deleteRepo(name)
    this.download(package_name)
  },
  /**
   * MPM.download(package_name)
   * Downloads the repository with name `package_name`.
   * @param {string} package_name - The package name formatted as :user/:repo.
   */
  download: function (package_name: string): void {
    FGit.retrieveSHA(package_name)
      .then((sha) => {
        FGit.fetchRepo(package_name, sha)
          .then((package_tree) => {
            FGit.cloneRepo(package_name, 'master', package_tree)
          })
      })
  }
}
