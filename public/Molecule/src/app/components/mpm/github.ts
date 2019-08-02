import * as fs from 'fs'
import { join } from 'path'

import HTTPS from 'HTTPS'
import { loop, readDir, getCommitFromURL } from 'helpers'

const rm = fs.unlinkSync
const mkdir = fs.mkdirSync
const rmdir = fs.rmdirSync
const write = fs.writeFileSync

const parse = JSON.parse
const headers = {
  'User-Agent': 'Molecule',
  Accept: 'application/vnd.github.v3+json',
  'Content-Type': 'application/x-www-form-urlencoded'
}

export default {
  /**
   * FGit.queryGithub(string)
   * Searches Github's repositories to find packages for the application.
   * @param  {string}                 query - The search keywords.
   * @return {Promise<QueryResult[]>}       - Promise in which its callback
   * function's parameter is an instance of QueryResult[].
   */
  queryGithub: function (query: string): Promise<QueryResult[]> {
    return new Promise((resolve) => {
      let data: QueryResult[] = []
      HTTPS.GET('api.github.com/search/repositories', {
        q: `topic:molecule-package ${query}`
      }, headers)
        .then((response) => {
          const decodedResponse: GithubQuery = parse(response)
          const results = decodedResponse.items ? decodedResponse.items : []
          loop(results, 0, (result) => {
            data.push({ name: result.name, full_name: result.full_name, score: result.score })
          })
          resolve(data)
        })
        .catch((error) => { console.log(error)} )
    })
  },
  /**
   * FGit.retrieveSHA(repo[, branch])
   * Gets a SHA Key from the repository's ref.
   * @param  {string}          repo              - The target repository. Format
   * as :user/:repo.
   * @param  {string}          [branch='master'] - The target branch of the
   * repository.
   * @return {Promise<string>}                   - Promise with callback
   * parameter being the SHA key extracted.
   */
  retrieveSHA: function (repo: string, branch = 'master'): Promise<string> {
    return new Promise((resolve) => {
      HTTPS.GET(`api.github.com/repos/${repo}/git/refs/heads/${branch}`, '', headers)
        .then((response) => {
          const decodedResponse: GitRef = parse(response)
          const shaKey: string = decodedResponse.object.sha
          resolve(shaKey)
        })
        .catch((error) => { console.log(error)} )
    })
  },
  /**
   * FGit.fetchRepo(repo, string)
   * Gets all the files and subdirectories in a Github repository. Not meant for
   * truncated responses.
   * @param  {string}                    repo - The target repository. Format as
   * :user/:repo.
   * @param  {string}                    sha  - The SHA key retrieved by
   * FGit.retrieveSHA() or other means.
   * @return {Promise<RepositoryTree[]>}      - Promise with callback parameter
   * being an instance of RepositoryTree[].
   */
  fetchRepo: function (repo: string, sha: string): Promise<RepositoryTree[]> {
    return new Promise((resolve) => {
      HTTPS.GET(`api.github.com/repos/${repo}/git/trees/${sha}`, {
        recursive: 1
      }, headers)
        .then((response) => {
          const decodedResponse: GitTree = parse(response)
          resolve(decodedResponse.tree)
        })
        .catch((error) => { console.log(error)} )
    })
  },
  /**
   * FGit.cloneRepo(repo, branch, tree)
   * Clones a Github repository to build/packages/:package_name.
   * @param {string}           repo   - The target repository. Format as
   * :user/:repo.
   * @param {string}           branch - The target branch.
   * @param {RepositoryTree[]} tree   - The repository's tree retrieved by
   * FGit.fetchRepo() or other means.
   */
  cloneRepo: function (repo: string, branch: string, tree: RepositoryTree[]): void {
    const name: string = repo.substr(repo.indexOf('/') + 1)
    const root: string = join(process.env.PWD, 'build', 'packages', name)
    let fileList: string[] = []
    let directories: string[] = []

    mkdir(root)
    loop(tree, 0, (twig) => {
      if (twig.type == 'blob') {
        fileList.push(twig.path)
      } else if (twig.type == 'tree') {
        directories.push(twig.path)
      }
    })
    // TODO: Currently trusting the Github API returns subdirectories after the parents.
    loop(directories, 0, (directory) => { mkdir(`${root}/${directory}`) })
    loop(fileList, 0, (file) => {
      HTTPS.GET(`raw.githubusercontent.com/${repo}/${branch}/${file}`, '', headers)
        .then((response) => {
          write(`${root}/${file}`, response)
        })
        .catch((error) => { console.log(error) })
    })
  },
  /**
   * FGit.deleteRepo(name)
   * Deletes a repository that has been cloned to build/packages.
   * @param {string} name - The name of the repository.
   */
  deleteRepo: function (name: string): void {
    const root: string = join(process.env.PWD, 'build', 'packages', name)
    const directories: string[] = readDir(root, (filePath) => { rm(filePath) })

    loop(directories, 0, (directory) => {
      rmdir(directory)
    }, () => {
      rmdir(root)
    })
  },
  /**
   * FGit.currentCommit(repo, sha[, branch])
   * Gets the latest commit from the repository at Github.
   * @param  {string}          repo              - The target repository. Format
   * as :user/:repo.
   * @param  {string}          sha               - The SHA key retrieved by
   * FGit.retrieveSHA() or other means.
   * @param  {string}          [branch='master'] - The target branch of the
   * repository.
   * @return {Promise<string>}                   - Promise with callback
   * parameter being the extracted commit hash.
   */
  currentCommit: function (repo: string, sha: string, branch = 'master'): Promise<string> {
    return new Promise((resolve) => {
      HTTPS.GET(`api.github.com/repos/${repo}/git/commits/${sha}`, '', headers)
        .then((response) => {
          const decodedResponse: GitCommit = parse(response)
          const commit: string = getCommitFromURL(decodedResponse.url || decodedResponse.html_url)
          resolve(commit)
        })
    })
  },
   /**
    * FGit.currentVersion(repo)
    * Gets the current version from the repository at Github.
    * @param  {string}  repo - The target repository. Format as :user/:repo.
    * @return {Promise}      - Promise who's callback parameter is an array with
    * contains the version tag and whether or not it was a pre-release.
    */
   currentVersion: function (repo: string): Promise<[string, boolean]> {
     return new Promise((resolve) => {
       HTTPS.GET(`api.github.com/repos/${repo}/releases/latest`, '', headers)
        .then((response) => {
          const decodedResponse: RepositoryRelease = parse(response)
          const version: string = decodedResponse.name || decodedResponse.tag_name
          const isPreRelease: boolean = decodedResponse.prerelease
          resolve([version, isPreRelease])
        })
        .catch((error) => { console.log(error) })
     })
   }
}
