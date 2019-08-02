interface OBJFormats {
  v: Function,
  vn: Function,
  vt: Function,
  f: Function
}

interface OBJData {
  verts: string[][],
  norms: string[][],
  texs: string[][],
  indices: {
    [name: string]: number
  }
}

interface WebGLBuffer {
  contentSize: number,
  contentLength: number
}

interface QueryResult {
  name: string,
  full_name: string,
  score: number
}

interface GithubUser {
  login: string,
  id: number,
  avatar_url: string,
  gravatar_id: string,
  url: string,
  html_url: string,
  followers_url: string,
  following_url: string,
  gists_url: string,
  starred_url: string,
  subscriptions_url: string,
  organizations_url: string,
  repos_url: string,
  events_url: string,
  received_events_url: string,
  type: string,
  site_admin: boolean
}

interface GithubQuery {
  total_count: number,
  incomplete_results: boolean,
  items: Array<{
    id: number,
    name: string,
    full_name: string,
    owner: GithubUser,
    private: boolean,
    html_url: string,
    description: string,
    fork: boolean,
    url: string,
    forks_url: string,
    keys_url: string,
    collaborators_url: string,
    teams_url: string,
    hooks_url: string,
    issue_events_url: string,
    events_url: string,
    assignees_url: string,
    branches_url: string,
    tags_url: string,
    blobs_url: string,
    git_tags_url: string,
    git_refs_url: string,
    trees_url: string,
    statuses_url: string,
    languages_url: string,
    stargazers_url: string,
    contributors_url: string,
    subscribers_url: string,
    subscription_url: string,
    commits_url: string,
    git_commits_url: string,
    comments_url: string,
    issue_comment_url: string,
    contents_url: string,
    compare_url: string,
    merges_url: string,
    archive_url: string,
    downloads_url: string,
    issues_url: string,
    pulls_url: string,
    milestones_url: string,
    notifications_url: string,
    labels_url: string,
    releases_url: string,
    deployments_url: string,
    created_at: string,
    updated_at: string,
    pushed_at: string,
    git_url: string,
    ssh_url: string,
    clone_url: string,
    svn_url: string,
    homepage: string,
    size: number,
    stargazers_count: number,
    watchers_count: number,
    language: string,
    has_issues: boolean,
    has_projects: boolean,
    has_downloads: boolean,
    has_wiki: boolean,
    has_pages: boolean,
    forks_count: number,
    mirror_url: any,
    open_issues_count: number,
    forks: number,
    open_issues: number,
    watchers: number,
    default_branch: string,
    score: number
  }>
}

interface RepositoryRelease {
  url: string,
  assets_url: string,
  upload_url: string,
  html_url: string,
  id: number,
  tag_name: string,
  target_commitish: string,
  name: string,
  draft: boolean,
  author: GithubUser,
  prerelease: false,
  created_at: string,
  published_at: string,
  assets: [{
    url: string,
    id: number,
    name: string,
    label: string,
    uploader: GithubUser,
    content_type: string,
    state: string,
    size: number,
    download_count: number,
    created_at: string,
    updated_at: string,
    browser_download_url: string
  }],
  tarball_url: string,
  zipball_url: string,
  body: string
}

interface GitRef {
  ref: string,
  url: string,
  object: {
    sha: string,
    type: string,
    url: string
  }
}

interface GitCommit {
  sha: string,
  url: string,
  html_url: string,
  author: {
    name: string,
    email: string,
    date: string
  },
  committer: {
    name: string,
    email: string,
    date: string
  },
  tree: {
    sha: string,
    url: string
  },
  message: string,
  parents: Array<{
    sha: string,
    url: string,
    html_url: string
  }>
}

interface GitTree {
  sha: string,
  url: string,
  tree: RepositoryTree[],
  truncated: boolean
}

interface RepositoryTree {
  path: string,
  mode: string,
  type: string,
  size: number,
  sha: string,
  url: string
}
