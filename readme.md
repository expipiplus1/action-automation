# Some helpful actions for Haskell projects

Probably useful for non-Haskell projects too.

## Tag Latest Release

This should be run on pushes to the main branch.

It will walk back along this branch and detect the commit which changed the
`version` field in `package.yaml` (or a .cabal file). It will tag this commit
as `v$version` and push the tags.

It can be configured to look for several `package.yaml` files and to use
different tag prefixes for each one with the `tagsAndFiles` option. This takes
lines of the form `tag-prefix path/to/package.yaml`.

The repository much be checked out to at least the depth required to find the
most recent change to the `version` attribute. It must also be checked out so
that `git push --tags` succeeds.

If you want these tags to trigger further actions then you'll need to clone the
repository with different credentials than `GITHUB_TOKEN` as actions performed
using this token do not trigger further events.

Example config:

```yaml
name: Tag Latest Release

on:
  push:
    branches:
    - master

jobs:
  update:
    runs-on: ubuntu-20.04
    steps:
    - uses: cachix/install-nix-action@v12
      with:
        nix_path: nixpkgs=channel:nixos-unstable
    - uses: actions/checkout@v2
      with:
        fetch-depth: 0 # Fetch everything
        ssh-key: ${{ secrets.DEPLOY_KEY }}
    - uses: expipiplus1/action-automation/tag-latest-release@HEAD
      with:
        tagsAndFiles: |
          v package.yaml
          other-v second/package.yaml
          third-v third/package.yaml

```

## Bump Version

This action will

- Update the `version` attribute in `package.yaml` files
- Label WIP changes in the changelog
- Commit these changes with a message mirroring the changelog contents.

Changelogs must be in a file named `changelog.md` alongside `package.yaml` and
have the format:

```markdown
...1
## WIP

foo...

## ...2
```

They will updated to

```markdown
...1
## WIP

## $version - [$date]

foo...

## ...2
```

This action takes two parameters:

- `packageInfos`: lines comprising `package-name tag-prefix directory`
- `packageVersions`: JSON matching `{ package1: version1, package2: version2, ... }`
  where the values can be one of: `patch`, `minor`, `major`, `supermajor` or an
  explicit version number.

The suggested method of use is with [slash command
dispatch](https://github.com/marketplace/actions/slash-command-dispatch).

An example command workflow:

```yaml
name: bump-command

on:
  repository_dispatch:
    types: [bump-command]

jobs:
  bump:
    runs-on: ubuntu-20.04
    steps:
      - uses: cachix/install-nix-action@v12
        with:
          nix_path: nixpkgs=channel:nixos-unstable

      # Checkout the pull request branch
      - uses: actions/checkout@v2
        with:
          ssh-key: ${{ secrets.DEPLOY_KEY }}
          repository: ${{ github.event.client_payload.pull_request.head.repo.full_name }}
          ref: ${{ github.event.client_payload.pull_request.head.ref }}
          submodules: recursive

      - uses: expipiplus1/action-automation/bump-version@HEAD
        with:
          packageInfos: |
            first v .
            second other-v second
            third third-v third
          packageVersions: |
            ${{ toJson(github.event.client_payload.slash_command.args.named) }}

      - run: |
          git push origin HEAD:${{ github.event.client_payload.pull_request.head.ref }}
```

This will allow one to comment, for example, `/bump first=major third=minor` on
a pull request to add a commit which bumps the package in directory `.` a major
version and the package in `third/` by a minor version.

-----------

## Setup instructions:

- Either create a PAT and use that with slash command dispatch, or for more
  restricted permissions use a GitHub app. and `tibdex/github-app-token` to
  disptach slash commands. I use
  https://github.com/settings/apps/threeoftwelve/installations

- Create a deploy key for the repo:
  `ssh-keygen -t ed25519 -f exact-real-deploy -C github.deploy@monoid.al`
  https://github.com/expipiplus1/exact-real/settings/keys

- Add the deploy private key to secrets

- Add `bump-command.yml` and set up the `packageInfos` argument.

- Add `tag-release.yml` and optionally set up the `tagsAndFiles` argument
