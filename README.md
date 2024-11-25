# XIOS Migration Experiments

XIOS is an XML configured Input Output Server implementation for scientific computing file I/O interactions

This is an attempt to mirror the code base within Git, keeping the subversion history

### Local & MyMirror to manage SVN Synchronisation 

To set this up, then my `Local` is configured to use `git svn` and to communicate with the IPSL code management.

#### Initial Creation

> Note: you probably don't need to do this step unless you need the `svn` connector

Create a `Local` git repository and link this to the IPSL subversion.

```
git init XIOS-repository-migration-experiments

cd XIOS-repository-migration-experiments

git svn init -s --prefix=ipsl-XIOS2/ --ignore-paths="(test_client\.exe|xios_server\.exe|xios_server1\.exe|xios_server2\.exe)" https://forge.ipsl.fr/ioserver/svn/XIOS2/

git svn fetch
```

(Notes:
* `--ignore-paths` is essential, as
    * there are removed build artefacts in the subversion history which are large
    * and cause problems with the repository history, github uploads, etc.
* `git svn fetch` MUST complete first time, do not restart after failure!
    * this takes ages; sorry
    * if it fails, delete and start over, do not restart
    * `~/.subversion/servers` can be configured, for example: explicitly specifying the `http-proxy-{host/port}`  
    * once this has succeeded once, future `git svn fetch` should only try to update, and be fast
)


Note: at this stage, it is worth cross-checking that your Local IPSL branch history matches commit SHAs with the Mirror `svn-*` branches (if in doubt, ask)


#### Working with Branches

Subversion synchronisation involves working in a `Local` git repository.

Updates from IPSL Subversion are obtained, then any `Mirror` branches that are being managed are rebased using the svn content.

```
git svn fetch
git checkout svn-trunk
git rebase ipsl-XIOS2/trunk

git checkout XIOS2
git rebase ipsl-XIOS2/trunk
```

The rebase procedure inserts the commits from Subversion in a continuous stream, then applies any branch commits at the top (such as CI testing changes)

This ensures that the subversion change stream is maintain in sync, and any `Mirror` changes are only applied as addendums to the managed and synchronised code.

