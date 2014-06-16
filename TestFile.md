##TestFile
========

Testing sending and receiving files from and to remote repository

In order to synchonize local and remote repository, I had to:
* git fetch
* git merge FETCH_HEAD
* git push


Adding files to repository
--------------------------
* git add .  adds all new files
* git add -u updates tracking for files that changed names or were deleted
* git add -A does both of the previous

* git commit -m "<message>"

* git push
