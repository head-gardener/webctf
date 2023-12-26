# WebCTF

Simple web-server serving CTF-like tasks. What follows is an overview of a possible
deployment file structure using `nix` and `bash` scripts.

## backup

<!-- :read ! ls webctf/backup/ | tail --!>
<!-- vip :s/.*/> \0\/ --!>

> data-26-10:11/
> data-26-10:16/
> data-26-10:21/
> data-26-10:26/
> data-26-10:31/
> data-26-10:36/
> data-26-10:41/
> data-26-10:46/
> data-26-10:51/
> data-26-10:56/

See [](#backup.sh) for details.

## backup.sh

```shell
while true; do
	dest="backup/$(date +data-%d-%H:%M)"
	cp runtime/data "$dest" -r
	chmod -w "$dest"
	echo "wrote $dest"

	sleep 300
done
```

Copy `data` dir to backup adding time to the name every 5 minutes.

## build.sh

```shell
nix-build -E 'with import <nixpkgs> {}; with haskellPackages; callPackage ./source/default.nix {}'
```

Wrapper around `nix-build`. Generates `result` symlink than contains binaries.

## log

<!-- vip :'<,'>s/\(.\{75\}\).*/> \1... --!>
> 20.197.51.98 - - [26/Dec/2023:20:13:14 +0300] "GET /.env HTTP/1.1" 404 111 ...
> 46.53.248.194 - - [26/Dec/2023:20:55:31 +0300] "GET / HTTP/1.1" 303 0 "" "M...
> 46.53.248.194 - - [26/Dec/2023:20:55:31 +0300] "GET /tasks HTTP/1.1" 200 - ...
> 46.53.248.194 - - [26/Dec/2023:20:55:31 +0300] "GET /static/0/styles.css?et...
> [Debug#SQL] SELECT "id", "name", "password", "pts", "solved" FROM "user" OR...
> 46.53.248.194 - - [26/Dec/2023:20:55:33 +0300] "GET /leaderboard HTTP/1.1" ...
> 46.53.248.194 - - [26/Dec/2023:20:55:34 +0300] "GET /about HTTP/1.1" 200 80...
> 192.241.239.36 - - [26/Dec/2023:21:14:56 +0300] "GET /ReportServer HTTP/1.1...
> 205.210.31.104 - - [26/Dec/2023:21:26:46 +0300] "GET / HTTP/1.1" 303 0 "" "...
> 205.210.31.104 - - [26/Dec/2023:21:26:46 +0300] "GET /tasks HTTP/1.1" 200 2...

A file that acts as a named pipe to which server's output is redirected.

## result

A symlink to server binaries. See [](#build.sh).

## runtime

> webctf/runtime/
> ├── common
> │   └── ...
> ├── data
> │   └── test.db3
> └── static
>     └── styles.css

`tmpfs` mounted directory containing static files and application data.

## start.sh

```shell
cd runtime

while true; do
	../result/bin/edge 8080 >> ../log 2>&1 &
	echo "reading for backup restore"
	while IFS= read -r line; do
		if [ -d "../backup/data-$line" ]; then
			echo "restoring $line"
			rm -r data/*
			cp ../backup/data-$line/* data/ -v
			kill %1
			break
		elif [ "$line" = "break" ]; then
			kill %1
			break
		fi
	done
done
```

Simple script that waits for `break` to restart the server or backup's time to roll back.
Depends on existence of [](#result).

## sync.sh

```shell
cp source/{common,static} runtime/ -rv
```

Shorthand to copy static files to [](#runtime).

## source

> ./webctf/source/
> ├── app
> │   ├── Application.hs
> │   ├── Entities.hs
> │   ├── Foundation.hs
> │   ├── Main.hs
> │   ├── Render.hs
> │   ├── Routers.hs
> │   └── routes
> ├── common
> │   └── ...
> ├── default.nix
> ├── LICENSE
> ├── manager
> │   └── Main.hs
> ├── package.yaml
> ├── shell.nix
> ├── src
> │   └── Data
> ├── static
> ├── webctf.cabal
> └── ...

Application sources. Built by `nix` with [](#build.sh).

### app

The server. Routers, handlers, etc., all here.

### common

Task files, like pictures or long text files.

### src

Task library. Contains task definitions, flags, tips, etc..

### static

Static stuff like CSS.

### package.yaml, shell.nix, webctf.cabal, default.nix

Dependency declaration, dev environment and such. The latter two files are auto-generated by
`hpack` and `cabal2nix`.

### Everything else

Code is the documentation. Most prominent packages used are `yesod`, `yesod-auth-hashdb` and `persistent`.