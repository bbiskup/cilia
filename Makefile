REPO_BUILD_TAG=bbiskup/cilia:$(TRAVIS_BRANCH)-build-$(TRAVIS_BUILD_NUMBER) 
REPO_LATEST_TAG=bbiskup/cilia:latest

bash:
	./scripts/docker-cmd.sh bash

run-eventlog:
	./scripts/docker-cmd.sh cabal run -- -c cilia.yml +RTS -l

build:
	./scripts/docker-cmd.sh "cabal sandbox init ; cabal update &&  cabal install --only-dependencies --enable-test && cabal configure --enable-test && cabal build"

test:
	./scripts/docker-cmd.sh "cabal test"	

# Uses dockerize Python package (https://github.com/larsks/dockerize)
# additional files required because:
# - without libresolv: "no such protocl: tcp" error
# - without ssl certs: handshake failure 
create-minimal-docker-container:
	./scripts/docker-cmd.sh strip /home/cilia/dist/build/cilia/cilia -o /home/cilia/dist/build/cilia/cilia-stripped
	./scripts/docker-cmd.sh dockerize --tag cilia-minimal \
		-a /lib/terminfo /lib/ \
		-a /etc/protocols /etc/ \
		-a /etc/ssl/certs /etc/ssl/ \
		-a /lib/x86_64-linux-gnu/libresolv-2.19.so /lib/x86_64-linux-gnu/libresolv.so.2 \
		-a /home/cilia/dist/build/cilia/cilia-stripped  /bin/cilia /bin/cilia


run-minimal-docker-container:
	docker run -ti -eTERM=$$TERM  -v$$PWD/cilia.yml:/root/cilia.yml --rm cilia-minimal

# To be run on Travis CI server
docker-push:
	echo "hier $(REPO_BUILD_TAG)"
	docker tag cilia-minimal $(REPO_BUILD_TAG)
	docker tag cilia-minimal $(REPO_LATEST_TAG)
	docker push  $(REPO_BUILD_TAG)
	docker push  $(REPO_LATEST_TAG)

build-debian-package:
	sudo chown -R root:root debian-build
	sudo mkdir -p debian-build/dist
	sudo mkdir -p debian-build/cilia/etc/skel
	sudo mkdir -p debian-build/cilia/usr/bin
	sudo cp dist/build/cilia/cilia debian-build/cilia/usr/bin/
	sudo cp examples/cilia.yml debian-build/cilia/etc/skel/
	sudo dpkg-deb --build debian-build/cilia
	sudo mv debian-build/cilia*.deb debian-build/dist
