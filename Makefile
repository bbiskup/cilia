REPO_BUILD_TAG=bbiskup/cilia-minimal:$(TRAVIS_BRANCH)-build-$(TRAVIS_BUILD_NUMBER) 
REPO_LATEST_TAG=bbiskup/cilia-minimal:latest

bash:
	./scripts/docker-cmd.sh bash

# create-minimal-docker-container:
# 	dockerize --tag cilia-minimal \
# 		-a /lib/terminfo /lib \
# 		-a /etc/protocols /etc \
# 		-a /etc/ssl/certs /etc/ssl \
# 		-a /lib/x86_64-linux-gnu/libresolv-2.23.so /lib/x86_64-linux-gnu/libresolv.so.2  \
# 		-a $$PWD/dist/build/cilia/cilia  /bin/cilia /bin/cilia

# Uses dockerize Python package (https://github.com/larsks/dockerize)
# additional files required because:
# - without libresolv: "no such protocl: tcp" error
# - without ssl certs: handshake failure 
create-minimal-docker-container:
	scripts/docker-cmd.sh dockerize --tag cilia-minimal \
		-a /lib/terminfo /lib/ \
		-a /etc/protocols /etc/ \
		-a /etc/ssl/certs /etc/ssl/ \
		-a /lib/x86_64-linux-gnu/libresolv-2.19.so /lib/x86_64-linux-gnu/libresolv.so.2 \
		-a $PWD/dist/build/cilia/cilia  /bin/cilia /bin/cilia


run-minimal-docker-container:
	docker run -ti -eTERM=$$TERM  -v$$PWD/cilia.yml:/root/cilia.yml --rm cilia-minimal

# To be run on Travis CI server
docker-push:
	echo "hier $(REPO_BUILD_TAG)"
	docker tag cilia-minimal $(REPO_BUILD_TAG)
	docker tag cilia-minimal $(REPO_LATEST_TAG)
	docker push  $(REPO_BUILD_TAG)
	docker push  $(REPO_LATEST_TAG)

tmptarget:
	echo "a: $(a)"
