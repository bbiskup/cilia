run-cilia2:
	docker-compose run --rm  cilia2

# Uses dockerize Python package (https://github.com/larsks/dockerize)
create-minimal-docker-container:
	dockerize --tag cilia-minimal -a /etc/protocols /etc -a /etc/services /etc -a /etc/ssl/certs /etc/ssl -a /etc/terminfo /etc -a /lib/terminfo /lib -a /usr/share/terminfo /usr/share -a /lib/x86_64-linux-gnu/libresolv-2.23.so /lib/x86_64-linux-gnu/libresolv.so.2  -a $$PWD/dist/build/cilia/cilia  /bin/cilia /bin/cilia

run-minimal-docker-container:
	docker run -ti -eTERM=xterm  -v$$PWD/cilia.yml:/root/cilia.yml --rm cilia-minimal
