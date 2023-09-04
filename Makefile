install-mac:
	chmod +x ./mac-install.sh
	./mac-install.sh
	rScript install.r

run:
	rScript bus-time.r