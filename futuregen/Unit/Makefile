all:
	find . -name "*Unit" -exec echo "cd {}; make; cd .." > building.sh \;
	chmod +x building.sh
	./building.sh

clean:
	find . -name "*Unit" -exec echo "cd {}; make clean; cd .." > cleaning.sh \;
	chmod +x cleaning.sh
	./cleaning.sh
