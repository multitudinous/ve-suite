all:
	find . -name "*Unit" -exec echo "cd {}; make; cd .." > building.sh \;
	chmod +x building.sh
	./building.sh
#	find . -perm +001 -and -not -type d -and -not -name "*.sh*" -printf "echo STARTING %f -----\n%p -ORBInitRef NameService=file:///tmp/ns.ior &\nsleep 2\n" > start.sh
	find . -perm +001 -and -not -type d -and -not -name "*.sh*" -printf "echo STARTING %f -----\n%p &\nsleep 2\n" > start.sh
	find . -perm +001 -and -not -type d -and -not -name "*.sh*" -printf "killall -9 %f\n" > end.sh
	chmod +x start.sh
	chmod +x end.sh

clean:
	find . -name "*.o" -exec rm {} \;
	find . -name "*Unit" -exec echo "cd {}; make clean; cd .." > cleaning.sh \;
	chmod +x cleaning.sh
	./cleaning.sh
	rm -f cleaning.sh
	rm -f building.sh
	rm -f start.sh
	rm -f end.sh
