import sys
import popen2

nodes = ["WRXRE38570N6F"]

# The double quoting here (""...."") is to get the second interpretation
# of a quoted command as the argument to CMD.EXE.

master_application = '"C:\\Program Files\\VE_Suite.0.9.3\\bin\\assemblyMaster.bat"'
slave_application = '"C:\\Program Files\\VE_Suite.0.9.3\\bin\\assemblySlave.bat"'

# TEST
#application = "notepad \\\\WRXRE38570001\\VRJShare\\test.txt"

first = 'start /WAIT cmd /K psexec \\\\'
last = " -i -c -f -e " + "-u vrjguest -p vrjuggler " + slave_application
#last = " -i " + application

# Start local copy
self = 'start /WAIT cmd /K ' + master_application
print self
(child_out0, child_in0) = popen2.popen2(self)

# Start first slave node
command = first + nodes[0] + last
print command
(child_out1, child_in1) = popen2.popen2(command)

# Start second slave node
#command = first + nodes[1] + last
#print command
#(child_out2, child_in2) = popen2.popen2(command)

# Start third slave node
#command = first + nodes[2] + last
#print command
#(child_out3, child_in3) = popen2.popen2(command)
