#python

#Menu options
print
print 
print 

def menu():
   print "VES.vrac [ options ]"
   print "         " + "-h = usage information"
   print
   print "The following indicates the workflow of getting the cfd data into the viewer."
   print "   " + "-cfd2VTK = translate cfd flowdata to VTK format"
   print "   " + "-preproc = precompute cutting planes"
   print 
   print "This will start the viewer using the specified display"
   print "        " + "-c6 = c6 cave display"
   print "    " + "-c4open = c4 open cave display"
   print "  " + "-c4closed = c4 closed cave display"
   print "       " + "-sim = sim mode"
   print "    " + "-c6mono = c6 in mono"
   print "    " + "-c4mono = c4 closed in mono"
   print
   print "When the viewer has started, start the menu in another shell"
   print "      " + "-menu = start java menu to control viewer"
   print
   print "The following utilities might be useful occasionaly"
   #print "     " + "-nserv = start corba server for menu"
   print "   " + "-surface = make VTK surface from fluid mesh"
   print "  " + "-surf2stl = convert surface file to stl"
   print "  " + "-transVTK = move flowdata to align with geometry"
   print
   print "Enter the following to exit out of options."
   print "      " + "-exit = exit menu options"
   print
menu()

print "Choose which operation you would like to perform."
print "Type the dashed abbreviation to make your selection."
print

options = ["-h","-cfd2VTK","-preproc","-c6","-c4open","-c4closed","-sim","-c6mono",\
           "-menu", "-nserv","-surface","-surf2stl","-transVTK","-exit"]

choice = raw_input("Choose an option:  ")
while choice != "-exit":
   if choice in options:
      print
      if choice == "-h":
         print "hi" 
      elif choice == "-cfd2VTK":
         print "Jared"
      elif choice == "-preproc":
         print "Abo"
      elif choice == "-c6":
         print "Red"
      elif choice == "-c4open":
         print "duck"
      elif choice == "-c4closed":
         print "bird"
      elif choice == "-sim":
         print "Molly"
      elif choice == "-c6mono":
         print "Jack"
      elif choice == "-c4mono":
         print "bye"
      elif choice == "-menu":
         print "menu"
      elif choice == "-nserv":
         print "serve"
      elif choice == "-surface":
         print "surface"
      elif choice == "-surf2stl":
         print "stl"
      elif choice == "-transVTK":
         print "nice"
   elif choice not in options:
      print
      print "That is not an option."
   print
   print "Choose another option or exit"
   print
   menu()
   print
   choice = raw_input()
