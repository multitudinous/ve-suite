I just checked in the code supporting use both OpenMP and CORBA, seems work. 
To run the OpenMP version, you need to: 

1. copy pre-sliced data files with name "octant<a number>.vtk" into <current directory>/POST_DATA 
2. setenv OMP_NUM_THREADS <number of data files> (optional) 
3. go to VE_Xplorer directory, do "source setup_OMP.tsh", and recompile everything. 

-- 
Song Li
VRAC(Virtual Reality Application Center),Iowa State University
Email: lisong@vrac.iastate.edu
Homepage: http://www.vrac.iastate.edu/~lisong
