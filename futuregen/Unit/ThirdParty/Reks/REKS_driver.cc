//#define REDUCED 1

#pragma warning(disable: 4786)
#pragma warning(disable: 4305)
#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cmath>
#include "reks_solve.h"

using std::cout;
using std::endl;
using std::cin;

int main(int argc, char* argv[])
{
	if (argc< 4)
	{
		cout<<"wrong number of parameters!"<<endl;
		cout<<"REKS_driver [mechnism files or thermo files] <case input file> <output file>"<<endl;
		exit(0);
	}

   //string kinetics_file = argv[1];
   //string thermo_file = argv[2];
   //string inp_file = argv[3];
	int i;
	vector<string> inp_files;

	for (i=1; i<argc-2; i++)
		inp_files.push_back(argv[i]);
	
	string case_inp_file = argv[argc-2];
	reks_container reks(inp_files, case_inp_file);
	reks.read_kinetics(inp_files);
/*   reks_container reks(kinetics_file, thermo_file);
   reks.set_case_flag("CONP");
   reks.set_time(5.6e-3);
   reks.set_delt(1.0e-5);
   reks.zero_mol_frac();
   reks.set_mol_frac("H2",0.3);
   reks.set_mol_frac("O2",0.7);
   reks.set_state(1.245190E+003,1013250.);*/

   // lignell's example
/*   reks_container reks(kinetics_file, thermo_file);
   reks.set_case_flag("CONST_DIST");
   reks.set_atol(1.0e-14);
   reks.set_rtol(1.0e-8);
   reks.set_delt(0.35);
   reks.set_state(1200.0,1013250.);
   reks.zero_mol_frac();
   reks.set_mol_frac("O2",0.19005);
   reks.set_mol_frac("N2",0.71493);
   reks.set_mol_frac("CH4",0.09502);
   reks.set_length(35.6379187);
   reks.set_area(10.0);
   reks.set_mass_flow(1.0);*/

   reks_solve reks_s;
	FILE* out_file = fopen(argv[4],"w");
   reks_s.solve(reks, out_file);
   return(0);
}
