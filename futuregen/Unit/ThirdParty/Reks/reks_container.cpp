
#include "reks_container.h"

reks_container::reks_container(vector<string> inp_files, string inp_file)
{
  	//ifstream ck_inp(kinetics_file.c_str());
	//ifstream thermo_inp(thermo_file.c_str());
	int line=0;
	ifstream inp(inp_file.c_str());
	if (!inp.is_open())
	  {
	    cout<<"Error opening "<<inp_file<<"! Exit."<<endl;
	    exit(-1);
	  }
	my_thrm_info.ReadThrm(inp_files);
	my_reader.parse_inp(inp, my_thrm_info.m_thrm_specs, line);  
	test_sys.reks_thrm_info=&my_thrm_info;
	inp.close();
}
///////////////////////
reks_container::reks_container(vector<string> inp_files)
{
	my_reader.rtol = RTOL;
	my_reader.atol = ATOL;
  	
	//ifstream ck_inp(kinetics_file.c_str());
	//ifstream thermo_inp(thermo_file.c_str());

	int line = 0;
	//my_parser.parseIt(ck_inp, thermo_inp);
	my_thrm_info.ReadThrm(inp_files);
	//ck_inp.close();
	//thermo_inp.close();
	test_sys.reks_thrm_info=&my_thrm_info;

	my_reader.the_gas.set_REKS_gas(my_thrm_info.m_thrm_specs,
      my_thrm_info.m_elements);
}
///////////////////////
bool reks_container::set_case_flag(string whichcase)
{
   if(whichcase=="CONP"){
      my_reader.set_case_type(0);
      return true;
   }else if(whichcase=="CONT"){
      my_reader.set_case_type(1);
      return true;
   }else if(whichcase=="CONV"){
      my_reader.set_case_type(2);
      return true;
   }else if(whichcase=="TTIM"){
      my_reader.set_case_type(3);
      return true;
   }else if(whichcase=="TGIV"){
     my_reader.set_case_type(4);
     return true;
   }else if(whichcase=="ENRG"){
     my_reader.set_case_type(5);
     return true;
   }else if(whichcase=="CONST_DIST"){
      my_reader.set_case_type(6);
      return true;
   }else if(whichcase=="CONSP_DIST"){
      my_reader.set_case_type(7);
      return true;
   }else if(whichcase=="CONSP_DIST_CONV"){
      my_reader.set_case_type(8);
      return true;
   }else if(whichcase=="CONSP_DIST_CAT_COMB"){
      my_reader.set_case_type(10);
      return true;
   }else if(whichcase=="CONSP_DIST_CONV_CAT_COMB"){
      my_reader.set_case_type(11);
      return true;
   }else if(whichcase=="CONST_SURF"){
      my_reader.set_case_type(12);
      return true;
   }else{
      cerr << "reks_container::set_case_flag: case not recognized" << endl;
      my_reader.set_case_type(-1);
      exit(1);
      return false;
   }
}
///////////////////////
bool reks_container::set_state(REAL temp, REAL pres, vector<REAL>& mol_frac)
{
   my_reader.the_gas.set_temperature(temp);
   my_reader.the_gas.set_pressure(pres);
   int nspc = mol_frac.size(), is;
   if(nspc!=my_reader.the_gas.specs.size()){
      cerr << "reks_container::set_state: mol_frac vector not correct size" << endl;
      return(false);
   }
   for(is=0; is<nspc; is++){
      my_reader.the_gas.specs[is].set_mole_fraction(mol_frac[is]);
   }
   return(true);
}
///////////////
void reks_container::set_state(REAL temp, REAL pres)
{
   my_reader.the_gas.set_temperature(temp);
   my_reader.the_gas.set_pressure(pres);
}
///////////////
void reks_container::set_temp(REAL temp)
{
   my_reader.the_gas.set_temperature(temp);
}
///////////////
void reks_container::set_pres(REAL pres)
{
   my_reader.the_gas.set_pressure(pres);
}
///////////////
bool reks_container::set_mol_frac(string name, REAL mol_frac)
{
   map<string, int>::const_iterator it;
   it = my_reader.the_gas.get_name_spec().find(name);
   if(it==my_reader.the_gas.get_name_spec().end()){
      cerr << "reks_container::set_mol_frac: species " << name << " not found" << endl;
      return false;
   }
   my_reader.the_gas.specs[it->second].set_mole_fraction(mol_frac);
   return(true);
}
///////////////
void reks_container::zero_mol_frac()
{
   int nspc = my_reader.the_gas.specs.size(), is;
   for(is=0; is<nspc; is++) my_reader.the_gas.specs[is].set_mole_fraction(0.0);
}
///////////////
void reks_container::molef2massf()
{
   my_reader.the_gas.molef2massf();
}
///////////////
void reks_container::set_enthalpy(REAL enth)
{
   my_reader.the_gas.set_enthalpy(enth);
}
///////////////
void reks_container::calc_enthalpy()
{
   my_reader.the_gas.calc_enthalpy();
}
///////////////
void reks_container::init_for_eql()
{
	my_reader.the_gas.set_REKS_gas(my_thrm_info.m_thrm_specs,
      my_thrm_info.m_elements);
	my_reader.the_gas.mean_mole_wgt_by_molef();
	my_reader.the_gas.init_for_eql();
}
///////////////
bool reks_container::equilb(bool const_temp, FILE* out_file)
{
   return my_reader.the_gas.equilb(const_temp, out_file);
}

