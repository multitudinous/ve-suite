#ifndef reks_container_h
#define reks_container_h
#include "REKS_Thrm_Info.h"
#include "InpReader.h"
#include "reks.h"
#include <iostream>
#include <fstream>

class reks_container{
 public:

    reks_container(){};
    reks_container(vector<string> inp_files, string inp_file);
    reks_container(vector<string> inp_files);
    ~reks_container(){};
    void set_time(REAL time) {my_reader.rxn_time = time;}
    void set_tau(REAL t) {my_reader.tau = t;}
    void set_delt(REAL delt0) {my_reader.deltT = delt0;}
    bool set_state(REAL temp, REAL pres, vector<REAL>& mol_frac);
    bool set_case_flag(string whichcase);
    void set_atol(REAL atol0) {my_reader.atol = atol0;}
    void set_rtol(REAL rtol0) {my_reader.rtol = rtol0;}
    void set_state(REAL temp, REAL pres);
    void set_temp(REAL temp);
    void set_pres(REAL pres);
    bool set_mol_frac(string name, REAL mol_frac);
    void zero_mol_frac();
    void set_qloss(REAL qloss0) {my_reader.qloss = qloss0;}
    void set_volume(REAL vol) {my_reader.volume = vol;}
    void set_tinl(REAL tmp) {my_reader.tinl = tmp;}
    void set_ttime_type(int t) {my_reader.ttime_type = t;}
    void set_start_temp(REAL t0) {my_reader.start_temp = t0;}
    void set_to_ttim(REAL tf) {my_reader.to_ttim = tf;}
    void set_slope_ttim(REAL slp) {my_reader.slope_ttim = slp;}
    void set_length(REAL length) {my_reader.rxr_length = length;}
    void set_area(REAL area) {my_reader.rxr_Acs = area;}
    void set_mass_flow(REAL m_dot) {my_reader.mass_flow_rate = m_dot;}
	void set_fuelConvWant(REAL conv) {my_reader.fuelConvWant = conv;}
//THE INTERFACE TO EXPOSURE HERE TO LET THE REACTIONS BE READED IN
	void read_kinetics(vector<string> files) {test_sys.ReadKinetics(files);};
    //accessors
    REKS_Thrm_Info& get_my_thrm_info() {return(my_thrm_info);}
    REKS_inp_reader& get_my_reader() {return(my_reader);}
	int get_num_specs() {return(my_reader.the_gas.specs.size());}
	string get_specie_name(int i) {return(my_reader.the_gas.specs[i].spec->m_spec_name);}
	REAL get_specie_mole_fraction(int i) {return(my_reader.the_gas.specs[i].mole_fraction);}
	REAL get_temperature() {return(my_reader.the_gas.temperature);}
	REAL get_pressure() {return(my_reader.the_gas.pressure);}
	REAL get_rxr_length() {return my_reader.rxr_length;}
    REKS_sys& get_test_sys() {return(test_sys);}
    const map<string,int>& get_name_spec() const {return(my_reader.the_gas.get_name_spec());}
	void norm_molf() {my_reader.the_gas.norm_molf();}	
	void molef2massf();
	void set_enthalpy(REAL enth);
	void calc_enthalpy();

	void init_for_eql();
   bool equilb(bool const_temp, FILE* out_file);

 protected:
	REKS_Thrm_Info my_thrm_info;
	REKS_inp_reader my_reader;
	REKS_sys test_sys;

 public:
};

#endif
