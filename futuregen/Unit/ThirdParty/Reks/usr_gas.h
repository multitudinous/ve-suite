//usr_gas.h
//Definition of the Class REKS_usr_specie and REKS_gas
// Copyright 2002  Reaction Engineering International
// by Yang
//////////////////////////////////////////////////////////////

#ifndef USER_GAS_H
#define USER_GAS_H
#include "GlobalConst.h"
#include "SpecieThermo.h"
#include "REKS_Thrm_Info.h"
#include <V21Helper/Therm/matrix.h>

class REKS_gas;

class REKS_usr_specie {
 public:  
  REKS_specie_thermo *spec;
  REKS_gas* mygas;
  REAL mass_fraction;
  REAL mole_fraction;
  REAL molar_concentration;
  REAL spec_wgt;
 
  REAL ckcpml; // const p specific heat molar
  REAL ckhml; // enthalpy molar
  REAL cksml; // entropy molar
  REAL ckcvml; // const v specific heat molar
  REAL ckuml; // internal energy molar
  REAL ckgml; // gibbs free energy molar
  REAL ckaml; // helmholtz free energy molar
  REAL ckcpms; // const p specific heat molar
  REAL ckhms; // enthalpy molar
  REAL cksms; // entropy molar
  REAL ckcvms; // const v specific heat molar
  REAL ckums; // internal energy molar
  REAL ckgms; // gibbs free energy molar
  REAL ckcams; // helmholtz free energy molar

 public:
  REKS_usr_specie(REKS_specie_thermo *spec);

  inline void set_mass_fraction(REAL mas_f)
    {
      mass_fraction = mas_f;
      
      Fmass_fraction = true;
    }

  inline void set_mole_fraction(REAL mol_f)
    {
      mole_fraction = mol_f;
      Fmole_fraction = true;
    }
  
  inline void set_molar_concentration(REAL mol_c)
    {
      molar_concentration = mol_c;
      Fmolar_concentration = true;
    }

  inline void clear_all_flags()
    {
      Fmass_fraction=false;
      Fmole_fraction=false;
      Fmolar_concentration=false;
      
      Fckcpml=false;
      Fckhml=false;
      Fcksml=false;
      Fckcvml=false;
      Fckuml=false;
      Fckgml=false;
      Fckaml=false;
      Fckcpms=false;
      Fckhms=false;
      Fcksms=false;
      Fckcvms=false;
      Fckums=false;
      Fckgms=false;
      Fckcams=false;
    }

  REAL massf2molef_by_MMW(); //mass fraction to mole fraction by mean molecular weight
  REAL massf2molarc_by_dens(); //mass fraction to molar concentration by density 
  REAL massf2molarc_by_pres(); //mass fraction to molar concentration by pressure
  REAL molef2massf_by_MMW(); //mole fraction to mass fraction by mean molecular weight
  REAL molef2molarc_by_dens(); //mole fraction to molar concentration by density
  REAL molef2molarc_by_pres(); //mole fraction to molar concentration by pressure
  REAL molarc2massf(); //molar concentration to mass fraction
  REAL molarc2molef(); //molar concentration to mole fraction 
  REAL cp_molar_CKCPML(); //equation 19
  REAL enthalpy_molar_CKHML(); //equation 20
  REAL entropy_molar_CKSML(); //equation 21 
  REAL cv_molar_CKCVML(); //equation 22
  REAL internal_energy_molar_CKUML(); //equation 23
  REAL Gibbfree_energy_molar_CKGML(); //equation 24
  REAL Helmholtzfree_energy_molar_CKAML(); //equation 25
  REAL cp_mass_CKCPMS(); //equation 26 
  REAL enthalpy_mass_CKHMS(); //equation 27
  REAL entropy_mass_CKSMS(); //equation 28
  REAL cv_mass_CKCVMS(); //equation 29
  REAL internal_energy_mass_CKUMS(); //equation 30
  REAL Gibbfree_energy_mass_CKGMS(); //equation 31
  REAL Helmholtzfree_energy_mass_CKCAMS(); //equation 32


	bool Fmass_fraction;
	bool Fmole_fraction;
	bool Fmolar_concentration;
	 
	bool Fckcpml;
	bool Fckhml;
	bool Fcksml;
	bool Fckcvml;
	bool Fckuml;
	bool Fckgml;
	bool Fckaml;
	bool Fckcpms;
	bool Fckhms;
	bool Fcksms;
	bool Fckcvms;
	bool Fckums;
	bool Fckgms;
	bool Fckcams;

  // added functions for public interface
  REAL enthalpy_no_R(REAL& T);
  REAL entropy_no_R(REAL& T);
  REAL cp_no_R(REAL& T);
};


class REKS_gas {
 public:
  REKS_gas();
  REKS_gas(REKS_Thrm_Info& my_thrm_info);
  void set_pressure(REAL pres);
  void set_temperature(REAL temp);
  void clear_all_flags();
  void norm_molf();	

  REAL mean_mole_wgt_by_massf(); //mean molecular weight from mass fraction
  REAL mean_mole_wgt_by_molef(); //mean molecular weight from mole fraction
  REAL mean_mole_wgt_by_molar(); //mean molecular weight form molar concentration

  REAL pressure_by_molar(); //equation 1, instead of using Tk, I use the gas temperature
  REAL density_by_pres(); //compute density from presure and temperature Rho=P*mean_mole_wgt/(R*T)
  REAL density_by_molar(); //equaction 2, mass density, computed from molar concentration

  REAL mix_cp_molar_CKCPBL(); //equation 33
  REAL mix_cp_mass_CKCPBS(); //equation 34
  REAL mix_cv_molar_CKCVBL(); //equation 35
  REAL mix_cv_mass_CKCVBS(); //equation 36
  REAL mix_enthalpy_molar_CKHBML(); //equation 37
  REAL mix_enthalpy_mass_CKHBMS(); //equation 38
  REAL mix_internal_energy_molar_CKUBML(); //equation 39
  REAL mix_internal_energy_mass_CKUBMS(); //equation 40
  REAL mix_entropy_molar_CKSBML(); //equation 42
  REAL mix_entropy_mass_CKSBMS(); //equation 43
  REAL mix_Gibbfree_energy_molar_CKGBML(); //equation 44
  REAL mix_Gibbfree_energy_mass_CKGBMS(); //equation 45
  REAL mix_Helmholtzfree_energy_molar_CKABML(); //equation 46
  REAL mix_Helmholtzfree_energy_mass_CKABMS(); //equation 47

  REKS_usr_specie* get_specie_thermo(REKS_specie_thermo * p_specthrm);
  
 public:
  REAL density;
  REAL mean_mole_wgt;
  REAL pressure;
  REAL temperature;
  REAL ckcpbl;
  REAL ckcpbs;
  REAL ckcvbl;
  REAL ckcvbs;
  REAL ckhbml;
  REAL ckhbms;
  REAL ckubml;
  REAL ckubms;
  REAL cksbml;
  REAL cksbms;
  REAL ckgbml;
  REAL ckgbms;
  REAL ckabml;
  REAL ckabms;
  vector<REKS_usr_specie> specs;

	bool Fdensity;
	bool Fmean_mole_wgt;
	bool Fpressure;
	bool Fckcpbl;
	bool Fckcpbs;
	bool Fckcvbl;
	bool Fckcvbs;
	bool Fckhbml;
	bool Fckhbms;
	bool Fckubml;
	bool Fckubms;
	bool Fcksbml;
	bool Fcksbms;
	bool Fckgbml;
	bool Fckgbms;
	bool Fckabml;
	bool Fckabms;

   // public interface
	// accessors
   const map<string, int>& get_name_spec() const {return(name_spec);};
   const map<string, int>& get_name_el() const {return(name_el);};
    int get_num_species() const {return specs.size();}
    int get_num_elements() const {return name_el.size();}
    const REAL& get_mwt(int is) const 
      {return (specs[is].spec_wgt);}
    const std::vector< std::vector<REAL> >& get_mol_at_spc() const {return(mol_at_spc);}


   REAL enthalpy_no_R(int is, REAL temp);
   REAL entropy_no_R(int is, REAL temp);
   REAL cp_no_R(int is, REAL temp);
    bool set_state(REAL temp, REAL pres, vector<REAL>& mol_frac);
    void set_state(REAL temp, REAL pres);
    void set_temp(REAL temp);
    void set_pres(REAL pres);
    bool set_mass_frac(string name, REAL mass_frac);
    bool set_mass_frac(int is, REAL mass_frac);
    void zero_mol_frac();
    void massf2molef();
    void molef2massf();
    void massf2molarc_by_pres();
    void set_enthalpy(REAL enth);
    void calc_enthalpy();

    void init_for_eql();
    bool equilb(bool const_temp, FILE* out_file);

   // added for temporary public interface
   void set_REKS_gas(vector<REKS_specie_thermo *>& m_specs,
      REKS_element_list& elems);

 protected:
    map<string, int> name_spec, name_el; // maps from name to vector location

    // data for equilibrium
    std::vector<std::vector<REAL> > mol_at_spc;
    int nspc, nel;
    std::vector<std::string > phase;
    std::map<std::string, int> nam_phase;
    std::vector< std::string > phase_nam;
    std::vector<int> iphase;
    std::vector<REAL> sum_phase, sumh_phase;
    std::vector<REAL> totmol;
    
    std::vector<REAL> b0;
    std::vector<REAL> dmol, enth0, cp0;
    std::vector<REAL> gfe, gfe0; // Gibbs free energy
    
    //matrix solver for equilibrium
    matrix mtx;

};

#endif

