// Copyright 2002  Reaction Engineering International
// by Yang
////////////////////////////////////////////////////////////

#pragma warning(disable:4786)
#include "usr_gas.h"
#include <cmath>
/*******************************************************************************/
/************** SPECIE CLASS IMPLEMENTATION ************************************/
/*******************************************************************************/
REKS_usr_specie::REKS_usr_specie(REKS_specie_thermo *ispec)
{
  spec=ispec;
  mygas=NULL;
  
  mass_fraction=0;
  mole_fraction=0;
  molar_concentration=0;
  if (ispec)
    spec_wgt=ispec->spec_wgt;
  else
    spec_wgt=0;
  
  ckcpml=0;
  ckhml=0;
  cksml=0;
  ckcvml=0;
  ckuml=0;
  ckgml=0;
  ckaml=0;
  ckcpms=0;
  ckhms=0;
  cksms=0;
  ckcvms=0;
  ckums=0;
  ckgms=0;
  ckcams=0;
  clear_all_flags();
}



REAL REKS_usr_specie::massf2molef_by_MMW()  //mass fraction to mole fraction by mean molecular weight
{
  REAL result;
  if (!Fmass_fraction)
  {
	  cerr<<"Computation requires Mass Fraction to be computed or Set"<<endl;
	  return -1;
  }
  if (!mygas->Fmean_mole_wgt)
  {
	cerr<<"Computation requires the system's mean mole weight to be computed or Set"<<endl;
	return -1;
  }

  result = (mass_fraction * mygas->mean_mole_wgt)/spec_wgt;
  
  mole_fraction=result;
  Fmole_fraction = true;
  return result;
}

REAL REKS_usr_specie::massf2molarc_by_dens()
  //mass fraction to molar concentration by density  
{
  REAL result;
  
  if (!Fmass_fraction)
  {
	  cerr<<"Computation requires Mass Fraction to be computed or Set"<<endl;
	  return -1;
  }
  if (!mygas->Fdensity)
  {
	cerr<<"Computation requires the system's density to be computed or Set"<<endl;
	return -1;
  }
  result = (mygas->density*mass_fraction)/spec_wgt;

  molar_concentration = result;
  Fmolar_concentration = true;
  return result;
}

REAL REKS_usr_specie::massf2molarc_by_pres()
  //mass fraction to molar concentration by pressure
{
  REAL result=0;
  REAL temp=0;
  int i;
  
  if (!Fmass_fraction)
  {
	  cerr<<"Computation requires Mass Fraction to be computed or Set"<<endl;
	  return -1;
  }
  
  if (!mygas->Fpressure)
  {
	cerr<<"Computation requires the system's pressure to be computed or Set"<<endl;
	return -1;
  }

  temp = mygas->pressure*(mass_fraction/spec_wgt);
  for (i=0; i<(mygas->specs).size(); i++)
    result+=(mygas->specs[i]).mass_fraction*(mygas->temperature)/(mygas->specs[i]).spec_wgt;
  result=R*result;
  result=temp/result;

  molar_concentration = result;
  Fmolar_concentration = true;
  return result;
}

REAL REKS_usr_specie::molef2massf_by_MMW()
  //mole fraction to mass fraction by mean molecular weight
{
  REAL result;
  
  if (!Fmole_fraction)
  {
	  cerr<<"Computation requires Mole Fraction to be computed or Set"<<endl;
	  return -1;
  }
  if (!mygas->Fmean_mole_wgt) //Fdensity)
  {
	cerr<<"Computation requires the system's mean mole weight to be computed or Set"<<endl;
	return -1;
  }

  result = (mole_fraction*spec_wgt)/mygas->mean_mole_wgt;

  mass_fraction=result;
  Fmass_fraction = true;
  return result;
}

REAL REKS_usr_specie::molef2molarc_by_dens()
  //mole fraction to molar concentration by density
{
  REAL result;
  
  if (!Fmole_fraction)
  {
	  cerr<<"Computation requires Mole Fraction to be computed or Set"<<endl;
	  return -1;
  }
  if (!mygas->Fdensity)
  {
	cerr<<"Computation requires the system's density to be computed or Set"<<endl;
	return -1;
  }
  if (!mygas->Fmean_mole_wgt)
  {
	cerr<<"Computation requires the system's mean mole weight to be computed or Set"<<endl;
	return -1;
  }

  result = mole_fraction*(mygas->density)/(mygas->mean_mole_wgt);

  Fmolar_concentration = true;
  molar_concentration = result;
  return result;
}
  
REAL REKS_usr_specie::molef2molarc_by_pres()
  //mole fraction to molar concentration by pressure
{
  REAL result=0;
  int i;
  
  if (!Fmole_fraction)
  {
	  cerr<<"Computation requires Mole Fraction to be computed or Set"<<endl;
	  return -1;
  }
  if (!mygas->Fpressure)
  {
	cerr<<"Computation requires the system's pressure to be computed or Set"<<endl;
	return -1;
  }

  for (i=0; i<(mygas->specs).size(); i++)
    result+=(mygas->specs[i]).mole_fraction*(mygas->temperature);
  result*=R;
  result = mole_fraction * mygas->pressure/result;

  Fmolar_concentration = true;
  molar_concentration = result;
  return result;
}

REAL REKS_usr_specie::molarc2massf()
  //molar concentration to mass fraction
{
  REAL result = 0;
  int i;

  if (!Fmolar_concentration)
  {
	  cerr<<"molarc2massf Computation requires Molar Concentration to be computed or Set"<<endl;
	  return -1;
  }
  
  for (i=0; i<(mygas->specs).size(); i++)
    result+=(mygas->specs[i]).molar_concentration*(mygas->specs[i]).spec_wgt;
  result = molar_concentration*spec_wgt / result;

  mass_fraction=result;
  Fmass_fraction = true;
  return result;
}
  
REAL REKS_usr_specie::molarc2molef()
  //molar concentration to mole fraction 
{
  REAL result =0.0;
  int i;
  
  if (!Fmolar_concentration)
  {
	  cerr<<"molarc2molef Computation requires Molar Concentractioin to be computed or Set"<<endl;
	  return -1;
  }

  for (i=0; i<(mygas->specs).size(); i++)
    result+=(mygas->specs[i]).molar_concentration;
  result = molar_concentration / result;
  //cout<<"before:  "<<mole_fraction<<"   after:  "<<result<<endl;
  mole_fraction = result;
  Fmole_fraction = true;
  return result;
}

REAL REKS_usr_specie::cp_molar_CKCPML()
  //equation 19
{
  REAL result = 0;
  REAL T = mygas->temperature;
  
  if (Fckcpml)
	return ckcpml;

  if ((T<=spec->m_common_temp) && (T>=spec->m_low_temp))
    result = R*(spec->m_lower_temp_a[0]+
		T*(spec->m_lower_temp_a[1]+T*(spec->m_lower_temp_a[2]
		+T*(spec->m_lower_temp_a[3]+spec->m_lower_temp_a[4]*T))));
  else if ((T>spec->m_common_temp)&&(T<=spec->m_high_temp))
    result = R*(spec->m_upper_temp_a[0]+
		T*(spec->m_upper_temp_a[1]+T*(spec->m_upper_temp_a[2]
		+T*(spec->m_upper_temp_a[3]+spec->m_upper_temp_a[4]*T))));
  else
    {
      cerr<<"Gas temperature = "<<T<<" out of range!"<<endl;
	  result=0;
    }
  
  ckcpml=result;
  Fckcpml = true;
  return result;
}

REAL REKS_usr_specie::enthalpy_molar_CKHML() 
  //equation 20
{
  REAL result = 0;
  REAL T = mygas->temperature;

  if (Fckhml)
	return ckhml;

  if ((T<=spec->m_common_temp) && (T>=spec->m_low_temp))
    result = R*T*(spec->m_lower_temp_a[0]+
		  T*(spec->m_lower_temp_a[1]/2.0+T*(spec->m_lower_temp_a[2]/3.0
		     +T*(spec->m_lower_temp_a[3]/4.0+spec->m_lower_temp_a[4]/5.0*T)))
		  +spec->m_lower_temp_a[5]/T);
  else if ((T>spec->m_common_temp)&&(T<=spec->m_high_temp))
    result = R*T*(spec->m_upper_temp_a[0]+
		  T*(spec->m_upper_temp_a[1]/2.0+T*(spec->m_upper_temp_a[2]/3.0
		     +T*(spec->m_upper_temp_a[3]/4.0+spec->m_upper_temp_a[4]/5.0*T)))
		  +spec->m_upper_temp_a[5]/T);
  else
    {
      cerr<<"Gas temperature = "<<T<<" out of range!"<<endl;
      result=0;
    }

  ckhml = result;
  Fckhml = true;
  return result;
}

REAL REKS_usr_specie::entropy_molar_CKSML() 
  //equation 21 upper temperature
{
  REAL result = 0;
  REAL T = mygas->temperature;
  
  if (Fcksml)
	return cksml;

  if ((T<=spec->m_common_temp) && (T>=spec->m_low_temp))
    result = R*(spec->m_lower_temp_a[0]*log(T)+
		T*(spec->m_lower_temp_a[1]+T*(spec->m_lower_temp_a[2]/2.0
		 +T*(spec->m_lower_temp_a[3]/3.0+spec->m_lower_temp_a[4]*T/4.0)))
		+spec->m_lower_temp_a[6]);
  else if ((T>spec->m_common_temp)&&(T<=spec->m_high_temp))
    result = R*(spec->m_upper_temp_a[0]*log(T)+
		T*(spec->m_upper_temp_a[1]+T*(spec->m_upper_temp_a[2]/2.0
		   +T*(spec->m_upper_temp_a[3]/3.0+spec->m_upper_temp_a[4]/4.0*T)))
		+spec->m_upper_temp_a[6]);

  cksml = result;
  Fcksml = true;
  return result;
}

REAL REKS_usr_specie::cv_molar_CKCVML()
  //equation 22
{
  if (Fckcvml)
	return ckcvml;

  ckcvml = cp_molar_CKCPML() - R;
  Fckcvml = true;
  return ckcvml;
}

REAL REKS_usr_specie::internal_energy_molar_CKUML()
  //equation 23
{
  if (Fckuml)
	return ckuml;

  ckuml = enthalpy_molar_CKHML() - R*(mygas->temperature);
  Fckuml = true;
  return ckuml;
}

REAL REKS_usr_specie::Gibbfree_energy_molar_CKGML()
  //equation 24
{
  if (Fckgml)
	return ckgml;

  ckgml = enthalpy_molar_CKHML() - mygas->temperature*entropy_molar_CKSML();

  Fckgml = true;
  return ckgml;
}

REAL REKS_usr_specie::Helmholtzfree_energy_molar_CKAML()
  //equation 25
{
  if (Fckaml)
	return ckaml;

  ckaml = internal_energy_molar_CKUML() - mygas->temperature*entropy_molar_CKSML();

  Fckaml = true;
  return ckaml;
}
 
REAL REKS_usr_specie::cp_mass_CKCPMS()
  //equation 26
{
  if (Fckcpms)
	return ckcpms;

  ckcpms = cp_molar_CKCPML()/spec_wgt;

  Fckcpms = true;
  return ckcpms;
}

REAL REKS_usr_specie::enthalpy_mass_CKHMS()
  //equation 27
{
  if (Fckhms)
	return ckhms;

  ckhms = enthalpy_molar_CKHML() / spec_wgt;

  Fckhms = true;
  return ckhms;
}

REAL REKS_usr_specie::entropy_mass_CKSMS()
  //equation 28
{
  if (Fcksms)
	return cksms;

  cksms = entropy_molar_CKSML() / spec_wgt;

  Fcksms = true;
  return cksms;
}

REAL REKS_usr_specie::cv_mass_CKCVMS()
  //equation 29
{
  if (Fckcvms)
	return ckcvms;

  ckcvms = cv_molar_CKCVML() / spec_wgt;

  Fckcvms = true;
  return ckcvms;
}
 
REAL REKS_usr_specie::internal_energy_mass_CKUMS()
  //equation 30
{
  if (Fckums)
	return ckums;

  ckums = internal_energy_molar_CKUML() / spec_wgt;

  Fckums = true;
  return ckums;
}

REAL REKS_usr_specie::Gibbfree_energy_mass_CKGMS()
  //equation 31
{
  if (Fckgms)
	return ckgms;

  ckgms = Gibbfree_energy_molar_CKGML() / spec_wgt;

  Fckgms = true;
  return ckgms;
}

REAL REKS_usr_specie::Helmholtzfree_energy_mass_CKCAMS()
  //equation 32
{
  if (Fckcams)
	return ckcams;

  ckcams = Helmholtzfree_energy_molar_CKAML() / spec_wgt;

  Fckcams = true;
  return ckcams;
}

// added for public interface
REAL REKS_usr_specie::enthalpy_no_R(REAL &T1)
{
   REAL result;
   REAL T = T1;
   bool extrap = (T>spec->m_high_temp||T<spec->m_low_temp);
   if(T>spec->m_high_temp) T = spec->m_high_temp;
   if(T<spec->m_low_temp) T = spec->m_low_temp;
   if (T<=spec->m_common_temp)
      result = T*(spec->m_lower_temp_a[0]+
		  T*(spec->m_lower_temp_a[1]/2.0+T*(spec->m_lower_temp_a[2]/3.0
		     +T*(spec->m_lower_temp_a[3]/4.0+spec->m_lower_temp_a[4]/5.0*T)))
      +spec->m_lower_temp_a[5]/T);
   else 
      result = T*(spec->m_upper_temp_a[0]+
		  T*(spec->m_upper_temp_a[1]/2+T*(spec->m_upper_temp_a[2]/3
		     +T*(spec->m_upper_temp_a[3]/4+spec->m_upper_temp_a[4]/5*T)))
      +spec->m_upper_temp_a[5]/T);
   if(extrap){
      REAL cpi = cp_no_R(T);
      result += cpi*(T1 - T);
   }
   return result;
}

REAL REKS_usr_specie::entropy_no_R(REAL &T1)
{
   REAL result;
   REAL T = T1;
   bool extrap = (T>spec->m_high_temp||T<spec->m_low_temp);
   if(T>spec->m_high_temp) T = spec->m_high_temp;
   if(T<spec->m_low_temp) T = spec->m_low_temp;
  if (T<=spec->m_common_temp)
    result = (spec->m_lower_temp_a[0]*log(T)+
	      T*(spec->m_lower_temp_a[1]+T*(spec->m_lower_temp_a[2]/2.0
		 +T*(spec->m_lower_temp_a[3]/3.0+spec->m_lower_temp_a[4]/4.0*T)))
		+spec->m_lower_temp_a[6]);
  else
    result = (spec->m_upper_temp_a[0]*log(T)+
	      T*(spec->m_upper_temp_a[1]+T*(spec->m_upper_temp_a[2]/2.0
		 +T*(spec->m_upper_temp_a[3]/3.0+spec->m_upper_temp_a[4]/4.0*T)))
		+spec->m_upper_temp_a[6]);
   if(extrap)
   {
      REAL cpi = cp_no_R(T);
      result += cpi*log(T1/T);
   }

  return result;
}

REAL REKS_usr_specie::cp_no_R(REAL &T1)
{
   REAL result;
   REAL T = T1;
   bool extrap = (T>spec->m_high_temp||T<spec->m_low_temp);
   if(T>spec->m_high_temp) T = spec->m_high_temp;
   if(T<spec->m_low_temp) T = spec->m_low_temp;
   if (T<=spec->m_common_temp)
      result = (spec->m_lower_temp_a[0]+
		T*(spec->m_lower_temp_a[1]+T*(spec->m_lower_temp_a[2]
      +T*(spec->m_lower_temp_a[3]+spec->m_lower_temp_a[4]*T))));
   else 
      result = (spec->m_upper_temp_a[0]+
		T*(spec->m_upper_temp_a[1]+T*(spec->m_upper_temp_a[2]
      +T*(spec->m_upper_temp_a[3]+spec->m_upper_temp_a[4]*T))));
   
   return result;
}


/*******************************************************************************/
/************** GAS CLASS IMPLEMENTATION ***************************************/
/*******************************************************************************/

REKS_gas::REKS_gas()
{
	temperature = -1000000;
	clear_all_flags();
}

REKS_gas::REKS_gas(REKS_Thrm_Info& my_thrm_info)
{
	set_REKS_gas(my_thrm_info.m_thrm_specs,
      my_thrm_info.m_elements);
   init_for_eql();
}

void REKS_gas::set_pressure(REAL pres)
{
	pressure = pres;
	Fpressure = true;
}

void REKS_gas::set_temperature(REAL temp)
{
	temperature = temp;
}

void REKS_gas::clear_all_flags()
{
	Fdensity=false;
	Fmean_mole_wgt=false;
	Fpressure=false;
	Fckcpbl=false;
	Fckcpbs=false;
	Fckcvbl=false;
	Fckcvbs=false;
	Fckhbml=false;
	Fckhbms=false;
	Fckubml=false;
	Fckubms=false;
	Fcksbml=false;
	Fcksbms=false;
	Fckgbml=false;
	Fckgbms=false;
	Fckabml=false;
	Fckabms=false;
}

REAL REKS_gas::mean_mole_wgt_by_massf()
  //mean molecular weight from mass fraction
{
  int i;
  REAL result=0.0;
  
  for (i=0; i < specs.size(); i++)
  {
	if (!specs[i].Fmass_fraction)
	{
	  cerr<<"Computation requires Mass Fraction to be computed or Set"<<endl;
	  return -1;
	}

	result+=(specs[i].mass_fraction/specs[i].spec_wgt);
  }
	result = 1.0/result;

  mean_mole_wgt=result;
  Fmean_mole_wgt=true;
  return result;
}

REAL REKS_gas::mean_mole_wgt_by_molef() 
  //mean molecular weight from mole fraction
{
  int i;
  REAL result=0.0;

  for (i=0; i<specs.size(); i++)
  {
	if (!specs[i].Fmole_fraction)
	{
	  cerr<<"Computation requires Mole Fraction to be computed or Set"<<endl;
	  return -1;
	}
    result+=specs[i].spec_wgt*specs[i].mole_fraction;
  }

  mean_mole_wgt=result;
  Fmean_mole_wgt=true;
  return result;
}

REAL REKS_gas::mean_mole_wgt_by_molar()
  //mean molecular weight form molar concentration
{
  int i;
  REAL result = 0.0;
  REAL temp = 0.0;
  for (i=0; i<specs.size(); i++)
    {
	  if (!specs[i].Fmolar_concentration)
		{
			cerr<<"MMW_by_molar Computation requires Molar Concentration to be computed or Set"<<endl;
			return -1;
		}
      result+=specs[i].molar_concentration*specs[i].spec_wgt;
      temp+=specs[i].molar_concentration;
    }
  result = result / temp;

  mean_mole_wgt=result;
  Fmean_mole_wgt=true;
  return result;
}

REAL REKS_gas::pressure_by_molar()
  //equation 1, instead of using Tk, I use the REKS_gas temperature
{
  int i;
  REAL result = 0;

  for(i=0; i<specs.size(); i++)
  {
    if (!specs[i].Fmolar_concentration)
	{
	  cerr<<"pressure_by_molar Computation requires Molar Concentration to be computed or Set"<<endl;
	  return -1;
	}
	result+=specs[i].molar_concentration*R*temperature;
  }

  pressure = result;
  Fpressure = true;
  return result; 
  
}

REAL REKS_gas::density_by_pres()
  //compute press from density and temperature Rho=P*mean_mole_wgt/(R*T)
{
  REAL result=0;
  
  if (!Fpressure)
  {
    cerr<<"Computation requires pressure o be computed or Set"<<endl;
    return -1;
  }

  if (!Fmean_mole_wgt)
  {
	  cerr<<"Computation requires Mean Mole Weight to be computed or Set"<<endl;
	  return -1;
  }

  result = pressure*mean_mole_wgt/(R*temperature);
  
  density = result;
  Fdensity = true;
  return result;
}

REAL REKS_gas::density_by_molar()
  //equaction 2, mass density, computed from molar concentration
{
  int i;
  REAL result=0;
  
  for (i=0; i<specs.size(); i++)
  {
	if (!specs[i].Fmolar_concentration)
	{
	  cerr<<"density_by_molar Computation requires Molar Concentration to be computed or Set"<<endl;
	  return -1;
	}
    result+=specs[i].molar_concentration*specs[i].spec_wgt;
  }

  density = result;
  Fdensity = true;
  return result;
}

REAL REKS_gas::mix_cp_molar_CKCPBL()
  //equation 33
{
  int i;
  REAL result =0;

  if (Fckcpbl)
	  return ckcpbl;

  for (i=0; i < specs.size(); i++)
    result +=specs[i].cp_molar_CKCPML()*specs[i].mole_fraction;

  ckcpbl = result;
  Fckcpbl = true;

  return result;
}
 
REAL REKS_gas::mix_cp_mass_CKCPBS()
  //equation 34
{
  if (Fckcpbs)
	  return ckcpbs;

  ckcpbs = mix_cp_molar_CKCPBL() / mean_mole_wgt;
  Fckcpbs = true;

  return ckcpbs;
}
 
REAL REKS_gas::mix_cv_molar_CKCVBL()
  //equation 35
{
  int i;
  REAL result =0;

  if (Fckcvbl)
	return ckcvbl;

  for (i=0; i < specs.size(); i++)
    result +=specs[i].cv_molar_CKCVML()*specs[i].mole_fraction;
  
  ckcvbl = result;
  Fckcvbl = true;
  return result;
}
 
REAL REKS_gas::mix_cv_mass_CKCVBS()
  //equation 36
{
  if (Fckcvbs)
	return ckcvbs;

  ckcvbs = mix_cv_molar_CKCVBL() / mean_mole_wgt;
  Fckcvbs = true;

  return ckcvbs;
}
 
REAL REKS_gas::mix_enthalpy_molar_CKHBML()
  //equation 37
{
  int i;
  REAL result =0;
  
  if (Fckhbml)
	return ckhbml;

  for (i=0; i < specs.size(); i++)
    result +=specs[i].enthalpy_molar_CKHML()*specs[i].mole_fraction;
  
  ckhbml = result;
  Fckhbml = true;
  return result;  
}
 
REAL REKS_gas::mix_enthalpy_mass_CKHBMS()
  //equation 38
{
  if (Fckhbms)
	return ckhbms;

  ckhbms = mix_enthalpy_molar_CKHBML() / mean_mole_wgt;

  Fckhbms = true;
  return ckhbms;
}
 
REAL REKS_gas::mix_internal_energy_molar_CKUBML()
  //equation 39
{
  int i;
  REAL result =0;

  if (Fckubml)
	return ckubml;

  for (i=0; i < specs.size(); i++)
    result +=specs[i].internal_energy_molar_CKUML()*specs[i].mole_fraction;
  
  ckubml = result;
  Fckubml = true;
  return result;   
}
 
REAL REKS_gas::mix_internal_energy_mass_CKUBMS()
  //equation 40
{
  if (Fckubms)
	return ckubms;

  ckubms = mix_internal_energy_molar_CKUBML()/mean_mole_wgt;

  Fckubms = true;
  return ckubms;
}

REAL REKS_gas::mix_entropy_molar_CKSBML()
  //equation 42
{
  int i;
  REAL result =0;
  REAL temp;
  if (Fcksbml)
	return cksbml;

  for (i=0; i<specs.size(); i++)
    {
      temp=(specs[i].entropy_molar_CKSML()-R*log(specs[i].mole_fraction)
				-R*log(pressure/patom))*specs[i].mole_fraction;
      result +=temp;
    }
  
  cksbml = result;
  Fcksbml = true;
  return result;
}

REAL REKS_gas::mix_entropy_mass_CKSBMS()
  //equation 43
{
  if (Fcksbms)
	return cksbms;

  cksbms = mix_entropy_molar_CKSBML() / mean_mole_wgt;
  Fcksbms = true;
  return cksbms;
}
 
REAL REKS_gas::mix_Gibbfree_energy_molar_CKGBML()
  //equation 44
{
  int i;
  REAL result =0;
  REAL temp;
  REAL T = temperature;

  if (Fckgbml)
	return ckgbml;

  for (i=0; i<specs.size(); i++)
    {
      temp=(specs[i].enthalpy_molar_CKHML()-T*(specs[i].entropy_molar_CKSML()-
			R*log(specs[i].mole_fraction)-R*log(pressure/patom)))*specs[i].mole_fraction;
      result +=temp;
    }
  
  ckgbml = result;
  Fckgbml = true;
  return result;  
}

REAL REKS_gas::mix_Gibbfree_energy_mass_CKGBMS()
  //equation 45
{
  if (Fckgbms)
	return ckgbms;

  ckgbms = mix_Gibbfree_energy_molar_CKGBML() / mean_mole_wgt;
  Fckgbms = true;

  return ckgbms;
}
 
REAL REKS_gas::mix_Helmholtzfree_energy_molar_CKABML()
  //equation 46
{
  int i;
  REAL result =0;
  REAL temp;
  REAL T = temperature;
  
  if (Fckabml)
	return ckabml;

  for (i=0; i<specs.size(); i++)
    {
      temp=(specs[i].internal_energy_molar_CKUML()-T*(specs[i].entropy_molar_CKSML()
			-R*log(specs[i].mole_fraction)-R*log(pressure/patom)))*specs[i].mole_fraction;
      result +=temp;
    }
  
  ckabml = result;
  Fckabml = true;
  return result;   
}
 
REAL REKS_gas::mix_Helmholtzfree_energy_mass_CKABMS()
  //equation 47
{
  if (Fckabms)
	return ckabms;

  ckabms = mix_Helmholtzfree_energy_molar_CKABML()/mean_mole_wgt;
  Fckabms = true;
  return ckabms;
}

void REKS_gas::norm_molf()
{
	int i;
	REAL temp=0.0;
	for (i=0; i<specs.size(); i++)
		temp+=specs[i].mole_fraction;
	for (i=0; i<specs.size(); i++)
		specs[i].mole_fraction = specs[i].mole_fraction/temp;
}

REKS_usr_specie* REKS_gas::get_specie_thermo(REKS_specie_thermo * p_specthrm)
{
  int i;
  REKS_usr_specie* cur_sp;

  for (i=0; i<specs.size(); i++)
    if (specs[i].spec==p_specthrm)
      return &specs[i];

  cur_sp = new REKS_usr_specie(p_specthrm);
  cur_sp->mygas = this;	  
  cur_sp->set_mole_fraction(0);
 // cur_sp->molef2molarc_by_pres();
  
  specs.push_back(*cur_sp);
  delete cur_sp;
  cur_sp = &specs[specs.size()-1];
  
  return cur_sp;

}

// added for temporary public interface
void REKS_gas::set_REKS_gas(vector<REKS_specie_thermo *>& m_specs,
      REKS_element_list& elems)
{
  REKS_usr_specie *cur_sp;
  specs.clear(); //originally commented out, changed by yang
  name_spec.clear();
  int num = m_specs.size(), i;
  for(i=0; i<num; i++){
    cur_sp = new REKS_usr_specie(m_specs[i]);
    cur_sp->set_mole_fraction(0.0);
    cur_sp->mygas = this;
    specs.push_back(*cur_sp);
    delete cur_sp;
    //	  get_specie_thermo(m_specs[i]); Yang's change here, try to garantee the order of the specs list, the above block is added
    
    name_spec[m_specs[i]->m_spec_name] = i;
  }
  name_el.clear();
  num = elems.size();
  for(i=0; i<num; i++){
    name_el[elems[i]->m_name] = i;
  }
}

REAL REKS_gas::enthalpy_no_R(int is, REAL temp)
{
  return(specs[is].enthalpy_no_R(temp));
}

REAL REKS_gas::entropy_no_R(int is, REAL temp)
{
  return(specs[is].entropy_no_R(temp));
}

REAL REKS_gas::cp_no_R(int is, REAL temp)
{
  return(specs[is].cp_no_R(temp));
}

////////////////
void REKS_gas::init_for_eql()
{
  nel = name_el.size();
  nspc = specs.size();
   mol_at_spc.resize(nel);
   int iel, is;
   for(iel=0; iel<nel; iel++){
     mol_at_spc[iel].resize(nspc);
     for(is=0; is<nspc; is++) mol_at_spc[iel][is] = 0.0;
   }
   phase.resize(nspc);
   iphase.resize(nspc);
   phase_nam.clear();
   std::map< std::string, int >::const_iterator it;
   std::string name;
   char str[3] = {'\0','\0','\0'};
   int i, num, iph;
   for(is=0; is<nspc; is++){
     num = specs[is].spec->m_atom_form.size();
     for(i=0; i<num; i++){
       char str[3] = {'\0','\0','\0'};
       for(int j=0; j<2; j++) str[j] = specs[is].spec->m_atom_form[i].symbol[j];
       if(str[1]==' ') str[1] = '\0';
       name = str;
         it = name_el.find(name);
         if(it==name_el.end()){
	   cout << "element not found" << endl;
         }else{
	   iel = it->second;
	   mol_at_spc[iel][is] = specs[is].spec->m_atom_form[i].num;
         }
     }
      str[0] = specs[is].spec->m_phase;
      str[1] = '\0';
      name = str;
      phase[is] = name;
      it = nam_phase.find(name);
      if(it!=nam_phase.end()){
         iphase[is] = (*it).second;
      }else{
         phase_nam.push_back(phase[is]);
         iph = phase_nam.size()-1;
         nam_phase[phase[is]] = iph;
         iphase[is] = iph;
      }
   }

   b0.resize(nel);
   gfe.resize(nspc);
   gfe0.resize(nspc);
   dmol.resize(nspc);
   enth0.resize(nspc);
   cp0.resize(nspc);

   int nph = phase_nam.size();
   totmol.resize(nph);
   sum_phase.resize(nph);
   sumh_phase.resize(nph);

   int nmat = nel + phase_nam.size() + 1;
   mtx.resize(nmat);
}
///////////////////////
bool REKS_gas::set_state(REAL temp, REAL pres, vector<REAL>& mol_frac)
{
   set_temperature(temp);
   set_pressure(pres);
   int nspc = mol_frac.size(), is;
   if(nspc!=specs.size()){
      cerr << "REKS_gas::set_state: mol_frac vector not correct size" << endl;
      return(false);
   }
   for(is=0; is<nspc; is++){
      specs[is].set_mole_fraction(mol_frac[is]);
   }
   return(true);
}
///////////////
void REKS_gas::set_state(REAL temp, REAL pres)
{
   set_temperature(temp);
   set_pressure(pres);
}
///////////////
void REKS_gas::set_temp(REAL temp)
{
   set_temperature(temp);
}
///////////////
void REKS_gas::set_pres(REAL pres)
{
   set_pressure(pres);
}
///////////////
bool REKS_gas::set_mass_frac(string name, REAL mass_frac)
{
   map<string, int>::const_iterator it;
   it = get_name_spec().find(name);
   if(it==get_name_spec().end()){
      cerr << "REKS_gas::set_mol_frac: species " << name << " not found" << endl;
      return false;
   }
   specs[it->second].set_mass_fraction(mass_frac);
   return(true);
}
///////////////
bool REKS_gas::set_mass_frac(int is, REAL mass_frac)
{
   if(is>specs.size()-1) return(false);
   specs[is].set_mass_fraction(mass_frac);
   return(true);
}
///////////////
void REKS_gas::zero_mol_frac()
{
   int nspc = specs.size(), is;
   for(is=0; is<nspc; is++) specs[is].set_mole_fraction(0.0);
}
///////////////
void REKS_gas::massf2molef()
{
	mean_mole_wgt_by_massf();
	for(int is=0; is<get_num_species(); is++) specs[is].massf2molef_by_MMW();
}
///////////////
void REKS_gas::molef2massf()
{
   mean_mole_wgt_by_molef();
   for(int is=0; is<get_num_species(); is++) specs[is].molef2massf_by_MMW();
}
///////////////
void REKS_gas::massf2molarc_by_pres()
{
  REAL tempresult=0;
  REAL temp=0;
  int i;
  
  if (!Fpressure)
  {
	cerr<<"Computation requires the system's pressure to be computed or Set"<<endl;
	return ;
  }

  
  for (i=0; i<specs.size(); i++)
    {
      if (!specs[i].Fmass_fraction)
	{
	  cerr<<specs[i].spec->m_spec_name<<" Computation requires Mass Fraction to be computed or Set"<<endl;
	  return ;
	}
      
      tempresult+=specs[i].mass_fraction*(temperature)/(specs[i]).spec_wgt;
    }
  tempresult=R*tempresult;
   
  for (i=0; i<specs.size(); i++)
    {
      temp = pressure*(specs[i].mass_fraction/specs[i].spec_wgt);
      specs[i].molar_concentration=temp/tempresult;
      specs[i].Fmolar_concentration = true;
    }
  
}
void REKS_gas::set_enthalpy(REAL enth)
{
	ckhbms = enth;
}
///////////////
void REKS_gas::calc_enthalpy()
{
	mix_enthalpy_mass_CKHBMS();
}
//////////////////////////////////////////////////////////////////////
bool REKS_gas::equilb(bool const_temp, FILE* out_file)
{
	// incoming units:
	// ---------------
	// mass = mass fractions (kgi/kgtotal) from within REKs
	// mol  = molar masses (kgmol/kgtotal)

	// for fixed temperature calculations, enth is IGNORED
	// routine gets elemental composition from "mass"
	// but "mass" is otherwise unchanged

	// the result of equil calc is put in "mol" and has units of kgmol/kg

   int is;
   //test_sys.gas_sys = &(my_reader.the_gas);	
   fprintf(out_file, "Initial conditions: \n");
	//test_sys.dump(out_file);

   std::vector<REAL>& mat = mtx.get_mat();
   std::vector<REAL>& bb = mtx.get_bb();
   std::vector<REAL>& uu = mtx.get_uu();
   int& nmat = mtx.get_nmat();
   if(const_temp) nmat--;
   std::vector<REAL> mol(nspc);
   int num = mol.size();
   if(num!=nspc){
      cout << " the species arrays must be sized to " << nspc << endl;
      cout << " consistent with therm file " << endl;
      return false;
   }
   REAL& temp = temperature;
   REAL& pres = pressure;
   REAL& enth = ckhbms;
   int ie, iph, nph = phase_nam.size();
   for(iph=0; iph<nph; iph++) totmol[iph] = 0.0;
   for(ie=0; ie<nel; ie++) b0[ie] = 0.0;
   for(is=0; is<num; is++){
      mol[is] = specs[is].mass_fraction/specs[is].spec_wgt;
      iph = iphase[is];
      totmol[iph] += mol[is];
      gfe0[is] = enthalpy_no_R(is,temp)/temp - entropy_no_R(is,temp);
      if(phase[is]=="G") gfe0[is] += log(pres/patom);
      for(ie=0; ie<nel; ie++) b0[ie] += mol_at_spc[ie][is]*mol[is];
      mol[is] += (REAL)1.0e-10;
   }
   
   REAL term, act_coef = 1.0;
   int i, nmat2 = nmat*nmat, iter, niter = 500;
   for(iter=0; iter<niter; iter++){
      for(i=0; i<nmat2; i++) mat[i] = 0.0;
      for(i=0; i<nmat; i++) bb[i] = 0.0;
      for(iph=0; iph<nph; iph++) totmol[iph] = 0.0;
      for(is=0; is<nspc; is++){
         iph = iphase[is];
         totmol[iph] += mol[is];
      }
      for(is=0; is<nspc; is++) {
         enth0[is] = enthalpy_no_R(is,temp)/temp;
         cp0[is] = cp_no_R(is,temp);
         gfe0[is] = enth0[is] - entropy_no_R(is,temp);
         if(phase[is]=="G") gfe0[is] += log(pres/patom);
         gfe[is] = gfe0[is];
         iph = iphase[is];
         // set activity coef here
         if(mol[is]) gfe[is] += log(mol[is]/(totmol[iph]+1.0e-20));
      }
      int j;
      for(i=0; i<nel; i++){
         for(j=0; j<nel; j++){
            for(is=0; is<nspc; is++){
               mat[i+j*nmat] += mol_at_spc[i][is]*mol_at_spc[j][is]*mol[is];
            }
         }
         for(is=0; is<nspc; is++){
            term = mol_at_spc[i][is]*mol[is];
            iph = iphase[is];
            mat[i+(nel+iph)*nmat] += term;
            if(!const_temp) mat[i+(nmat-1)*nmat] += term*enth0[is];
         }
      }
      for(j=0; j<nel; j++){
         for(is=0; is<nspc; is++){
            iph = iphase[is];
            mat[nel+iph+j*nmat] += mol_at_spc[j][is]*mol[is];
         }
      }
      for(iph=0; iph<nph; iph++){
         sum_phase[iph] = 0.0;
         sumh_phase[iph] = 0.0;
      }
      REAL sumh = 0.0;
      for(is=0; is<nspc; is++){
         iph = iphase[is];
         sum_phase[iph] += mol[is];
         term = mol[is]*enth0[is];
         sumh_phase[iph] += term;
         sumh += term;
      }
      for(iph=0; iph<nph; iph++){
         mat[nel+iph+(nel+iph)*nmat] += sum_phase[iph] - totmol[iph];
         if(!const_temp) mat[nel+iph+(nmat-1)*nmat] += sumh_phase[iph];
      }
      if(!const_temp){
         for(j=0; j<nel; j++){
            for(is=0; is<nspc; is++){
               mat[nmat-1+j*nmat] += mol_at_spc[j][is]*mol[is]*enth0[is];
            }
         }
         for(iph=0; iph<nph; iph++)
            mat[nmat-1+(nel+iph)*nmat] += sumh_phase[iph];
         REAL sum1 = 0.0;
         for(is=0; is<nspc; is++){
            sum1 += mol[is]*(cp0[is]+enth0[is]*enth0[is]);
         }
         mat[nmat-1+(nmat-1)*nmat] += sum1;
      } // if(!const_temp)
      for(i=0; i<nel; i++){
         bb[i] += b0[i];
         for(is=0; is<nspc; is++){
            term = mol_at_spc[i][is]*mol[is];
            bb[i] += term*gfe[is];
            bb[i] -= term;
         }
      }
      for(is=0; is<nspc; is++){
         term = mol[is]*gfe[is];
         iph = iphase[is];
         bb[nel+iph] += term;
         if(!const_temp) bb[nmat-1] += term*enth0[is];
      }
      for(iph=0; iph<nph; iph++)
         bb[nel+iph] += totmol[iph] - sum_phase[iph];
      if(!const_temp) bb[nmat-1] += enth/(R*temp) - sumh;
      mtx.solv_mat((REAL)1.0e-8);
      for(is=0; is<nspc; is++){
         iph = iphase[is];
         dmol[is] = uu[nel+iph] + enth0[is]*uu[nmat-1] - gfe[is];
         for(i=0; i<nel; i++) dmol[is] += mol_at_spc[i][is]*uu[i];
      }
      REAL lam1 = 1.0, lam2 = 1.0, lam, max, max1;
      for(is=0; is<nspc; is++){
         if(dmol[is]>0.0){
            iph = iphase[is];
            if(mol[is]/(totmol[iph]+1.0e-20)>(REAL)1.0e-8){
               max = 0.0;
               max1 = fabs(5.0*uu[nel+iph]);
               if(max1>max) max = max1;
               max1 = fabs(5.0*uu[nmat-1]);
               if(max1>max) max = max1;
               max1 = fabs(dmol[is]);
               if(max1>max) max = max1;
               lam = (REAL)2.0/max;
               if(lam<lam1) lam1 = lam;
            }else{
               lam = fabs((-log(mol[is]/(totmol[iph]+1.0e-20))-(REAL)9.2103404)
                  /(dmol[is] - uu[nel+iph]));
               if(lam<lam2) lam2 = lam;
            }
         } // if(dmol[is]>0.0
      } // for(is
      lam = 1.0;
      if(lam1<1.0) lam = lam1;
      if(lam2<lam) lam = lam2;
      max = 0.0;
      REAL term1;
      for(is=0; is<nspc; is++){
         term1 = lam*dmol[is];
         if(term1>70.0) term1 = 70.0;
         mol[is] *= exp(term1);
         iph = iphase[is];
         term1 = mol[is]/(totmol[iph]+1.0e-20)*dmol[is];
         if(fabs(term1)>max) max = fabs(term1);
      }
      for(iph=0; iph<nph; iph++){
         term1 = lam*uu[nel+iph];
         if(term1>70.0) term1 = 70.0;
         totmol[iph] *= exp(term1);
         if(fabs(uu[nel+iph])>max) max = fabs(uu[nel+iph]);
      }
      if(!const_temp) temp *= exp(lam*uu[nmat-1]);
      if(max<=0.5e-4){
         if(!const_temp){
            if(fabs(uu[nmat-1])<=1.0e-4){              
               break;
            }
         }else{
            break;
         }
      }
   } // for(iter
   if(const_temp) nmat++;

   cout << "temperature " << temp << endl;
   REAL mwt = 0.0;
   for(is=0; is<nspc; is++){
      mwt += mol[is];
   }
   mwt = (REAL)1.0/mwt;
   cout << " mixture mwt " << mwt << endl;
   cout << " equilibrium mole fractions" << endl;
   REAL sum = 0.0;
   for(is=0; is<nspc; is++){
      mol[is] *= mwt;
      cout << specs[is].spec->m_spec_name << " " << mol[is] << endl;
      sum += mol[is];
      specs[is].mole_fraction = mol[is];
   }
   cout << sum << endl;
   fprintf(out_file, "\nEquilibrium: \n");
	//test_sys.dump(out_file);

   if(iter==niter) {
     //cout << "Equilibrium may not be converged" << endl;
     return false;
   }

   return true;
}
