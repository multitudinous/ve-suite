// REKS_Thrm_Info.h: interface for the REKS_Thrm_Info class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_REKS_THRM_INFO_H__E8C8FDA0_6B60_48EC_9E64_8777B6D1F9B4__INCLUDED_)
#define AFX_REKS_THRM_INFO_H__E8C8FDA0_6B60_48EC_9E64_8777B6D1F9B4__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "GlobalConst.h"

#include "Element.h"
#include "ElementReader.h"
#include "SpecieReader.h"
#include "SpecieThermo.h"
#include "MixedThermoReader.h"

class REKS_specie_thermo;

class REKS_element_reader;
class REKS_specie_reader;
class REKS_mixed_thermo_reader;

class REKS_Thrm_Info  
{
public:
	REKS_Thrm_Info();
   REKS_Thrm_Info(vector<string> files);
	virtual ~REKS_Thrm_Info();

	//The elements used in the mechanism
	REKS_element_list m_elements;
	//The Species used in the mechanism
	vector<string> m_specs;
	//The Species's thermo information
	REAL lowestT;
	REAL commonT;
	REAL highestT;
	vector<REKS_specie_thermo *> m_thrm_specs;
	
	void ReadThrm(vector<string> files);
	void REKS_Thrm_Info::ClearInfo();
   const REAL& get_atwt(int iel) const 
      {return(m_elements[iel]->m_atomic_weight);}
   const std::string& get_spec_nam(int is) const
      {return (m_specs[is]);}
   const std::string& get_el_nam(int iel) const
      {return(m_elements[iel]->m_name);}
};

#endif // !defined(AFX_REKS_THRM_INFO_H__E8C8FDA0_6B60_48EC_9E64_8777B6D1F9B4__INCLUDED_)
