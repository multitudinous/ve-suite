// REKS_Thrm_Info.cpp: implementation of the REKS_Thrm_Info class.
//
//////////////////////////////////////////////////////////////////////

#include "REKS_Thrm_Info.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

REKS_Thrm_Info::REKS_Thrm_Info()
{
	
}

REKS_Thrm_Info::REKS_Thrm_Info(vector<string> inp_files)
{
   ReadThrm(inp_files);
}

REKS_Thrm_Info::~REKS_Thrm_Info()
{
	ClearInfo();
}

void REKS_Thrm_Info::ReadThrm(vector<string> inp_files)
{

	REKS_element_reader elem_reader;
	REKS_specie_reader spec_reader;
	REKS_mixed_thermo_reader mixed_trm_reader;

	elem_reader.reks_thrm_info=this;
	spec_reader.reks_thrm_info=this;
	mixed_trm_reader.reks_thrm_info=this;
	elem_reader.parse(inp_files);
	spec_reader.parse(inp_files);
	mixed_trm_reader.parse(inp_files);
}

void REKS_Thrm_Info::ClearInfo()
{
	int num = m_elements.size(), i;
	for(i=0; i<num; i++) 
	  {
	    //   cout<<i<<endl;
	    delete m_elements[i];
	  }

	num = this->m_thrm_specs.size();
	for(i=0; i<num; i++) 
	  {
	    delete m_thrm_specs[i];
	    // cout<<i;
	  }
	
	//m_elements.clear();
	//m_specs.clear();
	//m_thrm_specs.clear();
}
