
#include "Unit_i.h"
#include "interface.h"

/*
***************************************************
IECM basics:

to read calc var - use index with CALC in it (in the calculate section of header files)

to write a calc var - use index with IN in it (in the input section of header files)

example:

   To Read: use CMN_IDX_CALC_COM_RATIO

   To Write: use CMN_IDX_IN_COM_RATIO AND CMN_IDX_IN_COM_RATIO+1 (bool=0)

   #define CMN_IDX_IN_COM_RATIO 11
    // [11] = override value  [12] = bool (0 to specify value)  [13] = uncert.
   #define CMN_SZ_IN_COM_RATIO 3     // length

****************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Implementation skeleton constructor

Body_Unit_i::Body_Unit_i (Body::Executive_ptr exec, std::string name)
: executive_(Body::Executive::_duplicate(exec))
{
	UnitName_=name;
	cmu_run.Initialization();
	cmu_run.LoadDefaults();
	
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Implementation skeleton destructor
Body_Unit_i::~Body_Unit_i (void)
{

}

////////////////////////////////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::StartCalc (ACE_ENV_SINGLE_ARG_DECL)
							 ACE_THROW_SPEC ((CORBA::SystemException, Error::EUnknown))
{
	int sig_dig = 10;
	
	if(_id_map["GASTYPE"]) cmu_run._rei_gasi = true;
	else					 cmu_run._rei_gasi = false;
	
	if(_id_map.find("WGSR")==_id_map.end()) Run_Base();
	else									  Run_Sour();
	
	std::map<std::string, int>::iterator iter;
	
	for(iter=_id_map.begin(); iter!=_id_map.end(); iter++) {

		Interface intf;
		intf._type = 0;
		intf._category = 0;
		intf._id = iter->second;
		
		int ll = -99999;
		
		if(iter->first=="Power") {
			
			ll = 25;
			
			intf.setDouble("N2",	cmu_run.linkLog[ll][LINK_IDX_GAS+0]);
			intf.setDouble("O2",	cmu_run.linkLog[ll][LINK_IDX_GAS+1]);
			intf.setDouble("H2O", cmu_run.linkLog[ll][LINK_IDX_GAS+2]);
			intf.setDouble("CO2", cmu_run.linkLog[ll][LINK_IDX_GAS+3]);
			intf.setDouble("CO",	cmu_run.linkLog[ll][LINK_IDX_GAS+4]);
			intf.setDouble("HCl", cmu_run.linkLog[ll][LINK_IDX_GAS+5]);
			intf.setDouble("SO2", cmu_run.linkLog[ll][LINK_IDX_GAS+6]);
			intf.setDouble("SO3", cmu_run.linkLog[ll][LINK_IDX_GAS+7]);
			intf.setDouble("NO",	cmu_run.linkLog[ll][LINK_IDX_GAS+8]);
			intf.setDouble("NO2", cmu_run.linkLog[ll][LINK_IDX_GAS+9]);
			intf.setDouble("NH3", cmu_run.linkLog[ll][LINK_IDX_GAS+10]);
			intf.setDouble("Ar",	cmu_run.linkLog[ll][LINK_IDX_GAS+11]);
			
			intf.setDouble("H2",	0.0);
			intf.setDouble("CH4", 0.0);
			intf.setDouble("H2S", 0.0);
			intf.setDouble("COS", 0.0);
			
			intf.setDouble("TEMPERATURE", floor(cmu_run.linkLog[ll][LINK_IDX_TMP]-458.67));
			intf.setDouble("PRESSURE", cmu_run.linkLog[ll][LINK_IDX_PRES]);
		}
		else if(iter->first=="old REI REI_Gasi") {
			ll = 28;
			
			intf.setDouble("CO",	REI_gasi_link_[LINK_IDX_SYNGAS+0]);
			intf.setDouble("H2",	REI_gasi_link_[LINK_IDX_SYNGAS+1]);
			intf.setDouble("CH4", REI_gasi_link_[LINK_IDX_SYNGAS+2]);
			intf.setDouble("H2S", REI_gasi_link_[LINK_IDX_SYNGAS+3]);
			intf.setDouble("COS", REI_gasi_link_[LINK_IDX_SYNGAS+4]);
			intf.setDouble("NH3", REI_gasi_link_[LINK_IDX_SYNGAS+5]);
			intf.setDouble("HCl", REI_gasi_link_[LINK_IDX_SYNGAS+6]);
			intf.setDouble("CO2", REI_gasi_link_[LINK_IDX_SYNGAS+7]);
			intf.setDouble("H2O", REI_gasi_link_[LINK_IDX_SYNGAS+8]);
			intf.setDouble("N2",	REI_gasi_link_[LINK_IDX_SYNGAS+9]);
			intf.setDouble("Ar",	REI_gasi_link_[LINK_IDX_SYNGAS+10]);
			intf.setDouble("O2",	REI_gasi_link_[LINK_IDX_SYNGAS+11]);
			
			intf.setDouble("SO2", 0.0);
			intf.setDouble("SO3", 0.0);
			intf.setDouble("NO",	0.0);
			intf.setDouble("NO2", 0.0);
			
			intf.setDouble("TEMPERATURE", floor(REI_gasi_link_[LINK_IDX_TMP]-458.67));
			intf.setDouble("PRESSURE", REI_gasi_link_[LINK_IDX_PRES]);
		}
		else {
			if (iter->first=="ASU"  ) { 
				ll = 30;
			}
			else if(iter->first=="WGSR" ) {
				ll = 34;
			}
			else if(iter->first=="SRS"  ) {
				ll = 31;
			}
			else if(iter->first=="STACK") { 
				ll = 26;
				float mw = cmu_run.cmn_outputs[CMN_IDX_OUT_MW_NET];  // needed for calcs below
				double co2 = cmu_run.stk_outputs[STK_IDX_OUT_STACK_CO2]*907.18474/mw;
				double co = cmu_run.stk_outputs[STK_IDX_OUT_STACK_CO]*907.18474/mw;
				double hcl = cmu_run.stk_outputs[STK_IDX_OUT_STACK_HCL]*907.18474/mw;
				double so2 = cmu_run.stk_outputs[STK_IDX_OUT_STACK_SO2]*907.18474/mw;
				double so3 = cmu_run.stk_outputs[STK_IDX_OUT_STACK_SO3]*907.18474/mw;
				double no = cmu_run.stk_outputs[STK_IDX_OUT_STACK_NO]*907.18474/mw;           // all units are kg/MW*h
				double no2 = cmu_run.stk_outputs[STK_IDX_OUT_STACK_NO2]*907.18474/mw;
				double ash = cmu_run.stk_outputs[STK_IDX_OUT_STACK_ASH]*907.18474/mw;
				double nox = cmu_run.stk_outputs[STK_IDX_OUT_STACK_NOX]*907.18474/mw;
				double sox = cmu_run.stk_outputs[STK_IDX_OUT_STACK_SOX]*907.18474/mw;
				
				co2 = floor(co2);
				co = floor(co*100+0.5)/100;
				hcl = floor(hcl*100+0.5)/100;
				so2 = floor(so2*100+0.5)/100;
				so3 = floor(so3*100+0.5)/100;
				no = floor(no*100+0.5)/100;
				no2 = floor(no2*100+0.5)/100;
				ash = floor(ash*100+0.5)/100;
				nox = floor(nox*100+0.5)/100;
				sox = floor(sox*100+0.5)/100;
				
				intf.setDouble("CO2_EMISSION", co2);
				intf.setDouble("CO_EMISSION", co);
				intf.setDouble("HCL_EMISSION", hcl);
				intf.setDouble("SO2_EMISSION", so2);
				intf.setDouble("SO3_EMISSION", so3);
				intf.setDouble("NO_EMISSION", no);
				intf.setDouble("NO2_EMISSION", no2);
				intf.setDouble("ASH_EMISSION", ash);    // names for doug
				intf.setDouble("NOX_EMISSION", nox);
				intf.setDouble("SOX_EMISSION", sox);
			}
			else if(iter->first=="GASI" || iter->first=="REI_Gasi") {
				ll = 28;
				intf.setDouble("COAL_IN", floor(cmu_run.tex_outputs[TEX_IDX_OUT_14]+0.5));
				intf.setDouble("WATER_IN", floor(cmu_run.tex_outputs[TEX_IDX_OUT_15]+0.5));
				intf.setDouble("OX_IN", floor(cmu_run.tex_outputs[TEX_IDX_OUT_2]+0.5));
			}
			else if(iter->first=="SELX" ) {
				ll = 33;
				double co2_in = floor(cmu_run.linkLog[31][LINK_IDX_SYNGAS+7]* 44.0/2000);
				double co2_out = floor(cmu_run.linkLog[33][LINK_IDX_SYNGAS+7]* 44.0/2000);
				intf.setDouble("CO2_IN", co2_in);
				intf.setDouble("CO2_OUT", co2_out);
				intf.setDouble("CO2_CAP", co2_in - co2_out);
			}
			else if(iter->first=="NETWORK" ) {
				ll = -1;
				intf.setDouble("MW_GROSS", floor(cmu_run.cmn_outputs[CMN_IDX_OUT_MW_GROSS]));
				intf.setDouble("MW_NET", floor(cmu_run.cmn_outputs[CMN_IDX_OUT_MW_NET]));
				intf.setDouble("NET_EFF", floor(cmu_run.cmn_outputs[CMN_IDX_OUT_NET_EFF]*sig_dig+0.5)/sig_dig);
				intf.setDouble("CAPITAL_CST", floor(cmu_run.stk_outputs[STK_IDX_OUT_TOT_CST_TBL+1]*100+0.5)/100);
				intf.setDouble("ELEC_CST", floor(cmu_run.stk_outputs[STK_IDX_OUT_TOT_CST_TBL+3]*10+0.5)/10);
			}
			else if(iter->first=="GLOBAL") {
				ll = 100000;
			}
			else if(iter->first=="Coal") {
				ll = 16;
			}
			else {
				std::cout << "PROPER MODEL NOT FOUND FOR " << iter->first << std::endl;
			}
			
			if(ll>=0 && ll<99999) {
				intf.setDouble("CO",  cmu_run.linkLog[ll][LINK_IDX_SYNGAS+0]);
				intf.setDouble("H2",  cmu_run.linkLog[ll][LINK_IDX_SYNGAS+1]);
				intf.setDouble("CH4", cmu_run.linkLog[ll][LINK_IDX_SYNGAS+2]);
				intf.setDouble("H2S", cmu_run.linkLog[ll][LINK_IDX_SYNGAS+3]);
				intf.setDouble("COS", cmu_run.linkLog[ll][LINK_IDX_SYNGAS+4]);
				intf.setDouble("NH3", cmu_run.linkLog[ll][LINK_IDX_SYNGAS+5]);
				intf.setDouble("HCl", cmu_run.linkLog[ll][LINK_IDX_SYNGAS+6]);
				intf.setDouble("CO2", cmu_run.linkLog[ll][LINK_IDX_SYNGAS+7]);
				intf.setDouble("H2O", cmu_run.linkLog[ll][LINK_IDX_SYNGAS+8]);
				intf.setDouble("N2",  cmu_run.linkLog[ll][LINK_IDX_SYNGAS+9]);
				intf.setDouble("Ar",  cmu_run.linkLog[ll][LINK_IDX_SYNGAS+10]);
				intf.setDouble("O2",  cmu_run.linkLog[ll][LINK_IDX_SYNGAS+11]);
				
				intf.setDouble("SO2", 0.0);
				intf.setDouble("SO3", 0.0);
				intf.setDouble("NO",  0.0);
				intf.setDouble("NO2", 0.0);
				
				intf.setDouble("TEMPERATURE", floor(cmu_run.linkLog[ll][LINK_IDX_TMP]-458.67));
				if(ll==30) intf.setDouble("PRESSURE", 615.0);
				else       intf.setDouble("PRESSURE", cmu_run.linkLog[ll][LINK_IDX_PRES]);
			}
		}
		
		if(ll>=0 && ll<99999) {
			double tot 
				= intf.getDouble("H2")  + intf.getDouble("CH4") + intf.getDouble("H2S")
				+ intf.getDouble("N2")  + intf.getDouble("O2")  + intf.getDouble("H2O")
				+ intf.getDouble("CO2") + intf.getDouble("CO")  + intf.getDouble("HCl")
				+ intf.getDouble("SO2") + intf.getDouble("SO3") + intf.getDouble("NO")
				+ intf.getDouble("NO2") + intf.getDouble("NH3") + intf.getDouble("Ar")
				+ intf.getDouble("COS");
			
			double flrt
				= intf.getDouble("H2")	* 2.0
				+ intf.getDouble("CH4") * 16.0
				+ intf.getDouble("H2S") * 34.0
				+ intf.getDouble("N2")	* 28.0
				+ intf.getDouble("O2")	* 32.0
				+ intf.getDouble("H2O") * 18.0
				+ intf.getDouble("CO2") * 44.0
				+ intf.getDouble("CO")	* 28.0
				+ intf.getDouble("HCl") * 36.5
				+ intf.getDouble("SO2") * 64.0
				+ intf.getDouble("SO3") * 80.0
				+ intf.getDouble("NO")	* 30.0
				+ intf.getDouble("NO2") * 46.0
				+ intf.getDouble("NH3") * 17.0
				+ intf.getDouble("Ar")	* 40.0
				+ intf.getDouble("COS") * 60.0;
			
			// to mole fraction
			//intf.setDouble("H2",	intf.getDouble("H2")/tot);
			//intf.setDouble("CH4", intf.getDouble("CH4")/tot);
			//intf.setDouble("H2S", intf.getDouble("H2S")/tot);
			//intf.setDouble("N2",	intf.getDouble("N2")/tot);
			//intf.setDouble("O2",	intf.getDouble("O2")/tot);
			//intf.setDouble("H2O", intf.getDouble("H2O")/tot);
			//intf.setDouble("CO2", intf.getDouble("CO2")/tot);
			//intf.setDouble("CO",	intf.getDouble("CO")/tot);
			//intf.setDouble("HCl", intf.getDouble("HCl")/tot);
			//intf.setDouble("SO2", intf.getDouble("SO2")/tot);
			//intf.setDouble("SO3", intf.getDouble("SO3")/tot);
			//intf.setDouble("NO",	intf.getDouble("NO")/tot);
			//intf.setDouble("NO2", intf.getDouble("NO2")/tot);
			//intf.setDouble("NH3", intf.getDouble("NH3")/tot);
			//intf.setDouble("Ar",	intf.getDouble("Ar")/tot);
			//intf.setDouble("COS", intf.getDouble("COS")/tot);
			
			intf.setDouble("FLOWRATE", floor(flrt/2000*10+0.5)/10);
		}
		
		if(ll!=-99999) {
			CORBA::Long id = (CORBA::Long)intf._id;
			CORBA::Long pi = 0;
			
			std::string packed;
			intf.pack(packed);
			
			executive_->SetExportData(id, pi, packed.c_str());
		}
}
_id_map.clear();
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::StopCalc (
							ACE_ENV_SINGLE_ARG_DECL
							)
							ACE_THROW_SPEC ((
							CORBA::SystemException
							, Error::EUnknown
							))
{
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::PauseCalc (
							 ACE_ENV_SINGLE_ARG_DECL
							 )
							 ACE_THROW_SPEC ((
							 CORBA::SystemException
							 , Error::EUnknown
							 ))
{
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::Resume (
						  ACE_ENV_SINGLE_ARG_DECL
						  )
						  ACE_THROW_SPEC ((
						  CORBA::SystemException
						  , Error::EUnknown
						  ))
{
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////
char * Body_Unit_i::GetStatusMessage (
									  ACE_ENV_SINGLE_ARG_DECL
									  )
									  ACE_THROW_SPEC ((
									  CORBA::SystemException
									  , Error::EUnknown
									  ))
{
	return CORBA::string_dup(status_.c_str());
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////
char * Body_Unit_i::GetUserData (
								 ACE_ENV_SINGLE_ARG_DECL
								 )
								 ACE_THROW_SPEC ((
								 CORBA::SystemException
								 , Error::EUnknown
								 ))
{
	return CORBA::string_dup("NOTHING");
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::SetParams (const char * param
							 ACE_ENV_SINGLE_ARG_DECL)
							 ACE_THROW_SPEC ((
							 CORBA::SystemException
							 , Error::EUnknown
							 ))
{
	// Unpack the Parameters from the param
	Interface intf;
	intf.unpack_ids(&param[0]);
	intf.unpack(&param[96]);
	
	_id_map[intf.getString("NAME_")] = intf._id;
	
	if(intf.getString("NAME_")=="NETWORK") {
		//std::cout << "CMU_UNIT says: got a network\n";
	}
	else if(intf.getString("NAME_")=="INIT") {
		//std::cout << "CMU_UNIT says: got an init\n";	  
	}
	else if(intf.getString("NAME_")=="ASU") {
		//std::cout << "CMU_UNIT says: got an asu\n";
		//cmu_run.asu_inputs[ASU_IDX_IN_NTRAINS] = intf.getInt("asu_idx_in_ntrains");  // not in gui, but leave just in case
		//cmu_run.asu_inputs[ASU_IDX_IN_NTRAINS] = 0;
		
		cmu_run.asu_inputs[ASU_IDX_IN_NSPARES]     = intf.getInt("asu_idx_in_nspares");  // number of spare trains (spare asu's)

		cmu_run.asu_inputs[ASU_IDX_IN_OXID_O2_PCT] = intf.getDouble("o2_purity");           // o2 purity

	}
	else if(intf.getString("NAME_")=="COAL") {
		//std::cout << "CMU_UNIT says: got a coal\n";    
	}
	else if(intf.getString("NAME_")=="CMN") {
		//std::cout << "CMU_UNIT says: got a cmn\n";	  
	}
	else if(intf.getString("NAME_")=="CSEQ") {
		//std::cout << "CMU_UNIT says: got a cseq\n";   
	}
	else if(intf.getString("NAME_")=="Power") {
		//std::cout << "CMU_UNIT says: got a Power\n"; 
		cmu_run.gts_inputs[GTS_IDX_IN_NTURB] = intf.getInt("gts_idx_in_nturb");  // link to gasifier
		cmu_run.gts_inputs[GTS_IDX_IN_PRES_RATIO_COMP] = intf.getDouble("gts_idx_in_pres_ratio");
		cmu_run.gts_inputs[GTS_IDX_IN_HR_STEAM] = intf.getDouble("gts_idx_in_heat_rate");
		cmu_run.igcc_inputs[IGCC_IDX_IN_SO3_PCT] = intf.getDouble("gts_idx_in_sox_so3");
		cmu_run.igcc_inputs[IGCC_IDX_IN_EF_NOX] = intf.getDouble("gts_idx_in_nox_cons");
		cmu_run.igcc_inputs[IGCC_IDX_IN_PCT_NO] = intf.getDouble("gts_idx_in_nox_no");
		cmu_run.igcc_inputs[IGCC_IDX_IN_PCT_CO] = intf.getDouble("gts_idx_in_carbon_co");
		
	}
	else if(intf.getString("NAME_")=="IGCC") {
		//std::cout << "CMU_UNIT says: got an igcc\n"; 
	}
	else if(intf.getString("NAME_")=="SELX") {
		//std::cout << "CMU_UNIT says: got a selx\n";
		//cmu_run.selx_inputs[SELX_IDX_IN_NTOWERS] = intf.getInt("selx_idx_in_ntowers");
		//cmu_run.selx_inputs[SELX_IDX_IN_NTOWERS+1] = 0;
		
		cmu_run.selx_inputs[SELX_IDX_IN_NSPARES] = intf.getInt("selx_idx_in_nspares");
		
		cmu_run.selx_inputs[SELX_IDX_IN_CO2_EFF] = intf.getDouble("co2_removal_d");
		cmu_run.selx_inputs[SELX_IDX_IN_CO2_EFF+1] = 0;
	}
	else if(intf.getString("NAME_")=="SRS") {
		//std::cout << "CMU_UNIT says: got a srs\n";
		//cmu_run.srs_inputs[SRS_IDX_IN_SELX_NTOWERS] = intf.getInt("srs_idx_in_selx_ntowers");
		//cmu_run.srs_inputs[SRS_IDX_IN_SELX_NTOWERS+1] = 0;

		cmu_run.srs_inputs[SRS_IDX_IN_H2S_CLAUS_EFF] = intf.getDouble("sulf_rcv_eff");  // normal input

		cmu_run.srs_inputs[SRS_IDX_IN_H2S_SELX_EFF]   = intf.getDouble("h2s_rmv_eff");  // override calc input #
		cmu_run.srs_inputs[SRS_IDX_IN_H2S_SELX_EFF+1] = 0;                              // override calc input bool

		cmu_run.srs_inputs[SRS_IDX_IN_H2S_BEVS_EFF] = intf.getDouble("tail_gas_sre");   // normal input

	}
	else if(intf.getString("NAME_")=="STACK") {
		//std::cout << "CMU_UNIT says: got a stack\n";
	}
	else if(intf.getString("NAME_")=="GASI") {
		//std::cout << "CMU_UNIT says: got a gasi\n";
		cmu_run.tex_inputs[TEX_IDX_IN_NTRAINS] = intf.getInt("tex_idx_in_ntrains");
		cmu_run.tex_inputs[TEX_IDX_IN_NTRAINS+1] = 0;
		cmu_run.tex_inputs[TEX_IDX_IN_NSPARES] = intf.getInt("tex_idx_in_nspares");
		cmu_run.tex_inputs[TEX_IDX_IN_TEMP] = intf.getDouble("tex_idx_in_temp");
		cmu_run.tex_inputs[TEX_IDX_IN_RATIO_H2O_SLURRY] = intf.getDouble("tex_idx_in_wc_ratio");

		_id_map["GASTYPE"] = 0;
	}
	else if(intf.getString("NAME_")=="WGSR") {
		//std::cout << "CMU_UNIT says: got a wgsr\n";

		cmu_run.wgsr_inputs[WGSR_IDX_IN_CO_EFF]   = intf.getDouble("co_conv_eff");      // CO Conversion efficiency
		cmu_run.wgsr_inputs[WGSR_IDX_IN_CO_EFF+1] = 0;                                  // override boolean

		cmu_run.wgsr_inputs[WGSR_IDX_IN_MSR_CO]   = intf.getDouble("steam_added");      // Steam added (mH2O/mCO)
		cmu_run.wgsr_inputs[WGSR_IDX_IN_MSR_CO+1] = 0;                                  // override boolean

	}
	else if(intf.getString("NAME_")=="REI_Gasi") {
		//std::cout << "CMU_UNIT says: got a rei_gasi\n";
		
		_id_map["GASTYPE"] = 1;
	} 
	else if(intf.getString("NAME_")=="Coal") {
		//std::cout << "CMU_UNIT says: got a Coal\n";
		
		double c = intf.getDouble("ulti_c_d");
		double h = intf.getDouble("ulti_h_d");
		double o = intf.getDouble("ulti_o_d");
		double n = intf.getDouble("ulti_n_d");
		double s = intf.getDouble("ulti_s_d");
		double cl = intf.getDouble("ulti_cl_d");
		double ashu = intf.getDouble("ulti_ash_d");
		double h2o = intf.getDouble("prox_h2o_d");
		double vm = intf.getDouble("prox_vm_d");
		double ashp = intf.getDouble("prox_ash_d");
		double fc = intf.getDouble("prox_fc_d");
		
		// Not Used
		// intf.getDouble("ashc_bao_d")
		// intf.getDouble("ashc_sro_d")
		// intf.getDouble("coal_name_s")
		
		cmu_run.igcc_inputs[IGCC_IDX_IN_ROM_CST] = intf.getDouble("coal_cost_d");
		std::cout << cmu_run.cmn_inputs[IGCC_IDX_IN_ROM_CST] << std::endl;
		cmu_run.cmn_inputs[CMN_IDX_IN_COAL_HHV] = intf.getDouble("hhv_d");
		cmu_run.cmn_inputs[CMN_IDX_IN_COAL_C] = c * (1.0 - h2o / 100.0);
		cmu_run.cmn_inputs[CMN_IDX_IN_COAL_H] = h * (1.0 - h2o / 100.0);
		cmu_run.cmn_inputs[CMN_IDX_IN_COAL_O] = o * (1.0 - h2o / 100.0);
		cmu_run.cmn_inputs[CMN_IDX_IN_COAL_CL] = cl * (1.0 - h2o / 100.0);
		cmu_run.cmn_inputs[CMN_IDX_IN_COAL_S] = s * (1.0 - h2o / 100.0);
		cmu_run.cmn_inputs[CMN_IDX_IN_COAL_N] = n * (1.0 - h2o / 100.0);
		cmu_run.cmn_inputs[CMN_IDX_IN_COAL_ASH] = ashu * (1.0 - h2o / 100.0);
		cmu_run.cmn_inputs[CMN_IDX_IN_COAL_H2O] = h2o;
		cmu_run.cmn_inputs[CMN_IDX_IN_ASH_SIO2] = intf.getDouble("ashc_sio2_d");
		cmu_run.cmn_inputs[CMN_IDX_IN_ASH_AL2O3] = intf.getDouble("ashc_al2o3_d");
		cmu_run.cmn_inputs[CMN_IDX_IN_ASH_FE2O3] = intf.getDouble("ashc_fe2o3_d");
		cmu_run.cmn_inputs[CMN_IDX_IN_ASH_CAO] = intf.getDouble("ashc_cao_d");
		cmu_run.cmn_inputs[CMN_IDX_IN_ASH_MGO] = intf.getDouble("ashc_mgo_d");
		cmu_run.cmn_inputs[CMN_IDX_IN_ASH_NA2O] = intf.getDouble("ashc_na2o_d");
		cmu_run.cmn_inputs[CMN_IDX_IN_ASH_K2O] = intf.getDouble("ashc_k2o_d");
		cmu_run.cmn_inputs[CMN_IDX_IN_ASH_TIO2] = intf.getDouble("ashc_tio2_d");
		cmu_run.cmn_inputs[CMN_IDX_IN_ASH_P2O5] = intf.getDouble("ashc_p2o5_d");
		cmu_run.cmn_inputs[CMN_IDX_IN_ASH_SO3] = intf.getDouble("ashc_so3_d");
		cmu_run.cmn_inputs[CMN_IDX_IN_ASH_MNO2] = 0.0;

		std::cout << "c " << cmu_run.cmn_inputs[CMN_IDX_IN_COAL_C] << std::endl;
		std::cout << "h " << cmu_run.cmn_inputs[CMN_IDX_IN_COAL_H] << std::endl;
		std::cout << "o " << cmu_run.cmn_inputs[CMN_IDX_IN_COAL_O] << std::endl;
		std::cout << "n " << cmu_run.cmn_inputs[CMN_IDX_IN_COAL_N] << std::endl;
		std::cout << "s " << cmu_run.cmn_inputs[CMN_IDX_IN_COAL_S] << std::endl;
		std::cout << "cl " << cmu_run.cmn_inputs[CMN_IDX_IN_COAL_CL] << std::endl;
		std::cout << "a " << cmu_run.cmn_inputs[CMN_IDX_IN_COAL_ASH] << std::endl;
		std::cout << "w " << cmu_run.cmn_inputs[CMN_IDX_IN_COAL_H2O] << std::endl;

		//cmu_run.cmn_inputs[CMN_IDX_IN_COAL_C] = 73.81;
		//cmu_run.cmn_inputs[CMN_IDX_IN_COAL_H] = 4.88;
		//cmu_run.cmn_inputs[CMN_IDX_IN_COAL_O] = 5.41;
		//cmu_run.cmn_inputs[CMN_IDX_IN_COAL_N] = 1.42;
		//cmu_run.cmn_inputs[CMN_IDX_IN_COAL_S] = 2.13;
		//cmu_run.cmn_inputs[CMN_IDX_IN_COAL_CL] = 0.06;
		//cmu_run.cmn_inputs[CMN_IDX_IN_COAL_ASH] = 7.24;
		//cmu_run.cmn_inputs[CMN_IDX_IN_COAL_H2O] = 5.05;



	}
	else if(intf.getString("NAME_")=="GLOBAL") {
		//std::cout << "CMU_UNIT says: got a GLOBAL\n";
		
		cmu_run.cmn_inputs[CMN_IDX_IN_CF] = intf.getDouble("plant_capacity");
		cmu_run.cmn_inputs[CMN_IDX_IN_COST_YR] = atof(intf.getString("year_costs").c_str());
		if(intf.getString("cst_cur_dollar")=="Constant") cmu_run.cmn_inputs[CMN_IDX_IN_INFLAT_CTL] = 1;
		else											   cmu_run.cmn_inputs[CMN_IDX_IN_INFLAT_CTL] = 0;
		if(intf.getInt("which_to_use")==1) {
			cmu_run.cmn_inputs[CMN_IDX_IN_FCF] = intf.getDouble("fixed_charge");
			cmu_run.cmn_inputs[CMN_IDX_IN_FCF+1] = 0;
			cmu_run.cmn_inputs[CMN_IDX_IN_DISCOUNT] = intf.getDouble("discnt_rate");
			cmu_run.cmn_inputs[CMN_IDX_IN_DISCOUNT+1] = 0;
		} else {
			cmu_run.cmn_inputs[CMN_IDX_IN_FCF] = 0;
			cmu_run.cmn_inputs[CMN_IDX_IN_FCF+1] = 1;
			cmu_run.cmn_inputs[CMN_IDX_IN_DISCOUNT] = 0;
			cmu_run.cmn_inputs[CMN_IDX_IN_DISCOUNT+1] = 1;
			
			cmu_run.cmn_inputs[CMN_IDX_IN_INF] = intf.getDouble("inflation_rate");
			cmu_run.cmn_inputs[CMN_IDX_IN_INF+1] = 0;
			cmu_run.cmn_inputs[CMN_IDX_IN_BOOK_LIFE] = intf.getDouble("plant_life");
			cmu_run.cmn_inputs[CMN_IDX_IN_REAL_BOND] = intf.getDouble("bond_interest");
			cmu_run.cmn_inputs[CMN_IDX_IN_PREF_STOCK] = intf.getDouble("preferred_stock_return");
			cmu_run.cmn_inputs[CMN_IDX_IN_COM_STOCK] = intf.getDouble("common_stock_return");
			cmu_run.cmn_inputs[CMN_IDX_IN_DEBT_RATIO] = intf.getDouble("percent_debt");
			cmu_run.cmn_inputs[CMN_IDX_IN_PREF_RATIO] = intf.getDouble("percent_p_equity");
			
			cmu_run.cmn_inputs[CMN_IDX_IN_COM_RATIO] = intf.getDouble("percent_c_equity");
			cmu_run.cmn_inputs[CMN_IDX_IN_COM_RATIO+1] = 0;
			
			cmu_run.cmn_inputs[CMN_IDX_IN_TAX_FED] = intf.getDouble("fed_tax");
			cmu_run.cmn_inputs[CMN_IDX_IN_TAX_STATE] = intf.getDouble("state_tax");
			cmu_run.cmn_inputs[CMN_IDX_IN_TAX_PROP] = intf.getDouble("property_tax");
			cmu_run.cmn_inputs[CMN_IDX_IN_ITC] = intf.getDouble("invest_tax_credit");
		}
	}
	else {
		std::cout << "CMU_UNIT says: got an UNKNOWN " << intf.getString("NAME_") << std::endl;
	}
  }
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  void Body_Unit_i::SetID (
	  CORBA::Long id
	  ACE_ENV_ARG_DECL
	  )
	  ACE_THROW_SPEC ((
	  CORBA::SystemException
	  , Error::EUnknown
	  ))
  {
	  // Add your implementation here
	  id_=id;
  }
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  CORBA::Long Body_Unit_i::GetID (
	  ACE_ENV_SINGLE_ARG_DECL
	  )
	  ACE_THROW_SPEC ((
	  CORBA::SystemException
	  , Error::EUnknown
	  ))
  {
	  // Add your implementation here
	  return id_;
  }
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  void Body_Unit_i::SetName (
	  const char * name
	  ACE_ENV_ARG_DECL
	  )
	  ACE_THROW_SPEC ((
	  CORBA::SystemException
	  , Error::EUnknown
	  ))
  {
	  // Add your implementation here
	  UnitName_ = std::string(name);
  }
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  char * Body_Unit_i::GetName (
	  ACE_ENV_SINGLE_ARG_DECL
	  )
	  ACE_THROW_SPEC ((
	  CORBA::SystemException
	  , Error::EUnknown
	  ))
  {
	  return CORBA::string_dup(UnitName_.c_str());
  }
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  int Body_Unit_i::Run_Base()
  {
	  int tex_sour_runconf[14] = {36, -20, 15, 24, 16, 30, 28, 31, 25, 26, 23, 0};
	  int tex_sour_runconf_sz = 12;
	  int tex_sour_runconf_id = CONFIG_IGCC;
	  int i;
	  
	  run_conf_sz_ = tex_sour_runconf_sz;
	  run_conf_id_ = tex_sour_runconf_id;
	  for (i=0; i<run_conf_sz_; i++)
		  run_conf_[i] = tex_sour_runconf[i];
	  cmu_run.run_orig_model(run_conf_, run_conf_sz_, run_conf_id_);
	  
	  return 0;
  }
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  int Body_Unit_i::Run_Sour()
  {
	  int tex_sour_runconf[14] = {36, -20, 15, 24, 16, 30, 28, 34, 31, 33, 25, 26, 23, 0};
	  int tex_sour_runconf_sz = 14;
	  int tex_sour_runconf_id = CONFIG_IGCC;
	  int i;
	  
	  run_conf_sz_ = tex_sour_runconf_sz;
	  run_conf_id_ = tex_sour_runconf_id;
	  for (i=0; i<run_conf_sz_; i++)
		  run_conf_[i] = tex_sour_runconf[i];
	  cmu_run.run_orig_model(run_conf_, run_conf_sz_, run_conf_id_);
	  return 0;
  }
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // This call is obsolete
  int Body_Unit_i::Run_REI()
  {
	  Run_Sour();
	  
	  int tex_sour_runconf[14] = {34, 31, 33, 25, 26, 23, 15, 24, 0}; // added 15, 24
	  int tex_sour_runconf_sz = 9;
	  int tex_sour_runconf_id = CONFIG_IGCC;
	  int i;
	  
	  run_conf_sz_ = tex_sour_runconf_sz;
	  run_conf_id_ = tex_sour_runconf_id;
	  for (i=0; i<run_conf_sz_; i++)
		  run_conf_[i] = tex_sour_runconf[i];
	  
	  double tot = cmu_run.linkLog[28][LINK_IDX_SYNGAS+0] +
		  cmu_run.linkLog[28][LINK_IDX_SYNGAS+1] +
		  cmu_run.linkLog[28][LINK_IDX_SYNGAS+2] +
		  cmu_run.linkLog[28][LINK_IDX_SYNGAS+3] +
		  cmu_run.linkLog[28][LINK_IDX_SYNGAS+4] +
		  cmu_run.linkLog[28][LINK_IDX_SYNGAS+5] +
		  cmu_run.linkLog[28][LINK_IDX_SYNGAS+6] +
		  cmu_run.linkLog[28][LINK_IDX_SYNGAS+7] +
		  cmu_run.linkLog[28][LINK_IDX_SYNGAS+8] +
		  cmu_run.linkLog[28][LINK_IDX_SYNGAS+9] +
		  cmu_run.linkLog[28][LINK_IDX_SYNGAS+10] +
		  cmu_run.linkLog[28][LINK_IDX_SYNGAS+11];
	  
	  for (i=0; i<LINK_SZ; i++)
		  REI_gasi_link_[i] = cmu_run.linkLog[28][i];
	  
	  REI_gasi_link_[LINK_IDX_SYNGAS+0] = 0.469581;    // CO
	  REI_gasi_link_[LINK_IDX_SYNGAS+1] = 0.179776;   // H2
	  REI_gasi_link_[LINK_IDX_SYNGAS+2] = 0.0;		  // CH4
	  REI_gasi_link_[LINK_IDX_SYNGAS+3] = 0.00432159;  // H2S
	  REI_gasi_link_[LINK_IDX_SYNGAS+4] = 0.00045447;	 // COS
	  REI_gasi_link_[LINK_IDX_SYNGAS+5] = 0.0;		  // NH3
	  REI_gasi_link_[LINK_IDX_SYNGAS+6] = 0.0;		  // HCl
	  REI_gasi_link_[LINK_IDX_SYNGAS+7] = 0.0941138;  // CO2
	  REI_gasi_link_[LINK_IDX_SYNGAS+8] = 0.22854;	 // H2O
	  REI_gasi_link_[LINK_IDX_SYNGAS+9] = 0.016406;  // N2
	  REI_gasi_link_[LINK_IDX_SYNGAS+10] = 0.00228225; // Ar
	  REI_gasi_link_[LINK_IDX_SYNGAS+11] = 0.0; 	  // O2
	  
	  double tot2 = 
		  REI_gasi_link_[LINK_IDX_SYNGAS+0] +
		  REI_gasi_link_[LINK_IDX_SYNGAS+1] + 
		  REI_gasi_link_[LINK_IDX_SYNGAS+2] + 
		  REI_gasi_link_[LINK_IDX_SYNGAS+3] + 
		  REI_gasi_link_[LINK_IDX_SYNGAS+4] + 
		  REI_gasi_link_[LINK_IDX_SYNGAS+5] + 
		  REI_gasi_link_[LINK_IDX_SYNGAS+6] + 
		  REI_gasi_link_[LINK_IDX_SYNGAS+7] + 
		  REI_gasi_link_[LINK_IDX_SYNGAS+8] + 
		  REI_gasi_link_[LINK_IDX_SYNGAS+9] + 
		  REI_gasi_link_[LINK_IDX_SYNGAS+10] +
		  REI_gasi_link_[LINK_IDX_SYNGAS+11];
	  
	  REI_gasi_link_[LINK_IDX_SYNGAS+0]  = REI_gasi_link_[LINK_IDX_SYNGAS+0]/tot2*tot;
	  REI_gasi_link_[LINK_IDX_SYNGAS+1]  = REI_gasi_link_[LINK_IDX_SYNGAS+1]/tot2*tot;
	  REI_gasi_link_[LINK_IDX_SYNGAS+2]  = REI_gasi_link_[LINK_IDX_SYNGAS+2]/tot2*tot;
	  REI_gasi_link_[LINK_IDX_SYNGAS+3]  = REI_gasi_link_[LINK_IDX_SYNGAS+3]/tot2*tot;
	  REI_gasi_link_[LINK_IDX_SYNGAS+4]  = REI_gasi_link_[LINK_IDX_SYNGAS+4]/tot2*tot;
	  REI_gasi_link_[LINK_IDX_SYNGAS+5]  = REI_gasi_link_[LINK_IDX_SYNGAS+5]/tot2*tot;
	  REI_gasi_link_[LINK_IDX_SYNGAS+6]  = REI_gasi_link_[LINK_IDX_SYNGAS+6]/tot2*tot;
	  REI_gasi_link_[LINK_IDX_SYNGAS+7]  = REI_gasi_link_[LINK_IDX_SYNGAS+7]/tot2*tot;
	  REI_gasi_link_[LINK_IDX_SYNGAS+8]  = REI_gasi_link_[LINK_IDX_SYNGAS+8]/tot2*tot;
	  REI_gasi_link_[LINK_IDX_SYNGAS+9]  = REI_gasi_link_[LINK_IDX_SYNGAS+9]/tot2*tot;
	  REI_gasi_link_[LINK_IDX_SYNGAS+10] = REI_gasi_link_[LINK_IDX_SYNGAS+10]/tot2*tot;
	  REI_gasi_link_[LINK_IDX_SYNGAS+11] = REI_gasi_link_[LINK_IDX_SYNGAS+11]/tot2*tot;
	  
	  cmu_run.run_model_withlink(run_conf_, run_conf_sz_, run_conf_id_, REI_gasi_link_, LINK_SZ);
	  return 0;
  }
