#include "OPPDUnit_i.h"
#include "VE_Conductor/Framework/string_ops.h"

// Implementation skeleton constructor
Body_Unit_i::Body_Unit_i (Body::Executive_ptr exec, std::string name)
  : executive_(Body::Executive::_duplicate(exec))
{
  UnitName_=name;
  return_state = 0;
  _paramHack = NULL;
  //Wrapper = new OPPDWrapper();
  flag = 0;
}
  
// Implementation skeleton destructor
Body_Unit_i::~Body_Unit_i (void)
  {
  }
  
void Body_Unit_i::StartCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    
    // Add your implementation here
    if (flag == 0)
	{
		Wrapper = new OPPDWrapper();
   		Wrapper->loadExcel();
		flag = 1;
		killexcel = 0;
	}
	Wrapper->updateSheet(tempmethod,tempcalcmethod,detectortype,flametype);
	Wrapper->updateFuel(fuelpardbls,fuelselindex);
	Wrapper->updateComp(compardbls,intlinthickdbl,matselindex);
	Wrapper->updateAmbient(ambpardbls);
	Wrapper->updateVisib(massfuelburndbl,solidfuelareadbl,vismatselindex,durmatselindex,vispropselindex,viscombselindex);
	Wrapper->updatePlume(evalabvfiredbl);
	Wrapper->updateVent(ventpardbls);
	Wrapper->updateDetector(detectpardbls,detrtiselindex,dettempratselindex,detspaceselindex);
	Wrapper->updateCable(cblburnareadbl,cableselindex);
	Wrapper->setFuelProps(fuelselindex);
	Wrapper->setMaterialProps(matselindex);
	Wrapper->setVisMatProps(vismatselindex);
	Wrapper->setDurMatProps(durmatselindex);
	Wrapper->setVisProps(vispropselindex);
	Wrapper->setVisCombProps(viscombselindex);
	Wrapper->setDetRtiProps(detrtiselindex);
	Wrapper->setDetTempProps(dettempratselindex);
	Wrapper->setDetSpaceProps(detspaceselindex);
	Wrapper->setCableProps(cableselindex);
	Wrapper->reCalculate();
	Wrapper->getAnswers();
	tsec = Wrapper->tsec;
	tmin = Wrapper->tmin;
	hrrkw = Wrapper->hrrkw;
	hrrbtu = Wrapper->hrrbtu;
	detsprinktime = Wrapper->detsprinktime;
	detsmtime = Wrapper->detsmtime;
	detfthtime = Wrapper->detfthtime;
	flwallinehgt = Wrapper->flwallinehgt;
	flcornerhgt = Wrapper->flcornerhgt;
	flwallhgt = Wrapper->flwallhgt;
	hrrhrr = Wrapper->hrrhrr;
	hrrburndur = Wrapper->hrrburndur;
	hrrhgthesk = Wrapper->hrrhgthesk;
	hrrhgtthom = Wrapper->hrrhgtthom;
	pltemp = Wrapper->pltemp;
	tcltemp = Wrapper->tcltemp;
	visdist = Wrapper->visdist;
	if (killexcel == 1)
	{
		delete Wrapper;
		flag = 0;
		killexcel = 0;
	}

	Package p;
	const char* result;
	bool rv;
	p.SetPackName("result");
	p.SetSysId("result.xml");
	p.intfs.clear();
	p.intfs.resize(1); //each port has its own package
	p.intfs[0].setString("Burn_Dur_Solids_Seconds",to_string(tsec));
	p.intfs[0].setString("Burn_Dur_Solids_Minutes",to_string(tmin));
	p.intfs[0].setString("Cable_Tray_HRR_KW",to_string(hrrkw));
	p.intfs[0].setString("Cable_Tray_HRR_BTU",to_string(hrrbtu));
	p.intfs[0].setString("Sprinkler_Response_Time_Minutes",to_string(detsprinktime));
	p.intfs[0].setString("SmokeDet_Response_Time_Minutes",to_string(detsmtime));
	p.intfs[0].setString("FTHDet_Response_Time_Minutes",to_string(detfthtime));
	p.intfs[0].setString("Line_Wall_Fire_Flame_Height_FT",to_string(flwallinehgt)); 
	p.intfs[0].setString("Corner_Fire_Flame_Height_FT",to_string(flcornerhgt));
	p.intfs[0].setString("Wall_Fire_Flame_Height_FT",to_string(flwallhgt));
	p.intfs[0].setString("Pool_Fire_HRR_KW",to_string(hrrhrr));
	p.intfs[0].setString("Pool_Fire_Burn_Dur_Minutes",to_string(hrrburndur));
	p.intfs[0].setString("Pool_Fire_Flame_Height_Hesk_FT",to_string(hrrhgthesk));
	p.intfs[0].setString("Pool_Fire_Flame_Height_Thomas_FT",to_string(hrrhgtthom));
	p.intfs[0].setString("Plume_Centerline_Temp_F",to_string(pltemp));
	p.intfs[0].setString("Hot_Gas_Layer_Temp_Closed_F",to_string(tcltemp));
	p.intfs[0].setString("Vis_Dist_Through_Smoke",to_string(visdist));
	result = p.Save(rv);
	return_state = 1;
	executive_->SetModuleResult(id_, result); //this marks the end the execution
  }
  
void Body_Unit_i::StopCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    executive_->SetModuleMessage(id_,msg.c_str());
  }
  
void Body_Unit_i::PauseCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    executive_->SetModuleMessage(id_,msg.c_str());
  }
  
void Body_Unit_i::Resume (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    executive_->SetModuleMessage(id_,msg.c_str());
  }
  
char * Body_Unit_i::GetStatusMessage (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    const char *status;
    bool rv;
    Package p;
    p.SetPackName("Status");
    p.SetSysId("status.xml");
    p.intfs.resize(1);
    p.intfs[0].setInt("return_state", return_state);
    status = p.Save(rv);
    return CORBA::string_dup(status);
  }
  
char * Body_Unit_i::GetUserData (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    std::cout<<UnitName_<<" :GetUserData called"<<endl;
    return CORBA::string_dup(data_.c_str());
  }
  
void Body_Unit_i::SetParams (
    const char * param
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    if (string(param)=="")
      return;
    std::cout<<UnitName_<<" :SetParams called"<<endl;
	if ( _paramHack != NULL )
		delete [] _paramHack;
	_paramHack = new char[ strlen(param) + 1 ];
	strcpy(_paramHack, param); 
	Package p;
        
    p.SetSysId("gui.xml");
    p.Load(param, strlen(param));
    //Now make use of p.intfs to get your GUI vars out
	fuelpardbls = p.intfs[0].getDouble1D("fuelpardbls");
	compardbls = p.intfs[0].getDouble1D("compardbls");
	ambpardbls = p.intfs[0].getDouble1D("ambpardbls");
	ventpardbls = p.intfs[0].getDouble1D("ventpardbls");
	detectpardbls = p.intfs[0].getDouble1D("detectpardbls");
	intlinthickdbl = p.intfs[0].getDouble("intlinthickdbl");
	massfuelburndbl = p.intfs[0].getDouble("massfuelburndbl");
	solidfuelareadbl = p.intfs[0].getDouble("solidfuelareadbl");
	evalabvfiredbl = p.intfs[0].getDouble("evalabvfiredbl");
	cblburnareadbl = p.intfs[0].getDouble("cblburnareadbl");
	tempmethod = p.intfs[0].getInt("tempmethod");
	tempcalcmethod = p.intfs[0].getInt("tempcalcmethod");
	detectortype = p.intfs[0].getInt("detectortype");
	flametype = p.intfs[0].getInt("flametype");
	detacttemp = p.intfs[0].getInt("detacttemp");
	matselindex = p.intfs[0].getInt("matselindex");
	fuelselindex = p.intfs[0].getInt("fuelselindex");
	vismatselindex = p.intfs[0].getInt("vismatselindex");
	durmatselindex = p.intfs[0].getInt("durmatselindex");
	vispropselindex = p.intfs[0].getInt("vispropselindex"); 
	viscombselindex = p.intfs[0].getInt("viscombselindex");
	detrtiselindex = p.intfs[0].getInt("detrtiselindex");
	dettempratselindex = p.intfs[0].getInt("dettempratselindex");
	detspaceselindex = p.intfs[0].getInt("detspaceselindex");
	cableselindex = p.intfs[0].getInt("cableselindex");
	killexcel = p.intfs[0].getInt("killexcel");	
  }
  
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
    std::cout<<UnitName_<<" :SetID called"<<endl;
  }
  
CORBA::Long Body_Unit_i::GetID (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    std::cout<<UnitName_<<" :GetID called"<<endl;
    return id_;
  }
  
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
    std::cout<<UnitName_<<" :SetName called"<<endl;
  }
  
char * Body_Unit_i::GetName (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    std::cout<<UnitName_<<" :GetName called"<<endl;
    return CORBA::string_dup(UnitName_.c_str());
  }

void Body_Unit_i::error (std::string msg)
{
  Package p;
  const char* result;
  bool rv;
  p.SetPackName("result");
  p.SetSysId("result.xml");
  msg+="\n";
  executive_->SetModuleMessage(id_, msg.c_str());
  p.intfs.clear();
  result = p.Save(rv);
  return_state = 1;
  executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg+="\n";
  executive_->SetModuleMessage(id_, msg.c_str());
}
