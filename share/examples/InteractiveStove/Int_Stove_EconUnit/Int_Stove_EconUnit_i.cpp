#include "ExcelWrapper.h"

#include "Int_Stove_EconUnit_i.h"

#include <buildwin/src/ves/open/moduleS.h>  //"VE_Open/skel/moduleS.h"
// no longet using this cag 12-15-08 #include "VE_Conductor/Network/string_ops.h"

#include <iostream>

using namespace std;

// Implementation skeleton constructor
Body_Unit_i::Body_Unit_i (Body::Executive_ptr exec, std::string name)
  : executive_(Body::Executive::_duplicate(exec))
{
  UnitName_=name;
  return_state = 0;
  closesheets = 0;
  excelRunning = false;
  numbaffles = 0;
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
	const char* baffle_info = 0;
    const char* result = 0;
    bool ok = true;
    //Package p;
	bool rv = false;

	fflush(NULL);
/*
    baffle_info = executive_->GetImportData(id_, 0); //port 0 will be the gas input port;

	if (string(baffle_info)=="")
    {
	   //error("Int_Stove_Econ: Missing input.");
	   return;
    }

	for ( int j=0; j<5; j++ )
	{
		baffle1.push_back(0);
		baffle2.push_back(0);
		baffle3.push_back(0);
		baffle4.push_back(0);
		baffle5.push_back(0);
		baffle6.push_back(0);
		baffle7.push_back(0);
	}

    //p.SetSysId("Int_Stove_Econ_in.xml");
    //p.Load(baffle_info, strlen(baffle_info));
	//numbaffles = p.intfs[0].getInt("numbaffles", &ok);
    //baffle1 = p.intfs[0].getDouble1D("baffle1", &ok);
	//baffle2 = p.intfs[0].getDouble1D("baffle2", &ok);
	//baffle3 = p.intfs[0].getDouble1D("baffle3", &ok);
	//baffle4 = p.intfs[0].getDouble1D("baffle4", &ok);
	//baffle5 = p.intfs[0].getDouble1D("baffle5", &ok);
	//baffle6 = p.intfs[0].getDouble1D("baffle6", &ok);
	//baffle7 = p.intfs[0].getDouble1D("baffle7", &ok);


	if ( !excelRunning && closesheets != 1 )
	{
		Wrapper = new ExcelWrapper();
		Wrapper->loadExcel();
		excelRunning = true;
	}

	//DO SOME EXCEL STUFF HERE!!!!!!!!!!!!!!!!!!!!!!
	Wrapper->updateSheet(baffle1,baffle2,baffle3,baffle4,baffle5,
							baffle6,baffle7,cost_array,numbaffles);

	Wrapper->getAnswers();
	double baf_mat_cost = Wrapper->baf_mat_cost;
	double baf_const_cost = Wrapper->baf_const_cost;
	double tot_stove_cost = Wrapper->tot_stove_cost; 

	if ( excelRunning && closesheets == 1 )
	{
	 	delete Wrapper;
		excelRunning = false;
	}

	//p.SetPackName("result");
	//p.SetSysId("result.xml");
	//p.intfs.clear();
	//p.intfs.resize(1); //each port has its own package
	//p.intfs[0].setString("Baffle_Material_Cost",to_string(baf_mat_cost));
	//p.intfs[0].setString("Baffle_Construction_Cost",to_string(baf_const_cost));
	//p.intfs[0].setString("Total_Stove_Cost",to_string(tot_stove_cost));
	//result = p.Save(rv);
	return_state = 1;
	executive_->SetModuleResult(id_, result); //this marks the end the execution

    */
	std::string msg;
    msg = "Interactive Stove Economics Model Completed\n";
    executive_->SetModuleMessage(id_,msg.c_str());

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
    std::string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    executive_->SetModuleMessage(id_,msg.c_str());
  }
  /*
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
    return NULL;//CORBA::string_dup(data_.c_str());
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
    Package p;
        
    p.SetSysId("gui.xml");
    p.Load(param, strlen(param));
    //Now make use of p.intfs to get your GUI vars out
    cost_array = p.intfs[0].getDouble1D("cost_array");
    closesheets = p.intfs[0].getInt("closesheets");
    
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
*/
