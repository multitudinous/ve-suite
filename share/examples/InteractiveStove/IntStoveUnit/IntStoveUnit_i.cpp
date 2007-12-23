#include "IntStoveUnit_i.h"
#include "create_stove.h"

//#include "VE_Conductor/Network/string_ops.h"

#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/model/ModelCreator.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>

#include <ves/ce/unitwrapper/SetInputsEventHandler.h>
#include <ves/ce/unitwrapper/EventHandler.h>

XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;

// Implementation skeleton constructor
Body_Unit_i::Body_Unit_i (Body::Executive_ptr exec, std::string name)
  : UnitWrapper(exec,name)
{
  UnitName_=name;
  return_state = 0;
  runnum = 0;
  m_baffNum = 0;
  m_runNum = 0;
  New_stove = new Create_stove();
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
    std::cout<<UnitName_<<" : Starting Calculations"<<std::endl;
    std::ostringstream strm;
    strm << activeId;

    system("rm -f ../star.vtu");

    xmlModelMap[ strm.str() ]->GetInput( "numbaffles" )->GetDataValuePair( "numbaffles" )->GetData( numbaffles );
    xmlModelMap[ strm.str() ]->GetInput( "baffle1" )->GetDataValuePair( "baffle1" )->GetData( baffle1 );
    xmlModelMap[ strm.str() ]->GetInput( "baffle2" )->GetDataValuePair( "baffle2" )->GetData( baffle2 );
    xmlModelMap[ strm.str() ]->GetInput( "baffle3" )->GetDataValuePair( "baffle3" )->GetData( baffle3 );
    xmlModelMap[ strm.str() ]->GetInput( "baffle4" )->GetDataValuePair( "baffle4" )->GetData( baffle4 );
    xmlModelMap[ strm.str() ]->GetInput( "baffle5" )->GetDataValuePair( "baffle5" )->GetData( baffle5 );
    xmlModelMap[ strm.str() ]->GetInput( "baffle6" )->GetDataValuePair( "baffle6" )->GetData( baffle6 );
    xmlModelMap[ strm.str() ]->GetInput( "baffle7" )->GetDataValuePair( "baffle7" )->GetData( baffle7 );
    //xmlModelMap[ strm.str() ]->GetInput( "m_runNum" )->GetDataValuePair( "m_runNum" )->GetData( runNum );

      runnum++;

      New_stove->RunNewStove( numbaffles, baffle1, baffle2, baffle3, 
                                       baffle4, baffle5, baffle6, baffle7, runnum );
      //cout << "|--- Creating VTK Files -------------|"<<endl;
      //system( "./createvtk > /dev/null" );

   /*char sep;
   sep = ' ';

   for ( int i=0; i<7; i++)
   {
      if ( numbaffles == 0 )
         return;
      if ( numbaffles > 0 )
        baffle1str.at(i) = baffle1.at(i);
      if ( numbaffles > 1 )
        baffle2str.at(i) = baffle2.at(i);
      if ( numbaffles > 2 )
        baffle3str.at(i) = baffle3.at(i);
      if ( numbaffles > 3 )
        baffle4str.at(i) = baffle4.at(i);
      if ( numbaffles > 4 )
        baffle5str.at(i) = baffle5.at(i);
      if ( numbaffles > 5 )
        baffle6str.at(i) = baffle6.at(i);
      if ( numbaffles > 6 )
        baffle7str.at(i) = baffle7.at(i);
   }*/

   //Package p;
	//const char* result;
	//const char* outport;
	//bool rv;

	//p.intfs.resize(1);

	//p.intfs[0].setDouble("numbaffles",numbaffles);
	//p.intfs[0].setDouble1D("baffle1",baffle1);
	//p.intfs[0].setDouble1D("baffle2",baffle2);
	//p.intfs[0].setDouble1D("baffle3",baffle3);
	//p.intfs[0].setDouble1D("baffle4",baffle4);
	//p.intfs[0].setDouble1D("baffle5",baffle5);
	//p.intfs[0].setDouble1D("baffle6",baffle6);
	//p.intfs[0].setDouble1D("baffle7",baffle7);
	//p.SetPackName("ExportData");
	//p.SetSysId("test.xml");
	//outport = p.Save(rv);
	//executive_->SetExportData(id_, 0, outport);

	/*p.SetPackName("result");
	p.SetSysId("result.xml");
	p.intfs.clear();
	result = p.Save(rv);
	return_state = 1;
	executive_->SetModuleResult(id_, result); //this marks the end the execution*/

    string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    //executive_->SetModuleMessage(activeId,msg.c_str());
  }
/*
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
    Package p;
        
    p.SetSysId("gui.xml");
    p.Load(param, strlen(param));
    //Now make use of p.intfs to get your GUI vars out
    numbaffles = p.intfs[0].getInt("numbaffles");
    baffle1 = p.intfs[0].getDouble1D("baffle1");
    baffle2 = p.intfs[0].getDouble1D("baffle2");
    baffle3 = p.intfs[0].getDouble1D("baffle3");
    baffle4 = p.intfs[0].getDouble1D("baffle4");
    baffle5 = p.intfs[0].getDouble1D("baffle5");
    baffle6 = p.intfs[0].getDouble1D("baffle6");
    baffle7 = p.intfs[0].getDouble1D("baffle7");
    
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
*/
void Body_Unit_i::error (std::string msg)
{
  //Package p;
  //const char* result;
  //bool rv;
  //p.SetPackName("result");
  //p.SetSysId("result.xml");
  //msg+="\n";
  //executive_->SetModuleMessage(id_, msg.c_str());
  //p.intfs.clear();
  //result = p.Save(rv);
  //return_state = 1;
  //executive_->SetModuleResult(id_, result); //this marks the end the execution
}

void Body_Unit_i::warning (std::string msg)
{
  msg+="\n";
  executive_->SetModuleMessage(activeId, msg.c_str());
}
