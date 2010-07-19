#include "VirtualLabUnit_i.h"
//#include "cfdHelper.h"
using namespace std;
#include <string>
#include <stdlib.h>

Body_Unit_i::Body_Unit_i (Body::Executive_ptr exec, std::string name)
    : 
    UnitWrapper(exec,name)
{
   UnitName_=name;  
}

Body_Unit_i::~Body_Unit_i (void)
{
}

void Body_Unit_i::StartCalc (ACE_ENV_SINGLE_ARG_DECL )
  ACE_THROW_SPEC (( CORBA::SystemException, Error::EUnknown ))
{
    // Add your implementation here
    std::cout<<UnitName_<<" : Starting Calculations"<<std::endl;
   /*
    *	Simulation Steps:
    *
    *	1. Store the current working directory so we can change back to it and then
    *	   go to the Virtual Lab directory
    *	2. Write a txt file that used be VL as designtable
    *	3. Start VL simulation using a predefined script and model
    *	4. Gather information from VL result output for Excel and GraphicalPlugin
    *	5. Return to original working directory 
    *	6. Start Excel Spread Shreet calculation
    *	7. Gather information from Excel for GraphicalPlugin
    *	8. Send information to GraphicalPlugin
    */
  
   char *cwd;
   char buffer[_MAX_PATH];
   if ((cwd = _getcwd(buffer, _MAX_PATH)) == NULL)
   {
      std::cout << "Couldn't get the current working directory!" << std::endl;
      return ;
   }

   //_chdir( "D:\\VehicleCon\\DADS8000" );
   _chdir("\\VehicleCon\\DADS8000");
  
   system("\\VehicleCon\\DADS8000\\vlrun.bat");

   _chdir( cwd );        // return to original directory


}

void Body_Unit_i::StopCalc ( ACE_ENV_SINGLE_ARG_DECL )
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
	if (std::string(param)=="")
      return;

   std::cout<<UnitName_<<" :SetParams called"<<endl;
  
   
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
   //std::cout<<UnitName_<<" :GetID called"<<endl;
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
   //std::cout<<UnitName_<<" :SetName called"<<endl;
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
   //std::cout<<UnitName_<<" :GetName called"<<endl;
   return CORBA::string_dup(UnitName_.c_str());
}

