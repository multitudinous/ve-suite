#include "NetworkOneUnit_i.h"

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
///////////////////////////////////////////////////////////////////////////////
Body_Unit_i::Body_Unit_i (Body::Executive_ptr exec, std::string name)
    : 
    UnitWrapper(exec,name)
{
	UnitName_ = name;
}
///////////////////////////////////////////////////////////////////////////////
Body_Unit_i::~Body_Unit_i (void)
{
}
///////////////////////////////////////////////////////////////////////////////  
void Body_Unit_i::StartCalc (ACE_ENV_SINGLE_ARG_DECL )
  ACE_THROW_SPEC (( CORBA::SystemException, Error::EUnknown ))
{
    // Add your implementation here
    std::cout<<UnitName_<<" : Starting Calculations"<<std::endl;
    std::ostringstream strm;
    strm << activeId;
    const std::vector< CommandPtr > inputsVec = xmlModelMap[ strm.str() ]->GetInputs();
    std::cout << "Active ID = " << activeId << std::endl;
    std::cout << " model " << std::endl << xmlModelMap[ strm.str() ] << std::endl;
    for( size_t i = 0; i < inputsVec.size(); ++i )
    {
        std::cout << "Input i = " << i << " = " 
            << inputsVec.at( i )->GetCommandName() << std::endl;
    }

    xmlModelMap[ strm.str() ]->GetInput( "mText" )->
        GetDataValuePair( "mText" )->GetData( mText );
    std::cout << "Input mText : "<< mText << std::endl;
    std::string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    //executive_->SetModuleMessage(activeId,msg.c_str());
}
///////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::error (std::string msg)
{

}
///////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::warning (std::string msg)
{
	msg+="\n";
	executive_->SetModuleMessage(activeId, msg.c_str());
}
