#include "NetworkThreeUnit_i.h"

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
  : UnitWrapper(exec,name)
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
    xmlModelMap[ strm.str() ]->GetInput( "mNetworkThreeInputs" )->
		GetDataValuePair( "mNetworkThreeInputs" )->
        GetData( mNetworkThreeInputs );

    const std::vector< CommandPtr > inputsVec = 
		xmlModelMap[ strm.str() ]->GetInputs();
    std::cout << "Active ID = " << activeId << " Num Inputs = " 
		<< inputsVec.size() << std::endl;

    for( size_t i = 0; i < inputsVec.size(); ++i )
    {
        std::cout << "Input i = " << i << " = " 
            << inputsVec.at( i )->GetCommandName() << std::endl;

        size_t tempValue = inputsVec.at( i )->GetNumberOfDataValuePairs();
        for( size_t j=0; j<tempValue; ++j )
        {
            for( size_t k=0; k<mNetworkThreeInputs.size(); ++k )
            {
                std::cout << "Unit Three Input " << k << ": " << mNetworkThreeInputs.at( k ) << std::endl; 
            }
        }
    }

	DataValuePairPtr dvp( new DataValuePair() );
	dvp->SetData( "UNIT 3 VECTOR TEST", mNetworkThreeInputs );

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );

    command->SetCommandName( "SimpleNetworkTest-Three Unit" );
	command->AddDataValuePair( dvp );

    double temp = 3.0;
    DataValuePairPtr dvpNum( new DataValuePair() );
	dvpNum->SetData( "UNIT 3 ORDER TEST", temp );
	command->AddDataValuePair( dvpNum );

    xmlModelMap[ strm.str() ]->SetResult( command );

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
