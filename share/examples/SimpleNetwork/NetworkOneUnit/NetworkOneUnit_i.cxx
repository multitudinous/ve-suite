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
//#include <ves/open/xml/XMLObject.h>
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
void Body_Unit_i::StartCalc()
{
    // Add your implementation here
    std::cout<<UnitName_<<" : Starting Calculations"<<std::endl;
    std::ostringstream strm;
    strm << activeId;
    const std::vector< CommandPtr > inputsVec = 
        xmlModelMap[ strm.str() ]->GetInputs();
    std::cout << "Active ID = " << activeId << std::endl;
    std::cout << " model " << std::endl 
        << xmlModelMap[ strm.str() ] << std::endl;

    double inputOne;
    double inputTwo;

    for( size_t i = 0; i < inputsVec.size(); ++i )
    {
        std::cout << "Input i = " << i << " = " 
            << inputsVec.at( i )->GetCommandName() << std::endl;

        size_t tempValue = inputsVec.at( i )->GetNumberOfDataValuePairs();
        for( size_t j=0; j<tempValue; ++j )
        {
            std::string tempString;
       
            if( boost::dynamic_pointer_cast< Command >( inputsVec.at( i )->
                GetDataValuePair( j )->GetDataXMLObject() ) )
            {
                size_t tempValue2 = boost::dynamic_pointer_cast< Command >( 
                    inputsVec.at( i )->GetDataValuePair( j )->GetDataXMLObject())->
                    GetNumberOfDataValuePairs();
                for( size_t k=0; k<tempValue2; ++k )
                {
                    inputOne = boost::dynamic_pointer_cast< Command >( 
                        inputsVec.at( i )->GetDataValuePair( j )->
                        GetDataXMLObject() )->GetDataValuePair( k )->GetDataValue();

                    tempString = boost::dynamic_pointer_cast< Command >( 
                        inputsVec.at( i )->GetDataValuePair( j )->
                        GetDataXMLObject() )->GetDataValuePair( k )->GetDataString();
                    
                    std::cout << "Order test value " << inputOne 
                        << " " << tempString << std::endl;
                }
            }
            else
            {
                tempString = inputsVec.at( i )->
                    GetDataValuePair( j )->GetDataString();

                std::cout << "DataValuePair j = " << j << " = " 
                    << tempString << std::endl;
            }
        }
    }

    CommandPtr tempCommand( new Command() );
    xmlModelMap[ strm.str() ]->GetInput( "mTextOne" )->
        GetDataValuePair( "mTextOne" )->GetData( mTextOne );
    std::cout << "Input mTextOne : "<< mTextOne << std::endl;
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
