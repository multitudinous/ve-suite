#include "UnitWrapper.h"

#include "VE_Open/XML/Model/Network.h"
#include "VE_Open/XML/Model/Link.h"
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/Model/Point.h"
#include "VE_Open/XML/Model/Port.h"
#include "VE_Open/XML/Command.h"

#include "VE_Open/XML/XMLObjectFactory.h"
#include "VE_Open/XML/XMLCreator.h"
#include "VE_Open/XML/CAD/CADCreator.h"
#include "VE_Open/XML/Shader/ShaderCreator.h"
#include "VE_Open/XML/Model/ModelCreator.h"

// Event handlers
#include "VE_CE/UnitWrapper/SetInputsEventHandler.h"
#include "VE_CE/UnitWrapper/EventHandler.h"

#include <sstream>
////////////////////////////////////////////////////////////////////////////////
UnitWrapper::UnitWrapper (Body::Executive_ptr exec, std::string name)
  : executive_(Body::Executive::_duplicate(exec))
{
   UnitName_=name;
   return_state = 0;
   
   ///Initialize VE-Open
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "XML",new VE_XML::XMLCreator() );
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader",new VE_Shader::ShaderCreator() );
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "Model",new VE_Model::ModelCreator() );
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "CAD",new VE_CAD::CADCreator() );

   eventHandlerMap[ "Set XML Model Inputs" ] = new VE_CE::SetInputsEventHandler();
   //eventHandlerMap[ "Get XML Model Inputs" ] = new VE_CE::SetInputsEventHandler();
   eventHandlerMap[ "Get XML Model Results" ] = new VE_CE::SetInputsEventHandler();
   //eventHandlerMap[ "Get XML Model Port Data" ] = new VE_CE::SetInputsEventHandler();
   //eventHandlerMap[ "Set XML Model Port Data" ] = new VE_CE::SetInputsEventHandler();
}
////////////////////////////////////////////////////////////////////////////////
// Implementation skeleton destructor
UnitWrapper::~UnitWrapper (void)
{
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::StartCalc (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::StopCalc (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
   // Add your implementation here
   std::string msg;
   msg = UnitName_+" : Instant calculation, already finished\n";
   executive_->SetModuleMessage( activeId, msg.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::PauseCalc (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
   // Add your implementation here
   std::string msg;
   msg = UnitName_+" : Instant calculation, already finished\n";
   executive_->SetModuleMessage( activeId, msg.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::Resume (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
   // Add your implementation here
   std::string msg;
   msg = UnitName_+" : Instant calculation, already finished\n";
   executive_->SetModuleMessage( activeId, msg.c_str());
}
////////////////////////////////////////////////////////////////////////////////
char * UnitWrapper::GetStatusMessage (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
   // Add your implementation here
   VE_XML::Command returnState;

   returnState.SetCommandName("statusmessage");
   VE_XML::DataValuePair* data=returnState.GetDataValuePair(-1);
   data->SetDataName("RETURN_STATE");
   data->SetDataType("UNSIGNED INT");
   data->SetDataValue(return_state);

   std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;

   nodes.push_back( 
         std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ) 
            );
   VE_XML::XMLReaderWriter commandWriter;
   std::string status="returnString";
   commandWriter.UseStandaloneDOMDocumentManager();
   commandWriter.WriteXMLDocument( nodes, status, "Command" );
   return CORBA::string_dup(status.c_str());
}
////////////////////////////////////////////////////////////////////////////////
char * UnitWrapper::GetUserData (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	char * result = 0;
	return result;
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::SetParams ( ::CORBA::Long id, const char * param )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
   //just send a sinlge command to SetParams
   // the eventhandler will handle the rest
   VE_XML::XMLReaderWriter networkWriter;
   networkWriter.UseStandaloneDOMDocumentManager();
   networkWriter.ReadFromString();
   networkWriter.ReadXMLData( param, "Command", "vecommand" );
   std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();
   std::ostringstream idString;
   idString << id;
   //inputsMap[ idString.str() ] = objectVector;
   eventHandlerMap[ "Set XML Model Inputs" ]->SetBaseObject( xmlModelMap[ idString.str() ] );
   eventHandlerMap[ "Set XML Model Inputs" ]->Execute( objectVector.at( 0 ) );
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::SetID (
    ::CORBA::Long id
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
   std::ostringstream strm;
   strm << id;
   
   std::map< std::string, VE_Model::Model* >::iterator iter;
   iter = xmlModelMap.find( strm.str() );
   if ( iter == xmlModelMap.end() )
   {
      xmlModelMap[ strm.str() ] = new VE_Model::Model();
   }

   std::cout<<UnitName_<<" :SetID called"<<std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::SetCurID( ::CORBA::Long id )
   ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ))
{
   activeId = id;
}
////////////////////////////////////////////////////////////////////////////////
::Types::ArrayLong* UnitWrapper::GetID()
   ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ))
{
	std::cout<<UnitName_<<" :GetID called"<<std::endl;
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
::CORBA::Long UnitWrapper::GetCurID ()
   ACE_THROW_SPEC(( ::CORBA::SystemException, ::Error::EUnknown ))
{
   // This function returns the id of the currently executed module
   return activeId;
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::SetName (
    const char * name
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	UnitName_ = std::string(name);
    std::cout<<UnitName_<<" :SetName called"<<std::endl;
}
////////////////////////////////////////////////////////////////////////////////
char * UnitWrapper::GetName (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
    std::cout<<UnitName_<<" :GetName called"<<std::endl;
    return CORBA::string_dup(UnitName_.c_str());
}
////////////////////////////////////////////////////////////////////////////////
char * UnitWrapper::Query ( const char* command
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
   VE_XML::XMLReaderWriter networkWriter;
   networkWriter.UseStandaloneDOMDocumentManager();
   networkWriter.ReadFromString();
   networkWriter.ReadXMLData( command, "Command", "vecommand" );
   std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();
   
   std::string network;
   //The query function assumes 1 command to be processed at a time
   if ( objectVector.size() > 1)
   {
      network.assign( "Must send 1 command at a time" );
      return CORBA::string_dup( network.c_str() );
   }
   
   std::ostringstream strm;
   strm << activeId;

   VE_XML::Command* params = dynamic_cast< VE_XML::Command* >( objectVector.at( 0 ) );
   std::string commandName = params->GetCommandName();
   std::map< std::string, VE_CE::EventHandler* >::iterator currentEventHandler;
   currentEventHandler = eventHandlerMap.find( commandName );
   if ( currentEventHandler != eventHandlerMap.end() )
   {
      currentEventHandler->second->SetBaseObject( xmlModelMap[ strm.str() ] );
      network = currentEventHandler->second->Execute( objectVector.at( 0 ) );
   }

	return CORBA::string_dup( network.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::DeleteModuleInstance( ::CORBA::Long module_id )
   ACE_THROW_SPEC(( ::CORBA::SystemException, ::Error::EUnknown ))
{
   std::ostringstream strm;
   strm << module_id;
   
   std::map< std::string, VE_Model::Model* >::iterator iter;
   iter = xmlModelMap.find( strm.str() );
   if ( iter != xmlModelMap.end() )
   {
      delete iter->second;
      xmlModelMap.erase( iter );
   }
}
