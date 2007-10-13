/*************** <auto-copyright.pl BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> **************/
#include <ves/ce/unitwrapper/UnitWrapper.h>

#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/Command.h>

#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/model/ModelCreator.h>

// Event handlers
#include <ves/ce/unitwrapper/SetInputsEventHandler.h>
#include <ves/ce/unitwrapper/GetInputsEventHandler.h>
#include <ves/ce/unitwrapper/EventHandler.h>

#include <sstream>
////////////////////////////////////////////////////////////////////////////////
UnitWrapper::UnitWrapper (Body::Executive_ptr exec, std::string name)
  : executive_(Body::Executive::_duplicate(exec))
{
   UnitName_=name;
   return_state = 0;
   
   ///Initialize VE-Open
   ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "XML",new ves::open::xml::XMLCreator() );
   ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader",new ves::open::xml::shader::ShaderCreator() );
   ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "Model",new ves::open::xml::model::ModelCreator() );
   ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "CAD",new ves::open::xml::cad::CADCreator() );

   eventHandlerMap[ "Set XML Model Inputs" ] = new VE_CE::SetInputsEventHandler();
   eventHandlerMap[ "Get XML Model Inputs" ] = new VE_CE::GetInputsEventHandler();
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
   std::cout<<UnitName_<<" :Start Calc called"<<std::endl;
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
   ves::open::xml::Command returnState;

   returnState.SetCommandName("statusmessage");
   ves::open::xml::DataValuePairWeakPtr data = new ves::open::xml::DataValuePair();
   data->SetDataName("RETURN_STATE");
   data->SetDataType("UNSIGNED INT");
   data->SetDataValue(return_state);
   returnState.AddDataValuePair( data );

   std::vector< std::pair< ves::open::xml::XMLObject*, std::string > > nodes;
   nodes.push_back( std::pair< ves::open::xml::XMLObject*, 
        std::string >( &returnState, "vecommand" ) );

   ves::open::xml::XMLReaderWriter commandWriter;
   std::string status = "returnString";
   commandWriter.UseStandaloneDOMDocumentManager();
   commandWriter.WriteXMLDocument( nodes, status, "Command" );
   return CORBA::string_dup( status.c_str() );
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
   //just send a list command to SetParams
   // the eventhandler will handle the rest
   ves::open::xml::XMLReaderWriter networkWriter;
   networkWriter.UseStandaloneDOMDocumentManager();
   networkWriter.ReadFromString();
   networkWriter.ReadXMLData( param, "Command", "vecommand" );
   std::vector< ves::open::xml::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();
   std::ostringstream idString;
   idString << id;
   eventHandlerMap[ "Set XML Model Inputs" ]->SetBaseObject( xmlModelMap[ idString.str() ] );
   if ( !objectVector.empty() )
   {
      eventHandlerMap[ "Set XML Model Inputs" ]->Execute( objectVector );
   }
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
   
   std::map< std::string, ves::open::xml::model::Model* >::iterator iter;
   iter = xmlModelMap.find( strm.str() );
   if ( iter == xmlModelMap.end() )
   {
      xmlModelMap[ strm.str() ] = new ves::open::xml::model::Model();
   }

   std::cout<<UnitName_<<" :SetID called"<<std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::SetCurID( ::CORBA::Long id )
   ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ))
{
   activeId = id;
   std::cout<<UnitName_<<" :SetCurID called"<<std::endl;
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
   //std::cout << "UnitWrapper::Query called = " << command << std::endl;
   ves::open::xml::XMLReaderWriter networkWriter;
   networkWriter.UseStandaloneDOMDocumentManager();
   networkWriter.ReadFromString();
   networkWriter.ReadXMLData( command, "Command", "vecommand" );
   std::vector< ves::open::xml::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();

   std::string network;
   //The query function assumes 1 command to be processed at a time
   if ( objectVector.size() > 1)
   {
      network.assign( "NULL" );
      return CORBA::string_dup( network.c_str() );
   }

   std::ostringstream strm;
   strm << activeId;

   ves::open::xml::Command* params = dynamic_cast< ves::open::xml::Command* >( objectVector.at( 0 ) );
   std::string commandName = params->GetCommandName();
   std::map< std::string, VE_CE::EventHandler* >::iterator currentEventHandler;
   currentEventHandler = eventHandlerMap.find( commandName );
   if( currentEventHandler != eventHandlerMap.end() )
   {
      currentEventHandler->second->SetBaseObject( xmlModelMap[ strm.str() ] );
      network = currentEventHandler->second->Execute( objectVector );
   }

   if( network.empty() )
   {
      network = "NULL";
   }

   return CORBA::string_dup( network.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::DeleteModuleInstance( ::CORBA::Long module_id )
   ACE_THROW_SPEC(( ::CORBA::SystemException, ::Error::EUnknown ))
{
   std::ostringstream strm;
   strm << module_id;
   
   std::map< std::string, ves::open::xml::model::Model* >::iterator iter;
   iter = xmlModelMap.find( strm.str() );
   if ( iter != xmlModelMap.end() )
   {
      delete iter->second;
      xmlModelMap.erase( iter );
   }
}
