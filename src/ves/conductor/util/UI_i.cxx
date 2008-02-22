/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/conductor/util/UI_i.h>
#include <ves/conductor/util/OrbThread.h>

#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/model/Model.h>

using namespace ves::conductor::util;
// Implementation skeleton constructor
Body_UI_i::Body_UI_i( Body::Executive_ptr exec, std::string name )
        : UIName_( name ), executive_( Body::Executive::_duplicate( exec ) )
{
    UIName_ = name;
    m_commandNameMap[ "NULL" ] = new ves::open::xml::Command();
    m_commandNameMap[ "NULL" ]->SetCommandName( "NULL" );
}

// Implementation skeleton destructor
Body_UI_i::~Body_UI_i( void )
{}

void Body_UI_i::UpdateNetwork(
    const char * network
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    // Add your implementation here
    if( network != NULL )
        std::cout << network << std::endl;
    std::cout << UIName_ << " :UpdateNetwork called" << std::endl;

    //ui_network_->Load(network);

}

void Body_UI_i::UpdateModuleUI(
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    // Add your implementation here
    if( msg != NULL )
        std::cout << module_id << " : " << msg << std::endl;
    std::cout << UIName_ << " :UpdateModuleUI called" << std::endl;

    //Real code the update UI
    //Package p;
    //p.SetSysId("moduleUI.xml");
    //p.Load(msg, strlen(msg));

    //ui_network_->s_mutexProtect.Lock();
    //ui_network_->modules[module_id].GetPlugin()->UnPack(&(p.GetInterfaceVector())[0]);
    //ui_network_->s_mutexProtect.Unlock();
    //ui_network_->Refresh();
}

void Body_UI_i::UpdateModuleResult(
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    // Add your implementation here
    if( msg != NULL )
        std::cout << module_id << " : " << msg << std::endl;
    std::cout << UIName_ << " :UpdateModuleResult called" << std::endl;
}

void Body_UI_i::UpdateLinkContent(
    CORBA::Long id,
    const char * msg
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    // Add your implementation here
    if( msg != NULL )
        std::cout << id << " : " << msg << std::endl;
    std::cout << UIName_ << " :UpdateLinkContent called" << std::endl;
}

void Body_UI_i::Raise(
    const char * notification
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    // Add your implementation here
    std::string tempString( notification );
    if( !tempString.empty() )
    {
        logWindow->SetMessage( tempString.c_str() );
    }
}

void Body_UI_i::SetXplorerData(
    const char * xplorerData
)
ACE_THROW_SPEC((
                   ::CORBA::SystemException,
                   ::Error::EUnknown
               ) )
{
    //std::cout << xplorerData << std::endl;
    //Receive the data from Xplorer
    std::string tempString( const_cast<char*>( xplorerData ) );
    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( tempString, "Command", "vecommand" );

    std::vector<ves::open::xml::XMLObjectPtr> xmlObjects;
    xmlObjects = networkReader.GetLoadedXMLObjects();

    //std::cout << xmlObjects.size() << std::endl;
    std::vector< ves::open::xml::XMLObjectPtr >::iterator iter;
    //Not sure why this is not working...
    //for( iter = xmlObjects.begin(); iter != xmlObjects.end(); ++iter )
    {
        //iter
        //  VE_XML::Command* temp = dynamic_cast< VE_XML::Command* >( *iter );
        ves::open::xml::CommandPtr temp = xmlObjects.at( 0 );
        if( !temp )
        {
            std::cout << " bad stuff " << std::endl;
        }
        m_commandNameMap[ temp->GetCommandName()] = temp;
        //iter = xmlObjects.erase( iter );
        xmlObjects.clear();
    }
    //logWindow->SetMessage( xplorerData );
}
////////////////////////////////////////////////////////////////////////////////
void Body_UI_i::SetLogWindow( PEThread* logWindow )
{
    this->logWindow = logWindow;
}
///////////////////////////////////////////////////////////////////////////////
const ves::open::xml::CommandPtr Body_UI_i::GetXplorerData( const std::string& commandName )
{
    std::map< std::string, ves::open::xml::CommandPtr >::iterator iter;
    iter = m_commandNameMap.find( commandName );
    if( iter == m_commandNameMap.end() )
    {
        return m_commandNameMap[ "NULL" ];
    }
    ves::open::xml::CommandPtr temp = iter->second;
    m_commandNameMap.erase( iter );
    return temp;
}
