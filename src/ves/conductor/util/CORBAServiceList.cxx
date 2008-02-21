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
#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <ves/conductor/util/OrbThread.h>

#include <tao/BiDir_GIOP/BiDirGIOP.h>
#include <ace/SString.h>
#include <ace/SStringfwd.h>

#include <sstream>

#include <wx/app.h>
#include <wx/utils.h>

using namespace ves::open::xml;
using namespace ves::conductor::util;

vprSingletonImp( CORBAServiceList );

////////////////////////////////////////////////////////////////////////////////
CORBAServiceList::CORBAServiceList( void )
:
mOrbCounter( 0 )
{
    mTimeZero = ACE_Time_Value::zero;
    mTimeOutValue.msec( 3 );
    nullTextPtr = new ves::open::xml::Command();
    nullTextPtr->SetCommandName( "NULL" );
}
////////////////////////////////////////////////////////////////////////////////
CORBAServiceList::~CORBAServiceList()
{
    ;
} // Never gets called, don't implement
////////////////////////////////////////////////////////////////////////////////
void CORBAServiceList::SetArgcArgv( int argc, char** argv )
{
    p_ui_i = 0;
    pelog = 0;

    //Copy the command line args because tao deletes them after processing them
    peArgc = argc;
    peArgv = new char*[ argc ];
    for( int i = 0; i < peArgc; ++ i )
    {
        int stringLength = strlen( argv[ i ] );
        peArgv[ i ] = new char[ stringLength + 1 ];
        strcpy( peArgv[ i ], argv[ i ] );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CORBAServiceList::CleanUp()
{
    //Gets deleted by wx now
    /*if ( pelog )
    {
       delete pelog;
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void CORBAServiceList::SetNamingContext( CosNaming::NamingContext_ptr naming_context )
{
    namingContext = naming_context;
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::IsConnectedToXplorer( void )
{
    if( CORBA::is_nil( vjobs.in() ) )
    {
        return ConnectToXplorer();
    }

    try
    {
        vjobs->_non_existent();
    }
    catch ( ... )
    {
        return ConnectToXplorer();
    }

    return true;
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::IsConnectedToCE( void )
{
    if( CORBA::is_nil( veCE.in() ) )
    {
        return ConnectToCE();
    }

    try
    {
        veCE->_non_existent();
    }
    catch ( ... )
    {
        return ConnectToCE();
    }

    return true;
}
/////////////////////////////////////////////////////////////
std::vector< std::string > CORBAServiceList::GetListOfServices( void )
{
    // unsigned long numServices;
    //namingContext.list( numServices, bindList, nameList );
    //Need to look at CORBA book for for loop
    return serviceList;
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::ConnectToCE( void )
{
    if( pelog == NULL )
    {
        pelog = new PEThread();
    }

    if( !IsConnectedToNamingService() )
    {
        return false;
    }

    if (( p_ui_i != 0 ) && !CORBA::is_nil( veCE.in() ) )
    {
        return true;
    }

    try
    {
        CreateCORBAModule();
        if (( p_ui_i == 0 ) || CORBA::is_nil( veCE.in() ) )
        {
            return false;
        }
    }
    catch ( CORBA::Exception& ex )
    {
        GetMessageLog()->SetMessage( "Can't find executive or UI registration error\n" );
        GetMessageLog()->SetMessage( ex._info().c_str() );
        return false;
    }
    return true;
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::ConnectToXplorer( void )
{
    if( pelog == NULL )
    {
        pelog = new PEThread();
    }

    if( !IsConnectedToNamingService() )
    {
        return false;
    }

    ///This is the old way of communication
    try
    {
        CosNaming::Name name( 1 );
        name.length( 1 );
        //Now get the reference of the VE server
        name[0].id   = CORBA::string_dup( "Master" );
        name[0].kind = CORBA::string_dup( "VE_Xplorer" );
        CORBA::Object_var naming_context_object =
            orb->resolve_initial_references( "NameService" );
        CosNaming::NamingContext_var naming_context1 =
            CosNaming::NamingContext::_narrow( naming_context_object.in() );
        CORBA::Object_var ve_object = naming_context1->resolve( name );
        vjobs = VjObs::_narrow( ve_object.in() );

        GetMessageLog()->SetMessage( "Connected to VE server.\n" );
    }
    catch ( CORBA::Exception& ex )
    {
        GetMessageLog()->SetMessage( "Can't find VE server\n" );
        GetMessageLog()->SetMessage( ex._info().c_str() );
        return false;
    }

    ///Cannot assume that p_ui_i and m_ui have been initialized yet
    if (( p_ui_i == 0 ) || CORBA::is_nil( m_ui.in() ) )
    {
        CreateCORBAModule();
        if (( p_ui_i == 0 ) || CORBA::is_nil( m_ui.in() ) )
        {
            return false;
        }
    }

    ///This is the new way of communication
    try
    {
        CosNaming::Name xplorerCom( 1 );
        xplorerCom.length( 1 );
        //Now get the reference of the VE server
        xplorerCom[0].id   = CORBA::string_dup( "Test" );
        xplorerCom[0].kind = CORBA::string_dup( "VE_Xplorer" );
        CORBA::Object_var naming_context_object =
            orb->resolve_initial_references( "NameService" );
        CosNaming::NamingContext_var naming_context1 =
            CosNaming::NamingContext::_narrow( naming_context_object.in() );
        CORBA::Object_var ve_object = naming_context1->resolve( xplorerCom );
        m_xplorer = Body::VEXplorer::_narrow( ve_object.in() );
        m_xplorer->RegisterUI( p_ui_i->UIName_.c_str(), m_ui.in() );
        GetMessageLog()->SetMessage( "Connected to two-way VE server.\n" );
    }
    catch ( CORBA::Exception& ex )
    {
        GetMessageLog()->SetMessage( "Can't find NEW VE server\n" );
        GetMessageLog()->SetMessage( ex._info().c_str() );
        return false;
    }
    return true;
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::IsConnectedToNamingService( void )
{
    if( CORBA::is_nil( naming_context.in() ) )
    {
        return ConnectToNamingService();
    }

    return true;
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::ConnectToNamingService( void )
{
    //Copy the command line args because tao deletes them after processing them
    int argc = peArgc;
    char** argv = new char*[ argc ];
    for( int i = 0; i < argc; ++ i )
    {
        int stringLength = strlen( peArgv[ i ] );
        argv[ i ] = new char[ stringLength + 1 ];
        strcpy( argv[ i ], peArgv[ i ] );
    }

    try
    {
        // First initialize the ORB,
        orb = CORBA::ORB_init( argc, argv, "" ); // the ORB name, it can be anything!
        //delete the left over char*
        for( int i = 0; i < argc; ++ i )
        {
            delete [] argv[ i ];
            argv[i] = 0;
        }
        delete [] argv;
        argv = 0;

        //Here is the part to contact the naming service and get the reference for the executive
        CORBA::Object_var naming_context_object =
            orb->resolve_initial_references( "NameService" );
        naming_context = CosNaming::NamingContext::_narrow( naming_context_object.in() );
        GetMessageLog()->SetMessage( "Initialized ORB and connection to the Naming Service\n" );
        return true;
    }
    catch ( CORBA::Exception& ex )
    {
        orb->destroy();
        GetMessageLog()->SetMessage( "CORBA exception raised! Can't init ORB or can't connect to the Naming Service\n" );
        GetMessageLog()->SetMessage( ex._info().c_str() );
        return false;
    }
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::DisconnectFromCE( void )
{
    try
    {
        veCE->UnRegisterUI( p_ui_i->UIName_.c_str() );
        delete p_ui_i;
        p_ui_i = NULL;

        GetMessageLog()->SetMessage( "Disconnect successful.\n" );
    }
    catch ( CORBA::SystemException& ex )
    {
        GetMessageLog()->SetMessage( "Disconnect failed.\n" );
        GetMessageLog()->SetMessage( ex._info().c_str() );
        return false;
    }

    return true;
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::DisconnectFromXplorer( void )
{
    VjObs::_tao_release( vjobs );
    GetMessageLog()->SetMessage( "Disconnect VE suceeded.\n" );
    Body::VEXplorer::_tao_release( m_xplorer );
    return true;
}
/////////////////////////////////////////////////////////////
void CORBAServiceList::CheckORBWorkLoad( void )
{
    try
    {
        if( CORBA::is_nil( orb.in() ) )
        {
            return;
        }

        //This sleep is not needed in this use case since the ord work is in
        // the main wx event thread. Therefore a sleep just slows down the 
        // event loop and does not reduce the resources on the computer and
        // only frustrates the user.
        //::wxMilliSleep( 500 );
        /*if( mOrbCounter > 4 )
        {
            mOrbCounter = 0;
            return;
        }
        else
        {
            mOrbCounter++;
        }*/
        
        if( orb->work_pending( mTimeOutValue ) )
        {
            orb->perform_work( mTimeZero );
        }

        const ves::open::xml::CommandPtr textOutput = GetGUIUpdateCommands( "TEXT_FEEDBACK" );
        if( textOutput->GetCommandName() != "NULL" )
        {
            GetMessageLog()->SetMessage( textOutput->GetDataValuePair( "TEXT_OUTPUT" )->GetDataString().c_str() );
        }
    }
    catch ( ... )
    {
        ;
    }
}
/////////////////////////////////////////////////////////////
void CORBAServiceList::CreateCORBAModule( void )
{
    try
    {
        long id = time( NULL );
        std::ostringstream dirStringStream;
        dirStringStream << "UIClient" << id;
        std::string UINAME = dirStringStream.str();
        CosNaming::Name name( 1 );
        name.length( 1 );
        name[0].id = CORBA::string_dup( "Executive" );
        CORBA::Object_var naming_context_object = orb->resolve_initial_references( "NameService" );
        naming_context = CosNaming::NamingContext::_narrow( naming_context_object.in() );

        CORBA::Object_var exec_object = naming_context->resolve( name );
        veCE = Body::Executive::_narrow( exec_object.in() );
        //Create the Servant
        if( p_ui_i == NULL )
        {
            p_ui_i = new Body_UI_i( veCE.in(), UINAME );
            //pass the Frame's pointer to the UI corba implementation
            p_ui_i->SetLogWindow( GetMessageLog() );
            //Here is the code to set up the ROOT POA
            CORBA::Object_var poa_object = orb->resolve_initial_references( "RootPOA" ); // get the root poa
            poa_root = PortableServer::POA::_narrow( poa_object.in() );
            PortableServer::POAManager_var poa_manager = poa_root->the_POAManager();

            CORBA::PolicyList policies( 1 );
            policies.length( 1 );

            CORBA::Any pol;
            pol <<= BiDirPolicy::BOTH;
            policies[0] =
                orb->create_policy( BiDirPolicy::BIDIRECTIONAL_POLICY_TYPE, pol );

            // Create POA as child of RootPOA with the above policies.  This POA
            // will receive request in the same connection in which it sent
            // the request
            try
            {
                poa = poa_root->create_POA( "childPOA", poa_manager.in(),
                                            policies );
            }
            catch ( const PortableServer::POA::AdapterAlreadyExists & )
            {
                std::cout << " Child POA Already Connected : Do nothing " << std::endl;
            }

            // Creation of childPOA is over. Destroy the Policy objects.
            for( CORBA::ULong i = 0; i < policies.length(); ++i )
            {
                policies[i]->destroy();
            }

            poa_manager->activate();
            PortableServer::ObjectId_var idObject =
                PortableServer::string_to_ObjectId( UINAME.c_str() );
            poa->activate_object_with_id( idObject.in() , p_ui_i );

            //Activate it to obtain the object reference
            CORBA::Object_var objectRef = poa->id_to_reference( idObject.in() );
            m_ui = Body::UI::_narrow( objectRef.in() );

            try
            {
                veCE->RegisterUI( p_ui_i->UIName_.c_str(), m_ui.in() );
            }
            catch ( CORBA::Exception& ex )
            {
                GetMessageLog()->SetMessage( "Can't find executive or UI registration error.\n" );
                GetMessageLog()->SetMessage( ex._info().c_str() );
            }
        }
        else
        {
            try
            {
                PortableServer::ObjectId_var idObject = PortableServer::string_to_ObjectId( UINAME.c_str() );

                //Activate it to obtain the object reference
                CORBA::Object_var objectRef = poa->id_to_reference( idObject.in() );
                m_ui = Body::UI::_narrow( objectRef.in() );

                veCE->RegisterUI( p_ui_i->UIName_.c_str(), m_ui.in() );
            }
            catch ( CORBA::Exception& ex )
            {
                GetMessageLog()->SetMessage( "Can't find executive or UI registration error.\n" );
                GetMessageLog()->SetMessage( ex._info().c_str() );
            }
        }
    }
    catch ( CORBA::Exception& ex )
    {
        GetMessageLog()->SetMessage( "Can't find executive or UI registration error.\n" );
        GetMessageLog()->SetMessage( ex._info().c_str() );
    }
}
////////////////////////////////////////////////////////////////////////////////
bool CORBAServiceList::SendCommandStringToXplorer( const ves::open::xml::CommandWeakPtr& veCommand )
{
    //Calling function is responsible for the command memory
    if( !IsConnectedToXplorer() )
    {
        return false;
    }

    //Now send the data to xplorer
    ves::open::xml::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();

    // New need to destroy document and send it
    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( veCommand, "vecommand" ) );
    std::string xmlDocument( "returnString" );
    netowrkWriter.WriteXMLDocument( nodes, xmlDocument, "Command" );

    if( CORBA::is_nil( vjobs.in() ) || xmlDocument.empty() )
    {
        return false;
    }

    try
    {
        // CORBA releases the allocated memory so we do not have to
        vjobs->SetCommandString( xmlDocument.c_str() );
    }
    catch ( ... )
    {
        return false;
    }
    return true;
}
////////////////////////////////////////////////////////////////////////////////
VjObs_ptr CORBAServiceList::GetXplorerPointer( void )
{
    return vjobs.in();
}
////////////////////////////////////////////////////////////////////////////////
PEThread* CORBAServiceList::GetMessageLog( void )
{
    if( pelog == NULL )
    {
        pelog = new PEThread();
        //pelog->activate();
    }

    return pelog;
}
////////////////////////////////////////////////////////////////////////////////
bool CORBAServiceList::SetID( int moduleId, std::string moduleName )
{
    if( !CORBAServiceList::IsConnectedToCE() )
    {
        return false;
    }

    try
    {
        veCE->SetID( moduleName.c_str(), moduleId );
    }
    catch ( ... )
    {
        return false;
    }
    return true;
}
///////////////////////////////////////////////////////////////////////
const ves::open::xml::CommandPtr& CORBAServiceList::GetGUIUpdateCommands( const std::string& commandName )
{
    if( p_ui_i == 0 )
    {
        return nullTextPtr;
    }
    return p_ui_i->GetXplorerData( commandName );
}
////////////////////////////////////////////////////////////////////////////////
std::string CORBAServiceList::GetNetwork( void )
{
    if( !CORBAServiceList::IsConnectedToCE() )
    {
        return std::string();
    }

    try
    {
        std::string network = veCE->GetNetwork( p_ui_i->UIName_.c_str() );
        return network;
    }
    catch ( ... )
    {
        return std::string();
    }
}
////////////////////////////////////////////////////////////////////////////////
void CORBAServiceList::StopCalc( void )
{
    if( !CORBAServiceList::IsConnectedToCE() )
    {
        return;
    }

    try
    {
        veCE->StopCalc();
    }
    catch ( ... )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CORBAServiceList::StartCalc( void )
{
    if( !CORBAServiceList::IsConnectedToCE() )
    {
        return;
    }

    try
    {
        veCE->StartCalc();
    }
    catch ( ... )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CORBAServiceList::PauseCalc( void )
{
    if( !CORBAServiceList::IsConnectedToCE() )
    {
        return;
    }

    try
    {
        veCE->PauseCalc();
    }
    catch ( ... )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CORBAServiceList::Resume( void )
{
    if( !CORBAServiceList::IsConnectedToCE() )
    {
        return;
    }

    try
    {
        veCE->Resume();
    }
    catch ( ... )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
std::string CORBAServiceList::Query( std::string command )
{
    if( !CORBAServiceList::IsConnectedToCE() )
    {

        return std::string( "Not Connected" );
    }

    try
    {
        std::string network = veCE->Query( command.c_str() );
        return network;
    }
    catch ( ... )
    {
        return std::string( "Error" );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CORBAServiceList::SetNetwork( std::string command )
{
    if( !CORBAServiceList::IsConnectedToCE() )
    {
        return;
    }

    try
    {
        veCE->SetNetwork( command.c_str() );
    }
    catch ( ... )
    {
        return;
    }
}
