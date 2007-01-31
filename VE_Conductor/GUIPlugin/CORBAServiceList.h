/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CORBA_SERVICE_LIST
#define CORBA_SERVICE_LIST
/*!\file CORBAServiceList.h
CORBAServiceList API
*/
/*!\class VE_Conductor::CORBAServiceList
* 
*/
#include <orbsvcs/CosNamingC.h>
#include "VE_Open/skel/moduleC.h"
#include "VE_Open/skel/VjObsC.h"
#include "VE_Conductor/GUIPlugin/UI_i.h"
//do this to remove compile warning from linux platforms
#undef _REENTRANT
#include <vpr/Util/Singleton.h>

#include <vector>
#include <string>

class PEThread;
class Body_UI_i;
class wxTextCtrl;
class wxUpdateUIEvent;

namespace VE_XML
{
   class Command;
}

#include "VE_Installer/include/VEConfig.h"
namespace VE_Conductor
{
class VE_GUIPLUGINS_EXPORTS CORBAServiceList //: public wxObject
{
private:
   // Required so that vpr::Singleton can instantiate this class.
   //friend class vpr::Singleton< cfdPfSceneManagement >;
   //cfdPfSceneManagement(const cfdPfSceneManagement& o) { ; }
   //cfdPfSceneManagement& operator=(const cfdPfSceneManagement& o) { ; }
   CORBAServiceList( void ){ ; }
   ~CORBAServiceList(){ ; } // Never gets called, don't implement
   vprSingletonHeader( CORBAServiceList );
public:
   ///Constructor
   void SetArgcArgv( int argc, char** argv );
   ///Destructor
   //~CORBAServiceList( void );
   void CleanUp( void );
   ///Set a naming context
   ///\param naming_context
   void SetNamingContext( CosNaming::NamingContext_ptr naming_context );
   ///Return the list of services that are connected to the name server
   std::vector< std::string > GetListOfServices( void );
   
   ///Function to tell whether we are connected to xplorer
   bool IsConnectedToXplorer( void );
   ///Function to tell whether we are connected to ce
   bool IsConnectedToCE( void );
   ///Connect to xplorer
   bool ConnectToXplorer( void );
   ///connect to ce
   bool ConnectToCE( void );
   ///Disconnect to xplorer
   bool DisconnectFromXplorer( void );
   ///Disconnect to ce
   bool DisconnectFromCE( void );
   ///Set xplorer command string 
   ///\param command string containing command
   bool SendCommandStringToXplorer(  VE_XML::Command* veCommand  );
   ///Set ce network string 
   ///\param network string containing network
   bool SendNetworkStringToCE( std::string network );
   ///Keep the orb running and check for corba commands to be processed
   void CheckORBWorkLoad( void );
   ///Connect to the CORBA naming service
   bool ConnectToNamingService( void );
   ///Connect to the CORBA naming service
   bool IsConnectedToNamingService( void );
   ///Return the pointer to the xplorer corba object
   VjObs_ptr GetXplorerPointer( void );
   ///Return log pointer
   PEThread* GetMessageLog( void );
   ///Call set id on all modules in the current network
   bool SetID( int moduleId, std::string moduleName );
   ///Event to update the text feedback window
   void OnUpdateUIPop( wxUpdateUIEvent& event );

   ///Excutive wrapper functions
   std::string GetNetwork( void );
   void SetNetwork( std::string );
   void StopCalc( void );
   void StartCalc( void );
   void PauseCalc( void );
   void Resume( void );
   std::string Query( std::string command );
   
private:
   void CreateCORBAModule( void );
      
   std::vector< std::string > serviceList;
   // CORBA var
   CosNaming::BindingList_var bindList;
   CosNaming::Name_var nameList;
   CosNaming::NamingContext_var namingContext;
   CORBA::ORB_var orb;
   PortableServer::POA_var poa;
   PortableServer::POA_var poa_root;
   CosNaming::NamingContext_var naming_context;
   VjObs_var vjobs;
   //Body::Executive_var module;
   Body::VEEngine_var veXplorer;
   Body::Executive_var veCE;
   Body_UI_i* p_ui_i;
   PEThread* pelog;
   int peArgc;
   char** peArgv;
};
}
#endif
