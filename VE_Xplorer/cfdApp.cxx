/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdApp.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// .NAME cfdApp - An virtual reality (VRJuggler) application class.
// .SECTION Description
// A class to execute an CFD application in the virtual environment.
// It is derived by using the VTK and VRJuggler classes.

#include "cfdApp.h"
//#include <Performer/pfdu.h>
//#include <Performer/pf/pfNode.h>
// Scene graph dependant headers
#include <Performer/pf.h>
#include <Performer/pf/pfGroup.h>
#include <sys/types.h>

#include "cfdEnum.h"
#include "fileIO.h"
#include "cfdPfSceneManagement.h"
#include "cfdEnvironmentHandler.h"
#include "cfdSteadyStateVizHandler.h"
#include "cfdTransientVizHandler.h"
#include "cfdModelHandler.h"
#include "cfdCommandArray.h"
#include "cfdGroup.h"
#include "cfdDCS.h"
#include "cfdObjects.h"
#include "cfdTempAnimation.h"
#include "cfdSequence.h"
#include "cfdIHCCModel.h"
#include "CorbaManager.h"
#include "VjObs_i.h"

#ifdef _TAO
#include "cfdExecutive.h"
#endif //_TAO

#include <vpr/Util/Debug.h>
#include <vpr/System.h>

#include <cstdlib>
using namespace vrj;
using namespace std;

inline cfdApp::cfdApp( CorbaManager* input )
{
   this->_sceneManager =         NULL;
   this->_environmentHandler =   NULL;
   this->_steadystateHandler =   NULL;
   //this->_transientHandler =     NULL;
   this->_modelHandler =         NULL;
   this->ihccModel =             NULL;
   _corbaManager = input;
}
/*
void cfdApp::SetCORBAVariables( CosNaming::NamingContext_ptr naming, CORBA::ORB_ptr orb, PortableServer::POA_ptr poa )
{
   this->naming_context = CosNaming::NamingContext::_duplicate( naming );
   this->orb = CORBA::ORB::_duplicate( orb );
   this->poa = PortableServer::POA::_duplicate( poa );
}
*/
void cfdApp::exit()
{
   delete filein_name;

   // we don't have a destructor, so delete items here...
   if ( this->_sceneManager )
   {  
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->_sceneManager" << std::endl << vprDEBUG_FLUSH;
      delete this->_sceneManager;
   }

   if ( this->_environmentHandler )
   {  
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->_environmentHandler" << std::endl << vprDEBUG_FLUSH;
      delete this->_environmentHandler;
   }

   if ( this->_steadystateHandler )
   {  
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->_steadystateHandler" << std::endl << vprDEBUG_FLUSH;
      delete this->_steadystateHandler;
   }

   /*if ( this->_transientHandler )
   {  
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->_transientHandler" << std::endl << vprDEBUG_FLUSH;
      delete this->_transientHandler;
   }*/

   if ( this->_modelHandler )
   {  
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->_modelHandler" << std::endl << vprDEBUG_FLUSH;
      delete this->_modelHandler;
   }

   if ( this->ihccModel )
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->ihccModel" << std::endl << vprDEBUG_FLUSH;
      delete this->ihccModel;
   }

#ifdef _TAO
   if ( this->executive ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->executive" << std::endl << vprDEBUG_FLUSH;
      delete this->executive;
   }
#endif // _TAO

   vprDEBUG(vprDBG_ALL,0) 
     << " pfExit" << std::endl << vprDEBUG_FLUSH;
   delete _corbaManager;
   pfExit();
}

inline void cfdApp::init( )
{
   //_corbaManager = new CorbaManager();
   vprDEBUG(vprDBG_ALL,0) << "cfdApp::init" << std::endl << vprDEBUG_FLUSH;
   filein_name = new char [ 256 ];
   do
   {
      std::cout << "|   Enter VE_Xplorer parameter filename: ";
      std::cin >> filein_name;
      if ( ! fileIO::isFileReadable( this->filein_name ) )
      {
         std::cerr << "\n\"" << this->filein_name << "\" is not readable." << std::endl;
      }
   }
   while ( ! fileIO::isFileReadable( this->filein_name ) );

   //std::cout << "filein_name: " << this->filein_name << std::endl;

   std::cout << std::endl;
   std::cout << "| ***************************************************************** |" << std::endl;
   std::cout << "|  3. Initializing........................... Parameter File Reader |" << std::endl;

#ifdef _CLUSTER
  // Cluster Stuff
   vpr::GUID new_guid("15c09c99-ed6d-4994-bbac-83587d4400d1");
   std::string hostname = "abbott.vrac.iastate.edu";
   mStates.init(new_guid,hostname);
   //cluster::ApplicationData* hack = dynamic_cast<cluster::ApplicationData*>(&(*this->mStates));
   //hack->setIsLocal(hostname == cluster::ClusterNetwork::instance()->getLocalHostname());
#endif // _CLUSTER
}

inline void cfdApp::apiInit( )
{
   vprDEBUG(vprDBG_ALL,1) << "cfdApp::apiInit" << std::endl << vprDEBUG_FLUSH;
}

inline void cfdApp::preForkInit( )
{
   vprDEBUG(vprDBG_ALL,1) << "cfdApp::preForkInit"
                          << std::endl << vprDEBUG_FLUSH;
   //pfdInitConverter( "air_system.flt" );
}

inline pfGroup* cfdApp::getScene( )
{
  //vprDEBUG(vprDBG_ALL,1) << "cfdApp::getScene" << std::endl << vprDEBUG_FLUSH;
  return (pfGroup*)(this->_sceneManager->GetRootNode()->GetRawNode());//for test
}
/*
inline void cfdApp::preDrawChan(pfChannel* chan, void* chandata)
{
  vprDEBUG(vprDBG_ALL,3) << "cfdApp::preDrawChan" 
                         << std::endl << vprDEBUG_FLUSH;

  pfDisable(PFEN_TEXTURE);
  pfOverride(PFSTATE_TEXTURE,PF_ON); // Override texturing to turn it off (test)
}
*/

inline void cfdApp::preSync( )
{
  vprDEBUG(vprDBG_ALL,1) << "cfdApp::preSync" << std::endl << vprDEBUG_FLUSH;
}

/*
std::vector< int > cfdApp::getFrameBufferAttrs( void )
{
   std::vector< int > attrs;
   attrs.push_back( PFFB_SAMPLE_BUFFER );
   attrs.push_back( 1 );
   attrs.push_back( PFFB_SAMPLES );
   attrs.push_back( 1 );
   return ( attrs );
}
*/

inline void cfdApp::initScene( )
{
   vprDEBUG(vprDBG_ALL,0) << "cfdApp::initScene" << std::endl << vprDEBUG_FLUSH;

# ifdef _OPENMP
   std::cout << "\n\n\n";
   std::cout << "|===================================================================|" << std::endl;
   std::cout << "|          Compiled by an OpenMP-compliant implementation           |" << std::endl;
   std::cout << "|===================================================================|" << std::endl;
   std::cout << "|                                                                   |" << std::endl;
# endif // _OPENMP

   // define the rootNode, worldDCS, and lighting
   this->_sceneManager = new cfdPfSceneManagement( this->filein_name );
   this->_sceneManager->InitScene();

   // modelHandler stores the arrow and holds all data and geometry
   this->_modelHandler = new cfdModelHandler( this->filein_name, 
                                              this->_sceneManager->GetWorldDCS() );
   this->_modelHandler->SetCommandArray( _corbaManager->GetVjObs()->_cfdArray );
   this->_modelHandler->InitScene();

   // navigation and cursor 
   this->_environmentHandler = new cfdEnvironmentHandler( this->filein_name );
   this->_environmentHandler->SetWorldDCS( this->_sceneManager->GetWorldDCS() );
   this->_environmentHandler->SetRootNode( this->_sceneManager->GetRootNode() );
   this->_environmentHandler->SetArrow( this->_modelHandler->GetArrow() );
   this->_environmentHandler->SetCommandArray( _corbaManager->GetVjObs()->_cfdArray );
   this->_environmentHandler->InitScene();

   // create steady state visualization objects
   this->_steadystateHandler = new cfdSteadyStateVizHandler( this->filein_name );
   this->_steadystateHandler->SetWorldDCS( this->_sceneManager->GetWorldDCS() );
   this->_steadystateHandler->SetNavigate( this->_environmentHandler->GetNavigate() );
   this->_steadystateHandler->SetCursor( this->_environmentHandler->GetCursor() );
   this->_steadystateHandler->SetCommandArray( _corbaManager->GetVjObs()->_cfdArray );
   this->_steadystateHandler->SetActiveDataSet( this->_modelHandler->GetActiveDataSet() );
   this->_steadystateHandler->InitScene();

/*
   // TODO fix transient
   this->_transientHandler = new cfdTransientVizHandler( this->filein_name );
   this->_transientHandler->InitScene();
std::cout << "|  3d" << std::endl;
*/

   this->_corbaManager->GetVjObs()->SetHandlers( _steadystateHandler, _environmentHandler, _modelHandler );

#ifdef _TAO
   std::cout << "|  2. Initializing.................................... cfdExecutive |" << std::endl;
   this->executive = new cfdExecutive( _corbaManager->naming_context.in(), this->_sceneManager->GetWorldDCS() );
   this->executive->SetModelHandler( this->_modelHandler, this->_environmentHandler );

#endif // _TAO

    this->ihccModel = NULL;
/*
   //
   // Make IHCC Model - should be deleted at a later date
   //
   // Fix this with new read param method
   //if ( this->paramReader->ihccModel )
   {
      std::cout << "| 54. Initializing...................................... IHCC Model |" << std::endl;
      this->ihccModel = new cfdIHCCModel( NULL, this->_sceneManager->GetWorldDCS() );
   }
*/
   // This may need to be fixed
   this->_corbaManager->GetVjObs()->GetCfdStateVariables();
}

void cfdApp::preFrame( void )
{
   vprDEBUG(vprDBG_ALL,3) << "cfdApp::preFrame" << std::endl << vprDEBUG_FLUSH;

#ifdef _CLUSTER
   this->_corbaManager->GetVjObs()->GetUpdateClusterStateVariables();
#endif // _CLUSTER

   ///////////////////////
   vprDEBUG(vprDBG_ALL,3) << "cfdApp::this->_modelHandler->PreFrameUpdate()" << std::endl << vprDEBUG_FLUSH;
   this->_modelHandler->PreFrameUpdate();
   ///////////////////////
   vprDEBUG(vprDBG_ALL,3) << "cfdApp::this->_environmentHandler->PreFrameUpdate()" << std::endl << vprDEBUG_FLUSH;
   this->_environmentHandler->SetActiveDataSet( this->_modelHandler->GetActiveDataSet() );
   this->_environmentHandler->PreFrameUpdate();
   ///////////////////////
   vprDEBUG(vprDBG_ALL,3) << "cfdApp::this->_steadystateHandler->PreFrameUpdate()" << std::endl << vprDEBUG_FLUSH;
   this->_steadystateHandler->SetActiveDataSet( this->_modelHandler->GetActiveDataSet() );
   this->_steadystateHandler->PreFrameUpdate();
   ///////////////////////
   //this->_transientHandler->SetCommandArray( _cfdArray );
   //this->_transientHandler->PreFrameUpdate();
   ///////////////////////

   //pfdStoreFile( this->_sceneManager->GetRootNode()->GetRawNode(), "test1.pfb" );
   //this->exit();
   // This need to go very soon
   // IHCC hack
   // fix this soon
   if ( this->_corbaManager->GetVjObs()->_cfdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == UPDATE_SEND_PARAM )
   {
      double data[ 6 ];// = { 0 };
      data[ 0 ] = _corbaManager->GetVjObs()->cfdShort_data_array[ 1 ]; //200;  //Agitation (rpm)  initial value 200
      data[ 1 ] = _corbaManager->GetVjObs()->cfdShort_data_array[ 2 ]; //1.25; //Air Concentration initial value 1.25;
      data[ 2 ] = _corbaManager->GetVjObs()->cfdShort_data_array[ 3 ]; //6;    //Initial pH value    initial value 6
      data[ 3 ] = _corbaManager->GetVjObs()->cfdShort_data_array[ 4 ]; //0.1;  //Nitrate Concentration     initial value 0.1
      data[ 4 ] = _corbaManager->GetVjObs()->cfdShort_data_array[ 5 ]; //37;   //Temperate (Celsius)        initial value 37
      data[ 5 ] = _corbaManager->GetVjObs()->cfdShort_data_array[ 6 ]; //240;  //Simulate [a text box] Hours in 10 seconds, initial value 240

      //this->ihccModel->UpdateModelVariables( data );
      //this->ihccModel->Update();
   }
   else if ( this->_corbaManager->GetVjObs()->_cfdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == EXIT )   // exit cfdApp was selected
   {
#ifdef _TAO
      this->executive->UnbindORB();
#endif // _TAO
      this->mKernel->stop(); // Stopping kernel using the inherited member variable
   }

#ifdef _TAO
   if ( cfdObjects::GetActiveDataSet() != NULL )
   {
      this->executive->SetActiveDataSet( cfdObjects::GetActiveDataSet() );
   }
   this->executive->UpdateModules();
   this->executive->CheckCommandId( _corbaManager->GetVjObs()->_cfdArray );
#endif // 

   this->_corbaManager->GetVjObs()->PreFrameUpdate();
   vprDEBUG(vprDBG_ALL,3) << " cfdApp::End preFrame" << std::endl << vprDEBUG_FLUSH;
}

void cfdApp::intraFrame()
{
   vprDEBUG(vprDBG_ALL,3) << " intraFrame" << std::endl << vprDEBUG_FLUSH;
   // Do nothing here
   // Usually slows things down
}

void cfdApp::postFrame()
{
   vprDEBUG(vprDBG_ALL,3) << " postFrame" << std::endl << vprDEBUG_FLUSH;

   // if transient data is being displayed, then update gui progress bar
  /* if (  this->_modelHandler->GetActiveSequence() )
   {
      int currentFrame = this->_modelHandler->GetActiveSequence()->GetSequence()->GetFrameOfSequence();
      //cout << " Current Frame :  " << currentFrame << endl;
      if (this->lastFrame != currentFrame )
      {
         this->setTimesteps( currentFrame );
         this->lastFrame = currentFrame;
      }
   }*/

   this->_corbaManager->GetVjObs()->GetCfdStateVariables();
   vprDEBUG(vprDBG_ALL,3) << " End postFrame" << std::endl << vprDEBUG_FLUSH;
}


int main(int argc, char* argv[])
{
   //pfSharedArenaSize (1400 * 1024 * 1024);
   //pfSharedArenaBase ((void *) 0x20000000);
   //pfInitArenas(); 

   // cfdApp will call the destructor
   CorbaManager* manager = new CorbaManager();
    vpr::System::msleep( 10000 );  // half-second delay

   vrj::Kernel* kernel = vrj::Kernel::instance(); // Declare a new Kernel

   cfdApp* application = new cfdApp( manager );//kernel );  // Delcare an instance of my application


   for ( int i = 1; i < argc; i++ )          // Configure the kernel
   {
      kernel->loadConfigFile( argv[i] );   
   }
   
   kernel->start();                          // Start the kernel thread

   kernel->setApplication( application );    // Give application to kernel
   
   kernel->waitForKernelStop();              // Block until kernel stops

   return 0;
}


#ifdef _CLUSTER
void cfdApp::GetUpdateClusterStateVariables( void )
{
   //call the parent method
   VjObs_i::GetUpdateClusterStateVariables();

   //sync up the frames on all nodes in the
   //cluster
   if ( !mStates.isLocal() )
   {
      if ( this->_modelHandler->GetActiveSequence() != NULL )
      {
         cfdSequence* the_sequence = this->_modelHandler->GetActiveSequence()->GetSequence()->GetSequence();
         if ( the_sequence != NULL )
         {
            the_sequence->setCurrentFrame( this->getTimesteps() );
            //cout << " cfdTimesteps in preframe : " << cfdTimesteps << endl;
         }
      }
   }
}

#endif // _CLUSTER

