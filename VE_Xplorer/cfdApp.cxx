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

#ifdef _TAO
#include "cfdExecutive.h"
#endif //_TAO

#include <vpr/Util/Debug.h>

#include <cstdlib>

using namespace vrj;
using namespace std;

#ifdef _CLUSTER
int getStringTokens(char* buffer, char* delim, std::vector<std::string> &toks); // YANG, a string parsing utility, it is a not thread safe call.
#endif // _CLUSTER

inline cfdApp::cfdApp( )
{
   this->_sceneManager =         NULL;
   this->_environmentHandler =   NULL;
   this->_steadystateHandler =   NULL;
   //this->_transientHandler =     NULL;
   this->_modelHandler =         NULL;
}

void cfdApp::SetCORBAVariables( CosNaming::NamingContext_ptr naming, CORBA::ORB_ptr orb, PortableServer::POA_ptr poa )
{
   this->naming_context = CosNaming::NamingContext::_duplicate( naming );
   this->orb = CORBA::ORB::_duplicate( orb );
   this->poa = PortableServer::POA::_duplicate( poa );
}

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

   CosNaming::Name name(1);

   name.length(1);
   name[0].id   = (const char*) "Master";
   name[0].kind = (const char*) "VE_Xplorer";
   
   try
   {
      vprDEBUG(vprDBG_ALL,0) 
         << "naming_context->unbind for CORBA Object  " 
         << endl << vprDEBUG_FLUSH;
      naming_context->unbind( name );
      //naming_context->destroy();
   }
   catch( CosNaming::NamingContext::InvalidName& )
   {
      cerr << "Invalid name for CORBA Object  " << endl;
   }
   catch(CosNaming::NamingContext::NotFound& ex)
   {
      cerr << "Name not found for CORBA Object  " << ex.why << endl;
   }
   
   poa->destroy (1, 1);
   
   vprDEBUG(vprDBG_ALL,0) 
     << " destroying orb" << std::endl << vprDEBUG_FLUSH;

   orb->destroy();

   vprDEBUG(vprDBG_ALL,0) 
     << " pfExit" << std::endl << vprDEBUG_FLUSH;
   pfExit();
}

inline void cfdApp::init( )
{
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
   vpr::GUID new_guid("d6be4359-e8cf-41fc-a72b-a5b4f3f29aa2");
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
   this->_modelHandler->SetCommandArray( _cfdArray );
   this->_modelHandler->InitScene();

   // navigation and cursor 
   this->_environmentHandler = new cfdEnvironmentHandler( this->filein_name );
   this->_environmentHandler->SetWorldDCS( this->_sceneManager->GetWorldDCS() );
   this->_environmentHandler->SetRootNode( this->_sceneManager->GetRootNode() );
   this->_environmentHandler->SetArrow( this->_modelHandler->GetArrow() );
   this->_environmentHandler->SetCommandArray( _cfdArray );
   this->_environmentHandler->InitScene();

   // create steady state visualization objects
   this->_steadystateHandler = new cfdSteadyStateVizHandler( this->filein_name );
   this->_steadystateHandler->SetWorldDCS( this->_sceneManager->GetWorldDCS() );
   this->_steadystateHandler->SetNavigate( this->_environmentHandler->GetNavigate() );
   this->_steadystateHandler->SetCursor( this->_environmentHandler->GetCursor() );
   this->_steadystateHandler->SetCommandArray( _cfdArray );
   this->_steadystateHandler->SetActiveDataSet( this->_modelHandler->GetActiveDataSet() );
   this->_steadystateHandler->InitScene();

/*
   // TODO fix transient
   this->_transientHandler = new cfdTransientVizHandler( this->filein_name );
   this->_transientHandler->InitScene();
std::cout << "|  3d" << std::endl;
*/

   this->SetHandlers( _steadystateHandler, _environmentHandler, _modelHandler );

#ifdef _TAO
   std::cout << "|  2. Initializing.................................... cfdExecutive |" << std::endl;
   this->executive = new cfdExecutive( naming_context.in(), this->_sceneManager->GetWorldDCS() );
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

   // Create data in memory for transfered data
   this->CreateSoundInfo();
   this->CreateGeometryInfo();
   this->CreateDatasetInfo();
   this->CreateTeacherInfo();

   // This may need to be fixed
   this->GetCfdStateVariables();
}

void cfdApp::preFrame( void )
{
   vprDEBUG(vprDBG_ALL,3) << "cfdApp::preFrame" << std::endl << vprDEBUG_FLUSH;

#ifdef _CLUSTER
   this->GetUpdateClusterStateVariables();
#endif // _CLUSTER

   ///////////////////////
   this->_environmentHandler->PreFrameUpdate();
   ///////////////////////
   this->_modelHandler->PreFrameUpdate();
   ///////////////////////
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
   if ( _cfdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == UPDATE_SEND_PARAM )
   {
      double data[ 6 ];// = { 0 };
      data[ 0 ] = cfdShort_data_array[ 1 ]; //200;  //Agitation (rpm)  initial value 200
      data[ 1 ] = cfdShort_data_array[ 2 ]; //1.25; //Air Concentration initial value 1.25;
      data[ 2 ] = cfdShort_data_array[ 3 ]; //6;    //Initial pH value    initial value 6
      data[ 3 ] = cfdShort_data_array[ 4 ]; //0.1;  //Nitrate Concentration     initial value 0.1
      data[ 4 ] = cfdShort_data_array[ 5 ]; //37;   //Temperate (Celsius)        initial value 37
      data[ 5 ] = cfdShort_data_array[ 6 ]; //240;  //Simulate [a text box] Hours in 10 seconds, initial value 240

      //this->ihccModel->UpdateModelVariables( data );
      //this->ihccModel->Update();
   }
   else if ( _cfdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == EXIT )   // exit cfdApp was selected
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
#endif // _TAO

   this->PreFrameUpdate();
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

   this->GetCfdStateVariables();
   vprDEBUG(vprDBG_ALL,3) << " End postFrame" << std::endl << vprDEBUG_FLUSH;
}


int main(int argc, char* argv[])
{
   //pfSharedArenaSize (1400 * 1024 * 1024);
   //pfSharedArenaBase ((void *) 0x20000000);
   //pfInitArenas(); 

#ifdef _CLUSTER
   char buffer[1025];
   int ntoks, i;
   std::vector<std::string> toks;
   std::string hostfile;
   FILE * fhost;
   bool found=false;
   std::string masterhost="abbott";

   if (argc>1)
   {
      strcpy(buffer, argv[1]);
      ntoks=getStringTokens(buffer, "/", toks);
      //Now construct the name for the host file;
      hostfile="/";
      for (i=0; i<ntoks-1; i++)
      hostfile=hostfile+toks[i]+"/";
      hostfile+="component/host.config";
      cout<<"Here is the string for the hostfile :"<<hostfile<<endl;
      //Now we open that file and get the host name
      fhost=fopen(hostfile.c_str(), "r");
      if (fhost==NULL)
      {
         cout<<"Something bad in the path"<<endl;
         return -1;
      }

      while(!feof(fhost)&&!found)
      {
         fgets(buffer, 1024, fhost);
         ntoks=getStringTokens(buffer, "<>/ ", toks);
         for (i=0; i<ntoks; i++)
            if (toks[i]=="hostname" && i!=ntoks-1)
            {
               masterhost=toks[i+1];
               found=true;
               break;
            }
      }
      fclose(fhost);
   }
#endif // _CLUSTER

   int temp = 0;
   char** xargv;
   xargv = new char*[ temp ];
   //xargv[ 0 ] = "-ORBInitRef";
   //xargv[ 1 ] = "NameService=corbaname::cruncher.vrac.iastate.edu:2809";

#ifdef _TAO
   //xargv[ 0 ] = "-ORBInitRef";
   //xargv[ 1 ] = "NameService=file:///tmp/ns.ior";
   CORBA::ORB_var orb=CORBA::ORB_init( temp, xargv,"" );
#else
   CORBA::ORB_var orb=CORBA::ORB_init( temp, xargv );
   if ( CORBA::is_nil( orb.in() ) )
      exit(0);
#endif // _TAO
   //Here is the part to contact the naming service and get the reference for the executive
   CORBA::Object_var naming_context_object =
     orb->resolve_initial_references ("NameService"); 
   CORBA::String_var sior1(orb->object_to_string(naming_context_object.in ()));
   cout << "|  IOR of the server side : " << endl << sior1 << endl;

   CosNaming::NamingContext_var naming_context =
       CosNaming::NamingContext::_narrow (naming_context_object.in ());
   
   
    //Here is the code to set up the server
    CORBA::Object_var poa_object =
      orb->resolve_initial_references ("RootPOA"); // get the root poa

    PortableServer::POA_var poa = PortableServer::POA::_narrow(poa_object.in());
    PortableServer::POAManager_var poa_manager = poa->the_POAManager ();
    poa_manager->activate ();
//   CORBA::String_var sior2(orb->object_to_string( poa.in() ) );
//   cout << "|  IOR of the server side 2 : " << endl << sior2 << endl;

   vrj::Kernel* kernel = vrj::Kernel::instance(); // Declare a new Kernel

   cfdApp* application = new cfdApp( );//kernel );  // Delcare an instance of my application

#ifdef _CLUSTER
   char raw_hostname[256];
   std::string hostname;
   
   gethostname(raw_hostname, 255); //get the host name 
   hostname=raw_hostname;
   cout<<"Host name is "<<hostname<<endl;   
   getStringTokens(raw_hostname,".", toks);
   //now the toks[0] will be the short host name, which is the one without the domain name

   
   if (hostname==masterhost||toks[0]==masterhost)
   {
      cout<<"This is the master!"<<endl;

      VjObs_var vjobs = application->_this();
      CORBA::String_var sior(orb->object_to_string(vjobs.in()));
      cout << "|  IOR of the server(cfdApp) side : " << endl << sior << endl;
      CosNaming::Name name;
      name.length(1);

      name[0].id   = (const char*) "Master";
      name[0].kind = (const char*) "VE_Xplorer";
      //Bind the object
      try
      {
         naming_context->bind(name, vjobs.in());
      }
      catch(CosNaming::NamingContext::AlreadyBound& ex)
      {
         naming_context->rebind(name, vjobs.in());
      }
   }
#else // _CLUSTER
   VjObs_var vjobs = application->_this();
   if ( CORBA::is_nil( vjobs.in() ) )
     cout << "is nil " << endl;
   CORBA::String_var sior(orb->object_to_string( vjobs.in() ) );
   cout << "|  IOR of the server(cfdApp) side : " << endl << sior << endl;
   CosNaming::Name name;
   name.length(1);
   
   name[0].id   = (const char*) "Master";
   name[0].kind = (const char*) "VE_Xplorer";
   //Bind the object
   try
   {
      naming_context->bind(name, vjobs.in());
   }
   catch(CosNaming::NamingContext::AlreadyBound&)
   {
      naming_context->rebind(name, vjobs.in());
   }
#endif // _CLUSTER
   application->SetCORBAVariables( naming_context.in(), orb.in(), poa.in() );

   for ( int i = 1; i < argc; i++ )          // Configure the kernel
   {
      kernel->loadConfigFile( argv[i] );   
   }
   
   kernel->start();                          // Start the kernel thread

   kernel->setApplication( application );    // Give application to kernel
   
#ifdef _TAO
   // If this isn't here the app won't work with TAO 
   orb->run();
#endif // _TAO
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

int getStringTokens(char* buffer, char* delim, std::vector<std::string> &toks)
{
   char* token;
   int i=0;
   token = strtok(buffer, delim);

   toks.clear();
   while( token )
   {
      i++;
      toks.push_back(std::string(token));
      token = strtok(NULL, delim);
   }

   return i;
}
#endif // _CLUSTER

