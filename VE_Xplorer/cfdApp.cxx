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
 * Version:       $Revision: 1.153 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// .NAME cfdApp - An virtual reality (VRJuggler) application class.
// .SECTION Description
// A class to execute an CFD application in the virtual environment.
// It is derived by using the VTK and VRJuggler classes.

#include "cfdApp.h"
#include <sys/types.h>

//#include <malloc.h>
#include "fileIO.h"
#include "get_time.h"
#include "cfdPlanes.h"
#include "cfdPolyData.h"
#include "cfdTeacher.h"
#include "cfdFrame.h"
#include "cfdSound.h"
#include "cfdFileInfo.h"
#include "cfdTransientInfo.h"
#include "cfdTransientSet.h"
//#include "cfdDrawManager.h"
//#include "cfdDashboard.h"
#ifdef _TAO
#include "cfdExecutive.h"
#endif
#include <vpr/vpr.h>
#include <vpr/System.h>
#include <vpr/Thread/Thread.h>
#include <vpr/Util/Debug.h>
#include <snx/sonix.h>
#include <snx/SoundHandle.h>

#include <vtkPolyDataReader.h>
#include <vtkSystemIncludes.h>  // for VTK_POLY_DATA
#include <vtkPolyDataSource.h>

// yang-REI
// Now the only overlapped code between the corba servant thread and the cfdAPP 
// thread are the corba_mutex's internal data buffer, so both "PULL" and "PUSH"
// are safe.  snapShot(true/false) is used for PULL and PUSH on the cfdApp
// side, true for PULL and false for PUSH.  Data NEEDS TO BE PUSHED EVERY
// TIME IT CHANGES!!!
// yang-REI

#ifdef _CLUSTER
int getStringTokens(char* buffer, char* delim, std::vector<std::string> &toks); // YANG, a string parsing utility, it is a not thread safe call.
#endif

inline cfdApp::cfdApp( )//vrj::Kernel* kern ) 
//   : PfApp(kern)//, mMyDrawManager( new cfdDrawManager() )
{
   this->surface = NULL;
   this->menu = NULL;
   this->laser = NULL;
   this->cursor = NULL;
   this->scalarBarActor = NULL;
   this->isosurface = NULL;
   this->contour = NULL;
   this->momentum = NULL;
   this->vector = NULL;
   this->x_contour = NULL;
   this->y_contour = NULL;
   this->z_contour = NULL;
   this->x_momentum = NULL;
   this->y_momentum = NULL;
   this->z_momentum = NULL;
   this->x_vector = NULL;
   this->y_vector = NULL;
   this->z_vector = NULL;
   this->x_contours = NULL;
   this->y_contours = NULL;
   this->z_contours = NULL;
   this->x_momentums = NULL;
   this->y_momentums = NULL;
   this->z_momentums = NULL;
   this->x_vectors = NULL;
   this->y_vectors = NULL;
   this->z_vectors = NULL;
   this->streamlines = NULL;
   this->particles = NULL;
   this->image = NULL;
   this->animStreamer = NULL;
   this->animImg = NULL;
   this->transientSequence = NULL;
   this->_cfdTFM_X_Contour[0] = NULL;
   this->_cfdTFM_X_Contour[1] = NULL;
   this->_cfdTFM_Y_Contour = NULL;
   this->_cfdTFM_Z_Contour = NULL;
   this->_cfdTFM_X_Vector = NULL;
   this->_cfdTFM_Y_Vector = NULL;
   this->_cfdTFM_Z_Vector = NULL;
   this->_cfdTFM_Particle = NULL;
   this->_cfdTFM_Geometry[0] = NULL;
   this->_cfdTFM_Geometry[1] = NULL;
   this->lastSource = NULL;
   this->ihccModel = NULL;
   //biv -- the writeTraverser
   _cfdWT = 0;
}
#ifdef TABLET
void cfdApp::SetCORBAVariables( CosNaming::NamingContext_ptr naming, CORBA::ORB_ptr orb, PortableServer::POA_ptr poa )
{
   this->naming_context = CosNaming::NamingContext::_duplicate( naming );
   this->orb = CORBA::ORB::_duplicate( orb );
   this->poa = PortableServer::POA::_duplicate( poa );
}
#endif
void cfdApp::exit()
{
   // we don't have a destructor, so delete items here...
   this->runStreamersThread = false;
   this->runIntraParallelThread = false;   

   if ( this->nav ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->nav" << std::endl << vprDEBUG_FLUSH;
      delete this->nav;
   }

   if ( this->surface ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->surface" << std::endl << vprDEBUG_FLUSH;
      delete this->surface;
   }
   
   if ( this->menu ) 
   {
      vprDEBUG(vprDBG_ALL,2)
        << "deleting this->menu" << std::endl << vprDEBUG_FLUSH;
      delete this->menu;
   }

   if ( this->laser ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->laser" << std::endl << vprDEBUG_FLUSH;
      delete this->laser;
   }

   if ( this->cursor ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->cursor" << std::endl << vprDEBUG_FLUSH;
      delete this->cursor;
   }

   if ( this->scalarBarActor ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->scalarBarActor"   << std::endl << vprDEBUG_FLUSH;
      delete this->scalarBarActor;
   }

   if ( this->isosurface ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->isosurface" << std::endl << vprDEBUG_FLUSH;
      delete this->isosurface;
   }

   if ( this->contour ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->contour" << std::endl << vprDEBUG_FLUSH;
      delete this->contour;
   }

   if ( this->momentum ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->momentum;
   }

   if ( this->vector ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->vector" << std::endl << vprDEBUG_FLUSH;
      delete this->vector;
   }

   if ( this->x_contour ) 
   {
      vprDEBUG(vprDBG_ALL,2)   
        << "deleting this->x_contour" << std::endl << vprDEBUG_FLUSH;
      delete this->x_contour;
   }

   if ( this->y_contour ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->y_contour" << std::endl << vprDEBUG_FLUSH;
      delete this->y_contour;
   }

   if ( this->z_contour ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->z_contour" << std::endl << vprDEBUG_FLUSH;
      delete this->z_contour;
   }

   if ( this->x_momentum ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->x_momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->x_momentum;
   }

   if ( this->y_momentum ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->y_momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->y_momentum;
   }

   if ( this->z_momentum ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->z_momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->z_momentum;
   }

   if ( this->x_vector ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->x_vector" << std::endl << vprDEBUG_FLUSH;
      delete this->x_vector;
   }

   if ( this->y_vector ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->y_vector" << std::endl << vprDEBUG_FLUSH;
      delete this->y_vector;
   }

   if ( this->z_vector ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->z_vector" << std::endl << vprDEBUG_FLUSH;
      delete this->z_vector;
   }

   if ( this->x_contours ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->x_contours" << std::endl << vprDEBUG_FLUSH;
      delete this->x_contours;
   }

   if ( this->y_contours ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->y_contours" << std::endl << vprDEBUG_FLUSH;
      delete this->y_contours;
   }

   if ( this->z_contours ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->z_contours" << std::endl << vprDEBUG_FLUSH;
      delete this->z_contours;
   }

   if ( this->x_momentums ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->x_momentums" << std::endl << vprDEBUG_FLUSH;
      delete this->x_momentums;
   }

   if ( this->y_momentums ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->y_momentums" << std::endl << vprDEBUG_FLUSH;
      delete this->y_momentums;
   }

   if ( this->z_momentums ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->z_momentums" << std::endl << vprDEBUG_FLUSH;
      delete this->z_momentums;
   }

   if ( this->x_vectors ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->x_vectors" << std::endl << vprDEBUG_FLUSH;
      delete this->x_vectors;
   }

   if ( this->y_vectors ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
         << "deleting this->y_vectors" << std::endl << vprDEBUG_FLUSH;
      delete this->y_vectors;
   }

   if ( this->z_vectors ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
         << "deleting this->z_vectors" << std::endl << vprDEBUG_FLUSH; 
      delete this->z_vectors;
   }
   
   if ( this->streamlines ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->streamlines" << std::endl << vprDEBUG_FLUSH;
      delete this->streamlines;
   }

   if ( this->particles ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->particles" << std::endl << vprDEBUG_FLUSH;
      delete this->particles;
   }

   if ( this->image ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->image" << std::endl << vprDEBUG_FLUSH;
      delete this->image;
   }

   if ( this->animStreamer ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->animStreamer" << std::endl << vprDEBUG_FLUSH;
      delete this->animStreamer;
   }
   
   if ( this->transientSequence != NULL )
   {
      vprDEBUG(vprDBG_ALL,2) 
         << "deleting this->transientSequence" << std::endl << vprDEBUG_FLUSH;
      delete this->transientSequence;
   }

   if ( this->sounds.size() != 0 )
   {
      vprDEBUG(vprDBG_ALL,2) 
         << "deleting this->sounds" << std::endl << vprDEBUG_FLUSH;
      for(int i = 0; i < this->paramReader->soundFile; i++)
      {
         delete this->sounds[ i ];
      }
      this->sounds.clear();
   }
   this->arrow->Delete();
   
   if ( this->paramReader != NULL ) 
   {
      vprDEBUG(vprDBG_ALL,2)
         << "deleting this->paramReader" << std::endl << vprDEBUG_FLUSH;
      delete this->paramReader;
   }
   
   if ( this->lastSource )
   {
      vprDEBUG(vprDBG_ALL,2) 
         << "deleting this->lastSource" << std::endl << vprDEBUG_FLUSH;
      this->lastSource->Delete();
      this->lastSource = NULL;
   }

   if ( this->animImg ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->animImg" << std::endl << vprDEBUG_FLUSH;
      delete this->animImg;
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
#endif
#ifdef TABLET
   CosNaming::Name name(1);

   name.length(1);
   name[0].id   = (const char*) "Master";
   name[0].kind = (const char*) "VE_Xplorer";
   
   try	
   {
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
#endif
   vprDEBUG(vprDBG_ALL,0) 
     << " pfExit" << std::endl << vprDEBUG_FLUSH;
   pfExit();
}


inline void cfdApp::init( )
{
   vprDEBUG(vprDBG_ALL,1) << "cfdApp::init" << std::endl << vprDEBUG_FLUSH;
   std::cout << "|  4. Initializing.............................. Navigation systems |" << std::endl;
   this->nav = new cfdNavigate();

#ifdef _CLUSTER
  // Cluster Stuff
   vpr::GUID new_guid("d6be4359-e8cf-41fc-a72b-a5b4f3f29aa2");
   std::string hostname = "abbott.vrac.iastate.edu";
   mStates.init(new_guid,hostname);
  
   //cluster::ApplicationData* hack = dynamic_cast<cluster::ApplicationData*>(&(*this->mStates));
   //hack->setIsLocal(hostname == cluster::ClusterNetwork::instance()->getLocalHostname());
#endif
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
  return rootNode;//for test
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

/*
//App_adapt can be a prototype of the API object in the future
inline void cfdApp::App_adapt(int id)
{
  switch (id)
  {
    case 5:
    V22Reader = vtkPLOT3DReader::New();
    V22Reader->SetXYZFileName( "../v22/v22_bin/FAST.bin" );
    V22Reader->SetQFileName( "../v22/v22_bin/FAST.q" );
    V22Reader->SetScalarFunctionNumber( 153 );
    V22Reader->SetVectorFunctionNumber( 200 );
    V22Reader->Update();

    V22Data = ( vtkUnstructuredGrid * ) V22Reader->GetOutput();
    cfdObjects::GetActiveMeshedVolume()->setData(V22Data);
    cfdObjects::GetActiveMeshedVolume()->SetUserRange( 0.0f, 125.0f );

    view = new Plot3Dviewer();
    v22_DCS = new pfDCS;
    vtkPLOT3DReader *rotor1 = vtkPLOT3DReader::New();
        rotor1->SetXYZFileName( "../v22/v22_bin/FAST_R1.bin" );
   rotor1->Update();
   pltGeode[0] = view->Init( rotor1 );
   this->v22_DCS->addChild( pltGeode[0] );
   rotor1->Delete();

     vtkPLOT3DReader *rotor2 = vtkPLOT3DReader::New();
        rotor2->SetXYZFileName( "../v22/v22_bin/FAST_R2.bin" );
   rotor2->Update();
   pltGeode[1] = view->Init( rotor2 );
   this->v22_DCS->addChild( pltGeode[1] );
   rotor2->Delete();

     vtkPLOT3DReader *body1 = vtkPLOT3DReader::New();
        body1->SetXYZFileName( "../v22/v22_bin/FASTout.bdy1" );
   body1->Update();
   pltGeode[2] = view->Init( body1 );
   this->v22_DCS->addChild( pltGeode[2] );
   body1->Delete();

     vtkPLOT3DReader *body2 = vtkPLOT3DReader::New();
        body2->SetXYZFileName( "../v22/v22_bin/FASTout.bdy2" );
   body2->Update();
   pltGeode[3] = view->Init( body2 );
   this->v22_DCS->addChild( pltGeode[3] );
   body2->Delete();

     vtkPLOT3DReader *body3 = vtkPLOT3DReader::New();
        body3->SetXYZFileName( "../v22/v22_bin/FASTout.bdy3" );
   body3->Update();
   pltGeode[3] = view->Init( body3 );
   this->v22_DCS->addChild( pltGeode[3] );
   body3->Delete();

     vtkPLOT3DReader *body4 = vtkPLOT3DReader::New();
        body4->SetXYZFileName( "../v22/v22_bin/FASTout.bdy4" );
   body4->Update();
   pltGeode[3] = view->Init( body4 );
   this->v22_DCS->addChild( pltGeode[3] );
   body4->Delete();

     vtkPLOT3DReader *body5 = vtkPLOT3DReader::New();
        body5->SetXYZFileName( "../v22/v22_bin/FASTout.bdy5" );
   body5->Update();
   pltGeode[3] = view->Init( body5 );
   this->v22_DCS->addChild( pltGeode[3] );
   body5->Delete();

     vtkPLOT3DReader *body6 = vtkPLOT3DReader::New();
        body6->SetXYZFileName( "../v22/v22_bin/FASTout.bdy6" );
   body6->Update();
   pltGeode[3] = view->Init( body6 );
   this->v22_DCS->addChild( pltGeode[3] );
   body6->Delete();
     this->v22_DCS->setTrans(-350.0,-350.0,0.0);
     this->worldDCS->addChild(this->v22_DCS);
    break;
  case 6:
    this->datFileName = "./post_data/ford/flowdata.vtk";
    this->surfFName = "./post_data/ford/surface.vtk";
    cfdObjects::GetActiveMeshedVolume()->SetUserRange( 0.0f, 200.0f );
    break;
  }
}
*/

inline void cfdApp::initScene( )
{
   vprDEBUG(vprDBG_ALL,1) << "cfdApp::initScene" << std::endl << vprDEBUG_FLUSH;
   int i;

# ifdef _OPENMP
   std::cout << "\n\n\n";
   std::cout << "|===================================================================|" << std::endl;
   std::cout << "|          Compiled by an OpenMP-compliant implementation           |" << std::endl;
   std::cout << "|===================================================================|" << std::endl;
   std::cout << "|                                                                   |" << std::endl;
# endif

# ifdef _IRIX
   std::cout << "|   Performer Arena Size *** " << pfGetSharedArenaSize()/ (1024 * 1024) << std::endl;
	std::cout << "|   Shared arena base is *** " << pfGetSharedArenaBase() << std::endl;
   amallopt(M_MXCHK,10000000,pfGetSharedArena());
   amallopt(M_FREEHD, 1, pfGetSharedArena() );
# endif

   //
   // Establish Iris Performer Scenegraph.
   //
   std::cout << "|  1. Initializing................................ Performer scenes |" << std::endl;
   // Setup performer pipeline
   this->sunModel = new pfLightModel();
   this->rootNode = new pfGroup();
   this->worldDCS = new pfDCS();
   this->sun      = new pfLightSource();
   //this->lit      = new pfLightSource();
   this->gstate   = new pfGeoState();
   //this->scene    = new pfScene();

   // Setup geosets properties
   //this->sunModel->setTwoSide( PF_ON );
   //this->sunModel->apply();

   /* Override so that all geometry is lit with 'lmodel' */
   //pfOverride(PFSTATE_LIGHTMODEL, PF_ON);
   //this->gstate->setMode( PFSTATE_ENLIGHTING, PF_ON );
   //this->gstate->setMode( PFSTATE_ANTIALIAS, PFAA_ON );
   //this->gstate->setAttr( PFSTATE_LIGHTMODEL, sunModel );
   //this->gstate->setMode( PFSTATE_CULLFACE, PFCF_OFF );
   //this->scene->setGState( this->gstate );
   // Create lights
   this->sun->setPos( 100.0f, -100.0f, 100.0f, 0.0f );
   //this->sun->setPos( 0.0f, -1.0f, 0.0f, 0.0f );
   //this->sun->setColor( PFLT_DIFFUSE, 0.64f, 0.64f, 0.64f );
   this->sun->setColor( PFLT_DIFFUSE, 1.0f, 1.0f, 1.0f );
   //this->sun->setColor( PFLT_AMBIENT, 0.0f, 0.0f, 0.0f );
   this->sun->setColor( PFLT_AMBIENT, 0.4f, 0.4f, 0.4f );
   //this->sun->setColor( PFLT_SPECULAR, 0.64f, 0.64f, 0.64f );
   this->sun->setColor( PFLT_SPECULAR, 1.0f, 1.0f, 1.0f );
   this->sun->setVal(PFLS_INTENSITY, 1.0);
   this->sun->on();

   //this->lit->setPos( 100.0f, 0.0f, 0.0f, 0.0f );
   //this->sun->setPos( 0.0f, -1.0f, 0.0f, 0.0f );
   //this->lit->setColor( PFLT_DIFFUSE, 0.64f, 0.64f, 0.64f );
   //this->lit->setColor( PFLT_AMBIENT, 0.0f, 0.0f, 0.0f );
   //this->sun->setColor( PFLT_SPECULAR, 1.0f, 1.0f, 1.0f );
   //this->lit->setColor( PFLT_SPECULAR, 0.64f, 0.64f, 0.64f );
   //this->lit->on();
   // Add pfDCS and sun for the world
   this->rootNode->addChild( this->worldDCS );
   this->rootNode->addChild( this->sun );
   //this->rootNode->setGState( this->gstate );
   //this->rootNode->addChild( this->lit );
   this->temp_text = new pfDCS();

   this->pfb_count = 0;
   this->interactiveObject = false;
   this->animatedStreamlines = false;
   this->animatedImages = false;
   this->runStreamersThread = true;
   this->runIntraParallelThread = true;   
   this->activeSequenceObject = NULL;
   this->activeDataSetDCS = NULL;
   this->useLastSource= 0;
   this->computeActorsAndGeodes = false;
   // Initialize pointer to the active transient sequence
   this->lastFrame = -1;

   this->cfdId             = -1;
   this->cfdIso_value      = 999; 
   this->cfdSc             = 0;                    
   this->cfdMin            = 0;
   this->cfdMax            = 100;
   this->cfdGeo_state      = 0;
   this->cfdTeacher_state  = 0;
   this->cfdPostdata_state = 0;        
   this->cfdPre_state      = 0; 
   this->cfdTimesteps      = 0;
   this->cfdNumScalars     = 0;
   this->cfdNumVectors     = 0;
   this->cfdClients        = 0;

   char filein_name[100];
#ifdef _TAO
   std::cout << "|  2. Initializing.................................... cfdExecutive |" << std::endl;
   this->executive = new cfdExecutive( naming_context.in(), this->worldDCS );
#endif
   printf("|   Enter VE_Xplorer parameter filename: ");
   scanf("%s",filein_name);
   //std::cout << "filein_name: " << filein_name << std::endl;
   //**********************************Need to be changed************************************ HGX//
   std::cout << std::endl;
   std::cout << "| ***************************************************************** |" << std::endl;
   std::cout << "|  3. Initializing........................... Parameter File Reader |" << std::endl;
   this->paramReader = new cfdReadParam( filein_name );

   if ( this->paramReader->soundFile ) 
   {
      std::cout << "| Special: Initializing................................... cfdSound |" << std::endl;

      sonix::instance()->changeAPI( "OpenAL" );

      //this->sounds[i]->initApi();
      // this->sounds = new cfdSound[this->paramReader->soundFile];
      for(int i = 0; i < this->paramReader->soundFile; i++)
      {
         this->sounds.push_back(new cfdSound());
         strcpy(this->sounds[i]->fileName, this->paramReader->soundFiles[i]->fileName);
         this->sounds[i]->ambient        = this->paramReader->soundFiles[i]->ambient;
         this->sounds[i]->retriggerable  = this->paramReader->soundFiles[i]->retriggerable;
         this->sounds[i]->volume         = this->paramReader->soundFiles[i]->volume;
         this->sounds[i]->pitchbend      = this->paramReader->soundFiles[i]->pitchbend;
         this->sounds[i]->cutoff         = this->paramReader->soundFiles[i]->cutoff;
         this->sounds[i]->soundPositionX = this->paramReader->soundFiles[i]->soundPositionX;
         this->sounds[i]->soundPositionY = this->paramReader->soundFiles[i]->soundPositionY;
         this->sounds[i]->soundPositionZ = this->paramReader->soundFiles[i]->soundPositionZ;
         strcpy(this->sounds[i]->soundName, this->paramReader->soundFiles[i]->soundName);
         this->sounds[i]->initSound();
         
         std::cout << "Success  fileName: " << this->sounds[i]->fileName << std::endl;
         std::cout << "Success    volume: " << this->sounds[i]->volume << std::endl;
         std::cout << "Success soundName: " << this->sounds[i]->soundName << std::endl;
      }
   }

   this->worldDCS->setScale( this->paramReader->worldScale[ 0 ],
                             this->paramReader->worldScale[ 1 ],
                             this->paramReader->worldScale[ 2 ] );

   for ( i = 0; i < 3; i++)
   {
      this->nav->worldTrans[ i ] = this->paramReader->worldTrans[ i ];
      this->nav->worldRot[ i ] = this->paramReader->worldRot[ i ];
   }

   this->worldDCS->setTrans( -this->nav->worldTrans[ 0 ],
                             -this->nav->worldTrans[ 1 ],
                             -this->nav->worldTrans[ 2 ] );

   this->worldDCS->setRot( this->nav->worldRot[ 0 ],
                           this->nav->worldRot[ 1 ],
                           this->nav->worldRot[ 2 ] );

   // Locate and load the arrow file...
   char * arrowFile = fileIO::GetFile( "arrow", "/VE_Xplorer/data/arrow.vtk" );
   if ( arrowFile == NULL )
   {
      std::cout << " ERROR : The vtkPolyData arrow file could not be found "
                << "        Please create one and put it in the data dir "
                << std::endl;
      this->exit();
   }

   vtkPolyDataReader * arrowReader = vtkPolyDataReader::New();
   arrowReader->SetFileName( arrowFile );
   arrowReader->Update();

   this->arrow = vtkPolyData::New();
   this->arrow->ShallowCopy( arrowReader->GetOutput() );
   arrowReader->Delete();
   delete [] arrowFile;

/*
   // for each type 10 (transientInfo) group (currently limited to one)
   if ( this->paramReader->transientInfo.size() > 0 )
//       && this->paramReader->transientInfo[ 0 ]->GetFlowdataTransSet() )
   {
      // for each of the transient sets...
      for ( int i = 0; 
            i < this->paramReader->transientInfo[ 0 ]->GetNumberOfTransSets();
            i++)
      {
         cfdTransientSet * transientDataSet_i = 
                  this->paramReader->transientInfo[ 0 ]->GetTransSet( i );

         // compute the global scalar range...
         //transientDataSet_i->ReadScalarRanges();

         vprDEBUG(vprDBG_ALL,2) << "number of transient datasets: " 
                                << transientDataSet_i->GetNumberOfDataSets()
                                << std::endl << vprDEBUG_FLUSH;

         // for each transient dataset, put it on the global dataset list
         for ( int j = 0; j < (int)transientDataSet_i->GetNumberOfDataSets(); j++ )
         {
            vprDEBUG(vprDBG_ALL,2) << "working on transient dataset " << j 
                                   << std::endl << vprDEBUG_FLUSH;

            // put this transient dataset on the global dataset list
            this->paramReader->dataSets.push_back( 
                                         transientDataSet_i->GetDataSet( j ] );
         }
      }
   }
*/

   for ( i = 0; i < this->paramReader->GetNumberOfDataSets(); i++)
   {
      std::cout << "|   Loading data for file " 
                << this->paramReader->GetDataSet( i )->GetFileName()
                << std::endl;
      this->paramReader->GetDataSet( i )->LoadData();
      this->paramReader->GetDataSet( i )->SetArrow( this->arrow );
   }

   for ( i = 0; i < this->paramReader->GetNumberOfDataSets(); i++)
   {
      int cfdType = this->paramReader->GetDataSet( i )->GetType();
      vprDEBUG(vprDBG_ALL,1) << "cfdType: " << cfdType
                             << std::endl << vprDEBUG_FLUSH;

      // Initialize active meshed volume and polydata when suitable
      // datasets exist
      if ( cfdObjects::GetActiveMeshedVolume() == NULL && cfdType == 0 )
      {
         cfdObjects::SetActiveMeshedVolume( this->paramReader->GetDataSet(i) );

         vprDEBUG(vprDBG_ALL,1) << "Setting " 
            << cfdObjects::GetActiveMeshedVolume()->GetFileName()
            << " as active meshed volume"
            << std::endl << vprDEBUG_FLUSH;
      }
      else if ( cfdObjects::GetActiveParticleData() == NULL && cfdType == 1 )
      {
         cfdObjects::SetActiveParticleData( this->paramReader->GetDataSet(i) );

          vprDEBUG(vprDBG_ALL,1) << "Setting " 
            << cfdObjects::GetActiveParticleData()->GetFileName()
            << " as active particle set"
            << std::endl << vprDEBUG_FLUSH;
      }
      else if ( cfdObjects::GetActiveSurfaceData() == NULL && cfdType == 2 )
      {
         cfdObjects::SetActiveSurfaceData( this->paramReader->GetDataSet(i) );

          vprDEBUG(vprDBG_ALL,1) << "Setting " 
            << cfdObjects::GetActiveSurfaceData()->GetFileName()
            << " as active surface"
            << std::endl << vprDEBUG_FLUSH;
      }
   }
   // set default active dataset to be the meshed volume
   if ( cfdObjects::GetActiveMeshedVolume() )
      cfdObjects::SetActiveDataSet( cfdObjects::GetActiveMeshedVolume() );

   if ( cfdObjects::GetActiveDataSet() != NULL )
   {
      // set first scalar active
      cfdObjects::GetActiveDataSet()->SetActiveScalar( 0 );
      strcpy( oldDatasetName, cfdObjects::GetActiveDataSet()->GetFileName() );

      cfdVectorBase::SetThreshHoldPercentages( 0, 100 );
      cfdVectorBase::UpdateThreshHoldValues();
      cfdVectorBase::SetVectorRatioFactor( 1 );
   }

   // Process any geometry files listed in the parameter file...
   for ( int p = 0; p < this->paramReader->numGeoms; p++)
   {    
      std::cout << "|   Intializing Geometry file " 
                << this->paramReader->files[p]->fileName << std::endl;
      //biv --testing for windows
      cfdFILE* newGeom = 0;
      newGeom = new cfdFILE( this->paramReader->files[ p ],
                                          this->worldDCS );
      this->geomL.push_back( newGeom );
      this->geomL[ p ]->Initialize( 1.0 );

      // give the geometry DCS a name so that it can be detected during a CLEAR_ALL
      this->geomL[ p ]->getpfDCS()->setName("geometry");
   }
   this->changeGeometry = false;
#ifdef TABLET
   this->SetCfdReadParam( this->paramReader );
#endif
   std::cout << "| ***************************************************************** |" << std::endl;
   this->nav->Initialize( this->paramReader->delta, this->worldDCS );
   this->nav->SetWorldLocation( this->nav->worldTrans );
   //
   // Initiate shell of the data sets.
   //
   std::cout << "|  5. Initializing................................. Dataset surface |" << std::endl;
   this->surface = new cfdPolyData( 1.0 );
   this->surface->SetObjectType( POLYDATA );
   this->dataList.push_back( this->surface );
   this->chgMod = false;

   //
   // Initiate menu system.
   //
   std::cout << "|  6. Initializing..................................... Menu system |" << std::endl;

   char * menuFile = fileIO::GetFile( "menu", "/VE_Xplorer/data/menu.vtk" );

   if ( menuFile == NULL )
   {
      this->exit();
   }

   char * menuConfigFile = fileIO::GetFile( "menu configuration", "/VE_Xplorer/config/menu.cfg" );

   if ( menuConfigFile == NULL )
   {
      this->exit();
   }

   this->menu = new cfdMenu( menuFile, menuConfigFile );

   this->rootNode->addChild( this->menu->GetpfDCS() );
   this->menuB = true;
   this->cursorId = NONE;
   this->cfdId = -1;

   delete [] menuFile;
   delete [] menuConfigFile;

   //
   // Initiate wand's laser.
   //
   std::cout << "|  7. Initializing.................................... Wand's laser |" << std::endl;
   this->laser = new cfdLaser();
   this->rootNode->addChild( this->laser->GetpfDCS() );

   //
   // Initiate cursors.
   //
   std::cout << "|  8. Initializing................................. Virtual cursors |" << std::endl;
   this->cursor = new cfdCursor( this->arrow, this->worldDCS );
   this->cursor->Initialize( this->nav->GetCursorLocation(),this->nav->GetDirection() );

   //
   // Initiate the threads.
   //

   std::cout << "|  9. Initializing......................................... Threads |" << std::endl;
   this->vjThFunc[0] = new vpr::ThreadMemberFunctor<cfdApp> ( this, &cfdApp::intraParallelThread );
   this->vjTh[0] = new vpr::Thread( this->vjThFunc[0] );
   this->vjThFunc[1] = new vpr::ThreadMemberFunctor<cfdApp> ( this, &cfdApp::streamers );
   this->vjTh[1] = new vpr::Thread( this->vjThFunc[1] );
   
   //
   // Create the scalar bar for the currently active scalar of the active data set.
   //
   std::cout << "| 10. Initializing...................................... Scalar bar |" << std::endl;
   RefreshScalarBar();
   this->isTimeToUpdateScalarBar = false;
      
   this->cfdPostdata_state = 0;
   if ( cfdObjects::GetActiveMeshedVolume() != NULL )
   {
      if ( cfdObjects::GetActiveMeshedVolume()->GetPrecomputedXSlices() != NULL &&
           cfdObjects::GetActiveMeshedVolume()->GetPrecomputedXSlices()->GetPlanesData() != NULL )
         this->cfdPostdata_state += 1;

      if ( cfdObjects::GetActiveMeshedVolume()->GetPrecomputedYSlices() != NULL &&
           cfdObjects::GetActiveMeshedVolume()->GetPrecomputedYSlices()->GetPlanesData() != NULL )
         this->cfdPostdata_state += 2;

      if ( cfdObjects::GetActiveMeshedVolume()->GetPrecomputedZSlices() != NULL &&
           cfdObjects::GetActiveMeshedVolume()->GetPrecomputedZSlices()->GetPlanesData() != NULL )
         this->cfdPostdata_state += 4;

      //
      // Initiate the isosurface.
      //
      std::cout << "| 14. Initializing...................................... Isosurface |" << std::endl;
      this->isosurface = new cfdIsosurface( 10 );
      this->isosurface->SetObjectType( ISOSURFACE );
      this->dataList.push_back( this->isosurface );
     
      //
      // Initiate the interactive contour.
      //
      std::cout << "| 15. Initializing......................................... Contour |" << std::endl;
      this->contour = new cfdContour();
      this->contour->SetObjectType( CONTOUR );
      this->dataList.push_back( this->contour ); 
     
      // Make sure that this dataset has a vector and scalar...
      if ( cfdObjects::GetActiveMeshedVolume()->GetDataSet()
                                              ->GetPointData()->GetVectors() &&
           cfdObjects::GetActiveMeshedVolume()->GetDataSet()
                                              ->GetPointData()->GetScalars() )
      {
         //
         // Initiate the interactive momentum.
         //
         std::cout << "| 16. Initializing........................................ Momemtum |" << std::endl;
         this->momentum = new cfdMomentum();
         this->momentum->SetObjectType( MOMENTUM );
         this->dataList.push_back( this->momentum );
     
         //
         // Initiate the interactive vector.
         //
         std::cout << "| 17. Initializing.......................................... Vector |" << std::endl;
         this->vector = new cfdVector();
         this->vector->SetObjectType( VECTOR );
         this->dataList.push_back( this->vector );
      }
     
      if ( cfdObjects::GetActiveMeshedVolume()->GetDataSet()->GetPointData()->GetScalars() )
      {
         //
         // Initiate the preset x contour.
         //
         std::cout << "| 19. Initializing................................ Preset x Contour |" << std::endl;
         this->x_contour = new cfdPresetContour( 0, 10 );
         this->x_contour->SetObjectType( X_CONTOUR );
         this->dataList.push_back( this->x_contour );
        
         //
         // Initiate the preset y contour.
         //
         std::cout << "| 20. Initializing................................ Preset y Contour |" << std::endl;
         this->y_contour = new cfdPresetContour( 1, 10 );
         this->y_contour->SetObjectType( Y_CONTOUR );
         this->dataList.push_back( this->y_contour );
        
         //
         // Initiate the preset z contour.
         //
         std::cout << "| 21. Initializing................................ Preset z Contour |" << std::endl;
         this->z_contour = new cfdPresetContour( 2, 10 );
         this->z_contour->SetObjectType( Z_CONTOUR );
         this->dataList.push_back( this->z_contour );
      }
     
      // Make sure that this dataset has a vector field...
      if ( cfdObjects::GetActiveMeshedVolume()->GetDataSet()->GetPointData()->GetVectors() )
      {
         //
         // Initiate the preset x momentum.
         //
         std::cout << "| 22. Initializing............................... Preset x Momentum |" << std::endl;
         this->x_momentum = new cfdPresetMomentum( 0, paramReader->isoScale, 10 );
         this->x_momentum->SetObjectType( X_MOMENTUM );
         this->dataList.push_back( this->x_momentum );
        
         //
         // Initiate the preset y momentum.
         //
         std::cout << "| 23. Initializing............................... Preset y Momentum |" << std::endl;
         this->y_momentum = new cfdPresetMomentum( 1, paramReader->isoScale, 10 );
         this->y_momentum->SetObjectType( Y_MOMENTUM );
         this->dataList.push_back( this->y_momentum );
        
         //
         // Initiate the preset z momentum.
         //
         std::cout << "| 24. Initializing............................... Preset z Momentum |" << std::endl;
         this->z_momentum = new cfdPresetMomentum( 2, paramReader->isoScale, 10 );
         this->z_momentum->SetObjectType( Z_MOMENTUM );
         this->dataList.push_back( this->z_momentum );
        
         //
         // Initiate the preset x vector.
         //
         std::cout << "| 25. Initializing................................. Preset x Vector |" << std::endl;
         this->x_vector = new cfdPresetVector( 0, 10 );
         this->x_vector->SetObjectType( X_VECTOR );
         this->dataList.push_back( this->x_vector );
        
         //
         // Initiate the preset y vector.
         //
         std::cout << "| 26. Initializing................................. Preset y Vector |" << std::endl;
         this->y_vector = new cfdPresetVector( 1, 10 );
         this->y_vector->SetObjectType( Y_VECTOR );
         this->dataList.push_back( this->y_vector );

         //
         // Initiate the preset z vector.
         //
         std::cout << "| 27. Initializing................................. Preset z Vector |" << std::endl;
         this->z_vector = new cfdPresetVector( 2, 10 );
         this->z_vector->SetObjectType( Z_VECTOR );
         this->dataList.push_back( this->z_vector );
      }

      //
      // Initiate the preset x contour lines.
      //
      if ( cfdObjects::GetActiveMeshedVolume()->GetPrecomputedXSlices() != NULL &&
           cfdObjects::GetActiveMeshedVolume()->GetPrecomputedXSlices()->GetPlanesData() != NULL )
      {
         std::cout << "| 28. Initializing....................Multiple X-planes of Contours |" << std::endl;
         this->x_contours = new cfdContours( 0 );
         this->x_contours->SetObjectType( X_CONTOURS );
         this->dataList.push_back( this->x_contours );
      }

      //
      // Initiate the preset y contour lines.
      //
      if ( cfdObjects::GetActiveMeshedVolume()->GetPrecomputedYSlices() != NULL &&
           cfdObjects::GetActiveMeshedVolume()->GetPrecomputedYSlices()->GetPlanesData() != NULL )
      {
         std::cout << "| 29. Initializing....................Multiple Y-planes of Contours |" << std::endl;
         this->y_contours = new cfdContours( 1 );
         this->y_contours->SetObjectType( Y_CONTOURS );
         this->dataList.push_back( this->y_contours );
      }

      //
      // Initiate the preset z contour lines.
      //
      if ( cfdObjects::GetActiveMeshedVolume()->GetPrecomputedZSlices() != NULL &&
           cfdObjects::GetActiveMeshedVolume()->GetPrecomputedZSlices()->GetPlanesData() != NULL )
      {
         std::cout << "| 30. Initializing....................Multiple Z-planes of Contours |" << std::endl;
         this->z_contours = new cfdContours( 2 );
         this->z_contours->SetObjectType( Z_CONTOURS );
         this->dataList.push_back( this->z_contours );
      }

      // Make sure that this dataset has a vector and scalar...
      if ( cfdObjects::GetActiveMeshedVolume()->GetDataSet()
                                              ->GetPointData()->GetVectors() &&
           cfdObjects::GetActiveMeshedVolume()->GetDataSet()
                                              ->GetPointData()->GetScalars() )

      {
         //
         // Initiate the preset x momentums.
         //
         if ( cfdObjects::GetActiveMeshedVolume()->GetPrecomputedXSlices() != NULL &&
              cfdObjects::GetActiveMeshedVolume()->GetPrecomputedXSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 31. Initializing.......Multiple X-planes of Precomputed Momentums |" << std::endl;
            this->x_momentums = new cfdMomentums( 0, paramReader->isoScale );
            this->x_momentums->SetObjectType( X_MOMENTUMS );
            this->dataList.push_back( this->x_momentums );
         }

         //
         // Initiate the preset y momentums.
         //
         if ( cfdObjects::GetActiveMeshedVolume()->GetPrecomputedYSlices() != NULL &&
              cfdObjects::GetActiveMeshedVolume()->GetPrecomputedYSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 32. Initializing.......Multiple Y-planes of Precomputed Momentums |" << std::endl;
            this->y_momentums = new cfdMomentums( 1, paramReader->isoScale );
            this->y_momentums->SetObjectType( Y_MOMENTUMS );
            this->dataList.push_back( this->y_momentums );
         }

         //
         // Initiate the preset z momentums.
         //
         if ( cfdObjects::GetActiveMeshedVolume()->GetPrecomputedZSlices() != NULL &&
              cfdObjects::GetActiveMeshedVolume()->GetPrecomputedZSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 33. Initializing.......Multiple Z-planes of Precomputed Momentums |" << std::endl;
            this->z_momentums = new cfdMomentums( 2, paramReader->isoScale );
            this->z_momentums->SetObjectType( Z_MOMENTUMS );
            this->dataList.push_back( this->z_momentums );
         }
      }

      // Make sure that this dataset has a vector and scalar...
      if ( cfdObjects::GetActiveMeshedVolume()->GetDataSet()
                                              ->GetPointData()->GetVectors() )
      {
         //
         // Initiate the preset x vectors.
         //
         if ( cfdObjects::GetActiveMeshedVolume()->GetPrecomputedXSlices() != NULL &&
              cfdObjects::GetActiveMeshedVolume()->GetPrecomputedXSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 34. Initializing.........Multiple X-planes of Precomputed Vectors |" << std::endl;
            this->x_vectors = new cfdVectors( 0 );
            this->x_vectors->SetObjectType( X_VECTORS );
            this->dataList.push_back( this->x_vectors );
         }

         //
         // Initiate the preset y vectors.
         //
         if ( cfdObjects::GetActiveMeshedVolume()->GetPrecomputedYSlices() != NULL &&
              cfdObjects::GetActiveMeshedVolume()->GetPrecomputedYSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 35. Initializing.........Multiple Y-planes of Precomputed Vectors |" << std::endl;
            this->y_vectors = new cfdVectors( 1 );
            this->y_vectors->SetObjectType( Y_VECTORS );
            this->dataList.push_back( this->y_vectors );
         }
         
         //
         // Initiate the preset z vectors.
         //
         if ( cfdObjects::GetActiveMeshedVolume()->GetPrecomputedZSlices() != NULL &&
              cfdObjects::GetActiveMeshedVolume()->GetPrecomputedZSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 36. Initializing.........Multiple Z-planes of Precomputed Vectors |" << std::endl;
            this->z_vectors = new cfdVectors( 2 );
            this->z_vectors->SetObjectType( Z_VECTORS );
            this->dataList.push_back( this->z_vectors );
         }

         //
         // Initiate the streamlines.
         //
         std::cout << "| 37. Initializing..................................... Streamlines |" << std::endl;
         this->streamlines = new cfdStreamers( this->paramReader->diameter );
         this->streamlines->SetObjectType( STREAMLINES );
         this->dataList.push_back( this->streamlines );     

         //
         // Initiate the animated streamers.
         //
         std::cout << "| 39. Initializing............................. Animated Streamline |" << std::endl;
         this->animStreamer = new cfdAnimatedStreamlineCone( this->paramReader->diameter );
         this->animStreamer->SetObjectType( ANIMATED_STREAMLINES );
         this->dataList.push_back( this->animStreamer );     

	      //
         // Initiate the animated Images.
         //
         if ( this->paramReader->frames != 0 )
         {
            std::cout << "| 39.b Initializing............................. Animated Images |" << std::endl;
            this->animImg = new cfdAnimatedImage( this->paramReader );
            this->animImg->SetObjectType( ANIMATED_IMAGES );
            this->dataList.push_back( this->animImg);  
         }  
      }
   }

   //
   // Initiate the Text Prompt
   //
   //std::cout << "| 40. Initializing..................................... Text Prompt |" << std::endl;
   //this->tPrompt = new textPrompt();
   //this->text_sig = 0 ;

   //
   // Initiate the PolyData File
   //
   if ( cfdObjects::GetActiveParticleData() != NULL )
   {
      std::cout << "| 41. Initializing................................... PolyData File |" << std::endl;
      this->particles = new cfdPolyData();
      this->particles->SetObjectType( PARTICLES );
      this->dataList.push_back( this->particles );     
   } 

   //
   // Initiate PIV data from INEL
   //
   if ( this->paramReader->bmpFile ) 
   {
      std::cout << "| 42. Initializing.................................... Bitmap Image |" << std::endl;
      this->image = new cfdImage( this->paramReader->bmpFileName,
                                  this->paramReader->bmpPosition,
                                  this->paramReader->bmpOrientation );
      this->image->SetObjectType( IMAGE_EX );
      this->dataList.push_back( this->image );     
   } 

   //
   // Initiate Transient cfdAnimation
   //
   if ( this->paramReader->transientInfo.size() > 0 ) 
   {   
      std::cout << "| 42. Initializing.................................... cfdAnimation |" << std::endl;
      this->transientSequence = new cfdAnimation();
      this->transientSequence->SetDuration( this->paramReader->transientInfo[ 0 ]->GetDuration() );

      int numFrames;

      //
      // Initiate Transient X Contour Data
      //
      // windshield hack follows
      if ( this->paramReader->transientInfo[ 0 ]->Get_X_planeTransSet() &&
           this->paramReader->transientInfo[ 0 ]->Get_X_planeTransSet()->GetNumberOfScalars() ) 
      {
         int count = 0;
         for ( i = 0; i < (int)this->paramReader->transientInfo.size(); i++ )
         {
            std::cout << "| 43. Initializing........................ Transient X Contour Data |" << std::endl;
            this->_cfdTFM_X_Contour[ count ] = new cfdTransientFlowManager();
            this->_cfdTFM_X_Contour[ count ]->SetParameterFile( this->paramReader, i );
            this->_cfdTFM_X_Contour[ count ]->SetDirectory( this->paramReader->transientInfo[ 0 ]->Get_X_planeTransSet()->GetDirectory() );
            this->_cfdTFM_X_Contour[ count ]->SetFrameDataType( cfdFrame::VTK_SCALAR );
            numFrames = this->_cfdTFM_X_Contour[ count ]->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList.push_back( this->_cfdTFM_X_Contour[ count ] );     
            this->dataList.back()->SetObjectType( X_TRANSIENT_CONTOUR_AND_VECTOR );
            this->dataList.back()->SetpfSequence( this->transientSequence->GetpfSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_X_Contour[ count ] );
            count++;
            
            if ( count == 2 )
               break;
         }
      } 

      //
      // Initiate Transient Y Contour Data
      //
      if ( this->paramReader->transientInfo[ 0 ]->Get_Y_planeTransSet() &&
           this->paramReader->transientInfo[ 0 ]->Get_Y_planeTransSet()->GetNumberOfScalars() ) 
      {
         for ( i = 0; i < (int)this->paramReader->transientInfo.size(); i++ )
         {
            std::cout << "| 44. Initializing........................ Transient Y Contour Data |" << std::endl;
            this->_cfdTFM_Y_Contour = new cfdTransientFlowManager();
            this->_cfdTFM_Y_Contour->SetParameterFile( this->paramReader, i );
            this->_cfdTFM_Y_Contour->SetDirectory( this->paramReader->transientInfo[ 0 ]->Get_Y_planeTransSet()->GetDirectory() );
            this->_cfdTFM_Y_Contour->SetFrameDataType( cfdFrame::VTK_SCALAR );
            numFrames = this->_cfdTFM_Y_Contour->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList.push_back( this->_cfdTFM_Y_Contour );     
            this->dataList.back()->SetObjectType( Y_TRANSIENT_CONTOUR_AND_VECTOR );
            this->dataList.back()->SetpfSequence( this->transientSequence->GetpfSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_Y_Contour );
            break;
         }
      } 

      //
      // Initiate Transient Z Contour Data
      //
      if ( this->paramReader->transientInfo[ 0 ]->Get_Z_planeTransSet() &&
           this->paramReader->transientInfo[ 0 ]->Get_Z_planeTransSet()->GetNumberOfScalars() ) 
      {
         for ( i = 0; i < (int)this->paramReader->transientInfo.size(); i++ )
         {
            std::cout << "| 45. Initializing........................ Transient Z Contour Data |" << std::endl;
            this->_cfdTFM_Z_Contour = new cfdTransientFlowManager();
            this->_cfdTFM_Z_Contour->SetParameterFile( this->paramReader, i );
            this->_cfdTFM_Z_Contour->SetDirectory( this->paramReader->transientInfo[ 0 ]->Get_Z_planeTransSet()->GetDirectory() );
            this->_cfdTFM_Z_Contour->SetFrameDataType( cfdFrame::VTK_SCALAR );
            numFrames = this->_cfdTFM_Z_Contour->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList.push_back( this->_cfdTFM_Z_Contour );     
            this->dataList.back()->SetObjectType( Z_TRANSIENT_CONTOUR_AND_VECTOR );
            this->dataList.back()->SetpfSequence( this->transientSequence->GetpfSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_Z_Contour );
            break;
         }
      } 

      //
      // Initiate Transient Vector Data
      //
      if ( this->paramReader->transientInfo[ 0 ]->Get_X_planeTransSet() &&
           this->paramReader->transientInfo[ 0 ]->Get_X_planeTransSet()->GetNumberOfVectors() ) 
      {
         for ( i = 0; i < (int)this->paramReader->transientInfo.size(); i++ )
         {
            std::cout << "| 46. Initializing......................... Transient X Vector Data |" << std::endl;
            this->_cfdTFM_X_Vector = new cfdTransientFlowManager();
            this->_cfdTFM_X_Vector->SetParameterFile( this->paramReader, i );
            this->_cfdTFM_X_Vector->SetDirectory( this->paramReader->transientInfo[ 0 ]->Get_X_planeTransSet()->GetDirectory() );
            this->_cfdTFM_X_Vector->SetFrameDataType( cfdFrame::VTK_VECTOR );
            numFrames = this->_cfdTFM_X_Vector->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList.push_back( this->_cfdTFM_X_Vector );     
            this->dataList.back()->SetObjectType( X_TRANSIENT_CONTOUR_AND_VECTOR );
            this->dataList.back()->SetpfSequence( this->transientSequence->GetpfSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_X_Vector );
            break;
         }
      }

      //
      // Initiate Transient Vector Data
      //
      if ( this->paramReader->transientInfo[ 0 ]->Get_Y_planeTransSet() &&
           this->paramReader->transientInfo[ 0 ]->Get_Y_planeTransSet()->GetNumberOfVectors() ) 
      {
         for ( i = 0; i < (int)this->paramReader->transientInfo.size(); i++ )
         {
            std::cout << "| 47. Initializing......................... Transient Y Vector Data |" << std::endl;
            this->_cfdTFM_Y_Vector = new cfdTransientFlowManager();
            this->_cfdTFM_Y_Vector->SetParameterFile( this->paramReader, i );
            this->_cfdTFM_Y_Vector->SetDirectory( this->paramReader->transientInfo[ 0 ]->Get_Y_planeTransSet()->GetDirectory() );
            this->_cfdTFM_Y_Vector->SetFrameDataType( cfdFrame::VTK_VECTOR );
            numFrames = this->_cfdTFM_Y_Vector->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList.push_back( this->_cfdTFM_Y_Vector );     
            this->dataList.back()->SetObjectType( Y_TRANSIENT_CONTOUR_AND_VECTOR );
            this->dataList.back()->SetpfSequence( this->transientSequence->GetpfSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_Y_Vector );
            break;
         }
      }

      //
      // Initiate Transient Vector Data
      //
      if ( this->paramReader->transientInfo[ 0 ]->Get_Z_planeTransSet() &&
           this->paramReader->transientInfo[ 0 ]->Get_Z_planeTransSet()->GetNumberOfVectors() ) 
      {
         for ( i = 0; i < (int)this->paramReader->transientInfo.size(); i++ )
         {
            std::cout << "| 48. Initializing......................... Transient Z_Vector Data |" << std::endl;
            this->_cfdTFM_Z_Vector = new cfdTransientFlowManager();
            this->_cfdTFM_Z_Vector->SetParameterFile( this->paramReader, i );
            this->_cfdTFM_Z_Vector->SetDirectory( this->paramReader->transientInfo[ 0 ]->Get_Z_planeTransSet()->GetDirectory() );
            this->_cfdTFM_Z_Vector->SetFrameDataType( cfdFrame::VTK_VECTOR );
            numFrames = this->_cfdTFM_Z_Vector->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList.push_back( this->_cfdTFM_Z_Vector );     
            this->dataList.back()->SetObjectType( Z_TRANSIENT_CONTOUR_AND_VECTOR );
            this->dataList.back()->SetpfSequence( this->transientSequence->GetpfSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_Z_Vector );
            break;
         }
      }

      //
      // Initiate Transient Geometry Data
      //
      if ( this->paramReader->transientInfo[ 0 ]->GetGeometryDir() ) 
      {
         int count = 0;
         for ( i = 0; i < (int)this->paramReader->transientInfo.size(); i++ )
         {
            std::cout << "| 49. Initializing......................... Transient Geometry Data |" << std::endl;
            this->_cfdTFM_Geometry[ count ] = new cfdTransientFlowManager();
            this->_cfdTFM_Geometry[ count ]->SetParameterFile( this->paramReader, i );
            this->_cfdTFM_Geometry[ count ]->SetDirectory( 
                            this->paramReader->transientInfo[ 0 ]->GetGeometryDir() );
            this->_cfdTFM_Geometry[ count ]->SetFrameDataType( cfdFrame::GEOM );
            numFrames = this->_cfdTFM_Geometry[ count ]->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->_cfdTFM_Geometry[ count ]->SetObjectType( TRANS_GEOM );
            this->_cfdTFM_Geometry[ count ]->SetpfSequence( this->transientSequence->GetpfSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_Geometry[ count ] );
            this->dataList.push_back( this->_cfdTFM_Geometry[ count ] ); 

            this->_cfdTFM_Geometry[ count ]->LoadFrames();

            count++;
            if ( count == 2 )
               break;
         }
      } 

      //
      // Initiate Transient Particle Data
      //
      if ( this->paramReader->transientInfo[ 0 ]->GetDropletTransSet() ) 
      {
         for ( i = 0; i < (int)this->paramReader->transientInfo.size(); i++ )
         {
            std::cout << "| 50. Initializing......................... Transient Particle Data |" << std::endl;
            this->_cfdTFM_Particle = new cfdTransientFlowManager();
            this->_cfdTFM_Particle->SetParameterFile( this->paramReader, i );
            this->_cfdTFM_Particle->SetDirectory( this->paramReader->transientInfo[ 0 ]->GetDropletTransSet()->GetDirectory() );
            this->_cfdTFM_Particle->SetFrameDataType( cfdFrame::VTK_PARTICLE );
            numFrames = this->_cfdTFM_Particle->StoreFrameFileNames();
            vprDEBUG(vprDBG_ALL,2) << " transient numFrames " << numFrames
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList.push_back( this->_cfdTFM_Particle ); 
            this->dataList.back()->SetObjectType( PARTICLE_TRANSIENT );
            this->dataList.back()->SetpfSequence( this->transientSequence->GetpfSequence() );
            this->transientSequence->AddAFlowManager( this->_cfdTFM_Particle );
            break;
         }
      } 

      // Add the appropriate number of pfGroups to the sequence node...
      this->transientSequence->SetpfGroups();
   } 

   //
   // Initiate the Performer objects.
   //
   std::cout << "| 51. Initializing........................................ pfGeodes |" << std::endl;

   bool transient_flag = true;
   for ( i = 0; i < (int)this->dataList.size(); i++ )
   {
      this->dataList[ i ]->SetcfdReadParam( this->paramReader );
      this->dataList[ i ]->SetUpdateFlag( false );
      this->dataList[ i ]->SetGeodeFlag( false );
      this->dataList[ i ]->SetDCS( this->worldDCS );

      // for the objects in the virtual environment.
      cfdTransientFlowManager * tfmTest = dynamic_cast<cfdTransientFlowManager *>( this->dataList[ i ] );
      if ( transient_flag && tfmTest != NULL )
      {
         this->cfdTimesteps = tfmTest->GetNumberOfFrames();
         transient_flag = false;
      }
   }

   std::cout << "|   Maximum animated transient frame number = "
             << this->cfdTimesteps << std::endl;

   //
   // Initiate the Performer Stored Binary objects.
   //
   std::cout << "| 52. Initializing...................................... pfBinaries |" << std::endl;
   this->teacher = new cfdTeacher( "STORED_FILES", this->worldDCS );

   std::cout << "| 53. Initializing..................................... cfdQuatCams |" << std::endl;
   this->quatcamHandler = new cfdQuatCamHandler();
   if ( this->paramReader->quatCamFileName != NULL )
      this->quatcamHandler->LoadFromFile(this->paramReader->quatCamFileName);
#ifdef TABLET
   this->SetCfdTeacher( this->teacher );
#endif
   // initalize pfb_count to number of stored binaries in an attempt to prevent
   // overwriting of existing files...
   this->pfb_count = this->teacher->getNumberOfFiles();

   //
   // Make IHCC Model - should be deleted at a later date
   //
   if ( this->paramReader->ihccModel )
   {
      std::cout << "| 54. Initializing...................................... IHCC Model |" << std::endl;
      this->ihccModel = new cfdIHCCModel( NULL, this->worldDCS );
   }

   // Create data in memory for transfered data
#ifdef _TAO
   this->CreateSoundInfo();
   this->CreateGeometryInfo();
   this->CreateDatasetInfo();
   this->CreateTeacherInfo();
#endif

#ifdef TABLET
   this->pushDataToStateInfo();
   this->GetCfdStateVariables();
#endif
}

inline void cfdApp::flush_text(char * t)
{
   return;
    /*-- for 3D pop-up text message
      set add_text signal to activate text prompt:
      0 = no output(initial state)
      1 = add text to output(without timer)
      2 = add text to output(with timer)
      3 = hold the output(with timer)
      4 = delete text from output
      6 = hold the output(without timer)
      9 = not allow text output unless re-select the menu*/
      
   my_timer = time(&my_timer);

   /*
   if  (text_sig == 3 || text_sig == 6)
   {
     this->rootNode->removeChild(this->rootNode->getChild(3));

     this->rootNode->insertChild(3,this->tPrompt->add_text(t));
     this->tPrompt->DeleteText();
          
   }
   */          //for moving text -- job deferred   
    
   if (text_sig == 1)
   {
      this->temp_text = this->tPrompt->add_text(t);
      this->rootNode->insertChild(3,this->temp_text);
      vprDEBUG(vprDBG_ALL,1) << "TTT" << std::endl << vprDEBUG_FLUSH;
      //this->tPrompt->DeleteText();
      text_sig = 6;
   }

   if (text_sig == 2)
   {
      this->temp_text = this->tPrompt->add_text(t);
      this->rootNode->insertChild(3,this->temp_text);
      //this->tPrompt->DeleteText();
      text_sig = 3;     //  3 = hold the output(with timer)
   }

   if ( (my_timer - time_r) > 5 && text_sig == 3)
   {
      text_sig = 4;
   }

   if (text_sig == 4)    // 4 = delete text from output
   { 
      int text_num =  rootNode->searchChild(this->temp_text);
      if ( text_num != -1)
      {
         temp_text = this->rootNode->getChild(text_num);
         this->rootNode->removeChild(temp_text);
         pfDelete(temp_text);
         //this->tPrompt->DeleteText();
      }
      else
      {
         vprDEBUG(vprDBG_ALL,1) << "in cfdApp: no text to delete" 
                                << std::endl << vprDEBUG_FLUSH;
      }
      text_sig = 9;
   }
}

void clearGeodesFromNode( pfNode * node )
{
   if ( node == NULL )
      return;

   const char * name = node->getName();

   if ( name != NULL )
   {
      vprDEBUG(vprDBG_ALL,1) << " node name = \"" << name << "\""
                             << std::endl << vprDEBUG_FLUSH;
   }

   if ( name != NULL && 
         (  ( ! strcmp(name, "scalarBar") ) || 
            ( ! strcmp(name, "cfdExecutive_Node") ) || 
            ( ! strcmp(name, "geometry") ) 
         ) 
      )
   {
      vprDEBUG(vprDBG_ALL,1) << "\twon't touch this node"
                             << std::endl << vprDEBUG_FLUSH;
      return;
   }

   if ( node->isOfType( pfGroup::getClassType() ) )
   {
      int numChildren = ((pfGroup*)node)->getNumChildren();
      vprDEBUG(vprDBG_ALL,1) << " group node has numChildren = " << numChildren
                             << std::endl << vprDEBUG_FLUSH;

      // Iterate backwards for performance
      for ( int i = numChildren-1; i >= 0; i-- )
      {
         pfNode * childNode = ((pfGroup*)node)->getChild( i );

         clearGeodesFromNode( childNode );
      }
   }
   else if ( node->isOfType( pfGeode::getClassType() ) )
   {
      vprDEBUG(vprDBG_ALL,1) << "\tremoving this node"
                             << std::endl << vprDEBUG_FLUSH;

      int numParents = node->getNumParents();
      if ( numParents != 1 )
      {
         vprDEBUG(vprDBG_ALL,1) << "\t!!!!!!!!numParents = " << numParents
                                << std::endl << vprDEBUG_FLUSH;
      }

      node->getParent( 0 )->removeChild( node );
      //pfDelete( node );  // don't do this
   }
   else
   {
      cout << "\twon't remove this node" << endl;
   }
}

void cfdApp::preFrame( void )
{
   vprDEBUG(vprDBG_ALL,3) << "cfdApp::preFrame" << std::endl << vprDEBUG_FLUSH;

#ifdef _CLUSTER
   // CLuster Stuff
   this->GetUpdateClusterStateVariables();
#endif

   // Update Navigation variables
   this->nav->SetDataValues(this->cfdId, this->cfdIso_value);
   this->nav->updateNavigationFromGUI();

   // Check the menu is toggle on/off
   if ( this->menuB && cursorId == NONE )
   {
     // if menu is on, laser will detect where it is hitting on the menu's cell
      if ( this->laser->HitMenu( this->menu->GetBound(), 
                                 this->nav->GetLocation(), 
                                 this->nav->GetDirection() ) ) 
      {
         this->menu->UpdateHitCell( this->laser->GetHitXYZ() );
      }
   } 
   else 
   { 
      // if menu is off, the cursor will be active (based on the cursor id)
      this->cursor->Update( cursorId, this->nav->GetCursorLocation(),
                              this->nav->GetDirection(), this->nav->worldTrans );

      if ( this->cursorId == CUBE)
      {
          this->cursor->getExtent( this->cur_box );   //record current box cursor position
      }
   }

   if ( this->cfdId != -1 )
   {
      vprDEBUG(vprDBG_ALL,2) << "preFrame: id = " << this->cfdId 
                             << ", iso = " << this->cfdIso_value
                             << ", scalarIndex = " << this->cfdSc
                             << ", min = " << this->cfdMin 
                             << ", max = " << this->cfdMax 
                             << ", geo_state = " << this->cfdGeo_state
                             << ", pre_state = " << this->cfdPre_state
                             << ", teacher_state = " << this->cfdTeacher_state
                             << std::endl << vprDEBUG_FLUSH;
   }

   int i;

   if ( this->nav->digital[0]->getData() == gadget::Digital::TOGGLE_ON && 
        this->cursorId == NONE )
   {
      //the trigger is active and we are selecting something from the menu     
      //determine which item we are picking on the menu
      this->setId( this->menu->GetCellId() );;
   }
   else if ( this->cfdId == BLUE_MENU_TOGGLE )
   {
      if ( this->menuB == true )
      { 
         this->rootNode->removeChild( this->menu->GetpfDCS() );
         this->rootNode->removeChild( this->laser->GetpfDCS() );
         this->menuB = false;
      }
      else
      { 
         // If menu is down, add menu and laser wand, and keep cursor off
         this->rootNode->addChild( this->menu->GetpfDCS() );
         this->rootNode->addChild( this->laser->GetpfDCS() );
         this->menuB = true;
      }

      this->setId( -1 );
   }
   else if ( this->cfdId == SCALAR_BAR_TOGGLE )
   {
      if ( this->scalarBarActor )
      { 
          this->rootNode->removeChild( this->scalarBarActor->getpfDCS() );
          //this->worldDCS->removeChild( this->scalarBarActor->getpfDCS() );
          delete this->scalarBarActor;
          this->scalarBarActor = NULL;
      }
      else
      { 
          RefreshScalarBar();
      }

      this->setId( -1 );
   }
   else if ( this->cfdId == USE_LAST_STREAMLINE_SEEDPOINTS )
   {
      this->useLastSource = this->cfdIso_value;

      this->setId( -1 );
   }
   else if ( this->cfdId == CHANGE_STREAMLINE_CURSOR )
   {
      vprDEBUG(vprDBG_ALL,1) << "this->id = " << this->cfdId 
                << ", this->min = " << this->cfdMin 
                << ", this->max = " << this->cfdMax
                << std::endl << vprDEBUG_FLUSH;

      if ( this->cfdIso_value == NO_CURSOR )
      {
         vprDEBUG(vprDBG_ALL,1) 
           << "removing cursor with cursor->GetpfDCS() = "
           << this->cursor->GetpfDCS() << std::endl << vprDEBUG_FLUSH;

         this->cursorId = NONE;
         if ( this->rootNode->searchChild( this->cursor->GetpfDCS() ) >= 0 )
            this->rootNode->removeChild( this->cursor->GetpfDCS() );
      }
      else
      {
         if ( this->cfdIso_value == POINT_CURSOR )
            this->cursorId = SPHERE;
         else if ( this->cfdIso_value == X_PLANE_CURSOR)
            this->cursorId = XPLANE;
         else if ( this->cfdIso_value == Y_PLANE_CURSOR)
            this->cursorId = YPLANE;
         else if ( this->cfdIso_value == Z_PLANE_CURSOR)
            this->cursorId = ZPLANE;
         else if ( this->cfdIso_value == X_LINE_CURSOR)
            this->cursorId = XLINE;
         else if ( this->cfdIso_value == Y_LINE_CURSOR)
            this->cursorId = YLINE;
         else if ( this->cfdIso_value == Z_LINE_CURSOR)
            this->cursorId = ZLINE;
         else
         {
            vprDEBUG(vprDBG_ALL,0) 
              << "ERROR: Unknown cursorId -- Setting cursor to XPLANE"
              << std::endl << vprDEBUG_FLUSH;

            this->cursorId = XPLANE;
         }
         this->chgMod = true;

         vprDEBUG(vprDBG_ALL,1) 
           << "adding cursor with cursor->GetpfDCS() = "
           << this->cursor->GetpfDCS() << std::endl << vprDEBUG_FLUSH;

         // if disconnected from scene graph, add
         if ( this->rootNode->searchChild( this->cursor->GetpfDCS() ) < 0 )
         {
            this->rootNode->addChild( this->cursor->GetpfDCS() );
         }
      }

      if ( cfdObjects::GetActiveDataSet() != NULL )
         this->cursor->SetActiveDataSetDCS( cfdObjects::GetActiveDataSet()->GetDCS() );

      //if ( this->cursorId != NONE && this->cursorId != SPHERE && this->cursorId != CUBE )
      if ( this->cursorId != NONE && this->cursorId != SPHERE )
      {
         this->cursor->SetPlaneReso( this->cfdMin ); 

         // convert size percentage (0-100) request to plane size
         this->cursor->SetPlaneSize( this->cfdMax * 0.5 * 0.01 * 
                         cfdObjects::GetActiveMeshedVolume()->GetLength() );
      }
      this->setId( -1 );
   }

/*
      else  // changed from wand
      {
         if ( this->menuB == true )
         { 
            // If menu is up, remove menu and laser wand and cycle through cursor modes
            if ( this->cursorId == NONE )
            {
               this->cursorId = XPLANE;
               this->rootNode->removeChild( this->menu->GetpfDCS() );
               this->rootNode->removeChild( this->laser->GetpfDCS() );
               this->rootNode->addChild( this->cursor->GetpfDCS() );
            }
            else if ( this->cursorId == XPLANE )
               this->cursorId = YPLANE;
            else if ( this->cursorId == YPLANE )
               this->cursorId = ZPLANE;
            else if ( this->cursorId == ZPLANE )
               this->cursorId = SPHERE;
            else if ( this->cursorId == SPHERE )
               this->cursorId = ARROW;
            else if ( this->cursorId == ARROW )
               this->cursorId = XLINE;
            else if ( this->cursorId == XLINE )
               this->cursorId = YLINE;
            else if ( this->cursorId == YLINE )
               this->cursorId = ZLINE;
            else if ( this->cursorId == ZLINE )
               this->cursorId = CUBE;
            else if ( this->cursorId == CUBE )
            {
               // If on last cursor type, remove menu and reset cursor to off
               this->cursorId = NONE;
               this->menuB = false;
            }
         }
         else
         { 
            // If menu is down, add menu and laser wand, and keep cursor off
            this->rootNode->addChild( this->menu->GetpfDCS() );
            this->rootNode->addChild( this->laser->GetpfDCS() );
            this->menuB = true;
            this->rootNode->removeChild( this->cursor->GetpfDCS() );
         }
      }
*/

   else if ( this->cfdId == BACKWARD_INTEGRATION )
   {
      vprDEBUG(vprDBG_ALL,0) << " BACKWARD_INTEGRATION" 
                             << std::endl << vprDEBUG_FLUSH;

      this->streamlines->SetIntegrationDirection( 2 );
      this->setId( -1 );
   }
   else if ( this->cfdId == FORWARD_INTEGRATION )
   {
      vprDEBUG(vprDBG_ALL,0) << " FORWARD_INTEGRATION"
                             << std::endl << vprDEBUG_FLUSH;

      this->streamlines->SetIntegrationDirection( 1 );
      this->setId( -1 );
   }
   else if ( this->cfdId == TWO_DIRECTION_INTEGRATION )
   {
      vprDEBUG(vprDBG_ALL,0) << " 2_DIRECTION_INTEGRATION" 
                             << std::endl << vprDEBUG_FLUSH;

      this->streamlines->SetIntegrationDirection( 0 );
      this->setId( -1 );
   }
   else if ( this->cfdId == CHANGE_PROPAGATION_TIME )
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_PROPAGATION_TIME\t" 
         << this->cfdIso_value 
         << std::endl << vprDEBUG_FLUSH;
      
      this->streamlines->SetPropagationTime( this->cfdIso_value );
      this->setId( -1 );
   }
   else if ( this->cfdId == CHANGE_INT_STEP_LENGTH )
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_INT_STEP_LENGTH\t"
         << this->cfdIso_value 
         << std::endl << vprDEBUG_FLUSH;
      
      this->streamlines->SetIntegrationStepLength( this->cfdIso_value );
      this->setId( -1 );
   }
   else if ( this->cfdId == CHANGE_STEP_LENGTH )
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_STEP_LENGTH\t" << this->cfdIso_value 
                             << std::endl << vprDEBUG_FLUSH;
         
      this->streamlines->SetStepLength( this->cfdIso_value );
      this->setId( -1 );
   }
   else if ( this->cfdId == RECORD_SCENE )
   {
      // Generate a .pfb filename...
      char pfb_filename[100];
      sprintf( pfb_filename , "%s/stored_scene_%i.pfb",
               this->teacher->getDirectory(), this->pfb_count );

      vprDEBUG(vprDBG_ALL,0) << "scene stored as " << pfb_filename
                             << std::endl << vprDEBUG_FLUSH;

      // store the world DCS matrix..
      pfMatrix m;
      this->worldDCS->getMat( m );

      // temporarily reset the world DCS matrix to the identity
      pfMatrix I;
      I.makeIdent();
      this->worldDCS->setMat( I );

      //biv--convert the cfdSequence nodes to pfSequence nodes
      //for proper viewing in perfly
      writePFBFile(worldDCS,pfb_filename);
      


      // store the active geometry and viz objects as a pfb
      // (but not the sun, menu, laser, or text)
      int store_int = 0;

      vprDEBUG(vprDBG_ALL,1) << "|   Stored Scene Output " << store_int
                             << std::endl << vprDEBUG_FLUSH;
      
      // restore the world DCS matrix...
      this->worldDCS->setMat( m );


      // increment the counter and reset the id to -1...
      this->pfb_count ++;
      this->setId( -1 );
   }
   else if ( this->nav->digital[4]->getData() == gadget::Digital::TOGGLE_ON 
                                          || this->cfdId == CLEAR_ALL )
   { 
      //reset button is triggered so delete all the objects in the scene
      for ( i = 0; i < (int)this->dataList.size(); i++ )  
      {
         if ( this->dataList[ i ]->GetGeode() != NULL )
         {
            vprDEBUG(vprDBG_ALL,2) << "RemoveGeodeFromDCS"
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList[ i ]->RemoveGeodeFromDCS();
         }
         else if ( this->dataList[ i ]->GetpfSequence() != NULL )
         {
            vprDEBUG(vprDBG_ALL,2) << "stop the sequence and set to NULL"
                                   << std::endl << vprDEBUG_FLUSH;
            // stop the sequence and set the active sequence to NULL
            this->dataList[ i ]->StoppfSequence();
            // disconnect transient data from the graph
            this->dataList[ i ]->ClearpfSequence();
            // don't update progress bar any more
            this->activeSequenceObject = NULL;  
         }
         this->dataList[ i ]->SetUpdateFlag( false );
      }

      if ( this->pfb_count != 0 )
      {
         if ( this->teacher->getpfDCS()->getNumChildren() != 0 )
         {
            this->teacher->getpfDCS()->removeChild( 
                                  this->teacher->getpfDCS()->getChild( 0 ) );
         }
      }

      // change all transparent geometries back to opaque
      for( int q = 0; q < this->paramReader->numGeoms; q++)
      {
         if( this->geomL[q]->transparent )
            this->geomL[q]->setOpac( 1.0 );
      }

      // HACK follows: need to do better at adding and removing cfdObjects
      // double check that no garbage is left on the scene graph...
      vprDEBUG(vprDBG_ALL,1) << "after first CLEAR_ALL attempt, ..." 
                             << std::endl << vprDEBUG_FLUSH;

      clearGeodesFromNode( this->worldDCS );

      this->useLastSource = 0;
    
      this->setId( -1 );
   }
   // request to overide default scalar and vector option
   else if ( this->cfdId == SET_TRANSIENT_OPTIONS )
   {
      int objectType = this->cfdIso_value;
      vprDEBUG(vprDBG_ALL,1)
         << " Changing transient object type to: " << objectType
         << std::endl << vprDEBUG_FLUSH;

      // The gui should block out unauthorized access to the button. 
      // But use guards just in case...
      if ( objectType == X_TRANSIENT_CONTOUR ||
           objectType == X_TRANSIENT_CONTOUR_AND_VECTOR )
      {
         if ( this->_cfdTFM_X_Contour[0] ) 
            this->_cfdTFM_X_Contour[0]->SetObjectType( objectType );

         if ( this->_cfdTFM_X_Contour[1] )
            this->_cfdTFM_X_Contour[1]->SetObjectType( objectType );
      }

      if ( objectType == Y_TRANSIENT_CONTOUR ||
           objectType == Y_TRANSIENT_CONTOUR_AND_VECTOR )
      {
         if ( this->_cfdTFM_Y_Contour )
            this->_cfdTFM_Y_Contour->SetObjectType( objectType );
      }

      if ( objectType == Z_TRANSIENT_CONTOUR ||
           objectType == Z_TRANSIENT_CONTOUR_AND_VECTOR )
      {
         if ( this->_cfdTFM_Z_Contour  )
            this->_cfdTFM_Z_Contour->SetObjectType( objectType );
      }

      if ( objectType == X_TRANSIENT_VECTOR ||
           objectType == X_TRANSIENT_CONTOUR_AND_VECTOR )
      {
         if ( this->_cfdTFM_X_Vector ) 
            this->_cfdTFM_X_Vector->SetObjectType( objectType );
      }

      if ( objectType == Y_TRANSIENT_VECTOR ||
           objectType == Y_TRANSIENT_CONTOUR_AND_VECTOR )
      {
         if ( this->_cfdTFM_Y_Vector ) 
            this->_cfdTFM_Y_Vector->SetObjectType( objectType );
      }

      if ( objectType == Z_TRANSIENT_VECTOR ||
           objectType == Z_TRANSIENT_CONTOUR_AND_VECTOR )
      {
         if ( this->_cfdTFM_Z_Vector ) 
            this->_cfdTFM_Z_Vector->SetObjectType( objectType );
      }

      this->setId( -1 );
   }
   else if ( this->cfdId == TRANSIENT_RESET )
   { 
      //delete all transient objects in the scene
      if ( this->activeSequenceObject )
      {
         vprDEBUG(vprDBG_ALL,1) << "Sequence address : " 
           << this->activeSequenceObject->GetpfSequence()
           << std::endl << vprDEBUG_FLUSH;

         // Stop the sequence and set the active sequence to NULL
         // so that the progress bar will not try to update
         this->activeSequenceObject->StoppfSequence();
         this->activeSequenceObject = NULL;  

         for ( i = 0; i < (int)this->dataList.size(); i++ )
         {
            // remove geodes from sequence groups and clear cfdObjects geodes list
            cfdTransientFlowManager * tfmTest =
                   dynamic_cast<cfdTransientFlowManager *>( this->dataList[ i ] );

            if ( tfmTest != NULL )
            {
               if ( tfmTest->GetFrameDataType() != cfdFrame::GEOM )
               {
                  tfmTest->ClearpfSequence();
                  tfmTest->isTimeToUpdateSequence = 1;
               }
            }
         }
      }
      else
      {
         vprDEBUG(vprDBG_ALL,1) << " Don't have an active sequence object" 
                                << std::endl << vprDEBUG_FLUSH;
      }

      this->setId( -1 );
   }
   else if ( this->cfdId == TRANSIENT_STOP )   // request to pause the pfSequence
   {
      if ( this->activeSequenceObject )
      {
         vprDEBUG(vprDBG_ALL,1) << "Sequence address : " 
                   << this->activeSequenceObject->GetpfSequence()
                   << std::endl << vprDEBUG_FLUSH;
                   
         //pausing the transient flow to the graph
         vprDEBUG(vprDBG_ALL,1) << "pausing active sequence" 
                                << std::endl << vprDEBUG_FLUSH;

         this->activeSequenceObject->PausepfSequence();         
         // There is only one pfSequence for all transient 
         // models so we can break after one has been found
      }
      this->setId( -1 );
   }
   else if ( this->cfdId == TRANSIENT_BACKWARD )   // request to step back in the pfSequence
   {
      if ( this->activeSequenceObject )
      {  
         this->activeSequenceObject->ReversepfSequence();
      }
      this->setId( -1 );
   }
   else if ( this->cfdId == TRANSIENT_FORWARD )   // request to step back in the pfSequence
   {
      if ( this->activeSequenceObject )
      {  
         this->activeSequenceObject->ForwardpfSequence();
      }
      this->setId( -1 );
   }
   else if ( this->cfdId == LOAD_PFB_FILE )
   {
      vprDEBUG(vprDBG_ALL,1) << "LOAD_PFB_FILE: numChildren = " 
         << this->teacher->getpfDCS()->getNumChildren()
         << ", cfdTeacher_state = " << this->cfdTeacher_state
         << std::endl << vprDEBUG_FLUSH;

      if ( this->teacher->getpfDCS()->getNumChildren() == 0 )
      {
         vprDEBUG(vprDBG_ALL,2) << "LOAD_PFB_FILE: addChild" 
                                << std::endl << vprDEBUG_FLUSH;
            
         this->teacher->getpfDCS()->addChild( 
            this->teacher->getpfNode( this->cfdTeacher_state ) );
      }
      else
      {
         vprDEBUG(vprDBG_ALL,2) << "LOAD_PFB_FILE: replaceChild" 
                                << std::endl << vprDEBUG_FLUSH;

         this->teacher->getpfDCS()->replaceChild( 
            this->teacher->getpfDCS()->getChild( 0 ), 
            this->teacher->getpfNode( this->cfdTeacher_state ) );
      }
      this->setId( -1 );
   }
   else if ( this->cfdId == CLEAR_PFB_FILE )
   {      
      if ( this->teacher->getpfDCS()->getNumChildren() > 0 )
      {
         this->teacher->getpfDCS()->removeChild( 
                                this->teacher->getpfDCS()->getChild( 0 ) );
      }
      this->setId( -1 );
   }
   else if ( this->cfdId == CHANGE_STEADYSTATE_DATASET )
   {
      vprDEBUG(vprDBG_ALL,1) 
         << "CHANGE_STEADYSTATE_DATASET " << this->cfdIso_value
         //<< ", scalarIndex = " << this->cfdSc
         //<< ", min = " << this->cfdMin 
         //<< ", max = " << this->cfdMax
         << std::endl << vprDEBUG_FLUSH;

      i = this->cfdIso_value;
      if ( i < (int)this->paramReader->GetNumberOfDataSets() )
      {
         vprDEBUG(vprDBG_ALL,0) << " dataset = "
            << this->paramReader->GetDataSet( i )->GetFileName()
            << ", dcs = " << this->paramReader->GetDataSet( i )->GetDCS()
            << std::endl << vprDEBUG_FLUSH;

         int cfdType = this->paramReader->GetDataSet( i )->GetType();
         vprDEBUG(vprDBG_ALL,1) << " cfdType: " << cfdType
                                << std::endl << vprDEBUG_FLUSH;

         // set the dataset as the appropriate dastaset type
         // (and the active dataset as well)
         if ( cfdType == 0 )
         {
            cfdObjects::SetActiveMeshedVolume( this->paramReader->GetDataSet(i) );
         }
         else if ( cfdType == 1 )
         {
            cfdObjects::SetActiveParticleData( this->paramReader->GetDataSet(i) );
         }
         else if ( cfdType == 2 )
         {
            cfdObjects::SetActiveSurfaceData( this->paramReader->GetDataSet(i) );
         }
         else
         {
            std::cerr << "Unsupported cfdType: " << cfdType << std::endl;
         }

         vprDEBUG(vprDBG_ALL,1) << "last active dataset name = " 
                                << oldDatasetName
                                << std::endl << vprDEBUG_FLUSH;

         vprDEBUG(vprDBG_ALL,1) << "Activating steady state file " 
                   << cfdObjects::GetActiveDataSet()->GetFileName()
                   << std::endl << vprDEBUG_FLUSH;

         // make sure that the user did not just hit same dataset button
         // (or change scalar since that is routed through here too)
         if ( strcmp( oldDatasetName, 
                      cfdObjects::GetActiveDataSet()->GetFileName() ) )
         {
            vprDEBUG(vprDBG_ALL,1) << " setting dataset as newly activated" 
                                   << std::endl << vprDEBUG_FLUSH;
            cfdObjects::GetActiveDataSet()->SetNewlyActivated();
            strcpy( oldDatasetName, 
                    cfdObjects::GetActiveDataSet()->GetFileName() );
         }

         // update scalar bar for possible new scalar name
         this->setId( CHANGE_SCALAR );
      }
      else
      {
         std::cerr << "ERROR: requested steady state dataset " 
                   << this->cfdIso_value << " must be less than " 
                   << this->paramReader->GetNumberOfDataSets()
                   << std::endl;
         this->setId( -1 );
      }
   }
   else if ( this->cfdId == CHANGE_SCALAR || 
             this->cfdId == CHANGE_SCALAR_RANGE )
   { 
      int scalarIndex = this->cfdSc;
      vprDEBUG(vprDBG_ALL,1) << "CHANGE_SCALAR || CHANGE_SCALAR_RANGE"
         << ", scalarIndex = " << scalarIndex
         << ", min = " << this->cfdMin 
         << ", max = " << this->cfdMax
         << std::endl << vprDEBUG_FLUSH;

      cfdObjects::GetActiveDataSet()->SetActiveScalar( scalarIndex );
      cfdObjects::GetActiveDataSet()->GetParent()
                                    ->SetActiveScalar( scalarIndex );

      cfdObjects::GetActiveDataSet()->ResetScalarBarRange( 
                           this->cfdMin, this->cfdMax );
      cfdObjects::GetActiveDataSet()->GetParent()->ResetScalarBarRange( 
                           this->cfdMin, this->cfdMax );

      // if already displayed, set a flag to update the scalar bar
      if ( this->scalarBarActor )
      {
         this->isTimeToUpdateScalarBar = true;
      }

      // when scalar is changed reset vector thresholding to none...
      if ( this->cfdId == CHANGE_SCALAR  )
      {
         cfdVectorBase::UpdateThreshHoldValues();
      }
#ifdef _TAO
      if ( this->cfdId == CHANGE_SCALAR )
      {
         this->executive->SetCalculationsFlag( true );
      }
#endif
      this->setId( -1 );
   }
   else if ( this->cfdId == CHANGE_VECTOR )
   { 
      int vectorIndex = this->cfdSc;
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_VECTOR, vectorIndex = " << vectorIndex
                             << std::endl << vprDEBUG_FLUSH;

      cfdObjects::GetActiveDataSet()->SetActiveVector( vectorIndex );
      cfdObjects::GetActiveDataSet()->GetParent()
                                    ->SetActiveVector( vectorIndex );

      this->setId( -1 );
   }
   else if ( this->cfdId == CHANGE_VECTOR_THRESHOLD )
   { 
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_VECTOR_THRESHOLD, min = " 
                << this->cfdMin << ", max = " << this->cfdMax
                << std::endl << vprDEBUG_FLUSH;

      cfdVectorBase::SetThreshHoldPercentages( this->cfdMin, this->cfdMax );
      cfdVectorBase::UpdateThreshHoldValues();

      this->setId( -1 );
   }
   else if ( this->cfdId == CHANGE_VECTOR_MASK_RATIO )
   { 
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_VECTOR_MASK_RATIO, value = " 
            << this->cfdIso_value << std::endl << vprDEBUG_FLUSH;

      cfdVectorBase::SetVectorRatioFactor( this->cfdIso_value );

      this->setId( -1 );
   }
   else if ( this->cfdId == CHANGE_VECTOR_SCALE )
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_VECTOR_SCALE, value = " 
         << this->cfdIso_value << std::endl << vprDEBUG_FLUSH;

      cfdObjects::SetVectorScale( this->cfdIso_value );

      this->setId( -1 );
   }
   else if ( this->cfdId == CHANGE_PARTICLE_VIEW_OPTION)
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_PARTICLE_VIEW_OPTION, value = " 
         << this->cfdGeo_state << std::endl << vprDEBUG_FLUSH;

      // if view option is set to point cloud, set sphere scale out of range
      if ( this->cfdGeo_state == 0 )
      {
         vprDEBUG(vprDBG_ALL,0) << " setting sphere scale out of range" 
                                << std::endl << vprDEBUG_FLUSH;
         cfdObjects::SetSphereScale( -999 );
      }
      else if ( this->cfdGeo_state == 1 )
      {
         vprDEBUG(vprDBG_ALL,0) << " setting sphere scale to " 
            << this->cfdGeo_state << std::endl << vprDEBUG_FLUSH;
         cfdObjects::SetSphereScale( this->cfdGeo_state );
      }

      this->setId( -1 );
   }
   else if ( this->cfdId == CHANGE_SPHERE_SIZE )
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_SPHERE_SIZE, value = " 
         << this->cfdIso_value 
         << std::endl << vprDEBUG_FLUSH;

      cfdObjects::SetSphereScale( this->cfdIso_value );

      this->setId( -1 );
   }
   else if ( this->cfdId == SCALE_BY_VECTOR_MAGNITUDE )
   { 
      vprDEBUG(vprDBG_ALL,0)
         << "SCALE_BY_VECTOR_MAGNITUDE = " << this->cfdIso_value
         << std::endl << vprDEBUG_FLUSH;

      cfdVectorBase::SetScaleByVectorFlag( this->cfdIso_value );

      this->setId( -1 );
   }
   else if ( this->cfdId == UPDATE_GEOMETRY )
   {
      vprDEBUG(vprDBG_ALL,1)
         << this->cfdGeo_state << std::endl << vprDEBUG_FLUSH;

      long int test = this->paramReader->convertDecimalToBinary( this->cfdGeo_state );
      vprDEBUG(vprDBG_ALL,1)
         << " test : " << test << std::endl << vprDEBUG_FLUSH;

      this->paramReader->convertBinaryToArray( test, this->paramReader->numGeoms );
      this->changeGeometry = true;
      this->setId( -1 );
   }
   else if ( this->cfdId == CHANGE_CONTOUR_FILL )
   {
      vprDEBUG(vprDBG_ALL,0) << "CHANGE_CONTOUR_FILL to type " 
                             << this->cfdIso_value
                             << std::endl << vprDEBUG_FLUSH;

      cfdContourBase::SetFillType( this->cfdIso_value );
      this->setId( -1 );
   }
   else if ( this->cfdId == UPDATE_SOUNDS )
   {
      long int test = this->paramReader->convertDecimalToBinary( this->cfdIso_value );
      vprDEBUG(vprDBG_ALL,1)
         << " test : " << test << " : Number of Sound Files :" 
         << this->paramReader->soundFile << vprDEBUG_FLUSH;

      this->paramReader->convertBinaryToArray( test, this->paramReader->soundFile );

      for(int i = 0; i < this->paramReader->soundFile; i++)
      {
         if ( this->sounds[i]->IsSounding() && paramReader->guiVal[ i ] == 0 ) 
         {
            this->sounds[i]->stopSound();  //Stop sound stuff here
         }              
         else
         {
            this->sounds[i]->playSound();   //Start sound again
         } 
      }      

      this->setId( -1 );
   }
   else if ( this->cfdId == LOAD_POINT )
   {
      this->quatcamHandler->LoadData( this->nav->worldTrans, worldDCS );
      this->setId( -1 );
   }
   else if ( this->cfdId == WRITE_POINTS_TO_FILE )
   {
      this->quatcamHandler->WriteToFile( this->paramReader->quatCamFileName ); 
      this->setId( -1 ); 
   }
   else if ( this->cfdId == READ_POINTS_FROM_FILE )
   {
      this->quatcamHandler->LoadFromFile( this->paramReader->quatCamFileName );
      this->setId( -1 );
   }
   else if ( this->cfdId == MOVE_TO_SELECTED_LOCATION )
   {
      this->quatcamHandler->Relocate(cfdId, worldDCS, cfdIso_value, nav);
      this->setId( this->quatcamHandler->run );
   }
   else if ( this->cfdId == UPDATE_SEND_PARAM )
   {
      double data[ 6 ];// = { 0 };
      data[ 0 ] = cfdShort_data_array[ 1 ]; //200;  //Agitation (rpm)  initial value 200
      data[ 1 ] = cfdShort_data_array[ 2 ]; //1.25; //Air Concentration initial value 1.25;
      data[ 2 ] = cfdShort_data_array[ 3 ]; //6;    //Initial pH value    initial value 6
      data[ 3 ] = cfdShort_data_array[ 4 ]; //0.1;  //Nitrate Concentration     initial value 0.1
      data[ 4 ] = cfdShort_data_array[ 5 ]; //37;   //Temperate (Celsius)        initial value 37
      data[ 5 ] = cfdShort_data_array[ 6 ]; //240;  //Simulate [a text box] Hours in 10 seconds, initial value 240

      this->ihccModel->UpdateModelVariables( data );
      this->ihccModel->Update();
      this->setId( -1 );
   }
   else if ( this->cfdId == EXIT )   // exit cfdApp was selected
   {
#ifdef _TAO
      this->executive->UnbindORB();
#endif
      this->runStreamersThread = false;
      this->runIntraParallelThread = false;   
      this->mKernel->stop(); // Stopping kernel using the inherited member variable
   }
   else if ( 0 <= this->cfdId && this->cfdId < 100 )
   {
      vprDEBUG(vprDBG_ALL,1) << " selected ID number = " << this->cfdId
                             << std::endl << vprDEBUG_FLUSH;
      for ( int i = 0; i < (int)this->dataList.size(); i ++ )
      {
         if ( this->cfdId == dataList[ i ]->GetObjectType() )
         {
            // verify that if a transient sequence is desired, 
            // an appropriate DCS is active...
            if ( ( X_TRANSIENT_CONTOUR <= this->cfdId && this->cfdId <= PARTICLE_TRANSIENT ) &&
                 ! cfdObjects::GetActiveDataSet()->IsPartOfTransientSeries() )
            {
               std::cerr << "\nERROR: You must activate an appropriate transient "
                         << "dataset before proceeding\n" << std::endl;
               this->activeObject = NULL;
               this->computeActorsAndGeodes = false;
               this->setId( -1 );
               break;
            }

            vprDEBUG(vprDBG_ALL,1) << " setting viz object " << i 
                                   << " to activeObject"
                                   << std::endl << vprDEBUG_FLUSH;

            this->activeObject = this->dataList[ i ];

            if ( this->activeObject->GetObjectType() == IMAGE_EX )
            {
               this->activeDataSetDCS = worldDCS;
            }
            else
            {
               this->activeDataSetDCS = cfdObjects::GetActiveDataSet()->GetDCS();               
            }

            // add active dataset DCS to scene graph if not already there...
            vprDEBUG(vprDBG_ALL,1) << " setting DCS to activeDCS = "
                                   << this->activeDataSetDCS
                                   << std::endl << vprDEBUG_FLUSH;
            this->activeObject->SetDCS( this->activeDataSetDCS );

            this->activeObject->SetNormal( this->nav->GetDirection() );
            this->activeObject->SetOrigin( this->nav->GetObjLocation() );

            this->activeObject->SetRequestedValue( this->cfdIso_value );
            this->activeObject->SetCursorType( this->cursorId );
            this->activeObject->SetPreCalcFlag( this->cfdPre_state );

            this->setId( -1 );
            this->computeActorsAndGeodes = true;
            break;
         }
      }
   }

   // Update Scalar Bar on the scene graph
   // PLEASE NOTE !!!!!!!!!!!!!!!!
   // Only update the scene graph in pre or post frame 
   // not in intraParallelThread or in intraFrame
   if ( this->isTimeToUpdateScalarBar ) 
   {
      RefreshScalarBar();
      this->isTimeToUpdateScalarBar = false;
   }   
   else if ( this->changeGeometry ) 
   {
      // Update Scene Graph with selected or deselected geometry
      vprDEBUG(vprDBG_ALL,0) << " Change Geometry in Scene Graph."
                             << std::endl << vprDEBUG_FLUSH;

      vprDEBUG(vprDBG_ALL,1) << " numGeoms = " << this->paramReader->numGeoms 
                              << std::endl << vprDEBUG_FLUSH;
      int temp, q;
      temp = 0;

      for( q = 0; q < this->paramReader->numGeoms; q++)
      {
         vprDEBUG(vprDBG_ALL,1)
            << "guiVal[ " << q << " ] = " << paramReader->guiVal[ q ] 
            << std::endl
            << vprDEBUG_FLUSH;

         if ( paramReader->guiVal[ q ] == 1 && 
               this->worldDCS->searchChild( this->geomL[q]->getpfDCS() ) == -1 )
         {
            temp = this->worldDCS->addChild( this->geomL[q]->getpfDCS() );
         }
         else if ( paramReader->guiVal[ q ] == 0 && 
               this->worldDCS->searchChild( this->geomL[q]->getpfDCS() ) != -1 )
         {
            temp = this->worldDCS->removeChild( (pfNode*)this->geomL[q]->getpfDCS() );
         }
         vprDEBUG(vprDBG_ALL,1) << "|   Add Child Error  " << temp 
                              << std::endl << vprDEBUG_FLUSH;
      }
      this->changeGeometry = false;
   }
   else if ( this->chgMod == true)
   {
      // for the active model, change opaque geometries to transparent
      for ( int q = 0; q < this->paramReader->numGeoms; q++)
      {
         if ( this->geomL[q]->transparent == 1 )
         {
            vprDEBUG(vprDBG_ALL,2) << "Changing Transparency for geom : "
                                   << q << std::endl << vprDEBUG_FLUSH;
            this->geomL[q]->setOpac( 0.2 );
         }
      }
      this->chgMod = false;
   }  //change the model's property

   // check any virtual objects need to be updated
   if ( cfdObjects::GetTimeToUpdateFlag() )
   {
      for ( int i = 0; i < (int)this->dataList.size(); i++ )
      {
         if ( this->dataList[ i ]->GetGeodeFlag() )
         {
            vprDEBUG(vprDBG_ALL,2) << " have geode flag"
                                   << std::endl << vprDEBUG_FLUSH;

            if ( this->worldDCS->searchChild( this->dataList[ i ]->GetDCS() ) < 0 )
            {
               vprDEBUG(vprDBG_ALL,1) << " adding active DCS to worldDCS"
                                   << std::endl << vprDEBUG_FLUSH;
               this->worldDCS->addChild( this->dataList[ i ]->GetDCS() );
            }

            if ( this->dataList[ i ]->GetGeode() != NULL )
            {
               vprDEBUG(vprDBG_ALL,1) << " will add geode to sg"
                                      << std::endl << vprDEBUG_FLUSH;
               // Add steady state geodes to the scene graph
               this->dataList[ i ]->AddGeodeToDCS();
            }
            else if ( this->dataList[ i ]->GetpfSequence() != NULL )
            {
               vprDEBUG(vprDBG_ALL,1) << " will add viz object "
                                      << i << " to sequence"
                                      << std::endl << vprDEBUG_FLUSH;

               //add the transient flow to the graph
               // Hack for now
               cfdTransientFlowManager * tfmTest = 
                  dynamic_cast<cfdTransientFlowManager *>( this->dataList[ i ] );
               
               //cfdAnimatedImage* animTest = 
               //   dynamic_cast< cfdAnimatedImage* >( this->dataList[ i ] );

               if ( tfmTest != NULL ) //&&
               //     tfmTest->GetFrameDataType() != cfdFrame::GEOM )
               {
                  tfmTest->CreateNodeList();

                  this->dataList[ i ]->AddTopfSequence();
            
                  if ( this->activeSequenceObject )
                  {
                     vprDEBUG(vprDBG_ALL,1) << " ResumepfSequence"
                                            << std::endl << vprDEBUG_FLUSH;
                     this->dataList[ i ]->ResumepfSequence();
                  }
                  else
                  {
                     vprDEBUG(vprDBG_ALL,1) << " StartpfSequence"
                                            << std::endl << vprDEBUG_FLUSH;
                     this->dataList[ i ]->StartpfSequence();
                  }

                  // Set active sequence to current sequence
                  this->activeSequenceObject = this->dataList[ i ];
               }
               else //if ( animTest != NULL )
               {
                  this->dataList[ i ]->AddTopfSequence();
               }
            }
            vprDEBUG(vprDBG_ALL,2) << " End Update Loop"
                                   << std::endl << vprDEBUG_FLUSH;

            // Resetting these variables is very important
            this->dataList[ i ]->SetUpdateFlag( false );
            this->dataList[ i ]->SetGeodeFlag( false );
         }
      }
      cfdObjects::SetTimeToUpdateFlag( false );
   }

   // if transient data is being displayed, then update gui progress bar
   if (  this->activeSequenceObject )
   {
      int currentFrame = this->activeSequenceObject->GetFrameOfpfSequence();
      if (this->lastFrame != currentFrame )
      {
#ifdef TABLET 
         this->setTimesteps( currentFrame );
#endif
         this->lastFrame = currentFrame;
      }
   }

#ifdef _TAO
   if ( cfdObjects::GetActiveDataSet() != NULL )
   {
      this->executive->SetActiveDataSet( cfdObjects::GetActiveDataSet() );
   }
   this->executive->UpdateModules();
#endif
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
#ifdef TABLET
   this->GetCfdStateVariables();
#endif
   vprDEBUG(vprDBG_ALL,3) << " End postFrame" << std::endl << vprDEBUG_FLUSH;
}

#ifdef TABLET
void cfdApp::pushDataToStateInfo( void )
{
   this->setIsoValue( this->cfdIso_value );   
   this->setSc( this->cfdSc );                    
   this->setMin( this->cfdMin );
   this->setMax( this->cfdMax );                   
   this->setId( this->cfdId );                    
   this->setGeoState( this->cfdGeo_state );
   this->setPostdataState( this->cfdPostdata_state );        
   this->setPreState( this->cfdPre_state );             
   this->setTimesteps( this->cfdTimesteps );
   this->setTeacherState( this->cfdTeacher_state );         
}
#endif

void cfdApp::streamers( void * )
{
   vprDEBUG(vprDBG_ALL,1) << "In streamers" << std::endl << vprDEBUG_FLUSH;
   while ( this->runStreamersThread )
   { 
      // Wait for some  work
     while (   !this->interactiveObject && 
	            !this->animatedStreamlines &&
	            !this->animatedImages)
      {
         vpr::System::msleep( 500 );  // half-second delay
      }

      if ( this->interactiveObject )
      {
         this->activeObject->SetCursorType( this->cursorId );
         this->activeObject->SetNormal( this->nav->GetDirection() );
         this->activeObject->SetOrigin( this->nav->GetObjLocation() );

         if ( this->cursorId == CUBE )
         {
            this->activeObject->SetBoxSize( this->cur_box );
         }

         if ( ! this->useLastSource )
         {
            vprDEBUG(vprDBG_ALL,1) <<"creating fresh streamlines"
                                   << std::endl << vprDEBUG_FLUSH;
            if ( this->lastSource )
            {
               this->lastSource->Delete();
            }

            this->lastSource = vtkPolyData::New();

            this->lastSource->DeepCopy( 
               (vtkPolyData*)this->cursor->GetSourcePoints( this->cursorId ) );

            this->activeObject->SetSourcePoints( 
                                        (vtkPolyDataSource*)this->lastSource );
         }
         else //if ( cfdObjects::GetActiveMeshedVolume()->IsNewlyActivated() )// && this->useLastSource
         {
            vprDEBUG(vprDBG_ALL,1) << "using transformed last source"
                                   << std::endl << vprDEBUG_FLUSH;

            this->activeObject->SetSourcePoints( 
                                        (vtkPolyDataSource*)this->lastSource );
         }

         this->activeObject->Update();
         this->activeObject->UpdateGeode();
         
         this->interactiveObject = false;
         this->activeObject = NULL;
      }

      if ( this->animatedStreamlines )
      {                  
         this->animStreamer->SetPolyDataSource( this->streamlines->GetStreamersOutput() );
         this->animStreamer->Update();
         this->animatedStreamlines = false;
      }
      
      if (this->animatedImages)
	   {
	      this->animImg->Update();
	      this->animatedImages = false;
	   }
      cfdObjects::SetTimeToUpdateFlag( true );
   }
}


void cfdApp::intraParallelThread( void * )
{
   // DO NOT put scene graph manipulation code in this function
   // This thread is purely for creation of geodes
   int i;

   while ( this->runIntraParallelThread )
   {
      vpr::System::msleep( 500 );  // half-second delay

      // Basically waiting for work here
      // This is a guard 
      // Sample every half second
      if ( this->computeActorsAndGeodes )
      {      
         if ( this->activeObject != NULL )
         {
            cfdContour * contourTest = 
               dynamic_cast<cfdContour *>( this->activeObject );
            cfdVector * vectorTest = 
               dynamic_cast<cfdVector *>( this->activeObject );
            cfdMomentum * momentumTest = 
               dynamic_cast<cfdMomentum *>( this->activeObject );
            cfdStreamers * streamersTest = 
               dynamic_cast<cfdStreamers *>( this->activeObject );
            cfdAnimatedStreamlineCone * animStreamerTest = 
               dynamic_cast<cfdAnimatedStreamlineCone *>( this->activeObject );
	         cfdAnimatedImage* animImgTest = 
               dynamic_cast<cfdAnimatedImage *>( this->activeObject );
            cfdTransientFlowManager * tfmTest = 
               dynamic_cast<cfdTransientFlowManager *>( this->activeObject );

            vprDEBUG(vprDBG_ALL,0) << " Updating cfdObject..." 
               << std::endl << vprDEBUG_FLUSH;

            this->chgMod = true;

            vprDEBUG(vprDBG_ALL,2) << " Memory used before update ( bytes ) : "
              << pfMemory::getArenaBytesUsed() << std::endl << vprDEBUG_FLUSH;

            tt = GetTimeClock();
            if (  contourTest == NULL && 
                  vectorTest == NULL &&
                  momentumTest == NULL &&   
                  streamersTest == NULL &&
                  animStreamerTest == NULL &&
		            animImgTest == NULL &&
                  tfmTest == NULL )
            {
               // For everything except for the interactive and transient stuff
               vprDEBUG(vprDBG_ALL,1)
                 << "non-interactive object." << std::endl << vprDEBUG_FLUSH; 

               this->activeObject->Update(); 
               this->activeObject->UpdateGeode();     
               this->activeObject = NULL;
            }
            else if ( tfmTest != NULL )
            {
               vprDEBUG(vprDBG_ALL,1)
                 << "transient object." << std::endl << vprDEBUG_FLUSH;

               vprDEBUG(vprDBG_ALL,1) << "activeObject->GetObjectType() = "
                                      << this->activeObject->GetObjectType()
                                      << std::endl << vprDEBUG_FLUSH;

               for ( i = 0; i < (int)this->dataList.size(); i++ )
               {
                  // Transient Vector, Contour, and/or Particle Update
                  if ( this->activeObject->GetObjectType() == 
                       this->dataList[ i ]->GetObjectType() )
                  {
                     this->dataList[ i ]->SetDCS( this->activeDataSetDCS );
                     vprDEBUG(vprDBG_ALL,1) << "Trans Data : Object : " << i  
                                            << std::endl << vprDEBUG_FLUSH;

                     this->dataList[ i ]->Update();
                  }
                  // Transient Geometry Update
                  else if ( this->dataList[ i ]->GetObjectType() == TRANS_GEOM )
                  {
                     this->dataList[ i ]->SetDCS( this->activeDataSetDCS );
                     vprDEBUG(vprDBG_ALL,1) << "Trans Geom : Object : " << i
                                            << std::endl << vprDEBUG_FLUSH;

                     this->dataList[ i ]->Update();
                  }
               }
               this->activeObject = NULL;
            }
            else if ( streamersTest != NULL )
            {
               vprDEBUG(vprDBG_ALL,1) << "interactive object." 
                                       << std::endl << vprDEBUG_FLUSH;

               this->interactiveObject = true;
            }
            else if ( animStreamerTest != NULL )
            {
               this->animatedStreamlines = true;
               this->activeObject = NULL;
            }
            else if ( animImgTest != NULL )
            {
               this->animatedImages = true;
               this->activeObject = NULL;
            }

            vprDEBUG(vprDBG_ALL,1) << " Time: " << GetTimeClock()-tt
                                   << std::endl << vprDEBUG_FLUSH;
            vprDEBUG(vprDBG_ALL,2) <<" Memory used after update ( bytes ) : "
                                   << pfMemory::getArenaBytesUsed() 
                                   << std::endl << vprDEBUG_FLUSH;

            vprDEBUG(vprDBG_ALL,0) << " Done updating cfdObject" 
               << std::endl << std::endl << vprDEBUG_FLUSH; 
            
         }
         this->computeActorsAndGeodes = false;
         cfdObjects::SetTimeToUpdateFlag( true );
      }
   } // End of While loop
}
void cfdApp::writePFBFile(pfNode* graph,char* fileName)
{
   //make sure we have a writer
   if(!_cfdWT){
      _cfdWT = new cfdWriteTraverser(fileName);
   }else{
      _cfdWT->setOutputFileName(fileName);
   }
   //set the graph
   _cfdWT->setNode(graph);

   //write out the file
   _cfdWT->writePfbFile();

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
#endif

#ifdef TABLET
   int temp = 0;
   char** xargv;
   xargv = new char*[ temp ];
   //xargv[ 0 ] = "-ORBInitRef";
   //xargv[ 1 ] = "NameService=corbaname::cruncher.vrac.iastate.edu:2809";
   //the above line doesn't work when running from costello!!!
   //biv -- checking if the name server has been moved
   //xargv[ 1 ] = "NameService=corbaname::cruncher.vrac.iastate.edu:2809";

#ifdef _TAO
   //xargv[ 0 ] = "-ORBInitRef";
   //xargv[ 1 ] = "NameService=file:///tmp/ns.ior";
   CORBA::ORB_var orb=CORBA::ORB_init( temp, xargv,"" );
#else
	CORBA::ORB_var orb=CORBA::ORB_init( temp, xargv );
	if ( CORBA::is_nil( orb.in() ) )
		exit(0);
#endif
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
//	CORBA::String_var sior2(orb->object_to_string( poa.in() ) );
//	cout << "|  IOR of the server side 2 : " << endl << sior2 << endl;
#endif

	   vrj::Kernel* kernel = vrj::Kernel::instance(); // Declare a new Kernel

   cfdApp* application = new cfdApp( );//kernel );  // Delcare an instance of my application

 #ifdef TABLET

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
#else //the _CLUSTER
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
   catch(CosNaming::NamingContext::AlreadyBound&)
     {
       naming_context->rebind(name, vjobs.in());
     }
#endif //the _CLUSTER
   application->SetCORBAVariables( naming_context.in(), orb.in(), poa.in() );

#endif //the TABLET

	
   for ( int i = 1; i < argc; i++ )          // Configure the kernel
     {
       kernel->loadConfigFile( argv[i] );   
	 }
   
   kernel->start();                          // Start the kernel thread

   kernel->setApplication( application );    // Give application to kernel
   
#ifdef _TAO
   // If this isn't here the app won't work with TAO 
   orb->run();
#endif
   kernel->waitForKernelStop();              // Block until kernel stops

   return 0;
}

void cfdApp::RefreshScalarBar()
{
   if ( this->scalarBarActor )
   {
      this->rootNode->removeChild( this->scalarBarActor->getpfDCS() );
      //this->worldDCS->removeChild( this->scalarBarActor->getpfDCS() );
      delete this->scalarBarActor;
      this->scalarBarActor = NULL;
   }

   if ( cfdObjects::GetActiveDataSet() == NULL ||
        cfdObjects::GetActiveDataSet()->GetNumberOfScalars() == 0 )
   {
      vprDEBUG(vprDBG_ALL,0) << " RefreshScalarBar: no data" 
                             << std::endl << vprDEBUG_FLUSH;
      return;
   }

   this->scalarBarActor = new cfdScalarBarActor();

   // if the param file specified scalarBar settings, apply them here...
   if ( this->paramReader->scalarBarH != 0.0 )
   {
      this->scalarBarActor->SetPosition( this->paramReader->scalarBarPos );
      this->scalarBarActor->SetZRotation( this->paramReader->scalarBarZRot );
      this->scalarBarActor->SetHeight( this->paramReader->scalarBarH );
      this->scalarBarActor->SetWidth( this->paramReader->scalarBarW );
      this->scalarBarActor->SetTitleTextScale( 
                                     this->paramReader->scalarBarH / 15.0 );
   }

   this->scalarBarActor->SetRange( 
                  cfdObjects::GetActiveDataSet()->GetDisplayedScalarRange() );

   this->scalarBarActor->SetLookupTable( 
                  cfdObjects::GetActiveDataSet()->GetLookupTable() );

   vprDEBUG(vprDBG_ALL,1) << " RefreshScalarBar: " 
      << "cfdObjects::GetActiveDataSet()->GetLookupTable() = "
      << cfdObjects::GetActiveDataSet()->GetLookupTable()
      << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL,1) << "RefreshScalarBar: " 
      << "cfdObjects::GetActiveDataSet()->GetParent()->GetLookupTable() = "
      << cfdObjects::GetActiveDataSet()->GetParent()->GetLookupTable()
      << std::endl << vprDEBUG_FLUSH;

   // give a name to display over the scalarBar
   static char legend[50];
   strcpy( legend, cfdObjects::GetActiveDataSet()->GetDataSet()
                       ->GetPointData()->GetScalars()->GetName() );

   vprDEBUG(vprDBG_ALL,1) << "RefreshScalarBar: " 
                          << "desired scalar bar name: " << legend 
                          << std::endl << vprDEBUG_FLUSH;

   this->scalarBarActor->SetVtkVectorText( legend );

   this->scalarBarActor->Execute();

   // give the scalarBar DCS a name so that it can be detected during a CLEAR_ALL
   this->scalarBarActor->getpfDCS()->setName("scalarBar");
   this->rootNode->addChild( this->scalarBarActor->getpfDCS() );
   //this->worldDCS->addChild( this->scalarBarActor->getpfDCS() );
}


#ifdef _CLUSTER
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
#endif


