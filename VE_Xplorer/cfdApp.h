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
 * File:          $RCSfile: cfdApp.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_APP_H
#define CFD_APP_H

/// VR Juggler Stuff
#include <vrj/Kernel/Kernel.h>
#include <vrj/Draw/Pf/PfApp.h>    /* the performer application base type */

/// C/C++ libraries
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
//#include <omp.h>
#ifndef WIN32
#include <sys/time.h>
#endif
#include <vector>

#ifdef _TAO
class cfdExecutive;
#endif

class cfdPfSceneManagement;
class cfdEnvironmentHandler;
class cfdSteadyStateVizHandler;
class cfdTransientVizHandler;
class cfdModelHandler;
class cfdIHCCModel;
class CorbaManager;

// Scene graph dependent forward declarations
class pfGroup;

// The sleep time for sampling of threads.
const float SAMPLE_TIME = 1.0f;

// Declare my application class
class cfdApp : public vrj::PfApp
{
   public:
      //cfdApp( vrj::Kernel* kern);
      cfdApp( CorbaManager* );//vrj::Kernel* kern );

      virtual void init( );

      virtual void apiInit( );

      // Called After pfInit()
      virtual void preForkInit( );
      // Called Before pfConfig()

      // Initialize the scene graph
      virtual void initScene( );

      // Return the current scene graph
      virtual pfGroup* getScene( );

      // Function called by the DEFAULT drawChan function 
      // before clearing the channel
      // and drawing the next frame (pfFrame( ))
      //virtual void preDrawChan(pfChannel* chan, void* chandata);

      // Function called before pfSync
      virtual void preSync( );

      // Function called after pfSync and before pfDraw
      virtual void preFrame( );

      // Function called after pfDraw
      virtual void intraFrame( );

      // Function called after intraFrame
      virtual void postFrame();

      // Performer calls before exiting
      virtual void exit( void );

      // Used to override getFrameBufferAttrs()
      // Should be able to set multi sampling in the config
      // Look for a fix in future juggler releases
      //std::vector< int > getFrameBufferAttrs( void );

      void pushDataToStateInfo( void );

      //void SetCORBAVariables( CosNaming::NamingContext_ptr, CORBA::ORB_ptr, PortableServer::POA_ptr );

      cfdPfSceneManagement*      _sceneManager;
      cfdEnvironmentHandler*     _environmentHandler;
      cfdSteadyStateVizHandler*  _steadystateHandler;
      cfdTransientVizHandler*    _transientHandler;
      cfdModelHandler*           _modelHandler;
      cfdIHCCModel               *ihccModel;
      CorbaManager*              _corbaManager;
#ifdef _TAO
      cfdExecutive*     executive;
#endif

      //CosNaming::NamingContext_var naming_context;
      //CORBA::ORB_var orb;
      //PortableServer::POA_var poa;

   
      //biv -- transient stuff
      /*cfdTransientFlowManager* _cfdTFM_X_Contour[2];//added for windshield hack
      cfdTransientFlowManager* _cfdTFM_Y_Contour;
      cfdTransientFlowManager* _cfdTFM_Z_Contour;
      cfdTransientFlowManager* _cfdTFM_X_Vector;
      cfdTransientFlowManager* _cfdTFM_Y_Vector;
      cfdTransientFlowManager* _cfdTFM_Z_Vector;
      cfdTransientFlowManager* _cfdTFM_Geometry[2];
      cfdTransientFlowManager* _cfdTFM_Particle;*/

   // Only used in preframe for transient stuff
   int   lastFrame;

  
#ifdef _CLUSTER   
   virtual void GetUpdateClusterStateVariables( void );
#endif
   private:
      char * filein_name;
};

#endif
