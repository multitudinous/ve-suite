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

#ifndef WIN32
//#include <sys/time.h>
#endif
//#include <vector>

#ifdef _TAO
class cfdExecutive;
#endif

class cfdPfSceneManagement;
class cfdEnvironmentHandler;
class cfdSteadyStateVizHandler;
class cfdTransientVizHandler;
class cfdModelHandler;
class cfdIHCCModel;
//class CorbaManager;
class cfdVjObsWrapper;
// Scene graph dependent forward declarations


// The sleep time for sampling of threads.
const float SAMPLE_TIME = 1.0f;

#ifdef _PERFORMER
class pfGroup;
#include <vrj/Draw/Pf/PfApp.h>    /* the performer application base type */
// Declare my application class
class cfdApp : public vrj::PfApp
#elif _OSG
#include <osg/Group>
#include <vrj/Draw/OSG/osgApp.h>
class cfdApp: public vrj::OsgApp
#elif _OPENSG
#endif
{
   public:
      //cfdApp( vrj::Kernel* kern);
      cfdApp( void );//vrj::Kernel* kern );
     
      // Initialize the scene graph
      virtual void initScene( );

      virtual void init( );
#ifdef _PERFORMER

      virtual void apiInit( );

      // Called After pfInit()
      virtual void preForkInit( );
      // Called Before pfConfig()

      

      // Return the current scene graph
      virtual pfGroup* getScene( );
      
      // Function called before pfSync
      virtual void preSync( );

      // Performer calls before exiting
      virtual void exit( void );
#elif _OSG
      osg::Group* getScene();
      void bufferPreDraw();
      
      virtual void configSceneView(osgUtil::SceneView* newSceneViewer);
#elif _OPENSG
#endif
      

      // Function called by the DEFAULT drawChan function 
      // before clearing the channel
      // and drawing the next frame (pfFrame( ))
      //virtual void preDrawChan(pfChannel* chan, void* chandata);


      // Function called after pfSync and before pfDraw
      virtual void preFrame( );

      // Function called after pfDraw
      virtual void intraFrame( );

      // Function called after intraFrame
      virtual void postFrame();

      

      // Used to override getFrameBufferAttrs()
      // Should be able to set multi sampling in the config
      // Look for a fix in future juggler releases
      //std::vector< int > getFrameBufferAttrs( void );

      void pushDataToStateInfo( void );

      void SetWrapper( cfdVjObsWrapper* );
      //void SetCORBAVariables( CosNaming::NamingContext_ptr, CORBA::ORB_ptr, PortableServer::POA_ptr );

      cfdPfSceneManagement*      _sceneManager;
      cfdEnvironmentHandler*     _environmentHandler;
      cfdSteadyStateVizHandler*  _steadystateHandler;
      cfdTransientVizHandler*    _transientHandler;
      cfdModelHandler*           _modelHandler;
      cfdIHCCModel               *ihccModel;
      cfdVjObsWrapper*              _vjobsWrapper;
#ifdef _TAO
      cfdExecutive*     executive;
#endif


   // Only used in preframe for transient stuff
   int   lastFrame;
   private:
      char * filein_name;
};

#endif
