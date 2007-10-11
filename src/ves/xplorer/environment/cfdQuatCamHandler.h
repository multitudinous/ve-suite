/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _CFD_QUAT_CAM_HANDLER_H_
#define _CFD_QUAT_CAM_HANDLER_H_
/*!\file cfdQuatCamHandler.h
cfdQuatCamHandler API
*/
/*!\class VE_Xplorer::cfdQuatCamHandler
* 
*/
#include <ves/xplorer/scenegraph/DCS.h>

#include <gmtl/Math.h>
#include <gmtl/Vec.h>
#include <gmtl/Point.h>
#include <gmtl/Xforms.h>
#include <gmtl/Output.h>
#include <gmtl/Matrix.h>
#include <gmtl/Coord.h>
#include <gmtl/Generate.h>

#include <vpr/Util/Timer.h>
#include <vpr/Util/Singleton.h>

namespace VE_SceneGraph
{
   class DCS;
}

namespace VE_Xplorer
{
   class cfdQuatCam;
   class cfdCommandArray;
   class cfdReadParam;
}

namespace VE_XML
{
   class Command;
}

#include <vector>
#include <map>

#include <ves/xplorer/cfdGlobalBase.h>
#include <ves/xplorer/event/EventHandler.h>

#ifdef _OSG
#include <osg/ref_ptr>
#elif _PERFORMER
#endif

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdQuatCamHandler : public cfdGlobalBase
{
public:
   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray );
   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();
   ///Set the DCS 
   ///\param newDCS The new DCS
   void SetDCS( VE_SceneGraph::DCS* newDCS );
   ///This function is for quatecam handler only
   ///This should be removed once the new animation code is in place
   void SetMasterNode( bool masterNode );
   
   ///Clear out all the saved quaternions
   void ClearQuaternionData();

   void LoadData( VE_SceneGraph::DCS* );

   void WriteToFile( std::string );

   void LoadFromFile( std::string );

   void Relocate( VE_SceneGraph::DCS* worldDCS ); 

   void RemoveViewPt( void );

   void RemoveFlythroughPt( unsigned int, unsigned int );

   void AddViewPtToFlyThrough( unsigned int, unsigned int );

   void InsertViewPtInFlyThrough( unsigned int, unsigned int, unsigned int );

   void DeleteEntireFlythrough( unsigned int );

   void AddNewFlythrough( void );

   void TurnOffMovement( void );

   double getLinearDistance( gmtl::Vec3d, gmtl::Vec3d );

   int getNumLocs();

   std::vector< std::vector <int> > getFlyThroughs();

   std::vector < int > getCompletionTest();

   // If a quat is active this will move the cam to the next location
   void PreFrameUpdate();

   double GetQuatCamIncrementor( void );

   bool IsActive( void );

   // New function for testing the new VECommand structure
   void SetVECommand( VE_XML::Command* veCommand );

   unsigned int numQuatCams;
   unsigned int numFlyThroughs;
   unsigned int* numPointsInFlyThrough;

   int cfdId;
   int cfdIso_value;
protected:
    ///Update the gui with the new data
    void _updateViewGUIPointData();

   std::map<std::string,VE_EVENTS::EventHandler* > _eventHandlers;///<Map of event handlers for texture-based vis


private:

   cfdQuatCam* thisQuatCam;
   osg::ref_ptr< VE_SceneGraph::DCS > _worldDCS;
   cfdReadParam* _readParam;
   std::string   _param;
   double t;
   std::string quatCamFileName;
   std::string quatCamDirName;
   std::vector<cfdQuatCam*> QuatCams;
   int run;
   int cam_id;
   double rotvec[3];
   double angle;
   bool activecam;
   bool _runFlyThrough;
   int activeFlyThrough;
   unsigned int pointCounter;
   bool writeReadComplete;
   double movementIntervalCalc;
   double movementSpeed;
   bool onMasterNode;
   int lastCommandId;
   int currentFrame;
   int writeFrame;
   
   vpr::Timer* frameTimer;
   
   std::vector< std::vector <int> > flyThroughList;
   std::vector < int > completionTest;

   // class used to store xml command
   VE_XML::Command* command;

    // Required so that vpr::Singleton can instantiate this class.
   //friend class vpr::Singleton< cfdTextureBasedVizHandler >;
   cfdQuatCamHandler( void );

   virtual ~cfdQuatCamHandler( void );// Never gets called, don't implement
   vprSingletonHeader( cfdQuatCamHandler );
};
}
#endif
