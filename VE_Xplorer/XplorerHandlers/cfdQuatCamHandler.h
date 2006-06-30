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
 * File:          $RCSfile: cfdQuatCamHandler.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _CFD_QUAT_CAM_HANDLER_H_
#define _CFD_QUAT_CAM_HANDLER_H_

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
   class cfdDCS;
}

namespace VE_Xplorer
{
   class cfdQuatCam;
   class cfdNavigate;
   class cfdCommandArray;
   class cfdReadParam;
}

namespace VE_XML
{
   class Command;
}

#include <vector>
#include <map>

#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"
#include "VE_Xplorer/XplorerHandlers/EventHandler.h"

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdQuatCamHandler : public cfdGlobalBase
   {
      public:
         //Constructors
         //cfdQuatCamHandler( VE_SceneGraph::cfdDCS*, cfdNavigate*, std::string );
      
         //Destructor
         //~cfdQuatCamHandler();
         ///Singleton cleanup
         void CleanUp( void );

         // compare VjObs_i commandArray with its child's value
         virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray );

         // in future, multi-threaded apps will make a copy of VjObs_i commandArray
         virtual void UpdateCommand();

         //void CreateObjects( void );

         ///Set the cfdDCS 
         ///\param newDCS The new cfdDCS
         void SetDCS(VE_SceneGraph::cfdDCS* newDCS);

         ///Set cfdNavigate
         ///\param nav The new cfdNavigate
         void SetNavigation(VE_Xplorer::cfdNavigate* nav);
   
         void LoadData(double*, VE_SceneGraph::cfdDCS*);

         void WriteToFile(std::string);

         void LoadFromFile(std::string);

         void Relocate(VE_SceneGraph::cfdDCS* worldDCS, cfdNavigate* nav); 

         void RemoveViewPt( void );

         void RemoveFlythroughPt( unsigned int, unsigned int );

         void AddViewPtToFlyThrough( unsigned int, unsigned int );

         void InsertViewPtInFlyThrough( unsigned int, unsigned int, unsigned int );

         void DeleteEntireFlythrough( unsigned int );

         void AddNewFlythrough( void );

         void TurnOffMovement( void );

         double getLinearDistance( gmtl::Vec3f, gmtl::Vec3f );

         void FindMasterNode( void );

         int getStringTokens(char* buffer, char* delim, std::vector<std::string> &toks);

         int getNumLocs();

         std::vector< std::vector <int> > getFlyThroughs();

         std::vector < int > getCompletionTest();

         // If a quat is active this will move the cam to the next location
         void PreFrameUpdate();

         void SetQuatCamIncrementor( float );

         float GetQuatCamIncrementor( void );

         bool IsActive( void );

         // New function for testing the new VECommand structure
         void SetVECommand( VE_XML::Command* veCommand );
   
         unsigned int numQuatCams;
         unsigned int numFlyThroughs;
         unsigned int* numPointsInFlyThrough;

         int cfdId;
         int cfdIso_value;
   protected:
      std::map<std::string,VE_EVENTS::EventHandler* > _eventHandlers;///<Map of event handlers for texture-based vis


      private:
         cfdQuatCam* thisQuatCam;
         VE_SceneGraph::cfdDCS* _worldDCS;
         cfdNavigate* _nav;
         cfdReadParam* _readParam;
         std::string   _param;
         float t;
         std::string quatCamFileName;
         std::string quatCamDirName;
         std::vector<cfdQuatCam*> QuatCams;
         int run;
         int cam_id;
         float rotvec[3];
         float angle;
         bool activecam;
         bool _runFlyThrough;
         int activeFlyThrough;
         unsigned int pointCounter;
         bool writeReadComplete;
         float movementIntervalCalc;
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
         cfdQuatCamHandler(/* VE_SceneGraph::cfdDCS* worldDCS,cfdNavigate* nav, std::string param */);
  
         ~cfdQuatCamHandler( void ){ ; }// Never gets called, don't implement
         vprSingletonHeader( cfdQuatCamHandler );   
   };
}
#endif
