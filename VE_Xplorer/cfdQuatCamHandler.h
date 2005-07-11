/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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

#include <vector>

#include "VE_Xplorer/cfdGlobalBase.h"

namespace VE_Xplorer
{
   class cfdPoints
   {
      public:
         //Constructor
         cfdPoints(double*, gmtl::Matrix44f&);

         //Destructor
         ~cfdPoints();
         gmtl::Matrix44f& matrix(){return m;}
         gmtl::Vec3f& ptrans(){return trans;}
      private:
         gmtl::Matrix44f      m;
         gmtl::Vec3f    trans; 

   };
}

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdQuatCamHandler : public cfdGlobalBase
   {
      public:
         //Constructors
         cfdQuatCamHandler( VE_SceneGraph::cfdDCS*, cfdNavigate*, char* );
      
         //Destructor
         ~cfdQuatCamHandler();

         // compare VjObs_i commandArray with its child's value
         virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray );

         // in future, multi-threaded apps will make a copy of VjObs_i commandArray
         virtual void UpdateCommand();

         void CreateObjects( void );
   
         void LoadData(double*, VE_SceneGraph::cfdDCS*);

         void WriteToFile(char*);

         void LoadFromFile(char*);

         void Relocate(VE_SceneGraph::cfdDCS* worldDCS, cfdNavigate* nav); 

         int getNumLocs();

         // If a quat is active this will move the cam to the next location
         void PreFrameUpdate();
   
         int numQuatCams;
         int numFlyThroughs;
         int* numPointsInFlyThrough;

      private:
         cfdQuatCam* thisQuatCam;
         cfdPoints*  nextPoint;
         VE_SceneGraph::cfdDCS* _worldDCS;
         cfdNavigate* _nav;
         cfdReadParam* _readParam;
         char*       _param;
         float t;
         char* quatCamFileName;
         std::vector<cfdPoints*> cfdPointsVec;
         std::vector<cfdQuatCam*> QuatCams;
         int run;
         int cam_id;
         float rotvec[3];
         float angle;
         bool activecam;
         int activeFlyThrough;
         //std::vector<int> activeFlyThroughArray[ 4 ];
         std::vector<int*> flyThroughList;
   };
}
#endif
