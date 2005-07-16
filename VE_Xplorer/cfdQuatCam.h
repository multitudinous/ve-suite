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
 * File:          $RCSfile: cfdQuatCam.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _CFD_QUAT_CAM_H_
#define _CFD_QUAT_CAM_H_

#include "VE_Installer/include/VEConfig.h"

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
   class cfdNavigate;
}

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdQuatCam
   {
      public:
         //Constructors
         cfdQuatCam(gmtl::Matrix44f&, double*, float*);
         cfdQuatCam(float, float, float, float, float*);
   
         //Destructor
         ~cfdQuatCam(){;}

         void SetCamPos(double*, VE_SceneGraph::cfdDCS*);

         void MoveCam(double*, float, VE_SceneGraph::cfdDCS*);

         void RotSlerp(float);

         void TransLerp(float);

         void UpdateTrans(cfdNavigate*);

         void UpdateRotation(cfdNavigate*, VE_SceneGraph::cfdDCS* );

         gmtl::Matrix44f GetMatrix( void ){return nextMatrix;}

         gmtl::Vec3f GetTrans( void ){return vjVecNextTrans;}

         gmtl::Vec3f  vjVecCurrTrans;

         //gmtl::Matrix44f m2;

         float rotPoints[4];

         float angle;

      private:
         gmtl::Quatf LastPosQuat;
         gmtl::Quatf NextPosQuat;
         gmtl::Quatf CurPosQuat;
         gmtl::Vec3f vjVecNextTrans;
         gmtl::Vec3f vjVecLastTrans;
         gmtl::Matrix44f nextMatrix;

         float rotvec[3];
   };
}
#endif
