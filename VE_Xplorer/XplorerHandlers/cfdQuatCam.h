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

#ifndef _CFD_QUAT_CAM_H_
#define _CFD_QUAT_CAM_H_
/*!\file cfdQuatCam.h
cfdQuatCam API
*/
/*!\class VE_Xplorer::cfdQuatCam
* 
*/
#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/SceneGraph/DCS.h"

#include <gmtl/Math.h>
#include <gmtl/Vec.h>
#include <gmtl/Point.h>
#include <gmtl/Xforms.h>
#include <gmtl/Output.h>
#include <gmtl/Matrix.h>
#include <gmtl/Coord.h>
#include <gmtl/Generate.h>

#ifdef _OSG
#include <osg/ref_ptr>
#elif _PERFORMER
#endif

namespace VE_SceneGraph
{
   class DCS;
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
         ///Constructors   
         ///\param &m Matrix.
         ///\param worldTrans Translation of world.
         cfdQuatCam( gmtl::Matrix44f& m, float* worldTrans );
   
         ///Destructor
         ~cfdQuatCam(){;}

         ///Set camera position
         ///\param worldTrans
         ///\param worldDCS
         void SetCamPos( float* worldTrans, VE_SceneGraph::DCS* worldDCS);

         ///Move camera position, both translation and rotation slerp.
         ///\param t 
         void MoveCam( float t);

         ///Rotation slerp.
         ///\param t 
         void RotSlerp( float t);

         ///Translation slerp.
         ///\param t 
         void TransLerp( float t);

         ///Update translation
         ///\param nav
         void UpdateTrans( cfdNavigate* nav);

         ///Update rotation
         ///param worldDCS
         void UpdateRotation( VE_SceneGraph::DCS* worldDCS);

         ///Returns matrix for gmtl.
         gmtl::Matrix44f GetMatrix( void );

         ///Returns next tranlation vector.
         gmtl::Vec3f GetTrans( void );

         ///Returns last tranlation vector.
         gmtl::Vec3f GetLastTrans( void );

         gmtl::Vec3f  vjVecCurrTrans;///VRJuggler current translation vector.

         float rotPoints[4];///<rotation points.

         float angle;///<angle (nay not be in use).

      private:
         gmtl::Quatf LastPosQuat;///<gmtl last quaternion position.
         gmtl::Quatf NextPosQuat;///<gmtl next quaternion position.
         gmtl::Quatf CurPosQuat;///<gmtl current quaternion position.
         gmtl::Vec3f vjVecNextTrans;///<gmtl next translation.
         gmtl::Vec3f vjVecLastTrans;///<gmtl last translation.
         gmtl::Matrix44f nextMatrix;///<gmtl next matrix.

         float rotvec[3];///<rotation vector (may not be in use).
   };
}
#endif
