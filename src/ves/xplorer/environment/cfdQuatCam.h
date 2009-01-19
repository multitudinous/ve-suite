/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef _CFD_QUAT_CAM_H_
#define _CFD_QUAT_CAM_H_
/*!\file cfdQuatCam.h
cfdQuatCam API
*/
/*!\class ves::xplorer::cfdQuatCam
*
*/
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/DCS.h>

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

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;
}
}
}

namespace ves
{
namespace xplorer
{
class VE_XPLORER_EXPORTS cfdQuatCam
{
public:
    ///Constructors
    ///\param &m Matrix.
    ///\param worldTrans Translation of world.
    cfdQuatCam( gmtl::Matrix44d& m, double* worldTrans );

    ///Destructor
    ~cfdQuatCam()
    {
        ;
    }

    ///Set camera position
    ///\param worldTrans
    ///\param worldDCS
    void SetCamPos( double* worldTrans, ves::xplorer::scenegraph::DCS* worldDCS );

    ///Move camera position, both translation and rotation slerp.
    ///\param t
    void MoveCam( double t );

    ///Rotation slerp.
    ///\param t
    void RotSlerp( double t );

    ///Translation slerp.
    ///\param t
    void TransLerp( double t );

    ///Update rotation
    ///param worldDCS
    void UpdateRotation( ves::xplorer::scenegraph::DCS* worldDCS );

    ///Returns matrix for gmtl.
    gmtl::Matrix44d GetMatrix( void );

    ///Returns next tranlation vector.
    gmtl::Vec3d GetTrans( void );

    ///Returns last tranlation vector.
    gmtl::Vec3d GetLastTrans( void );

    gmtl::Vec3d  vjVecCurrTrans;///VRJuggler current translation vector.

    double rotPoints[4];///<rotation points.

    double angle;///<angle (nay not be in use).

private:
    gmtl::Quatd LastPosQuat;///<gmtl last quaternion position.
    gmtl::Quatd NextPosQuat;///<gmtl next quaternion position.
    gmtl::Quatd CurPosQuat;///<gmtl current quaternion position.
    gmtl::Vec3d vjVecNextTrans;///<gmtl next translation.
    gmtl::Vec3d vjVecLastTrans;///<gmtl last translation.
    gmtl::Matrix44d nextMatrix;///<gmtl next matrix.

    double rotvec[3];///<rotation vector (may not be in use).
};
}
}
#endif
