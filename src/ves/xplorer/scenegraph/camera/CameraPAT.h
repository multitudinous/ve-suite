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

#ifndef VES_XPLORER_SCENEGRAPH_CAMERA_CAMERAPAT_H
#define VES_XPLORER_SCENEGRAPH_CAMERA_CAMERAPAT_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Quat>
#include <osg/Vec3d>
#include <osg/PositionAttitudeTransform>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace camera
{
class Camera;

/*!\file CameraPAT.h
 *
 */

/*!\class CameraPAT
 * Class for
 */

/*!\namespace ves::xplorer::scenegraph::camera
 *
 */
class VE_SCENEGRAPH_EXPORTS CameraPAT : public osg::PositionAttitudeTransform
{
public:
    ///Constructor
    CameraPAT( Camera& camera );

    ///
    CameraPAT( const CameraPAT& cameraPAT,
               const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    ///\param nv
    virtual void accept( osg::NodeVisitor& nv );

    ///
    ///\return
    virtual const char* className() const;

    ///
    ///\param obj
    ///\return
    virtual bool isSameKindAs( const osg::Object* obj ) const;

    ///
    ///\return
    virtual const char* libraryName() const;

    ///
    void setPosition( const osg::Vec3d& pos );

    ///
    void setAttitude( const osg::Quat& quat );

    ///
    void setScale( const osg::Vec3d& scale );

protected:
    ///Destructor
    virtual ~CameraPAT();

private:
    ///
    Camera& m_camera;

};
} //end camera
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_CAMERA_CAMERAPAT_H
