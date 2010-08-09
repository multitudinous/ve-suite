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

#ifndef VES_XPLORER_SCENEGRAPH_CAMERA_CAMERAMANAGER_H
#define VES_XPLORER_SCENEGRAPH_CAMERA_CAMERAMANAGER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/camera/Definitions.h>

// --- OSG Includes --- //
#include <osg/Group>
#include <osg/Geode>
#include <osg/PositionAttitudeTransform>

namespace osgUtil
{
class LineSegmentIntersector;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace camera
{
class CameraObject;

/*!\file CameraManager.h
 * CameraManager API
 */

/*!\class ves::xplorer::scenegraph::camera::CameraManager
 *
 */
class VE_SCENEGRAPH_EXPORTS CameraManager : public osg::Group
{
public:
    ///Constructor
    CameraManager();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    CameraManager(
        const CameraManager& cameraManager,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph::camera, CameraManager );

    ///Override the addChild function to only accept Cameras
    virtual bool addChild();

    ///
    void Enable( const bool& enable = true );

    ///
    CameraObject* const GetActiveCameraObject() const;

    ///Get the RTT quad
    ///\return The geode with the RTT quad
    osg::Node* GetCameraManagerQuad();

    ///
    bool Handle(
        Event::Enum event,
        osgUtil::LineSegmentIntersector& deviceInput );

    ///Override the insertChild function to only accept Cameras
    virtual bool insertChild( unsigned int index, CameraObject* child );

    ///
    const bool IsEnabled() const;

    ///
    void RemoveAllCameraObjects();

    ///Override the replaceChild function to only accept Cameras
    virtual bool replaceChild(
        CameraObject* origChild, CameraObject* newChild );

    ///
    void SetActiveCameraObject( CameraObject* cameraObject );

    ///
    void SetCameraViewQuadResolution( unsigned int const& scale );

    ///Override the setChild function to only accept Cameras
    virtual bool setChild( unsigned int i, CameraObject* node );

    ///
    void WriteAllImageFiles( std::string const& filename );

protected:
    ///Destructor
    virtual ~CameraManager();

private:
    ///
    CameraObject* const ConvertNodeToCameraObject( osg::Node* const node );

    ///Create the quad to be used by the rtt cameras that this class manages
    ///\return The geode the holds the drawable with the rtt quad
    osg::Geode* CreateMasterCameraQuad();

    ///Is the camera manager turned on
    bool m_enabled;

    ///The active rtt view camera
    CameraObject* m_activeCameraObject;

    ///The rtt quad geode
    osg::ref_ptr< osg::Geode > m_rttQuad;

    ///PAT node for non desktop mode
    osg::ref_ptr< osg::PositionAttitudeTransform > m_rttQuadTransform;

};
} //end camera
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_CAMERA_CAMERAMANAGER_H
