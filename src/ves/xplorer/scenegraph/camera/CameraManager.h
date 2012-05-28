/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

// --- VES Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/camera/Definitions.h>

#include <ves/open/xml/DataValuePair.h>

// --- OSG Includes --- //
#include <osg/Group>
#include <osg/Geode>
#include <osg/PositionAttitudeTransform>

namespace osgUtil
{
class LineSegmentIntersector;
}

// --- STL Includes --- //
#include <string>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{

namespace technique
{
class ProjectionTechnique;
}

namespace camera
{
class CameraObject;

/*!\file CameraManager.h
 * CameraManager API
 * \class ves::xplorer::scenegraph::camera::CameraManager
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
    //META_Node( ves::xplorer::scenegraph::camera, CameraManager );

    ///Override the addChild function to only accept Cameras
    virtual bool addChild( std::string const& name );

    ///
    CameraObject* ConvertNodeToCameraObject( osg::Node* const node );

    ///
    void DisplayProjectionEffect(
        bool const& onOff,
        bool const& calledByGUI = true );

    ///
    void Enable( const bool& enable = true );

    ///
    void EnableCPT( const bool& enable = true );

    ///
    CameraObject* GetActiveCameraObject() const;

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
    bool IsEnabled() const;

    ///Returns if the camera placement tool is enabled for CAVE
    bool const& IsCPTEnabled() const;

    ///
    virtual void removeChildren();

    ///Override the replaceChild function to only accept Cameras
    virtual bool replaceChild(
        CameraObject* origChild, CameraObject* newChild );

    ///
    void SetActiveCameraObject(
        CameraObject* cameraObject,
        const bool& sendDataToConductor = false );

    ///
    void SetCameraViewQuadResolution( unsigned int const& scale );

    ///Override the setChild function to only accept Cameras
    virtual bool setChild( unsigned int i, CameraObject* node );

    ///
    void SetProjectionEffectOpacity( double const& value );

    ///Write all the images for all of the cameras currently available
    void WriteAllImageFiles( std::string const& saveImageDir );

    ///Write the images for the active camera only
    void WriteActiveCameraImageFile( std::string const& saveImageDir );

    ///Send camera manager data to conductor to enable the gui
    ///to be in sync with Xplorer
    void UpdateConductorData( ves::open::xml::DataValuePairPtr inDvp =
                                  ves::open::xml::DataValuePairPtr() );

    ///Set the picture mode to create and take a picture when the user presses
    ///a button on the keyboard or wand
    void SetPictureMode( bool isPictureMode );

    ///Get the picture mode setting
    bool IsPictureMode();

    ///Called every frame to update picture snap camera
    void LatePreFrameUpdate();

    ///Called after the draw function
    void PostFrameUpdate();

    ///Set the directory to use for saving images
    void SetImageStoreDirectory( const std::string& imageDir );

protected:
    ///Destructor
    virtual ~CameraManager();

private:
    ///Create the quad to be used by the rtt cameras that this class manages
    ///\return The geode the holds the drawable with the rtt quad
    osg::Geode* CreateMasterCameraQuad();

    ///This controls the state of the node mask
    bool m_enabled;

    ///This controls if the camera placement tool is enabled
    bool m_cptEnabled;

    ///
    bool m_projectEffect;

    ///The active rtt view camera
    CameraObject* m_activeCameraObject;

    ///The rtt quad geode
    osg::ref_ptr< osg::Geode > m_rttQuad;

    ///PAT node for non desktop mode
    osg::ref_ptr< osg::PositionAttitudeTransform > m_rttQuadTransform;

    ///
    technique::ProjectionTechnique* m_projectionTechnique;

    ///Used to generate texture coordinates for camera projection
    osg::ref_ptr< osg::TexGenNode > m_texGenNode;

    ///Set the picture mode
    bool m_isPictureMode;

    ///Get head shot camera
    osg::ref_ptr< CameraObject > m_headShotCamera;
    ///Tell wether we are taking screen caps
    bool m_isTakingScreenCap;

    ///The directory for storing images
    std::string m_imageDir;
};
} //end camera
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_CAMERA_CAMERAMANAGER_H
