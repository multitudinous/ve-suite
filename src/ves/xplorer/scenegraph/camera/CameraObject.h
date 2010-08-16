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

#ifndef VES_XPLORER_SCENEGRAPH_CAMERA_CAMERAOBJECT_H
#define VES_XPLORER_SCENEGRAPH_CAMERA_CAMERAOBJECT_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Group>
#include <osg/Texture2D>

namespace osg
{
class Camera;
class Geode;
class Geometry;
class Light;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;

namespace camera
{

/*!\file CameraObject.h
 *
 */

/*!\class CameraObject
 * Class for
 */

/*!\namespace ves::xplorer::scenegraph::camera
 *
 */
class VE_SCENEGRAPH_EXPORTS CameraObject : public osg::Group
{
public:
    ///Constructor
    CameraObject();

    ///
    CameraObject(
        const CameraObject& cameraObject,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph::camera, CameraObject );

    ///Set the quad that this rtt camera should render into
    void SetRenderQuadTexture( osg::Geode& geode );

    ///Helper function to create a 2D texture for OSG
    osg::Texture2D* CreateViewportTexture(
        GLenum internalFormat,
        GLenum sourceFormat,
        GLenum sourceType,
        osg::Texture2D::FilterMode filterMode,
        osg::Texture2D::WrapMode wrapMode,
        std::pair< int, int >& viewportDimensions );

    ///Have no earthly idea what this does
    void CalculateMatrixMVPT();

    ///
    void ComputeNearFarPlanes( bool const& enable = true );

    ///
    //void CustomKeyboardMouseSelection(
        //std::pair< unsigned int, unsigned int > mousePosition,
        //gmtl::Matrix44d localToWorldMatrix );

    ///
    //void DisplayDepthOfFieldEffect( bool onOff );

    ///
    //void DisplayProjectionEffect( bool onOff );

    ///
    //void DisplayCameraViewQuad( bool onOff );

    ///
    //void DisplayDepthHelperQuad( bool onOff );

    ///
    void EnableCamera( bool const& enable = true );

    ///
    osg::Camera& GetCamera();

    ///
    DCS& GetDCS();

    ///
    //ves::xplorer::scenegraph::DCS* GetQuadDCS();

    ///
    osg::Matrixd const& GetInitialViewMatrix();

    ///
    osg::Light& GetLight() const;

    ///
    //osg::TexGenNode* GetTexGenNode();

    ///
    //void setAttitude( const osg::Quat& quat );

    ///
    //void SetCameraViewQuadResolution( unsigned int value );

    ///
    //void SetDepthHelperQuadResolution( unsigned int value );

    ///
    //void SetFocalDistance( double value );

    ///
    //void SetFocalRange( double value );

    ///
    //void SetMaxCircleOfConfusion( double value );

    ///
    //void setPosition( const osg::Vec3d& pos );

    ///
    //void SetProjectionEffectOpacity( double value );

    ///
    //void setScale( const osg::Vec3d& scale );

    ///
    void ShowCameraGeometry( bool const& show = true );

    ///
    void ShowFrustumGeometry( bool const& show = true );

    ///
    void Update();

    ///
    void WriteImageFile( std::string const& saveImageDir );

protected:
    ///Destructor
    virtual ~CameraObject();

private:
    ///
    void Initialize();

    ///
    void CreateGeometry();

    ///
    //void CreateHitQuadGeode();

    ///
    //void CreateCameraViewQuad();

    ///
    //void CreateDepthHelperQuad();

    ///Temporary function to allow selection of cameras
    void SetNamesAndDescriptions();

    ///The initial view matrix of the camera
    osg::Matrixd m_initialViewMatrix;

    ///The matrix that takes a vertex from local coords into tex coords
    osg::Matrixd m_mvpt;

    ///Used to generate texture coordinates for camera projection
    osg::ref_ptr< osg::TexGenNode > m_texGenNode;

    ///
    //cpt::DepthOfFieldTechnique* mDepthOfFieldTechnique;

    ///
    //cpt::DepthHelperTechnique* mDepthHelperTechnique;

    ///
    //cpt::ProjectionTechnique* mProjectionTechnique;

    ///Pointer to the HeadsUpDisplay for xplorer window
    //ves::xplorer::HeadsUpDisplay* mHeadsUpDisplay;

    ///
    osg::ref_ptr< osg::Camera > m_camera;

    ///
    osg::ref_ptr< DCS > m_dcs;

    ///
    osg::ref_ptr< osg::Node > m_cameraNode;

    ///
    osg::ref_ptr< osg::Geode > m_frustumGeode;

    ///
    osg::ref_ptr< osg::Geometry > m_frustumGeometry;

    ///
    osg::ref_ptr< osg::Vec3Array > m_frustumVertices;

    ///RTT render quad
    osg::ref_ptr< osg::Geode > m_renderQuad;

    ///RTT texture for the rtt quad
    osg::ref_ptr< osg::Texture2D > m_colorMap;

    ///RTT Image for the camera; makes it easy to write image to file
    osg::ref_ptr< osg::Image > m_colorImage;

    ///The quad to show the intersection hit
    //osg::ref_ptr< osg::Geode > mHitQuadGeode;

    ///
    //osg::ref_ptr< osg::Geometry > mHitQuadGeometry;

    ///
    //osg::ref_ptr< osg::Vec3Array > mHitQuadVertices;

    ///The screen aligned quad to show the camera view
    //osg::ref_ptr< ves::xplorer::scenegraph::DCS > mCameraViewQuadDCS;

    ///
    //osg::ref_ptr< osg::Geode > mCameraViewQuadGeode;

    ///
    //osg::ref_ptr< osg::Geometry > mCameraViewQuadGeometry;

    ///
    //osg::ref_ptr< osg::Vec3Array > mCameraViewQuadVertices;

    ///
    //osg::ref_ptr< osgText::Text > mDistanceText;

    ///The screen aligned quad to show the depth of field helper view
    //osg::ref_ptr< ves::xplorer::scenegraph::DCS > mDepthHelperQuadDCS;

    ///
    //osg::ref_ptr< osg::Geode > mDepthHelperQuadGeode;

    ///
    //osg::ref_ptr< osg::Geometry > mDepthHelperQuadGeometry;

    ///
    //osg::ref_ptr< osg::Vec3Array > mDepthHelperQuadVertices;

    ///
    osg::ref_ptr< osg::Light > m_light;

};
} //end camera
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_CAMERA_CAMERAOBJECT_H
