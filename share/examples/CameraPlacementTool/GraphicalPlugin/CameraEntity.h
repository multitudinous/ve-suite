/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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

#ifndef CAMERA_ENTITY_H
#define CAMERA_ENTITY_H

// --- VE-Suite Includes --- //
namespace ves
{
namespace xplorer
{
class HeadsUpDisplay;

namespace scenegraph
{
class DCS;
class ResourceManager;
}
}
}

// --- vrJuggler Includes --- //
#include <gmtl/Matrix.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Camera>

namespace osg
{
class Geode;
class Geometry;
}

namespace osgText
{
class Text;
}

// --- C/C++ Libraries --- //
#include <string>

namespace cpt
{
class DepthOfFieldTechnique;
class ProjectionTechnique;
class CameraEntityCallback;

class CameraEntity : public osg::Camera
{
public:
    CameraEntity();
    CameraEntity(
        ves::xplorer::scenegraph::DCS* pluginDCS,
        ves::xplorer::HeadsUpDisplay* headsUpDisplay,
        ves::xplorer::scenegraph::ResourceManager* resourceManager );

    CameraEntity( const CameraEntity& cameraEntity,
                  const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    META_Node( cpt, CameraEntity );

    void CalculateMatrixMVPT();
    void CustomKeyboardMouseSelection(
        std::pair< unsigned int, unsigned int > mousePosition,
        gmtl::Matrix44d localToWorldMatrix );

    void DisplayCamera( bool onOff );
    void DisplayViewFrustum( bool onOff );
    void DisplayProjectionEffect( bool onOff );
    void DisplayScreenAlignedQuad( bool onOff );
    
    ves::xplorer::scenegraph::DCS* GetPluginDCS();
    ves::xplorer::scenegraph::DCS* GetDCS();
    ves::xplorer::scenegraph::DCS* GetQuadDCS();
    const osg::Matrixd& GetInitialViewMatrix();
    osg::TexGenNode* GetTexGenNode();

    void SetNamesAndDescriptions();
    void SetProjectionEffectOpacity( double value );
    void SetQuadResolution( unsigned int value );

    void Update();

protected:
    virtual ~CameraEntity();

private:
    void Initialize();

    void CreateCameraNode();
    void CreateViewFrustumGeode();
    void CreateHitQuadGeode();
    void CreateCameraViewQuadGeode();

    //The initial view matrix of the camera
    osg::Matrixd mInitialViewMatrix;
    //The matrix that takes a vertex from local coords into tex coords
    osg::Matrixd mMVPT;

    //Used to generate texture coordinates for camera projection
    osg::ref_ptr< osg::TexGenNode > mTexGenNode;

    cpt::DepthOfFieldTechnique* mDepthOfFieldTechnique;
    cpt::ProjectionTechnique* mProjectionTechnique;

    //A callback to update CameraEntity relative to its DCS
    osg::ref_ptr< cpt::CameraEntityCallback > mCameraEntityCallback;

    //Pointer to the HeadsUpDisplay for xplorer window
    ves::xplorer::HeadsUpDisplay* mHeadsUpDisplay;

    ves::xplorer::scenegraph::ResourceManager* mResourceManager;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mPluginDCS;
    
    //The loaded camera geometry node and frustum geometry lines
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mCameraDCS;
    osg::ref_ptr< osg::Node > mCameraNode;
    //
    osg::ref_ptr< osg::Geode > mFrustumGeode;
    osg::ref_ptr< osg::Geometry > mFrustumGeometry;
    osg::ref_ptr< osg::Vec3Array > mFrustumVertices;
    //The quad to show the intersection hit
    osg::ref_ptr< osg::Geode > mHitQuadGeode;
    osg::ref_ptr< osg::Geometry > mHitQuadGeometry;
    osg::ref_ptr< osg::Vec3Array > mHitQuadVertices;

    //The screen aligned quad to show the camera view
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mCameraViewQuadDCS;
    osg::ref_ptr< osg::Geode > mCameraViewQuadGeode;
    osg::ref_ptr< osg::Geometry > mCameraViewQuadGeometry;
    osg::ref_ptr< osg::Vec3Array > mCameraViewQuadVertices;

    osg::ref_ptr< osgText::Text > mDistanceText;

};

} //end cpt

#endif //CAMERA_ENTITY_H
