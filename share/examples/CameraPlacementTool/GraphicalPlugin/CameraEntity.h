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
namespace scenegraph
{
class DCS;
}
}
}

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Camera>

namespace osg
{
class Geode;
class Geometry;
class Texture2D;
class TexGenNode;
}

// --- C/C++ Libraries --- //
#include <string>

namespace cpt
{
// --- My Includes --- //
class CameraEntityCallback;

class CameraEntity : public osg::Camera
{
public:
    CameraEntity();
    CameraEntity( ves::xplorer::scenegraph::DCS* worldDCS );
    CameraEntity( const CameraEntity& cameraEntity,
                  const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    META_Node( cpt, CameraEntity );

    void CalculateMatrixMVPT();

    void DrawCameraGeometry( bool onOff );
    void DrawViewFrustum( bool onOff );
    
    ves::xplorer::scenegraph::DCS* GetDCS();
    osg::TexGenNode* GetTexGenNode();
    osg::Matrixd GetMatrixMVPT();

    void SetNameAndDescriptions( const std::string& name );

    void Update();

protected:
    virtual ~CameraEntity();

private:
    void Initialize( ves::xplorer::scenegraph::DCS* worldDCS );
    void CreateViewFrustumGeode();
    void CreateScreenAlignedQuadGeode();
    void CreateCameraViewTexture();

    //The matrix that takes a vertex from local coords into tex coords
    osg::Matrixd mMVPT;

    //The texture used as this CameraEntity's color buffer
    //              Texture Unit 1              //
    osg::ref_ptr< osg::Texture2D > mQuadTexture;

    //              Texture Unit 0              //
    osg::ref_ptr< osg::TexGenNode > mTexGenNode;

    //
    osg::ref_ptr< osg::Uniform > mNearPlaneUniform;
    osg::ref_ptr< osg::Uniform > mFarPlaneUniform;

    //Need to create a dcs for each selectable CAD object
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mCameraDCS;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mQuadDCS;

    //The loaded camera geometry
    osg::ref_ptr< osg::Node > mCameraNode;

    //The frustum geometry lines
    osg::ref_ptr< osg::Geode > mFrustumGeode;
    osg::ref_ptr< osg::Geometry > mFrustumGeometry;
    osg::ref_ptr< osg::Vec3Array > mFrustumVertices;
    osg::ref_ptr< osg::Vec4Array > mFrustumColor;

    //The screen aligned quad to show the camera view
    osg::ref_ptr< osg::Geode > mQuadGeode;
    osg::ref_ptr< osg::Geometry > mQuadGeometry;
    osg::ref_ptr< osg::Vec3Array > mQuadVertices;

    //A callback to update CameraEntity relative to its DCS
    osg::ref_ptr< cpt::CameraEntityCallback > mCameraEntityCallback;
};

} //end cpt

#endif //CAMERA_ENTITY_H
