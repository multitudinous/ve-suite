/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#ifndef __RENDER_TEXTURE_H__
#define __RENDER_TEXTURE_H__ 1


#include <osgViewer/Viewer>
#include <osgGA/TrackballManipulator>

#include <osg/Camera>
#include <osg/Geode>
#include <osg/Texture2D>


// This class manages an osgViewer::Viewer and configures it for use with RTT.

class RenderTexture
{
public:
    RenderTexture();
    ~RenderTexture() {}

    // Change clear color, fov, and minZNear before calling this.
    bool init( int argc, char** argv );

    // Call this once per frame.
    void update();

    // If you want to change the defaults, call these before init().
    void setClearColor( const osg::Vec4& c );
    void setFOV( float fov );
    void setMinZNear( float z );

    // Set the scene graph used to display the texture. If NULL, fullscreen quad is used.
    void setDisplayStage( osg::Node* node );

    // Set the RTT Camera manipulator. Default is TrackballManipulator.
    void setManipulator( osgGA::MatrixManipulator* manip );

    // ACCESSORS
    //   Get the Viewer managed by this class.
    osgViewer::Viewer* getViewer();
    //   Equivalent to getViewer()->getCamera().
    osg::Camera* getViewerCamera();
    //   Gets the RTT Camera, the camera that renders the main scene.
    osg::Camera* getRTTCamera();
    //   Returns the parent node; add or subtract any subgraphs for rendering to/from this node.
    osg::Group* getSceneParent();

protected:
    void configureCameras( osg::Camera* topCamera, osg::Camera* rttCamera );
    void configureTexture( osg::Texture2D* colorMap );
    void setProjectionMatrices( osg::Camera* topCamera, osg::Camera* rttCamera, const osg::Viewport* vp );
    osg::Geode* createFullScreenTexturedQuad();
    void configureDisplayStage();

    osg::ref_ptr< osgViewer::Viewer > _viewer;
    osg::ref_ptr< osg::Camera > _rttCamera;
    osg::ref_ptr< osg::Group > _parent;
    osg::ref_ptr< osg::Group > _displayStage;
    osg::ref_ptr< osg::Group > _root;

    osg::ref_ptr< osgGA::MatrixManipulator > _manipulator;
    osg::ref_ptr< osg::Viewport > _lastViewport;

    osg::ref_ptr< osg::Texture2D > _colorMap;

    osg::Vec4 _clearColor;
    float _fov;

    // This is the max dimensions of the RTT surface. Expanding the window size
    // beyond these dimensions will show some of the viewer camera's clear color.
    // This is bad and is a limitation of this code in its current form.
    float _maxWidth;
    float _maxHeight;
};


#endif
