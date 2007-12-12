//
// Copyright (C) 2007 Skew Matrix Software LLC (http://www.skew-matrix.com)
//
// This library is open source and may be redistributed and/or modified under  
// the terms of the OpenSceneGraph Public License (OSGPL) version 0.0 or 
// (at your option) any later version.  The full license is in LICENSE file
// included with this distribution, and on the openscenegraph.org website.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
// OpenSceneGraph Public License for more details.
//

#include "osgOQ/QueryState.h"
#include <osg/StateSet>
#include <osg/StateAttribute>
#include <osg/PolygonMode>
#include <osg/ColorMask>
#include <osg/PolygonOffset>
#include <osg/Depth>


namespace osgOQ {


osg::StateSet*
initOQState()
{
    osg::StateSet* state = new osg::StateSet;
    // TBD Possible bug, need to allow user to set render bin number.
    state->setRenderBinDetails( 9, "RenderBin" );

    state->setMode( GL_LIGHTING, osg::StateAttribute::OFF |
        osg::StateAttribute::PROTECTED);
    state->setTextureMode( 0, GL_TEXTURE_2D, osg::StateAttribute::OFF |
        osg::StateAttribute::PROTECTED);
    state->setMode( GL_CULL_FACE, osg::StateAttribute::ON |
        osg::StateAttribute::PROTECTED);

    osg::ColorMask* cm = new osg::ColorMask( false, false, false, false );
    state->setAttributeAndModes( cm, osg::StateAttribute::ON |
        osg::StateAttribute::PROTECTED);
    osg::Depth* d = new osg::Depth( osg::Depth::LEQUAL, 0.f, 1.f, false );
    state->setAttributeAndModes( d, osg::StateAttribute::ON |
        osg::StateAttribute::PROTECTED);
    osg::PolygonMode* pm = new osg::PolygonMode(
        osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::FILL );
    state->setAttributeAndModes( pm, osg::StateAttribute::ON |
        osg::StateAttribute::PROTECTED);

    osg::PolygonOffset* po = new osg::PolygonOffset( -1., -1. );
    state->setAttributeAndModes( po, osg::StateAttribute::ON |
        osg::StateAttribute::PROTECTED);

    return state;
}

osg::StateSet*
initOQDebugState()
{
    osg::StateSet* debugState = new osg::StateSet;

    debugState->setMode( GL_LIGHTING, osg::StateAttribute::OFF |
        osg::StateAttribute::PROTECTED);
    debugState->setTextureMode( 0, GL_TEXTURE_2D, osg::StateAttribute::OFF |
        osg::StateAttribute::PROTECTED);
    debugState->setMode( GL_CULL_FACE, osg::StateAttribute::ON |
        osg::StateAttribute::PROTECTED);

    osg::PolygonMode* pm = new osg::PolygonMode(
        osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::LINE );
    debugState->setAttributeAndModes( pm, osg::StateAttribute::ON |
        osg::StateAttribute::PROTECTED);

    osg::PolygonOffset* po = new osg::PolygonOffset( -1., -1. );
    debugState->setAttributeAndModes( po, osg::StateAttribute::ON |
        osg::StateAttribute::PROTECTED);

    return debugState;
}

}
