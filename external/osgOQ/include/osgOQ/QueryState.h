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

#ifndef OSGOQ_QUERY_STATE_H
#define OSGOQ_QUERY_STATE_H

namespace osg {
    class StateSet;
}


namespace osgOQ {

// Create a StateSet suitable for use with an OpenGL Occlusion Query.
osg::StateSet* initOQState();

// Create a StateSet suitable for displaying a debug representation of bounding geometry.
osg::StateSet* initOQDebugState();

}


#endif
