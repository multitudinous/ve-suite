//
// Copyright (c) 2008 Skew Matrix  Software LLC.
// All rights reserved.
//

#include "PrimitiveSetInstanced.h"
#include <osg/GLExtensions>
#include <osg/State>
#include <osg/Notify>


namespace osg {

DrawArraysInstanced::DrawArraysInstanced( GLenum mode )
  : osg::DrawArrays( mode ),
    _nInstances( 1 ),
    _extensionInitialized( false ),
    _glDrawArraysInstancedEXT( NULL )
{
}

DrawArraysInstanced::DrawArraysInstanced( GLenum mode, GLint first, GLsizei count, int numInstances )
  : osg::DrawArrays( mode, first, count ),
    _nInstances( numInstances ),
    _extensionInitialized( false ),
    _glDrawArraysInstancedEXT( NULL )
{
}

DrawArraysInstanced::DrawArraysInstanced( const DrawArraysInstanced& dai, const osg::CopyOp& copyop )
  : osg::DrawArrays( dai, copyop ),
    _nInstances( dai._nInstances ),
    _extensionInitialized( false ),
    _glDrawArraysInstancedEXT( NULL )
{
}

DrawArraysInstanced::~DrawArraysInstanced()
{
}



void
DrawArraysInstanced::setNumInstances( const int n )
{
    _nInstances = n;
}
int
DrawArraysInstanced::getNumInstances() const
{
    return _nInstances;
}

void
DrawArraysInstanced::draw( osg::State& state, bool useVertexBufferObjects ) const
{
    if( !_extensionInitialized )
    {
        if( osg::isGLExtensionSupported( state.getContextID(), "GL_EXT_draw_instanced" ) )
        {
            _glDrawArraysInstancedEXT = (DrawArraysInstancedEXTProc) osg::getGLExtensionFuncPtr("glDrawArraysInstancedEXT" );
            _extensionInitialized = ( _glDrawArraysInstancedEXT != NULL );
        }
        if( !_extensionInitialized )
            return;
    }

    _glDrawArraysInstancedEXT( _mode, _first, _count, _nInstances );
}

}
