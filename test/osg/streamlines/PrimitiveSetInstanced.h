//
// Copyright (c) 2008 Skew Matrix  Software LLC.
// All rights reserved.
//

#ifndef __PRIMITIVE_SET_INSTANCED_H__
#define __PRIMITIVE_SET_INSTANCED_H__ 1


#include <osg/GL>
#include <osg/PrimitiveSet>


namespace osg {

class DrawArraysInstanced : public osg::DrawArrays
{
public:
    DrawArraysInstanced( GLenum mode=0 );
    DrawArraysInstanced( GLenum mode, GLint first, GLsizei count, int numInstances=1 );
    DrawArraysInstanced( const DrawArraysInstanced& dai, const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY );


    virtual osg::Object* cloneType() const
    {
        return( new DrawArraysInstanced() );
    }
    virtual osg::Object* clone( const osg::CopyOp& copyop ) const
    {
        return( new DrawArraysInstanced( *this, copyop ) );
    }
    virtual bool isSameKindAs( const osg::Object* obj ) const
    { return( dynamic_cast< const DrawArraysInstanced* >( obj ) != NULL );
    }
    virtual const char* libraryName() const { return( "osg" ); }
    virtual const char* className() const { return( "DrawArraysInstanced" ); }

    virtual void draw( osg::State& state, bool useVertexBufferObjects ) const;

    void setNumInstances( const int n );
    int getNumInstances() const;

protected:
    ~DrawArraysInstanced();

    int _nInstances;

    mutable bool _extensionInitialized;

    typedef void (APIENTRY *DrawArraysInstancedEXTProc)( GLenum mode, GLint first, GLsizei count, GLsizei primcount );
    mutable DrawArraysInstancedEXTProc _glDrawArraysInstancedEXT;
};

}

#endif
