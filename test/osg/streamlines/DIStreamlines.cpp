//
// Copyright (c) 2008 Skew Matrix  Software LLC.
// All rights reserved.
//

#include <osgDB/ReadFile>
#include <osgViewer/Viewer>
#include <osgViewer/ViewerEventHandlers>
#include <osgGA/TrackballManipulator>

#include <osg/Geometry>
#include <osg/BlendFunc>
#include <osg/Depth>

#include "PrimitiveSetInstanced.h"


class PlayStateHandler
    : public osgGA::GUIEventHandler
{
public:
    PlayStateHandler()
      : elapsedTime( 0. ),
        scalar( 1. ),
        paused( false )
    {}

    double getCurrentTime() const
    {
        if( paused )
            return( elapsedTime );
        else
            return( elapsedTime + ( timer.time_s() * scalar ) );
    }

    virtual bool handle( const osgGA::GUIEventAdapter & event_adaptor,
                         osgGA::GUIActionAdapter & action_adaptor )
    {
        bool handled = false;
        switch( event_adaptor.getEventType() )
        {
            case ( osgGA::GUIEventAdapter::KEYDOWN ):
            {
                int key = event_adaptor.getKey();
                switch( key )
                {
                    case '+':
                    {
                        elapsedTime = getCurrentTime();
                        timer.setStartTick( timer.tick() );

                        // Increase speed by 33%
                        scalar *= ( 4./3. );

                        handled = true;
                    }
                    break;
                    case '-':
                    {
                        elapsedTime = getCurrentTime();
                        timer.setStartTick( timer.tick() );

                        // Decrease speed by 25%
                        scalar *= .75;

                        handled = true;
                    }
                    break;
                    
                    // write files
                    case ' ':
                    {
                        elapsedTime = getCurrentTime();
                        timer.setStartTick( timer.tick() );

                        paused = !paused;

                        handled = true;
                    }
                    break;

                }
            }
        }
        return( handled );
    }

private:
    osg::Timer timer;
    double elapsedTime;
    double scalar;
    bool paused;
};




const int m( 8 );
const int n( 8 );
const int nVerts( 4 );
const float dx( .5f );



void
createSLPoint( osg::Geometry& geom, int nInstances=1 )
{
    const float halfDimX( .5 );
    const float halfDimY( .5 );
    const osg::Vec2 minTC( 0., 0. );
    const osg::Vec2 maxTC( 1., 1. );

    osg::Vec3Array* v = new osg::Vec3Array;
    v->resize( 4 );
    geom.setVertexArray( v );

    osg::Vec3Array* n = new osg::Vec3Array;
    n->resize( 4 );
    geom.setNormalArray( n );
    geom.setNormalBinding( osg::Geometry::BIND_PER_VERTEX );

    (*v)[ 0 ] = osg::Vec3( -halfDimX, -halfDimY, 0. );
    (*v)[ 1 ] = osg::Vec3( halfDimX, -halfDimY, 0. );
    (*v)[ 2 ] = osg::Vec3( halfDimX, halfDimY, 0. );
    (*v)[ 3 ] = osg::Vec3( -halfDimX, halfDimY, 0. );

    (*n)[ 0 ] = osg::Vec3( 0., 0., 1. );
    (*n)[ 1 ] = osg::Vec3( 0., 0., 1. );
    (*n)[ 2 ] = osg::Vec3( 0., 0., 1. );
    (*n)[ 3 ] = osg::Vec3( 0., 0., 1. );

    if( nInstances > 1 )
        geom.addPrimitiveSet( new osg::DrawArraysInstanced( GL_QUADS, 0, 4, nInstances ) );
    else
        geom.addPrimitiveSet( new osg::DrawArrays( GL_QUADS, 0, 4 ) );
}



float*
createPositionArray( int m, int n )
{
    float* pos = new float[ m * n * 3 ];
    float* posI = pos;

    int iIdx;
    for( iIdx = 0; iIdx < m*n; iIdx++ )
    {
        *posI++ = iIdx*dx;
        *posI++ = 0.;
        *posI++ = sin( iIdx * .2 );
    }

    return pos;
}


osg::Node*
createInstanced( const int m, const int n )
{
    osg::Group* grp = new osg::Group;

    osg::Geode* geode = new osg::Geode;
    osg::Geometry* geom = new osg::Geometry;
    geom->setUseDisplayList( false );
    geom->setUseVertexBufferObjects( true );
    createSLPoint( *geom, m*n );
    geode->addDrawable( geom );
    grp->addChild( geode );

    osg::BoundingBox bb( 0., -.1, -2., m*n*dx, .1, 2. );
    geom->setInitialBound( bb );


    std::string vertexSource =

        "uniform vec2 sizes; \n"
        "uniform sampler2D texPos; \n"
        "uniform mat4 osg_ViewMatrixInverse; \n"

        "uniform float osg_SimulationTime; \n"
        "uniform float osg_FrameTime; \n"
        "uniform float totalInstances; \n"
        "uniform float fadeTime; \n"
        "uniform float repeatTime; \n"

        "void main() \n"
        "{ \n"
            // Using the instance ID, generate "texture coords" for this instance.
            "const float r = ((float)gl_InstanceID) / sizes.x; \n"
            "vec2 tC; \n"
            "tC.s = fract( r ); tC.t = floor( r ) / sizes.y; \n"

            // Create orthonormal basis to position and orient this instance.
            "vec4 pos = texture2D( texPos, tC ); \n"
            "pos.w = 1.; \n"
            "vec4 eye = osg_ViewMatrixInverse * vec4( 0, 0, 0, 1 ); \n"
            //Hm, not sure why this doesn't work.
            //"vec4 newZ = eye - pos; \n"
            "vec4 newZ = vec4( 0., -1., 0., 0. ); \n"
            "newZ.w = 0.; \n"
            "normalize( newZ ); \n"
            "vec3 actualZ = vec3( 0, 0, 1 ); \n"
            "vec3 newX; \n"
            "if( abs( dot( newZ.xyz, actualZ ) ) > .98 ) \n"
                "newX = vec3( newZ.z, -newZ.x, -newZ.y ); \n"
            "else \n"
                "newX = cross( actualZ, newZ.xyz ); \n"
            "normalize( newX ); \n"
            "vec3 newY = cross( newZ.xyz, newX ); \n"
            "normalize( newY ); \n"
            "mat4 mV = mat4( newX, 0., newY, 0., newZ.xyz, 0., pos ); \n"
            "gl_Position = (gl_ModelViewProjectionMatrix * mV * gl_Vertex); \n"

            // Compute a time offset from the InstanceID to
            // emulate motion.
            "float timeOffset = ( ((float)gl_InstanceID) / totalInstances ) * repeatTime; \n"
            "float repTimer = mod( ( osg_SimulationTime - timeOffset ), repeatTime ); \n"
            "float alpha = fadeTime - min( repTimer, fadeTime ); \n"
            "gl_FrontColor = vec4( 1, 1, 1, alpha ); \n"

        "} \n";

    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    vertexShader->setType( osg::Shader::VERTEX );
    vertexShader->setShaderSource( vertexSource );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( vertexShader.get() );

    osg::StateSet* ss = geode->getOrCreateStateSet();
    ss->setAttribute( program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    osg::ref_ptr< osg::Uniform > sizesUniform =
        new osg::Uniform( "sizes", osg::Vec2( (float)m, (float)n ) );
    ss->addUniform( sizesUniform.get() );

    osg::ref_ptr< osg::Uniform > totalInstancesUniform =
        new osg::Uniform( "totalInstances", (float)(m * n) );
    ss->addUniform( totalInstancesUniform.get() );

    osg::ref_ptr< osg::Uniform > fadeTimeUniform =
        new osg::Uniform( "fadeTime", .5f );
    ss->addUniform( fadeTimeUniform.get() );

    osg::ref_ptr< osg::Uniform > repeatTimeUniform =
        new osg::Uniform( "repeatTime", 3.f );
    ss->addUniform( repeatTimeUniform.get() );

    osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc(
        GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    ss->setAttributeAndModes( bf.get() );

    osg::ref_ptr< osg::Depth > depth = new osg::Depth( osg::Depth::ALWAYS );
    ss->setAttributeAndModes( depth.get() );


    float* pos = createPositionArray( m, n );

    osg::Image* iPos = new osg::Image;
    iPos->setImage( m, n, 1, GL_RGB32F_ARB, GL_RGB, GL_FLOAT,
        (unsigned char*) pos, osg::Image::USE_NEW_DELETE );
    osg::Texture2D* texPos = new osg::Texture2D( iPos );
    texPos->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
    texPos->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );

    ss->setTextureAttribute( 0, texPos );

    osg::ref_ptr< osg::Uniform > texPosUniform =
        new osg::Uniform( "texPos", 0 );
    ss->addUniform( texPosUniform.get() );

    //delete[] pos;


    return grp;
}

int
main( int argc,
      char ** argv )
{
    osg::ref_ptr< osg::Node > root;

    osg::notify( osg::ALWAYS ) << m*n << " instances." << std::endl;
    osg::notify( osg::ALWAYS ) << m*n*nVerts << " total vertices." << std::endl;

    root = createInstanced( m, n );

    osgViewer::Viewer viewer;
    viewer.addEventHandler( new osgViewer::StatsHandler );
    viewer.setUpViewOnSingleScreen( 0 );
    viewer.setSceneData( root.get() );

    viewer.setCameraManipulator( new osgGA::TrackballManipulator );

    // Create a PlayStateHandler to track elapsed simulation time
    // and play/pause state.
    osg::ref_ptr< PlayStateHandler > psh = new PlayStateHandler;
    viewer.addEventHandler( psh.get() );

    while (!viewer.done())
    {
        // Get time from the PlayStateHandler.
        double simTime = psh->getCurrentTime();
        //osg::notify( osg::ALWAYS ) << simTime << std::endl;

        viewer.frame( simTime );
    }

    return( 0 );
}

