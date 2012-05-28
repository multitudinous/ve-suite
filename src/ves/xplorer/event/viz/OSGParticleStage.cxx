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
#include <ves/xplorer/event/viz/OSGParticleStage.h>
#include <string>

#include <osg/PositionAttitudeTransform>
#include <osg/Texture>
#include <osg/Texture2D>
#include <osgDB/ReadFile>
#include <osgDB/FileUtils>
#include <osg/io_utils>

#include <osgwTools/Shapes.h>

#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkCleanPolyData.h>
#include <vtkXMLPolyDataWriter.h>

#include <iostream>

#include <cmath>

#include <ves/xplorer/util/FindVertexCellsCallback.h>
#include <ves/xplorer/util/GetScalarDataArraysCallback.h>
#include <ves/xplorer/util/ProcessScalarRangeCallback.h>

#include <ves/xplorer/scenegraph/Geode.h>

#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/Debug.h>

////////////////////////////////////////////////////////////////////////////////
OSGParticleStage::OSGParticleStage( void )
    :
    m_particleDiameter( 1 )
{
}
////////////////////////////////////////////////////////////////////////////////
OSGParticleStage::~OSGParticleStage( void )
{
    vprDEBUG( vesDBG, 1 ) << "|\t\tDeleting Point Array" << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void OSGParticleStage::createSLPoint( osg::Geometry& geom, int nInstances )
{
    // Configure a Geometry to draw a single point, but use the draw instanced PrimitiveSet
    // to draw the point multiple times.
    /*
    osg::Vec3Array* v = new osg::Vec3Array;
    v->resize( 1 );
    geom.setVertexArray( v );
    (*v)[ 0 ] = position;
    */
    geom.setUseDisplayList( false );
    geom.setUseVertexBufferObjects( true );

    //osgwTools::makeGeodesicSphere( 1.0, 0.0, &geom );
    osgwTools::makeAltAzSphere( float( 0.5 ), 4, 6, &geom );
    unsigned int numPrim = geom.getNumPrimitiveSets();
    for( unsigned int i = 0; i < numPrim; ++i )
    {
        geom.getPrimitiveSet( i )->setNumInstances( nInstances );
    }

    // Streamline color. Blending is non-saturating, so it never
    // reaches full intensity white. Alpha is modulated with the
    // point sprint texture alpha, so the value here is a maximum
    // for the "densist" part of the point sprint texture.
    /*osg::Vec4Array* c = new osg::Vec4Array;
    c->resize( 1 );
    geom.setColorArray( c );
    geom.setColorBinding( osg::Geometry::BIND_OVERALL );
    (*c)[ 0 ] = color;*/

    //geom.addPrimitiveSet( new osg::DrawArrays( GL_POINTS, 0, 1, nInstances ) );

    //osg::StateSet* ss = geom.getOrCreateStateSet();

    /*
    osg::Point* point = new osg::Point;
    point->setSize( 10. );
    // Use of shader (required for draw instanced) disables fixed-funxtion point parameters.
    // I'll need to investigate how to mimic this functionality in a shader.
    //point->setDistanceAttenuation( osg::Vec3( 0., 0., 0.05f) );
    ss->setAttributeAndModes( point );
    */

    // Turn on point sprites and specigy the point sprite texture.
    /*
    osg::PointSprite *sprite = new osg::PointSprite();
    ss->setTextureAttributeAndModes( 1, sprite, osg::StateAttribute::ON );
    osg::Texture2D *tex = new osg::Texture2D();
    tex->setImage( osgDB::readImageFile( "splotch.png" ) );
    ss->setTextureAttributeAndModes( 1, tex, osg::StateAttribute::ON );

    // Keep pixels with a significant alpha value (discard low-alpha pixels).
    osg::AlphaFunc* af = new osg::AlphaFunc( osg::AlphaFunc::GREATER, 0.05f );
    ss->setAttributeAndModes( af );
    */
}
////////////////////////////////////////////////////////////////////////////////
void OSGParticleStage::createStreamLines( ves::xplorer::scenegraph::Geode* geode )
{
    ves::xplorer::util::ProcessScalarRangeCallback* scalarRangeCbk =
        new ves::xplorer::util::ProcessScalarRangeCallback();
    ves::xplorer::util::DataObjectHandler* dataObjectHandler =
        new ves::xplorer::util::DataObjectHandler();
    dataObjectHandler->SetDatasetOperatorCallback( scalarRangeCbk );
    double dataRange[ 2 ] = { 10000000.0, -1000000.0 };
    double** tempRange = new double*[ 1 ];
    tempRange[ 0 ] = new double[ 2 ];

    vtkDataObject* tempDataSet = 0;

    for( size_t i = 0; i < m_transientDataSet.size(); ++i )
    {
        tempDataSet = m_transientDataSet.at( i )->GetDataSet();
        dataObjectHandler->OperateOnAllDatasetsInObject( tempDataSet );
        scalarRangeCbk->GetScalarRange( m_activeScalar, tempRange[ 0 ] );
        if( tempRange[ 0 ][ 0 ] < dataRange[ 0 ] )
        {
            dataRange[ 0 ] = tempRange[ 0 ][ 0 ];
        }

        if( tempRange[ 0 ][ 1 ] > dataRange[ 1 ] )
        {
            dataRange[ 1 ] = tempRange[ 0 ][ 1 ];
        }
    }
    delete [] tempRange[ 0 ];
    delete [] tempRange;

    delete scalarRangeCbk;
    delete dataObjectHandler;


    //double x[3];
    //We must set these to 0 becuase we do not want to doubly offset
    //our starting location of the vertecies
    //x[ 0 ] = 0.;//tempPoint.x[ 0 ];
    //x[ 1 ] = 0.;//tempPoint.x[ 1 ];
    //x[ 2 ] = 0.;//tempPoint.x[ 2 ];
    //osg::Vec3 loc(x[0], x[1], x[2] );

    //double bounds[6];
    //points->GetBounds(bounds);
    //VTK does bounds xmin, xmax,....
    //OSG does bounds xmin, ymin, zmin, xmax, ymax,...
    osg::BoundingBox bb( m_bb[0], m_bb[2], m_bb[4], m_bb[1], m_bb[3], m_bb[5] );

    size_t numStreamLines = m_streamlineList.size();
    for( size_t i = 0; i < numStreamLines; ++i )
    {
        std::deque< ves::xplorer::scenegraph::VTKParticleTextureCreator::Point > tempLine = m_streamlineList.at( i );

        osg::ref_ptr< ves::xplorer::scenegraph::VTKParticleTextureCreator > rawVTKData =
            new ves::xplorer::scenegraph::VTKParticleTextureCreator();
        rawVTKData->SetScalarData( m_lineDataCollection.at( i ) );
        rawVTKData->SetActiveVectorAndScalar( m_activeVector, m_activeScalar );
        rawVTKData->SetPointQueue( tempLine );
        rawVTKData->loadData();

        osg::Geometry* geom = new osg::Geometry;
        // Note:
        // Display Lists and draw instanced are mutually exclusive. Disable
        // display lists and use buffer objects instead.
        geom->setUseDisplayList( false );
        geom->setUseVertexBufferObjects( true );
        //createSLPoint( *geom, cLineNp * mult, loc, osg::Vec4( .5, 1., .6, 1.) );
        int totalNumberOfPoints = tempLine.size();
        createSLPoint( *geom, totalNumberOfPoints );
        geode->addDrawable( geom );

        // Note:
        // OSG has no idea where our vertex shader will render the points. For proper culling
        // and near/far computation, set an approximate initial bounding box.
        geom->setInitialBound( bb );

        osg::StateSet* ss = geom->getOrCreateStateSet();

        {
            osg::ref_ptr< osg::Uniform > particlesizeUniform =
                new osg::Uniform( "particleSize", m_particleDiameter );
            ss->addUniform( particlesizeUniform.get() );
        }

        //Apply the shader code here instead of calling it from a file as above
        std::string vertexSource =
            //Setup the color control textures
            "uniform vec2 scalarMinMax;\n"
            "uniform sampler1D texCS; \n"
            //particle vars
            "uniform vec3 sizes; \n"
            "uniform sampler3D texPos; \n"
            "uniform sampler3D scalar; \n"
            "uniform sampler3D diameter; \n"
            "vec4 \n"
            "simpleLighting( const in vec4 color, const in vec3 normal, const in float diffCont, const in float ambCont ) \n"
            "{ \n"
            "    vec4 amb = color * ambCont; \n"
            "    const vec3 eyeVec = vec3( 0.0, 0.0, 1.0 ); \n"
            "    float dotVal = max( dot( normal, eyeVec ), 0.0 ); \n"
            "    vec4 diff = color * dotVal * diffCont; \n"
            "    return( amb + diff ); \n"
            "} \n" //25
            " \n"
            "bool\n"
            "discardInstance( const in vec4 pos )\n"
            "{\n"
            "    if( pos.xyz == vec3( 0.,0.,0. ) )\n"
            "    {\n"
            "        gl_Position = vec4( 1.0, 1.0, 1.0, 0.0 );\n"
            "        return true;\n"
            "    }\n"
            "    return false;\n"
            "}\n"
            // Based on the global 'sizes' uniform that contains the 3D stp texture dimensions,
            // and the input parameter current instances, generate an stp texture coord that
            // indexes into a texture to obtain data for this instance.
            "vec3 \n"
            "generateTexCoord( in float fiid ) \n"
            "{ \n"
            "    float p1 = fiid / (sizes.x*sizes.y); \n"
            "    float t1 = fract( p1 ) * sizes.y; \n"

            "    vec3 tC; \n"
            "    tC.s = fract( t1 ); \n"
            "    tC.t = floor( t1 ) / sizes.y; \n"
            "    tC.p = floor( p1 ) / sizes.z; \n"

            "    return( tC ); \n"
            "} \n"

            " \n"
            ///Main program
            "uniform float osg_SimulationTime; \n"
            "uniform float totalInstances; \n"
            "uniform float fadeTime; \n"
            "uniform float repeatTime; \n"
            "uniform float particleSize; \n"

            //Phong shading variables
            "varying vec3 phongColor; \n"
            "varying vec3 lightPos; \n"
            //"varying vec3 objPos; \n"
            "varying vec3 eyePos; \n"
            "varying vec3 normal; \n"
            "varying float opacityVal;\n"
            "\n"
            "void main() \n"
            "{ \n"
            // Using the instance ID, generate "texture coords" for this instance.
            "   float fInstance = gl_InstanceID; \n"
            "   vec3 tC = generateTexCoord( fInstance ); \n"

            // Get position from the texture.
            "   vec4 pos = texture3D( texPos, tC ); \n"
            "   if( discardInstance( pos ) )\n"
            "   {\n"
            "       return;\n"
            "   }\n"
            "   float userScale = 0.05;\n"
            //Set to 0 to have proper addition with gl_Vertex
            "   pos.w = 0.; \n"
            "   vec4 newPos = vec4( (gl_Vertex.xyz * userScale) + pos.xyz, 1.0 );\n"
            "   vec4 v = gl_ModelViewMatrix * newPos; \n"
            "   gl_Position = gl_ProjectionMatrix * v; \n"
            //" gl_Position = gl_ModelViewProjectionMatrix * ( gl_Vertex + pos ); \n"

            // TBD. Need to make this configurable from a uniform.
            //"   gl_PointSize = (-50. / v.z) * particleSize; \n"

            // Compute a time offset from the InstanceID to emulate motion.
            ///This is just the fraction of the total time that this instance
            ///is of the overal simulation. It varies between 0 and repeat time.
            "   float timeOffset = ( fInstance / totalInstances ) * repeatTime; \n"
            ///This creates a sliding window for a number between 0 and repeatTime
            ///Without this sliding window we get no motion in our particles.
            "   float repTimer = mod( ( osg_SimulationTime - timeOffset ), repeatTime ); \n"

            "   float alpha = fadeTime - min( repTimer, fadeTime ); \n"
            "   if( alpha < 0.97 )\n"
            "   {\n"
            "       alpha = 0.;\n"
            //"        gl_Position = vec4( 1.0, 1.0, 1.0, 0.0 );\n"
            //"        return;\n"
            "   }\n"
            "   else\n"
            "   {\n"
            "       alpha = 1.;\n"
            "   }\n"

            // Orient the normal.
            "   vec3 norm = normalize( gl_NormalMatrix * gl_Normal ); \n"
            // Diffuse lighting with light at the eyepoint.
            //"   vec4 color = texture3D( scalar, tC ); \n"
            //"   color = color * dot( norm, vec3( 0, 0, 1 ) ); \n"
            //"   color[3]=1; \n"
            //"   gl_FrontColor = vec4( color ); \n"
            // Compute color and lighting.
            //const vec4 scalarV = texture3D( scalar, tC );
            //const vec4 oColor = texture1D( texCS, scalarV.a );
            //New way of mapping colors
            "   // Scalar texture containg key to color table. \n"
            "   vec4 activeScalar = texture3D( scalar, tC );\n"
            "   float normScalarVal = 0.;\n"
            "   normScalarVal = (activeScalar.a - scalarMinMax.x) / (scalarMinMax.y - scalarMinMax.x);\n"

            "   if( normScalarVal < 0. )\n"
            "   {\n"
            "       normScalarVal = 0.;\n"
            "   }\n"
            "   if( normScalarVal > 1. )\n"
            "   {\n"
            "       normScalarVal = 1.;\n"
            "   }\n"
            "   vec4 colorResult = texture1D( texCS, normScalarVal );\n"
            "   colorResult[3]=1.0; \n"
            "   vec4 color = simpleLighting( colorResult, norm, 0.7, 0.3 ); \n"
            //"   vec4 color = vec4( 1.0, 0.0, 0.0, 1.0 ); //texture2D( texSca, tC ); \n"
            "   color[3]=alpha; \n"
            "   gl_FrontColor = color; \n"
            /*"     // Setup varying variables. \n"
            "   opacityVal = alpha;\n"
            "     phongColor = vec3( 1.0, 0.0, 0.0 );\n"
            //"   objPos=gl_Vertex.xyz; \n"
            "     eyePos=vec3(gl_ModelViewMatrix*gl_Vertex); \n"
            "     lightPos=gl_LightSource[0].position.xyz; \n"
            //"   normal=vec3(gl_NormalMatrix*(gl_Normal+ normalize(vecOff.xyz) ) ); \n"
            "     normal=vec3(gl_NormalMatrix * gl_Normal); \n"
            "     gl_FrontSecondaryColor=vec4(1.0);\n"
            "     gl_BackSecondaryColor=vec4(0.0);\n"
            "     gl_BackColor = vec4( phongColor, opacityVal);\n"
            "     gl_FrontColor = vec4( phongColor, opacityVal);\n"*/
            "} \n";

        ///New shader

        std::string vertexSource2 =
            //Setup the color control textures
            "uniform vec2 scalarMinMax;\n"
            "uniform sampler1D texCS; \n"
            "uniform sampler3D scalar; \n"
            //Other uniforms
            "uniform vec3 sizes;\n"
            "uniform float totalInstances;\n"
            "uniform sampler3D texPos;\n"
            "\n"
            "uniform float osg_SimulationTime;\n"
            "\n"
            "\n"
            "// Total traces to draw on this streamline.\n"
            "// E.g., 1, 2, 3, etc.\n"
            "uniform int numTraces;\n"
            "\n"
            "// Time interval between traces. Total time for\n"
            "// a trace to run the length of all samples points:\n"
            "//     ( traceInterval * numTraces ) / osg_SimulationTime\n"
            "uniform float traceInterval;\n"
            "\n"
            "// Trace length, in number of sample points. Alpha fades\n"
            "// linearly over the traceLength.\n"
            "uniform int traceLength;\n"
            "\n"
            // Based on the global 'sizes' uniform that contains the 3D stp texture dimensions,
            // and the input parameter current instances, generate an stp texture coord that
            // indexes into a texture to obtain data for this instance.
            "vec3 \n"
            "generateTexCoord( in float fiid ) \n"
            "{ \n"
            "    float p1 = fiid / (sizes.x*sizes.y); \n"
            "    float t1 = fract( p1 ) * sizes.y; \n"

            "    vec3 tC; \n"
            "    tC.s = fract( t1 ); \n"
            "    tC.t = floor( t1 ) / sizes.y; \n"
            "    tC.p = floor( p1 ) / sizes.z; \n"

            "    return( tC ); \n"
            "} \n"
            "\n"
            "void main()\n"
            "{\n"
            //"    // Using the instance ID, generate \"texture coords\" for this instance.\n"
            //"    float fInstanceID = gl_InstanceID;\n"
            //"    float r = fInstanceID / sizes.x;\n"
            //"    vec2 tC;\n"
            //"    tC.s = fract( r ); tC.t = floor( r ) / sizes.y;\n"
            // Using the instance ID, generate "texture coords" for this instance.
            "    float fInstanceID = gl_InstanceID; \n"
            "    vec3 tC = generateTexCoord( fInstanceID ); \n"
            "    \n"
            "    // Get position from the texture.\n"
            "    vec4 pos = texture3D( texPos, tC );\n"
            "    pos.w = 0.; // w is 1.0 after lookup; do NOT add 1.0 to gl_Vertex.w\n"
            "    vec4 v = gl_ModelViewMatrix * ( gl_Vertex + pos );\n"
            "    gl_Position = gl_ProjectionMatrix * v;\n"
            "    \n"
            "    // TBD. Need to make this configurable from a uniform.\n"
            "    gl_PointSize = -2000. / v.z;\n"
            "    \n"
            "    \n"
            "    // Compute the length of a trace segment, in points.\n"
            "    float segLength = totalInstances / numTraces;\n"
            "    // Use current time to compute an offset in points for the animation.\n"
            "    float time = mod( osg_SimulationTime, traceInterval );\n"
            "    float pointOffset = ( time / traceInterval ) * segLength;\n"
            "    \n"
            "    // Find the segment tail for this point's relavant segment.\n"
            "    float segTail = floor( (fInstanceID - pointOffset) / segLength ) * segLength + pointOffset;\n"
            "    // ...and the head, which will have full intensity alpha.\n"
            "    float segHead = floor( segTail + segLength );\n"
            "    \n"
            "#if 1\n"
            "    // Use smoothstep to fade from the head to the traceLength.\n"
            "    float alpha = smoothstep( segHead-traceLength, segHead, fInstanceID );\n"
            "#else\n"
            "    // Alternative: Use step() instead for no fade.\n"
            "    float alpha = step( segHead-traceLength, fInstanceID );\n"
            "#endif\n"
            "    \n"
            //"    vec4 color = gl_Color;\n"
            //"    color.a *= alpha;\n"
            //"    gl_FrontColor = color;\n"
            "    // Scalar texture containg key to color table. \n"
            "    vec4 activeScalar = texture3D( scalar, tC );\n"
            "    float normScalarVal = 0.;\n"
            "    normScalarVal = (activeScalar.a - scalarMinMax.x) / (scalarMinMax.y - scalarMinMax.x);\n"

            "    if( normScalarVal < 0. )\n"
            "    {\n"
            "        normScalarVal = 0.;\n"
            "    }\n"
            "    if( normScalarVal > 1. )\n"
            "    {\n"
            "        normScalarVal = 1.;\n"
            "    }\n"
            "    vec4 color = texture1D( texCS, normScalarVal );\n"
            "    color[3]=alpha; \n"
            "    gl_FrontColor = color; \n"
            "}\n";

        osg::ref_ptr< osg::Program > program = new osg::Program();
        {
            osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
            vertexShader->setType( osg::Shader::VERTEX );
            vertexShader->setShaderSource( vertexSource );

            program->addShader( vertexShader.get() );
        }

        {
            std::string shaderName = osgDB::findDataFile( "null_glow.fs" );

            osg::ref_ptr< osg::Shader > fragShader =
                osg::Shader::readShaderFile( osg::Shader::FRAGMENT, shaderName );
            //ss->addUniform( new osg::Uniform( "tex", 1 ) );
            //ss->addUniform( new osg::Uniform( "texUnit", (unsigned int)1 ) );

            //osg::ref_ptr< osg::Shader > fragShader = new osg::Shader();
            //fragShader->setType( osg::Shader::FRAGMENT );
            //fragShader->setShaderSource( shaderName );
            program->addShader( fragShader.get() );
        }
        ss->setAttributeAndModes( program.get(),
                                  osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        //ss->setRenderBinDetails( 0, "RenderBin" );
        //ss->setNestRenderBins( true );

        // Note:
        // We will render the streamline points with depth test on and depth write disabled,
        // with an order independent blend. This means we need to draw the streamlines last
        // (so use bin # 10) but we don't need the depth sort, so use bin name "RenderBin".
        ss->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
        ss->setNestRenderBins( true );

        // Note:
        // When using a vertex shader, point size is taken from glPointSize and _not_
        // distance-attenuated. However, set the following mode ON, and then our vertex
        // shader can do its own distance attenuation and emit gl_PointSize.
        //ss->setMode( GL_VERTEX_PROGRAM_POINT_SIZE, osg::StateAttribute::ON );

        // Tells the shader the dimensions of our texture: tm x tn.
        // Required to compute correct texture coordinates from the instance ID.
        osg::Vec3s ts( rawVTKData->getTextureSizes() );

        osg::ref_ptr< osg::Uniform > sizesUniform =
            new osg::Uniform( "sizes", osg::Vec3( float( ts.x() ), float( ts.y() ), float( ts.z() ) ) );
        ss->addUniform( sizesUniform.get() );

        // Tell the shader the total number of instances: tm * tn.
        // Required for animation based on the instance ID.
        osg::ref_ptr< osg::Uniform > totalInstancesUniform =
            new osg::Uniform( "totalInstances", ( float )( rawVTKData->getDataCount() ) );
        ss->addUniform( totalInstancesUniform.get() );

        // Specify the time in seconds for a given streamline point to fade
        // from full intensity to zero intensity.
        // (May be altered with simulation time.)
        osg::ref_ptr< osg::Uniform > fadeTimeUniform =
            new osg::Uniform( "fadeTime", 1.f );
        ss->addUniform( fadeTimeUniform.get() );

        // Specify the time in seconds for the animation to loop.
        // (May be altered with simulation time.)
        osg::ref_ptr< osg::Uniform > repeatTimeUniform =
            new osg::Uniform( "repeatTime", 3.f );
        ss->addUniform( repeatTimeUniform.get() );

        // Note:
        // It turns out that SRC_ALPHA, ONE_MINUS_SRC_ALPHA actually is
        // non-saturating. Give it a color just shy of full intensity white,
        // and the result will never saturate to white no matter how many
        // times it is overdrawn.
        osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc(
            GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
        ss->setAttributeAndModes( bf.get() );

        // Note:
        // Leave the depth test enabled, but mask off depth writes (4th param is false).
        // This allows us to render the streamline points in any order, front to back
        // or back to front, and not lose any points by depth testing against themselves.
        //osg::ref_ptr< osg::Depth > depth = new osg::Depth( osg::Depth::LESS, 0., 1., false );
        //ss->setAttributeAndModes( depth.get() );

        // Note:
        // After drawing opaque objects, translucency can be order-independent only if
        // certain criteria are met:
        // 1. The alpha values of all pixels must be the same, OR
        //    The RGB valuues of all pixels must be the same.
        // 2. Depth write must be disabled so that far translucent pixels don't lose the
        //    depth test to near translucent pixels.
        // 3. The blend function must not reference destination alpha.


        // specify the position texture. The vertex shader will index into
        // this texture to obtain position values for each streamline point.
        //NOTE: Texture slot 1 is used by the splotch.png image
        {
            ss->setTextureAttribute( 0, rawVTKData->getPositionTexture() );
            osg::ref_ptr< osg::Uniform > texPosUniform =
                new osg::Uniform( "texPos", 0 );
            ss->addUniform( texPosUniform.get() );
        }

        {
            ss->setTextureAttribute( 2, rawVTKData->getScalarTexture() );
            osg::ref_ptr< osg::Uniform > texPosUniform =
                new osg::Uniform( "scalar", 2 );
            ss->addUniform( texPosUniform.get() );
        }

        {
            ss->setTextureAttribute( 3, rawVTKData->getDiameterTexture() );
            osg::ref_ptr< osg::Uniform > texPosUniform =
                new osg::Uniform( "diameter", 3 );
            ss->addUniform( texPosUniform.get() );
        }

        {
            // Pass the min/max for the scalar range into the shader as a uniform.
            osg::Vec2 ts( dataRange[ 0 ], dataRange[ 1 ] );
            osg::ref_ptr< osg::Uniform > scalarMinMaxUniform =
                new osg::Uniform( "scalarMinMax",
                                  osg::Vec2( float( ts.x() ), float( ts.y() ) ) );
            ss->addUniform( scalarMinMaxUniform.get() );

            // Set up the color spectrum.
            osg::Texture1D* texCS =
                new osg::Texture1D( rawVTKData->CreateColorTextures( dataRange ) );
            texCS->setFilter( osg::Texture::MIN_FILTER, osg::Texture2D::LINEAR );
            texCS->setFilter( osg::Texture::MAG_FILTER, osg::Texture2D::LINEAR );
            texCS->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );

            ss->setTextureAttribute( 4, texCS );
            osg::ref_ptr< osg::Uniform > texCSUniform =
                new osg::Uniform( "texCS", 4 );
            ss->addUniform( texCSUniform.get() );
        }
    }

}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::Geode* OSGParticleStage::createInstanced(
    const std::vector< ves::xplorer::DataSet* >& transData,
    const std::string& activeScalar,
    const std::string& activeVector )
{
    m_pointCollection.clear();

    m_transientDataSet = transData;
    m_activeVector = activeVector;
    m_activeScalar = activeScalar;

    ves::xplorer::util::FindVertexCellsCallback* findVertexCellsCbk =
        new ves::xplorer::util::FindVertexCellsCallback();
    ves::xplorer::util::DataObjectHandler* dataObjectHandler =
        new ves::xplorer::util::DataObjectHandler();
    dataObjectHandler->SetDatasetOperatorCallback( findVertexCellsCbk );

    size_t maxNumPoints = 0;
    for( size_t i = 0; i < m_transientDataSet.size(); ++i )
    {
        vtkDataObject* tempDataSet = m_transientDataSet.at( i )->GetDataSet();

        dataObjectHandler->OperateOnAllDatasetsInObject( tempDataSet );
        std::vector< std::pair< vtkIdType, double* > > tempCellGroups =
            findVertexCellsCbk->GetVertexCells();
        m_pointCollection.push_back( tempCellGroups );
        findVertexCellsCbk->ResetPointGroup();
        if( maxNumPoints < tempCellGroups.size() )
        {
            maxNumPoints = tempCellGroups.size();
        }
    }
    delete findVertexCellsCbk;

    ves::xplorer::util::GetScalarDataArraysCallback* getScalarDataArrayCbk =
        new ves::xplorer::util::GetScalarDataArraysCallback();
    dataObjectHandler->SetDatasetOperatorCallback( getScalarDataArrayCbk );
    for( size_t i = 0; i < m_transientDataSet.size(); ++i )
    {
        vtkDataObject* tempDataSet = m_transientDataSet.at( i )->GetDataSet();

        dataObjectHandler->OperateOnAllDatasetsInObject( tempDataSet );
        std::vector< std::pair< std::string, std::vector< double > > > tempCellGroups =
            getScalarDataArrayCbk->GetCellData();
        m_dataCollection.push_back( tempCellGroups );
        getScalarDataArrayCbk->ResetPointGroup();
    }

    delete getScalarDataArrayCbk;
    delete dataObjectHandler;

    /*for( size_t i = 0; i < transData.size(); ++i )
    {
        std::vector< std::pair< vtkIdType, double* > >* tempCellGroups = &m_pointCollection.at( i );
        //std::cout << transData.at( i )->GetFileName() << std::endl;
        for( size_t j = 0; j < tempCellGroups->size(); ++j )
        {
            double* tempData = tempCellGroups->at( j ).second;
            //std::cout << tempCellGroups->at( j ).first << " " << tempData[ 0 ] << " " << tempData[ 1 ] << " " << tempData[ 2 ] << std::endl;
        }
        //std::cout << std::endl;
    }*/

    m_bb[0] = 1000000;
    m_bb[1] = -1000000;
    m_bb[2] = 1000000;
    m_bb[3] = -1000000;
    m_bb[4] = 1000000;
    m_bb[5] = -1000000;

    ves::xplorer::scenegraph::VTKParticleTextureCreator::Point tempPoint;
    for( size_t i = 0; i < maxNumPoints; ++i )
    {
        //Iterate through all points
        std::deque< ves::xplorer::scenegraph::VTKParticleTextureCreator::Point > tempQueue;
        ///Add a pair for each scalar for each line
        std::vector< std::pair< std::string, std::vector< double > > > tempLineData;
        for( size_t k = 0; k < m_dataCollection.at( 0 ).size(); ++k )
        {
            std::vector< double > tempVec;
            tempLineData.push_back( std::make_pair< std::string, std::vector< double > >( m_dataCollection.at( 0 ).at( k ).first, tempVec ) );
        }

        for( size_t j = 0; j < m_transientDataSet.size(); ++j )
        {
            std::vector< std::pair< vtkIdType, double* > >* activeCellGroups =
                &m_pointCollection.at( j );
            std::vector< std::pair< std::string, std::vector< double > > >* dataCollection =
                &m_dataCollection.at( j );

            if( i < activeCellGroups->size() )
            {
                vtkIdType cellid = activeCellGroups->at( i ).first;
                double* pointid = activeCellGroups->at( i ).second;
                tempPoint.x[ 0 ] = pointid[ 0 ];
                tempPoint.x[ 1 ] = pointid[ 1 ];
                tempPoint.x[ 2 ] = pointid[ 2 ];
                tempPoint.vertId = cellid;
                //We can delete these hear because i never repeats and
                //is always moving forward
                delete [] pointid;
                activeCellGroups->at( i ).second = 0;
                for( size_t k = 0; k < dataCollection->size(); ++k )
                {
                    tempLineData.at( k ).second.push_back( dataCollection->at( k ).second.at( i ) );
                }
            }
            else
            {
                tempPoint.vertId = i;
                tempPoint.x[ 0 ] = 0.;
                tempPoint.x[ 1 ] = 0.;
                tempPoint.x[ 2 ] = 0.;
                for( size_t k = 0; k < dataCollection->size(); ++k )
                {
                    tempLineData.at( k ).second.push_back( 0.0 );
                }
            }
            tempQueue.push_back( tempPoint );
            //std::cout << tempCellGroups->at( i ).first << " "
            //    << tempCellGroups->at( i ).second << std::endl;
            //DataSet* tempData = transData.at( j );
            //vtkDataObject* tempDataObject = tempData->GetDataSet();
            if( tempPoint.x[0] < m_bb[0] )
            {
                m_bb[0] = tempPoint.x[0];
            }
            if( tempPoint.x[0] > m_bb[1] )
            {
                m_bb[1] = tempPoint.x[0];
            }
            if( tempPoint.x[1] < m_bb[2] )
            {
                m_bb[2] = tempPoint.x[1];
            }
            if( tempPoint.x[1] > m_bb[3] )
            {
                m_bb[3] = tempPoint.x[1];
            }
            if( tempPoint.x[2] < m_bb[4] )
            {
                m_bb[4] = tempPoint.x[2];
            }
            if( tempPoint.x[2] > m_bb[5] )
            {
                m_bb[5] = tempPoint.x[2];
            }
        }
        m_streamlineList.push_back( tempQueue );
        m_lineDataCollection.push_back( tempLineData );
        //std::cout << std::endl;
        //std::cout << std::endl;
        //std::cout << std::endl;
    }

    /*for( size_t i = 0; i < m_pointCollection.size(); ++i )
    {
        std::vector< std::pair< vtkIdType, double* > >* activeCellGroups =
            &m_pointCollection.at( i );
        size_t numParticleTracks = activeCellGroups->size();
        for( size_t j = 0; j < numParticleTracks; ++j )
        {
            delete [] activeCellGroups->at( j ).second;
        }
    }*/
    ///Clean up memory now that we have transferred it to the streamline list
    m_pointCollection.clear();
    m_dataCollection.clear();
    ves::xplorer::scenegraph::Geode* geode = new ves::xplorer::scenegraph::Geode();

    createStreamLines( geode );

    m_streamlineList.clear();
    m_lineDataCollection.clear();

    return geode;
}
////////////////////////////////////////////////////////////////////////////////
void OSGParticleStage::SetParticleDiameter( int pDiameter )
{
    m_particleDiameter = pDiameter;
}
////////////////////////////////////////////////////////////////////////////////
