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
#include <ves/xplorer/event/viz/OSGVectorStage.h>

#include <ves/xplorer/scenegraph/VTKTextureCreator.h>

#include <osg/PositionAttitudeTransform>
#include <osg/Texture>
#include <osg/Texture2D>
#include <osgDB/ReadFile>

#include <vtkLookupTable.h>
#include <vtkPolyData.h>

#include <iostream>

#include <cmath>

#include <ves/xplorer/scenegraph/Geode.h>

////////////////////////////////////////////////////////////////////////////////
OSGVectorStage::OSGVectorStage(void)
{
    tm=tn=0;
}
////////////////////////////////////////////////////////////////////////////////
OSGVectorStage::~OSGVectorStage(void)
{
}
////////////////////////////////////////////////////////////////////////////////
void OSGVectorStage::createArrow( osg::Geometry& geom, int nInstances, float scaleFactor )
{
    float sD( .05 ); // shaft diameter
    float hD( .075 ); // head diameter
    float len( 1. ); // length
    float sh( .65 ); // length from base to start of head

	sD = scaleFactor * sD;
	hD = scaleFactor * hD;
	len = scaleFactor * len;
	sh = scaleFactor * sh;

    osg::Vec3Array* v = new osg::Vec3Array;
    v->resize( 22 );
    geom.setVertexArray( v );

    osg::Vec3Array* n = new osg::Vec3Array;
    n->resize( 22 );
    geom.setNormalArray( n );
    geom.setNormalBinding( osg::Geometry::BIND_PER_VERTEX );

    // Shaft
    (*v)[ 0 ] = osg::Vec3( sD, 0., 0. );
    (*v)[ 1 ] = osg::Vec3( sD, 0., sh );
    (*v)[ 2 ] = osg::Vec3( 0., -sD, 0. );
    (*v)[ 3 ] = osg::Vec3( 0., -sD, sh );
    (*v)[ 4 ] = osg::Vec3( -sD, 0., 0. );
    (*v)[ 5 ] = osg::Vec3( -sD, 0., sh );
    (*v)[ 6 ] = osg::Vec3( 0., sD, 0. );
    (*v)[ 7 ] = osg::Vec3( 0., sD, sh );
    (*v)[ 8 ] = osg::Vec3( sD, 0., 0. );
    (*v)[ 9 ] = osg::Vec3( sD, 0., sh );

    (*n)[ 0 ] = osg::Vec3( 1., 0., 0. );
    (*n)[ 1 ] = osg::Vec3( 1., 0., 0. );
    (*n)[ 2 ] = osg::Vec3( 0., -1., 0. );
    (*n)[ 3 ] = osg::Vec3( 0., -1., 0. );
    (*n)[ 4 ] = osg::Vec3( -1., 0., 0. );
    (*n)[ 5 ] = osg::Vec3( -1., 0., 0. );
    (*n)[ 6 ] = osg::Vec3( 0., 1., 0. );
    (*n)[ 7 ] = osg::Vec3( 0., 1., 0. );
    (*n)[ 8 ] = osg::Vec3( 1., 0., 0. );
    (*n)[ 9 ] = osg::Vec3( 1., 0., 0. );

    //if( nInstances > 1 )
        geom.addPrimitiveSet( new osg::DrawArrays( GL_QUAD_STRIP, 0, 10, nInstances ) );
    //else
    //    geom.addPrimitiveSet( new osg::DrawArrays( GL_QUAD_STRIP, 0, 10 ) );

    // Head
    (*v)[ 10 ] = osg::Vec3( hD, -hD, sh );
    (*v)[ 11 ] = osg::Vec3( hD, hD, sh );
    (*v)[ 12 ] = osg::Vec3( 0., 0., len );
    osg::Vec3 norm = ((*v)[ 11 ] - (*v)[ 10 ]) ^ ((*v)[ 12 ] - (*v)[ 10 ]);
    norm.normalize();
    (*n)[ 10 ] = norm;
    (*n)[ 11 ] = norm;
    (*n)[ 12 ] = norm;

    (*v)[ 13 ] = osg::Vec3( hD, hD, sh );
    (*v)[ 14 ] = osg::Vec3( -hD, hD, sh );
    (*v)[ 15 ] = osg::Vec3( 0., 0., len );
    norm = ((*v)[ 14 ] - (*v)[ 13 ]) ^ ((*v)[ 15 ] - (*v)[ 13 ]);
    norm.normalize();
    (*n)[ 13 ] = norm;
    (*n)[ 14 ] = norm;
    (*n)[ 15 ] = norm;

    (*v)[ 16 ] = osg::Vec3( -hD, hD, sh );
    (*v)[ 17 ] = osg::Vec3( -hD, -hD, sh );
    (*v)[ 18 ] = osg::Vec3( 0., 0., len );
    norm = ((*v)[ 17 ] - (*v)[ 16 ]) ^ ((*v)[ 18 ] - (*v)[ 16 ]);
    norm.normalize();
    (*n)[ 16 ] = norm;
    (*n)[ 17 ] = norm;
    (*n)[ 18 ] = norm;

    (*v)[ 19 ] = osg::Vec3( -hD, -hD, sh );
    (*v)[ 20 ] = osg::Vec3( hD, -hD, sh );
    (*v)[ 21 ] = osg::Vec3( 0., 0., len );
    norm = ((*v)[ 20 ] - (*v)[ 19 ]) ^ ((*v)[ 21 ] - (*v)[ 19 ]);
    norm.normalize();
    (*n)[ 19 ] = norm;
    (*n)[ 20 ] = norm;
    (*n)[ 21 ] = norm;

    //if( nInstances > 1 )
        geom.addPrimitiveSet( new osg::DrawArrays( GL_TRIANGLES, 10, 12, nInstances ) );
    //else
    //    geom.addPrimitiveSet( new osg::DrawArrays( GL_TRIANGLES, 10, 12 ) );
}
////////////////////////////////////////////////////////////////////////////////
float* OSGVectorStage::createPositionArray( int m, int n , vtkPoints* points)
{
    float* pos = new float[ m * n * 3 ];
    float* posI = pos;

    int np = points->GetNumberOfPoints();
    double x[3];
    //float y[3];
    for (int i=0; i<m*n; i++)
    {
        if (i<np)
        {            
            points->GetPoint(i, x);
            *posI++=(float)x[0];
            *posI++=(float)x[1];
            *posI++=(float)x[2]; 
            //y[ 0 ] = x[0];
            //y[ 1 ] = x[1];
            //y[ 2 ] = x[2];
        }
        else
        {
            *posI++ = 0.;
            *posI++ =  0.;
            *posI++ = 0.;
            //y[ 0 ] = 0.0f;
            //y[ 1 ] = 0.0f;
            //y[ 2 ] = 0.0f;
        }
        //std::cout << "pos " << y[ 0] << " " << y[ 1] << " " << y[ 2 ] << std::endl;
    }
   
    return pos;
}
////////////////////////////////////////////////////////////////////////////////
float* OSGVectorStage::createAttitudeArray( int m, int n, vtkDataArray* dataArray)
{
    float* att = new float[ m * n * 3 ];
    float* attI = att;

    int nd = dataArray->GetNumberOfTuples();
    double x[3];
    for (int i=0; i<m*n; i++)
    {
        if (i<nd)
        {
            dataArray->GetTuple(i,x);
            osg::Vec3 v( x[0], x[1], x[2] );
            v.normalize();
            *attI++ = v.x();
            *attI++ = v.y();
            *attI++ = v.z();
        }
        else
        {
            *attI++ = 0.;
            *attI++ = 0.;
            *attI++ = 0.;
        }
    }
    return att;
}
////////////////////////////////////////////////////////////////////////////////
float* OSGVectorStage::createScalarArray( int m, int n, vtkDataArray* dataArray)
{
    float* sca = new float[ m * n * 3 ];
    float* scaI = sca;
    double dataRange[2]; 
    
    dataArray->GetRange(dataRange);
    
    //Here we build a color look up table
    vtkLookupTable *lut = vtkLookupTable::New(); 
    lut->SetHueRange (0.667, 0.0);
    lut->SetRange(dataRange);
    lut->SetRampToLinear();
    lut->Build();
    
    int nd = dataArray->GetNumberOfTuples();
    
    double x;
    double rgb[3];
    for (int i=0; i<m*n; i++)
    {
        if (i<nd)
        {
            dataArray->GetTuple(i,&x);
            lut->GetColor(x,rgb);
            //*scaI++ = x;
            //*scaI++ = 0.;
            //*scaI++ = 0.;
            *scaI++ = rgb[0];
            *scaI++ = rgb[1];
            *scaI++ = rgb[2];
        }
        else
        {
            *scaI++ = 0.;
            *scaI++ =  0.;
            *scaI++ = 0.;
        }
    }
    lut->Delete();
    return sca;
}
////////////////////////////////////////////////////////////////////////////////
int OSGVectorStage::mylog2(unsigned x)
{
    int l = -1; // mylog2(0) will return -1
    while (x != 0u)
    {
        x = x >> 1u;
        ++l;
    }
    return l;
}
////////////////////////////////////////////////////////////////////////////////
int OSGVectorStage::mypow2(unsigned x)
{
    int l = 1; // mypow2(0) will return 1
    while (x != 0u)
    {
        l = l << 1u;
        x--;
    }
    return l;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::Geode* OSGVectorStage::createInstanced(vtkPolyData* glyph, std::string vectorName, std::string scalarName, float scaleFactor )
{
    //std::cout << "creating osg planes" << std::endl;
    //Now pull in the vtk data
    vtkPolyData *polyData = glyph;
    if (polyData==NULL)
    {
        std::cout << "pd is null " << std::endl;
        return NULL;
    }
    polyData->Update();

    vtkPointData *pointData = polyData->GetPointData();
    if (pointData==NULL)
    {
        std::cout << " pd point data is null " << std::endl;
        return NULL;
    }
    //pointData->Update();

    vtkPoints *points = polyData->GetPoints();    
    if (points==NULL)
    {
        std::cout << " points are null " << std::endl;
        return NULL;
    }
    
    vtkDataArray *vectorArray = pointData->GetVectors(vectorName.c_str());
    vtkDataArray *scalarArray = pointData->GetScalars(scalarName.c_str());

    if (vectorArray==NULL)
    {
        std::cout << " vectors are null " << std::endl;
        return NULL;
    }

    if (scalarArray==NULL)
    {
        std::cout << " scalars are null " << std::endl;
        return NULL;
    }

    //osg::Group* grp = new osg::Group;
    //osg::Geode* geode = new osg::Geode;
    ves::xplorer::scenegraph::Geode* geode = new ves::xplorer::scenegraph::Geode();
    osg::Geometry* geom = new osg::Geometry;
    geom->setUseDisplayList( false );
    geom->setUseVertexBufferObjects( true );

    osg::ref_ptr< ves::xplorer::scenegraph::VTKTextureCreator > rawVTKData = new ves::xplorer::scenegraph::VTKTextureCreator();
    //ves::xplorer::scenegraph::VTKTextureCreator* rawVTKData = new ves::xplorer::scenegraph::VTKTextureCreator();
    rawVTKData->SetPolyData( glyph );
    rawVTKData->SetActiveVectorAndScalar( vectorName, scalarName );
    rawVTKData->loadData();

    createArrow( *geom, rawVTKData->getDataCount(), 1.0f );//scaleFactor );
    geode->addDrawable( geom );
    //grp->addChild( geode );

    geom->setInitialBound( rawVTKData->getBoundingBox() );

    //osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader( osg::Shader::VERTEX );
    //vertexShader->loadShaderSourceFromFile( osgDB::findDataFile( "vectorfield.vs" ) );

    //osg::ref_ptr< osg::Program > program = new osg::Program();
    //program->addShader( vertexShader.get() );

    //Create the rendering shader
    std::string vertexSource =

        "uniform vec3 sizes; \n"
        "uniform sampler3D texPos; \n"
        "uniform sampler3D texDir; \n"
        "uniform sampler3D scalar; \n"
        "uniform float userScale;\n"
        "uniform float modulo;\n"
        " \n"
        // There is no straightforward way to "discard" a vertex in a vertex shader,
        // (unlike discard in a fragment shader). So we use an aspect of clipping to
        // "clip away" unwanted vertices and vectors. Here's how it works:
        // The gl_Position output of the vertex shader is an xyzw value in clip coordinates,
        // with -w < xyz < w. All xyz outside the range -w to w are clipped by hardware
        // (they are outside the view volume). So, to discard an entire vector, we set all
        // its gl_Positions to (1,1,1,0). All vertices are clipped because -0 < 1 < 0 is false.
        // If all vertices for a given instance have this value, the entire instance is
        // effectively discarded.
        "bool\n"
        "discardInstance( const in float fiid )\n"
        "{\n"
        "    bool discardInstance = ( mod( fiid, modulo ) > 0.0 );\n"
        "    if( discardInstance )\n"
        "        gl_Position = vec4( 1.0, 1.0, 1.0, 0.0 );\n"
        "    return( discardInstance );\n"
        "}\n"
        " \n"
        // Based on the global 'sizes' uniform that contains the 3D stp texture dimensions,
        // and the input parameter current instances, generate an stp texture coord that
        // indexes into a texture to obtain data for this instance.
        "vec3 \n"
        "generateTexCoord( const in float fiid ) \n"
        "{ \n"
        "    float p1 = fiid / (sizes.x*sizes.y); \n"
        "    float t1 = fract( p1 ) * sizes.y; \n"

        "    vec3 tC; \n"
        "    tC.s = fract( t1 ); \n"
        "    tC.t = floor( t1 ) / sizes.y; \n"
        "    tC.p = floor( p1 ) / sizes.z; \n"

        "    return( tC ); \n"
        "} \n"
        "vec4 \n"
        "simpleLighting( const in vec4 color, const in vec3 normal, const in float diffCont, const in float ambCont ) \n"
        "{ \n"
        "    const vec4 amb = color * ambCont; \n"
        "    const vec3 eyeVec = vec3( 0.0, 0.0, 1.0 ); \n"
        "    const float dotVal = max( dot( normal, eyeVec ), 0.0 ); \n"
        "    const vec4 diff = color * dotVal * diffCont; \n"
        "    return( amb + diff ); \n"
        "} \n" //25
        " \n"
        "mat3 \n"
        "makeOrientMat( const in vec3 dir ) \n"
        "{ \n"
        // Compute a vector at a right angle to the direction.
        // First try projection direction into xy rotated -90 degrees.
        // If that gives us almost the same vector we started with,
        // then project into yz instead, rotated 90 degrees.
        "   vec3 c = vec3( dir.y, -dir.x, 0.0 ); \n"
        "   if( dot( c, c ) < 0.1 ) \n"
        "   { \n"
        "       c = vec3( 0.0, dir.z, -dir.y ); \n"
        "   } \n"
        //normalize( c.xyz );
        "   float l = length( c );\n"
        "   c /= l;\n"    
        " \n"
        "   const vec3 up = normalize( cross( dir, c ) ); \n"
        " \n"
        // Orientation uses the cross product vector as x,
        // the up vector as y, and the direction vector as z.
        "   return( mat3( c, up, dir ) ); \n"
        "} \n"
        " \n"
        "void main() \n"
        "{ \n"
        "    float fiid = gl_InstanceID; \n"
        "    if( discardInstance( fiid ) )\n"
        "        return;\n"
        "\n"
        // Generate stp texture coords from the instance ID.
        "    vec3 tC = generateTexCoord( fiid ); \n"

        // Create orthonormal basis to position and orient this instance.
        //"   vec4 newZ = texture3D( texDir, tC ); \n"
        //"   vec3 newX = cross( newZ.xyz, vec3( 0,0,1 ) ); \n"
        //"   normalize( newX ); \n"
        //"   vec3 newY = cross( newZ.xyz, newX ); \n"
        //"   normalize( newY ); \n"
        //"   vec4 pos = texture3D( texPos, tC ); \n"
        //"   mat4 mV = mat4( newX.x, newX.y, newX.z, 0., newY.x, newY.y, newY.z, 0., newZ.x, newZ.y, newZ.z, 0., pos.x, pos.y, pos.z, 1. ); \n"
        //"   gl_Position = (gl_ModelViewProjectionMatrix * mV * gl_Vertex); \n"
        "   vec4 pos = texture3D( texPos, tC ); \n"
        // Create an orientation matrix. Orient/transform the arrow.
        // Sample (look up) direction vector and obtain the scale factor
        "   vec4 dir = texture3D( texDir, tC );\n"
        "   mat3 orientMat = makeOrientMat( normalize( dir.xyz ) ); \n"
        "   float scale = userScale * length( dir.xyz );\n"
        "   vec3 oVec = orientMat * (scale * gl_Vertex.xyz);\n"
        "   vec4 hoVec = vec4( oVec + pos, 1.0 ); \n"
        "   gl_Position = gl_ModelViewProjectionMatrix * hoVec; \n"

        // Use just the orientation components to transform the normal.
        //"   mat3 mN = mat3( newX, newY, newZ ); \n"
        //"   vec3 norm = normalize(gl_NormalMatrix * mN * gl_Normal); \n"
        // Orient the normal.
        "   vec3 norm = normalize( gl_NormalMatrix * orientMat * gl_Normal ); \n"
        // Diffuse lighting with light at the eyepoint.
        "   vec4 color = texture3D( scalar, tC ); \n"
        //"   color = color * dot( norm, vec3( 0, 0, 1 ) ); \n"
        //"   color[3]=1; \n"
        //"   gl_FrontColor = vec4( color ); \n"
        // Compute color and lighting.
        //const vec4 scalarV = texture3D( scalar, tC );
        //const vec4 oColor = texture1D( texCS, scalarV.a );
        "   gl_FrontColor = simpleLighting( color, norm, 0.7, 0.3 ); \n"
        "} \n";

    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    vertexShader->setType( osg::Shader::VERTEX );
    vertexShader->setShaderSource( vertexSource );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( vertexShader.get() );


    osg::StateSet* ss = geom->getOrCreateStateSet();
    ss->setAttribute( program.get(),
    osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    // Posidion array.
    ss->setTextureAttribute( 0, rawVTKData->getPositionTexture() );
    osg::ref_ptr< osg::Uniform > texPosUniform =
    new osg::Uniform( "texPos", 0 );
    ss->addUniform( texPosUniform.get() );

    // Direction array.
    ss->setTextureAttribute( 1, rawVTKData->getDirectionTexture() );
    osg::ref_ptr< osg::Uniform > texDirUniform =
    new osg::Uniform( "texDir", 1 );
    ss->addUniform( texDirUniform.get() );

    // Scalar array.
    ss->setTextureAttribute( 2, rawVTKData->getScalarTexture() );
    osg::ref_ptr< osg::Uniform > texScalarUniform =
    new osg::Uniform( "scalar", 2 );
    ss->addUniform( texScalarUniform.get() );

    {
        // Pass the 3D texture dimensions to the shader as a "sizes" uniform.
        osg::Vec3s ts( rawVTKData->getTextureSizes() );
        osg::ref_ptr< osg::Uniform > sizesUniform =
            new osg::Uniform( "sizes", osg::Vec3( (float)ts.x(), (float)ts.y(), (float)ts.z() ) );
        ss->addUniform( sizesUniform.get() );
    }

    {
        // Pass the scale to the  "scaleUniform" uniform.
        osg::ref_ptr< osg::Uniform > scaleUniform =
            new osg::Uniform( "userScale", scaleFactor );
        ss->addUniform( scaleUniform.get() );
    }
    
    {
        osg::ref_ptr< osg::Uniform > uModulo = 
            new osg::Uniform( "modulo", 1.0f );
        uModulo->setDataVariance( osg::Object::DYNAMIC );
        ss->addUniform( uModulo.get() );
    }
    
    // Set up the color spectrum.
    //osg::Image* iColorScale = new osg::Image;
    //iColorScale->setImage( 8, 1, 1, GL_RGBA, GL_RGB, GL_FLOAT,
    //(unsigned char*)colorScale, osg::Image::NO_DELETE );
    //osg::Texture1D* texCS = new osg::Texture1D( iColorScale );
    //texCS->setFilter( osg::Texture::MIN_FILTER, osg::Texture2D::LINEAR);
    //texCS->setFilter( osg::Texture::MAG_FILTER, osg::Texture2D::LINEAR );

    //ss->setTextureAttribute( 3, texCS );
    //osg::ref_ptr< osg::Uniform > texCSUniform = new osg::Uniform( "texCS", 3 );
    //ss->addUniform( texCSUniform.get() );

    return geode;
}
////////////////////////////////////////////////////////////////////////////////
