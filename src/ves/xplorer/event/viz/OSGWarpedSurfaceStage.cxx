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
#include <ves/xplorer/event/viz/OSGWarpedSurfaceStage.h>

#include <ves/xplorer/scenegraph/Geode.h>

#include <string>
#include <vtkPolyData.h>
#include <vtkLookupTable.h>
#include <vtkCellArray.h>
#include <vtkPoints.h>
#include <vtkDataArray.h>
#include <vtkPointData.h>
#include <vtkTriangleFilter.h>
#include <vtkStripper.h>
#include <vtkPolyDataNormals.h>

#include <iostream>

#include <osg/Vec3>
#include <osg/Image>
#include <osg/Texture2D>
#include <osg/Texture1D>
#include <osg/BlendFunc>

using namespace ves::xplorer::event::viz;

////////////////////////////////////////////////////////////////////////////////
OSGWarpedSurfaceStage::OSGWarpedSurfaceStage(void)
    :
    tm( 0 ),
    tn( 0 ),
    m_surfaceWarpScale( 50 )
{
}
////////////////////////////////////////////////////////////////////////////////
OSGWarpedSurfaceStage::~OSGWarpedSurfaceStage(void)
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
int OSGWarpedSurfaceStage::mylog2(unsigned x)
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
int OSGWarpedSurfaceStage::mypow2(unsigned x)
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
ves::xplorer::scenegraph::Geode* OSGWarpedSurfaceStage::createMesh(vtkPolyData* polydata, const std::string displacement, const std::string colorScalar)
{
    //osg::ref_ptr< osg::Group > grp = new osg::Group;
    ves::xplorer::scenegraph::Geode* geode = new ves::xplorer::scenegraph::Geode();
    //grp->addChild( geode );

    osg::Geometry* geom = new osg::Geometry();
    geode->addDrawable( geom );

    createMeshData( geom, polydata, displacement, colorScalar); 

    osg::Vec4Array* c = new osg::Vec4Array;
    c->push_back( osg::Vec4( 1., 1., 1., 1. ) );
    geom->setColorArray( c );
    geom->setColorBinding( osg::Geometry::BIND_OVERALL );

    return geode;
}
////////////////////////////////////////////////////////////////////////////////
void OSGWarpedSurfaceStage::createMeshData( osg::Geometry* geom, 
    vtkPolyData* polydata, 
    const std::string disp, 
    const std::string colorScalar)
{
    polydata->Update();
    vtkTriangleFilter* triangleFilter = vtkTriangleFilter::New();
    vtkPointData* pointData = polydata->GetPointData();
    vtkDataArray* normals = 0;
    /*
    vtkDataArray* normals = pointData->GetVectors( "Normals" );

    if( !normals )
    {
        vtkPolyDataNormals* normalGen = vtkPolyDataNormals::New();
        normalGen->SetInput( polydata );
        normalGen->NonManifoldTraversalOn();
        normalGen->AutoOrientNormalsOn();
        normalGen->ConsistencyOn();
        normalGen->SplittingOn();

        triangleFilter->SetInput( normalGen->GetOutput() );
        triangleFilter->Update();
        normalGen->Delete();
    }
    else*/
    {
        triangleFilter->SetInput( polydata );
        triangleFilter->Update();
    }
    
    vtkStripper* triangleStripper = vtkStripper::New();
    triangleStripper->SetInput(triangleFilter->GetOutput());
    triangleStripper->Update();
    
    polydata = triangleStripper->GetOutput();
    
    int numStrips = polydata->GetNumberOfStrips();
    //int numPts = polydata->GetNumberOfPoints();
    pointData = polydata->GetPointData();
    normals = pointData->GetVectors( "Normals" );

    if( pointData==NULL )
    {
        std::cout << " pd point data is null " << std::endl;
        return;
    }

    vtkDataArray* vectorArray = pointData->GetVectors(disp.c_str());
    vtkPoints* points = polydata->GetPoints();
                        
    vtkDataArray* dataArray = pointData->GetScalars(colorScalar.c_str());
    double dataRange[2]; 
    
    dataArray->GetRange(dataRange);
    
    //Here we build a color look up table
    vtkLookupTable* lut = vtkLookupTable::New(); 
    lut->SetHueRange(0.667, 0);
    lut->SetRange(dataRange);
    lut->SetRampToLinear();
    //lut->SetRampToSCurve();
    //lut->SetRampToSQRT();
    lut->Build();
    //lut->Print( std::cout );
    vtkIdType numTuples = lut->GetTable()->GetNumberOfTuples();
    vtkIdType numComponents = lut->GetTable()->GetNumberOfComponents();
    unsigned char* charLut = 0;
    //std::cout << " rgb " << lut->GetTable()->GetNumberOfTuples() << " " << lut->GetTable()->GetNumberOfComponents() << " " << charLut << std::endl;
   //unsigned char* charLut2= lut->GetPointer( 0 );
    //std::cout << sizeof( charLut2 ) << std::endl;
    float* newScalarLutArray = new float[ numTuples * 3 ];
    for( int i = 0; i < numTuples; ++i )
    {
        int numLuts = (i*numComponents);
        int numColorIndex = (i*3);
        charLut = lut->GetTable()->WritePointer( numLuts, 0 );
        //std::cout << sizeof( charLut ) << " " << charLut<<  " " << (double*)charLut << std::endl;
        newScalarLutArray[  numColorIndex + 0 ] = float( charLut[ 0 ] )/255.0f;
        newScalarLutArray[  numColorIndex + 1 ] = float( charLut[ 1 ] )/255.0f;
        newScalarLutArray[  numColorIndex + 2 ] = float( charLut[ 2 ] )/255.0f;
        //newScalarLutArray[  numLuts + 3 ] = float( charLut[ 3 ] )/255.0f;
        //std::cout << newScalarLutArray[  numLuts + 0 ] << " " 
        //    << newScalarLutArray[  numLuts + 1 ] << " "
        //    << newScalarLutArray[  numLuts + 2 ] << " " 
        //    << newScalarLutArray[  numLuts + 3 ] << std::endl;
    }
    //std::string tempString( charLut );
    double cVal;
    double curColor[3];
    
    osg::Vec3Array* v = new osg::Vec3Array;
    osg::ref_ptr< osg::Vec3Array> vDest = new osg::Vec3Array;
    std::vector< double > scalarArray;
    osg::Vec3Array* n = new osg::Vec3Array;
    osg::Vec3Array* colors = new osg::Vec3Array;
    osg::Vec2Array* tc = new osg::Vec2Array;

    //int numCells = polydata->GetNumberOfCells();
    vtkCellArray* strips = polydata->GetStrips();

    //Number of vertex is potentially bigger than number of points, 
    //Since same point can appear in different triangle strip. 
    
    int numVetex= 0;
    vtkIdType* pts;
    vtkIdType cStripNp;    
    int stripNum=0;

    for( strips->InitTraversal(); 
        ((stripNum<numStrips) && (strips->GetNextCell(cStripNp, pts))); 
        ++stripNum )
    {
        numVetex += cStripNp;
    }

    int am = mylog2(numVetex)+1;
    int mm = am/2;
    int nn = am -am/2;
    
   // Dimensions of the textures.
    unsigned int s = mypow2(mm);
    unsigned int t = mypow2(nn);

    double bounds[6];
    points->GetBounds(bounds);
    //VTK does bounds xmin, xmax,....
    //OSG does bounds xmin, ymin, zmin, xmax, ymax,...
    osg::BoundingBox bb(bounds[0]-1,bounds[2]-1,bounds[4]-1,bounds[1]+1,bounds[3]+1,bounds[5]+1);

    double x[3];
    double cnormal[3];
    double displacement[3];
    
    {
        osg::Vec3 destVec;
        osg::Vec3 ccolor;
        osg::Vec3 startVec;
        osg::Vec3 normal;
        osg::Vec2 coord;
        
        stripNum=0;
        
        for( strips->InitTraversal(); 
            (stripNum<numStrips) && (strips->GetNextCell(cStripNp, pts)); 
            stripNum++)
        {
            for( int i=0; i<cStripNp; ++i )
            {
                points->GetPoint(pts[i], x);
                startVec.set( x[0], x[1], x[2] );
                normals->GetTuple(pts[i], cnormal);
                normal.set(cnormal[0],cnormal[1],cnormal[2]);
                
                v->push_back( startVec );
                n->push_back( normal );
                
                cVal = dataArray->GetTuple1(pts[i]);
                scalarArray.push_back( cVal );
                lut->GetColor(cVal,curColor);
                
                ccolor.set(curColor[0],curColor[1],curColor[2]);
                colors->push_back( ccolor);
                
                //coord is the cord in the texture for strip x and vertex y in the "scale term" of s and t
                
                int xx = (v->size()-1)%s;
                int yy = (v->size()-1)/s;
                coord.set( ((float)(xx)/s), ((float)(yy)/t));
                
                tc->push_back( coord );
                
                if( vectorArray )
                {
                    vectorArray->GetTuple(pts[i], displacement);
                }
                else
                {
                    displacement[ 0 ] = 0.;
                    displacement[ 1 ] = 0.;
                    displacement[ 2 ] = 0.;
                }
                destVec.set( x[0]+displacement[0], x[1]+displacement[1], x[2]+displacement[2] );
                vDest->push_back( destVec );
            }
        }
    }
        
    //int cs = colors->size();
    // No data for the Destination normals, not sure what to do. I don't think Paul calculation for his mesh works here

    geom->setVertexArray( v );
    geom->setNormalArray( n );
    geom->setNormalBinding( osg::Geometry::BIND_PER_VERTEX );
    geom->setTexCoordArray( 0, tc );
    geom->setInitialBound( bb );

    stripNum=0;
    int startVertexIdx=0;
    for (strips->InitTraversal(); ((stripNum<numStrips) && (strips->GetNextCell(cStripNp, pts))); stripNum++)
    {
        if (cStripNp<=0)
            continue;

        osg::ref_ptr< osg::DrawElementsUInt > deui = new osg::DrawElementsUInt( GL_TRIANGLE_STRIP, 0 );
        
        for (int i=0; i<cStripNp; ++i)
        {    
            deui->push_back( (unsigned int)(startVertexIdx+i));
        }
        
        startVertexIdx+=cStripNp;

        geom->addPrimitiveSet( deui.get() );
    }

    osg::StateSet* ss = geom->getOrCreateStateSet();

    // Compute the difference between vDest and v. There are the offset vectors.
    int texFinalSize = s*t;
    float* vecs = new float[ texFinalSize * 3 ];
    float* vecsPtr = vecs;
    float* vcolors = new float[ texFinalSize ];
    //float* vcolors = new float[ texFinalSize * 3 ];
    float* colsPtr = vcolors;

    for( int i=0; i< texFinalSize; i++ )
    {
        if( i >= numVetex )
        {
            *vecsPtr++ = 0.;
            *vecsPtr++ = 0.;
            *vecsPtr++ = 0.;
            //*colsPtr++ = 0.;
            //*colsPtr++ = 0.;
            *colsPtr++ = 0.;
        }
        else
        {
            osg::Vec3 vector( (*vDest)[ i ] - (*v)[ i ] );
            *vecsPtr++ = vector.x();
            *vecsPtr++ = vector.y();
            *vecsPtr++ = vector.z();
            //*colsPtr++ = (*colors)[i].x();
            //*colsPtr++ = (*colors)[i].y();
            //*colsPtr++ = (*colors)[i].z();
            *colsPtr++ = scalarArray.at( i );
        }
   }

    // specify the vector offset texture. The vertex shader will index into
    // this texture to obtain a vector to offset each xyz vertex.
    osg::Image* iVecs = new osg::Image;
    iVecs->setImage( s, t, 1, GL_RGB32F_ARB, GL_RGB, GL_FLOAT,
        (unsigned char*) vecs, osg::Image::USE_NEW_DELETE );
    osg::Texture2D* texVecs = new osg::Texture2D( iVecs );
    texVecs->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
    texVecs->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );
    ss->setTextureAttribute( 0, texVecs );

    osg::ref_ptr< osg::Uniform > texVecUniform =
        new osg::Uniform( "texVec", 0 );
    ss->addUniform( texVecUniform.get() );

    {
        // specify the color texture.
        osg::Image* iColors = new osg::Image;
        //iColors->setImage( s, t, 1, GL_RGB32F_ARB, GL_RGB, GL_FLOAT,
        //                  (unsigned char*) vcolors, osg::Image::USE_NEW_DELETE );
        iColors->setImage( s, t, 1, GL_ALPHA32F_ARB, GL_ALPHA, GL_FLOAT,
                          (unsigned char*) vcolors, osg::Image::USE_NEW_DELETE );
        osg::Texture2D* texColors = new osg::Texture2D( iColors );
        texColors->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
        texColors->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );
        ss->setTextureAttribute( 1, texColors );

        //osg::ref_ptr< osg::Uniform > texColorUniform =
        //    new osg::Uniform( "texColor", 1 );
        osg::ref_ptr< osg::Uniform > texColorUniform =
            new osg::Uniform( "scalar", 1 );
        ss->addUniform( texColorUniform.get() );
    }

    {
        osg::ref_ptr< osg::Uniform > surfaceWarpUniform =
            new osg::Uniform( "surfaceWarpScale", m_surfaceWarpScale );
        ss->addUniform( surfaceWarpUniform.get() );
    }

    {
        osg::ref_ptr< osg::Uniform > surfaceWarpUniform =
        new osg::Uniform( "twoSideLighting", false );
        ss->addUniform( surfaceWarpUniform.get() );
    }
    
    {
        osg::ref_ptr< osg::Uniform > opacityUniform =
            new osg::Uniform( "opacityVal", float( 1.) );
        ss->addUniform( opacityUniform.get() );
        
        osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc();
        bf->setFunction( osg::BlendFunc::SRC_ALPHA, 
                        osg::BlendFunc::ONE_MINUS_SRC_ALPHA );
        ss->setMode( GL_BLEND, osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        ss->setAttributeAndModes( bf.get(), osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }
    
    {
        // Pass the min/max for the scalar range into the shader as a uniform.
        osg::Vec2s ts( dataRange[ 0 ], dataRange[ 1 ] );//- (dataRange[ 1 ]*0.10) );
        osg::ref_ptr< osg::Uniform > scalarMinMaxUniform =
            new osg::Uniform( "scalarMinMax", osg::Vec2( (float)ts.x(), (float)ts.y() ) );
        ss->addUniform( scalarMinMaxUniform.get() );
    }
    
    {        
        // Set up the color spectrum.
        osg::Image* iColorScale = new osg::Image;
        iColorScale->setImage( int( numTuples ), 1, 1, GL_RGB32F_ARB, GL_RGB, GL_FLOAT,
                              (unsigned char*)newScalarLutArray, osg::Image::NO_DELETE );
        osg::Texture1D* texCS = new osg::Texture1D( iColorScale );
        texCS->setFilter( osg::Texture::MIN_FILTER, osg::Texture2D::LINEAR);
        texCS->setFilter( osg::Texture::MAG_FILTER, osg::Texture2D::LINEAR );
        texCS->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
        
        ss->setTextureAttribute( 2, texCS );
        osg::ref_ptr< osg::Uniform > texCSUniform = 
            new osg::Uniform( "texCS", 2 );
        ss->addUniform( texCSUniform.get() );        
    }

    std::string vertexSource =

        "uniform sampler2D texVec; \n"
        "uniform sampler1D texCS; \n"
        //"uniform sampler2D texColor; \n"
        //"uniform sampler2D texColor; \n"
        "uniform sampler2D scalar;\n"
        "uniform float osg_SimulationTime; \n"
        "uniform float surfaceWarpScale; \n"
        "uniform vec2 scalarMinMax;\n"
        "uniform float opacityVal;\n"

        //Phong shading variables
        "varying vec3 color; \n"
        "varying vec3 lightPos; \n"
        //"varying vec3 objPos; \n"
        "varying vec3 eyePos; \n"
        "varying vec3 normal; \n"
        
        "void main() \n"
        "{ \n"

        "   float a = mod( osg_SimulationTime*100.0, 314.0) * 0.01; \n"
        "   float warpScale =  sin(a) * surfaceWarpScale;\n"
        "   vec4 vecOff = warpScale * texture2D( texVec, gl_MultiTexCoord0.st ); \n"
        "   vec4 position = vec4( (gl_Vertex.xyz + vecOff.xyz), gl_Vertex.w ); \n"
            
        "   gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * position; \n"

        //"   vec4 tempColor = texture2D( texColor, gl_MultiTexCoord0.st ); \n"
        //"   tempColor[3]=1.0; \n"
        //"   gl_FrontColor = tempColor; \n"
        //Initialize phong shading variables
        //"   color=tempColor.rgb; \n"
            
        "   // Scalar texture containg key to color table. \n"
        "   vec4 activeScalar = texture2D( scalar, gl_MultiTexCoord0.st );\n"
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
        //"   gl_FrontColor = colorResult; \n"
        "     color = colorResult.rgb; \n"

        "     // Setup varying variables. \n"
        //"   objPos=gl_Vertex.xyz; \n"
        "     eyePos=vec3(gl_ModelViewMatrix*gl_Vertex); \n"
        "     lightPos=gl_LightSource[0].position.xyz; \n"
        //"   normal=vec3(gl_NormalMatrix*(gl_Normal+ normalize(vecOff.xyz) ) ); \n"
        "     normal=vec3(gl_NormalMatrix * gl_Normal); \n"
        "     gl_FrontSecondaryColor=vec4(1.0);\n"
        "     gl_BackSecondaryColor=vec4(0.0);\n"
        "     gl_BackColor = vec4( color, opacityVal);\n"
        "     gl_FrontColor = vec4( color, opacityVal);\n"
        "} \n";
    
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    vertexShader->setType( osg::Shader::VERTEX );
    vertexShader->setShaderSource( vertexSource );
    
    std::string fragmentSource(
        //"uniform vec3 ambientMaterial;\n"
        //"uniform vec3 diffuseMaterial;\n"
        //"uniform vec3 specularMaterial;\n"
        //"uniform float specularPower;\n"
        "uniform float opacityVal;\n"
        
        "varying vec3 color;\n"
        "varying vec3 lightPos;\n"
        //"varying vec3 objPos;\n"
        "varying vec3 eyePos;\n"
        "varying vec3 normal;\n"
        "\n"
        "void main()\n"
        "{\n"
        "    vec3 ambientMaterial = vec3( 0.368627, 0.368421, 0.368421 );\n"
        "    vec3 diffuseMaterial = vec3( 0.886275, 0.885003, 0.885003 );\n"
        "    vec3 specularMaterial = vec3( 0.490196, 0.488722, 0.488722 );\n"
        "    float specularPower = 20.0;\n"
        "\n"
        "    vec3 N=normalize(normal);\n"
        "    if(gl_SecondaryColor.r < .5)\n"
        //"    if( !gl_FrontFacing )\n"
        "    {\n"
        "       N=-N; \n"
        "    }\n"
        "    vec3 L=normalize(lightPos);\n"
        "    float NDotL=max(dot(N,L),0.0);\n"
        "\n"
        "    vec3 V=normalize(eyePos);\n"
        "    vec3 R=reflect(V,N);\n"
        "    float RDotL=max(dot(R,L),0.0);\n"
        "\n"
        "    vec3 TotalAmbient=gl_LightSource[0].ambient.rgb*ambientMaterial*color;\n"
        "    vec3 TotalDiffuse=gl_LightSource[0].diffuse.rgb*diffuseMaterial*color*NDotL;\n"
        "    vec3 TotalSpecular=gl_LightSource[0].specular.rgb*specularMaterial*pow(RDotL,specularPower);\n"
        "\n"
        "    vec4 newColor = vec4(TotalAmbient+TotalDiffuse+TotalSpecular,opacityVal);\n"
        "    gl_FragColor = newColor;\n"
        /*"    if(gl_SecondaryColor.r < .5)\n"
        "    {\n"
        "         gl_BackColor = newColor;\n"
        "    }\n"
        "    else\n"
        "    {\n"
        "         gl_FrontColor = newColor;\n"
        "    }\n"*/
        "}\n"
        );
    
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );
    ss->setAttribute( program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    
    triangleStripper->Delete();
    triangleFilter->Delete();
    lut->Delete();
}
////////////////////////////////////////////////////////////////////////////////
void OSGWarpedSurfaceStage::SetSurfaceWarpScale( float surfaceScale )
{
    m_surfaceWarpScale = surfaceScale;
}
////////////////////////////////////////////////////////////////////////////////
