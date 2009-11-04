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

#include <iostream>

#include <osg/Vec3>
#include <osg/Image>
#include <osg/Texture2D>

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
ves::xplorer::scenegraph::Geode* OSGWarpedSurfaceStage::createMesh(vtkPolyData* polydata, std::string displacement, std::string colorScalar)
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
void OSGWarpedSurfaceStage::createMeshData( osg::Geometry* geom, vtkPolyData* polydata, std::string disp, std::string colorScalar)
{
    polydata->Update();

    vtkTriangleFilter* triangleFilter = vtkTriangleFilter::New();
    triangleFilter->SetInput( polydata );
    triangleFilter->Update();
    
    vtkStripper* triangleStripper = vtkStripper::New();
    triangleStripper->SetInput(triangleFilter->GetOutput());
    triangleStripper->Update();
    
    polydata = triangleStripper->GetOutput();
    
    int numStrips = polydata->GetNumberOfStrips();
    //int numPts = polydata->GetNumberOfPoints();
    vtkPointData *pointData = polydata->GetPointData();

    if (pointData==NULL)
    {
        std::cout << " pd point data is null " << std::endl;
        return;
    }

    vtkDataArray *vectorArray = pointData->GetVectors(disp.c_str());
    vtkPoints *points = polydata->GetPoints();
    vtkDataArray *normals = pointData->GetVectors( "Normals" );
                        
    vtkDataArray* dataArray = pointData->GetScalars(colorScalar.c_str());
    double dataRange[2]; 
    
    dataArray->GetRange(dataRange);
    
    //Here we build a color look up table
    vtkLookupTable *lut = vtkLookupTable::New(); 
    lut->SetHueRange (0.667, 0);
    lut->SetRange(dataRange);
    lut->SetRampToLinear();
    //lut->SetRampToSCurve();
    //lut->SetRampToSQRT();
    lut->Build();

    double cVal;
    double curColor[3];
    
    osg::Vec3Array* v = new osg::Vec3Array;
    osg::ref_ptr< osg::Vec3Array> vDest = new osg::Vec3Array;
    osg::Vec3Array* n = new osg::Vec3Array;
    osg::Vec3Array* colors = new osg::Vec3Array;
    osg::Vec2Array* tc = new osg::Vec2Array;

    //int numCells = polydata->GetNumberOfCells();
    vtkCellArray* strips = polydata->GetStrips();

    //Number of vertex is potentially bigger than number of points, 
    //Since same point can appear in different triangle strip. 
    
    int numVetex= 0;
    vtkIdType *pts;
    vtkIdType cStripNp;    
    int stripNum=0;

    for (strips->InitTraversal(); ((stripNum<numStrips) && (strips->GetNextCell(cStripNp, pts))); stripNum++)
        numVetex+=cStripNp;

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
    
    stripNum=0;
    
    for (strips->InitTraversal(); ((stripNum<numStrips) && (strips->GetNextCell(cStripNp, pts))); stripNum++)
    {
        for (int i=0; i<cStripNp; i++)
        {
            points->GetPoint(pts[i], x);
            osg::Vec3 startVec( x[0], x[1], x[2] );
            normals->GetTuple(pts[i], cnormal);
            osg::Vec3 normal(cnormal[0],cnormal[1],cnormal[2]);
            
            v->push_back( startVec );
            n->push_back( normal );
            
            cVal = dataArray->GetTuple1(pts[i]);
            lut->GetColor(cVal,curColor);
            
            osg::Vec3 ccolor(curColor[0],curColor[1],curColor[2]);
            colors->push_back( ccolor);

            //coord is the cord in the texture for strip x and vertex y in the "scale term" of s and t

            int xx = (v->size()-1)%s;
            int yy = (v->size()-1)/s;
            osg::Vec2 coord( ((float)(xx)/s), ((float)(yy)/t));

            tc->push_back( coord );

            vectorArray->GetTuple(pts[i], displacement);
            osg::Vec3 destVec( x[0]+displacement[0], x[1]+displacement[1], x[2]+displacement[2] );
            vDest->push_back( destVec );
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
        
        for (int i=0; i<cStripNp; i++)
            deui->push_back( (unsigned int)(startVertexIdx+i));
        
        startVertexIdx+=cStripNp;

        geom->addPrimitiveSet( deui.get() );
    }

    osg::StateSet* ss = geom->getOrCreateStateSet();

    // Compute the difference between vDest and v. There are the offset vectors.
    float* vecs = new float[ s * t * 3 ];
    float* vecsPtr = vecs;
    float* vcolors = new float[ s * t * 3 ];
    float* colsPtr = vcolors;

    for( int i=0; i<s*t; i++ )
    {
        if( i >= numVetex )
        {
            *vecsPtr++ = 0.;
            *vecsPtr++ = 0.;
            *vecsPtr++ = 0.;
            *colsPtr++ = 0.;
            *colsPtr++ = 0.;
            *colsPtr++ = 0.;
        }
        else
        {
            osg::Vec3 vector( (*vDest)[ i ] - (*v)[ i ] );
            *vecsPtr++ = vector.x();
            *vecsPtr++ = vector.y();
            *vecsPtr++ = vector.z();
            *colsPtr++ = (*colors)[i].x();
            *colsPtr++ = (*colors)[i].y();
            *colsPtr++ = (*colors)[i].z();
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

    // specify the color texture.
    osg::Image* iColors = new osg::Image;
    iColors->setImage( s, t, 1, GL_RGB32F_ARB, GL_RGB, GL_FLOAT,
        (unsigned char*) vcolors, osg::Image::USE_NEW_DELETE );
    osg::Texture2D* texColors = new osg::Texture2D( iColors );
    texColors->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
    texColors->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );
    ss->setTextureAttribute( 1, texColors );
    
    {
        osg::ref_ptr< osg::Uniform > texColorUniform =
            new osg::Uniform( "texColor", 1 );
        ss->addUniform( texColorUniform.get() );
    }

    {
        osg::ref_ptr< osg::Uniform > surfaceWarpUniform =
            new osg::Uniform( "surfaceWarpScale", m_surfaceWarpScale );
        ss->addUniform( surfaceWarpUniform.get() );
    }
                
    std::string vertexSource =

        "uniform sampler2D texVec; \n"
        "uniform sampler2D texColor; \n"
        "uniform float osg_SimulationTime; \n"
        "uniform float surfaceWarpScale; \n"
        //Phong shading variables
        "varying vec3 color; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 objPos; \n"
        "varying vec3 eyePos; \n"
        "varying vec3 normal; \n"
        
        "void main() \n"
        "{ \n"

            "float a = mod( osg_SimulationTime*100.0, 314.0) * 0.01; \n"
            "float scalar =  sin(a) * surfaceWarpScale;\n"
            "vec4 vecOff = scalar * texture2D( texVec, gl_MultiTexCoord0.st ); \n"
            "vec4 tempColor = texture2D( texColor, gl_MultiTexCoord0.st ); \n"
            "vec4 position = vec4( (gl_Vertex.xyz + vecOff.xyz), gl_Vertex.w ); \n"
            
            "gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * position; \n"

            "tempColor[3]=1.0; \n"
            "gl_FrontColor = tempColor; \n"
        //Initialize phong shading variables
        "   color=tempColor.rgb; \n"
        "   objPos=gl_Vertex.xyz; \n"
        "   eyePos=vec3(gl_ModelViewMatrix*gl_Vertex); \n"
        "   lightPos=gl_LightSource[0].position.xyz; \n"
        "   normal=vec3(gl_NormalMatrix*gl_Normal); \n"
        "   gl_FrontSecondaryColor=vec4(1.0);\n"
        "   gl_BackSecondaryColor=vec4(0.0);\n"
        "} \n";
    
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    vertexShader->setType( osg::Shader::VERTEX );
    vertexShader->setShaderSource( vertexSource );
    
    std::string fragmentSource(
                        //"uniform vec3 ambientMaterial;\n"
                        //"uniform vec3 diffuseMaterial;\n"
                        //"uniform vec3 specularMaterial;\n"
                        //"uniform float specularPower;\n"
                        
                        "varying vec3 color;\n"
                        "varying vec3 lightPos;\n"
                        "varying vec3 objPos;\n"
                        "varying vec3 eyePos;\n"
                        "varying vec3 normal;\n"
                        
                        "void main()\n"
                        "{\n"
                        "    vec3 ambientMaterial = vec3( 0.368627, 0.368421, 0.368421 );\n"
                        "    vec3 diffuseMaterial = vec3( 0.886275, 0.885003, 0.885003 );\n"
                        "    vec3 specularMaterial = vec3( 0.490196, 0.488722, 0.488722 );\n"
                        "    float specularPower = 20.0;\n"
                        "\n"
                        "    vec3 N=normalize(normal);\n"
                        "    if(gl_SecondaryColor.r < .5)\n"
                        "    {\n"
                        "       N=-N; \n"
                        "    }\n"
                        "    vec3 L=normalize(lightPos);\n"
                        "    float NDotL=max(dot(N,L),0.0);\n"
                        
                        "    vec3 V=normalize(eyePos);\n"
                        "    vec3 R=reflect(V,N);\n"
                        "    float RDotL=max(dot(R,L),0.0);\n"
                        
                        "    vec3 TotalAmbient=gl_LightSource[0].ambient.rgb*ambientMaterial*color;\n"
                        "    vec3 TotalDiffuse=gl_LightSource[0].diffuse.rgb*diffuseMaterial*color*NDotL;\n"
                        "    vec3 TotalSpecular=gl_LightSource[0].specular.rgb*specularMaterial*pow(RDotL,specularPower);\n"
                        
                        "    gl_FragColor=vec4(TotalAmbient+TotalDiffuse+TotalSpecular,1.0);\n"
                        " }\n"
                        
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
