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
 * Date modified: $Date: 2009-10-15 22:09:35 -0500 (Thu, 15 Oct 2009) $
 * Version:       $Rev: 13580 $
 * Author:        $Author: mccdo $
 * Id:            $Id: OSGWarpedSurfaceStage.h 13580 2009-10-16 03:09:35Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/xplorer/event/viz/OSGStreamlineStage.h>
#include <string>

#include <osg/PositionAttitudeTransform>
#include <osg/Texture>
#include <osg/Texture2D>
#include <osgDB/ReadFile>

#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkCleanPolyData.h>

#include <iostream>

#include <cmath>

#include <ves/xplorer/scenegraph/Geode.h>

using namespace std;

OSGStreamlineStage::OSGStreamlineStage(void)
{

}

OSGStreamlineStage::~OSGStreamlineStage(void)
{
}

void OSGStreamlineStage::createSLPoint( osg::Geometry& geom, int nInstances, const osg::Vec3 position, const osg::Vec4 color )
{
    // Configure a Geometry to draw a single point, but use the draw instanced PrimitiveSet
    // to draw the point multiple times.
    osg::Vec3Array* v = new osg::Vec3Array;
    v->resize( 1 );
    geom.setVertexArray( v );
    (*v)[ 0 ] = position;

    // Streamline color. Blending is non-saturating, so it never
    // reaches full intensity white. Alpha is modulated with the
    // point sprint texture alpha, so the value here is a maximum
    // for the "densist" part of the point sprint texture.
    osg::Vec4Array* c = new osg::Vec4Array;
    c->resize( 1 );
    geom.setColorArray( c );
    geom.setColorBinding( osg::Geometry::BIND_OVERALL );
    (*c)[ 0 ] = color;

    geom.addPrimitiveSet( new osg::DrawArrays( GL_POINTS, 0, 1, nInstances ) );


    osg::StateSet* ss = geom.getOrCreateStateSet();

    osg::Point* point = new osg::Point;
    point->setSize( 10. );
    // Use of shader (required for draw instanced) disables fixed-funxtion point parameters.
    // I'll need to investigate how to mimic this functionality in a shader.
    //point->setDistanceAttenuation( osg::Vec3( 0., 0., 0.05f) );
    ss->setAttributeAndModes( point );

    // Turn on point sprites and specigy the point sprite texture.
    osg::PointSprite *sprite = new osg::PointSprite();
    ss->setTextureAttributeAndModes( 1, sprite, osg::StateAttribute::ON );
    osg::Texture2D *tex = new osg::Texture2D();
    tex->setImage( osgDB::readImageFile( "splotch.png" ) );
    ss->setTextureAttributeAndModes( 1, tex, osg::StateAttribute::ON );

    // Keep pixels with a significant alpha value (discard low-alpha pixels).
    osg::AlphaFunc* af = new osg::AlphaFunc( osg::AlphaFunc::GREATER, 0.05f );
    ss->setAttributeAndModes( af );
}



float* OSGStreamlineStage::createPositionArray( int numPoints , int mult, vtkPoints* points, const vtkIdType* pts, int &tm, int &tn)
{
    //mult is the multiplier to add extra points using linear interplation

    //calculate texture dimension
    std::cout << "number of points " << numPoints << std::endl;
    std::cout << "number of total points (including multiplier)" << numPoints*mult << std::endl;
    int am = mylog2(numPoints*mult)+1;
    int mm = am/2;
    int nn = am -am/2;
    tm = mypow2(mm);
    tn = mypow2(nn);
    std::cout << tm << " "<< tn << std::endl;

    float* pos = new float[ tm * tn * 3 ];
    float* posI = pos;

    int j=0;

    double curPoint[3];
    double nextPoint[3];
    points->GetPoint(pts[0], nextPoint); //get the first point
        
    for (int i=0; i<tm*tn; i++)
    {
        if (i<numPoints*mult)
        {    
            int mod = i%mult;
            if (mod == 0)
            {
                *posI++=(float)nextPoint[0];
                *posI++=(float)nextPoint[1];
                *posI++=(float)nextPoint[2];

                curPoint[0]=nextPoint[0];
                curPoint[1]=nextPoint[1];
                curPoint[2]=nextPoint[2];

                j++;
                if (j<numPoints)
                    points->GetPoint(pts[j], nextPoint);

            }
            else
            {
                mod = i%mult;
                *posI++=(float)(curPoint[0]+mod*(nextPoint[0]-curPoint[0])/mult);
                *posI++=(float)(curPoint[1]+mod*(nextPoint[1]-curPoint[1])/mult);
                *posI++=(float)(curPoint[2]+mod*(nextPoint[2]-curPoint[2])/mult);
            }
        }
        else
        {
            *posI++ = 0.;
            *posI++ =  0.;
            *posI++ = 0.;
        }
    }
   
    return pos;
}



int OSGStreamlineStage::mylog2(unsigned x)
{
    int l = -1; // mylog2(0) will return -1
    while (x != 0u)
    {
        x = x >> 1u;
        ++l;
    }
    return l;
}

int OSGStreamlineStage::mypow2(unsigned x)
{
    int l = 1; // mypow2(0) will return 1
    while (x != 0u)
    {
        l = l << 1u;
        x--;
    }
    return l;
}

float* OSGStreamlineStage::createScalarArray( vtkIdType numPoints , int mult, vtkPointData* pointData, vtkIdType* pts, int &tm, int &tn, const char* scalarName)
{
    int am = mylog2(numPoints*mult)+1;
    int mm = am/2;
    int nn = am -am/2;
    tm = mypow2(mm);
    tn = mypow2(nn);
    std::cout << tm << " "<< tn << std::endl;

    float* sca = new float[ tm * tn * 3 ];
    float* scaI = sca;

    double curColor[3];
    double nextColor[3];

    vtkDataArray* dataArray = pointData->GetScalars(scalarName);
    double dataRange[2]; 
    
    dataArray->GetRange(dataRange);
    
    //Here we build a color look up table
    vtkLookupTable *lut = vtkLookupTable::New(); 
    lut->SetHueRange (0.667, 0.0);
    lut->SetRange(dataRange);
    lut->SetRampToLinear();
    lut->Build();

    double nextVal = dataArray->GetTuple1(pts[0]);
    lut->GetColor(nextVal,nextColor);

    int j=0;
    for (int i=0; i<tm*tn; i++)
    {
        if (i<numPoints*mult)
        {    
            int mod = i%mult;
            if (mod == 0)
            {
                *scaI++=(float)nextColor[0];
                *scaI++=(float)nextColor[1];
                *scaI++=(float)nextColor[2];

                curColor[0]=nextColor[0];
                curColor[1]=nextColor[1];
                curColor[2]=nextColor[2];

                j++;
                if (j<numPoints)
                {
                    nextVal = dataArray->GetTuple1(pts[j]);
                    lut->GetColor(nextVal,nextColor);
                }
            }
            else
            {
                mod = i%mult;
                *scaI++=(float)(curColor[0]+mod*(nextColor[0]-curColor[0])/mult);
                *scaI++=(float)(curColor[1]+mod*(nextColor[1]-curColor[1])/mult);
                *scaI++=(float)(curColor[2]+mod*(nextColor[2]-curColor[2])/mult);
            }
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


//CreateStreamLines

void OSGStreamlineStage::createStreamLines(vtkPolyData* polyData, ves::xplorer::scenegraph::Geode* geode, int mult, const char* scalarName)
{
    vtkCleanPolyData* cleanPD = vtkCleanPolyData::New();
    cleanPD->PointMergingOn();
    cleanPD->SetInput( polyData );
    cleanPD->Update();
    vtkPolyData* streamlinePD = cleanPD->GetOutput();

    int numOfLine = streamlinePD->GetNumberOfLines();
    std::cout << numOfLine << std::endl;
    vtkCellArray* lines = streamlinePD->GetLines();

    vtkPointData *pointData = streamlinePD->GetPointData();
    vtkPoints *points = streamlinePD->GetPoints();
    double x[3];
    double bounds[6];
    points->GetBounds(bounds);
    //VTK does bounds xmin, xmax,....
    //OSG does bounds xmin, ymin, zmin, xmax, ymax,...
    osg::BoundingBox bb(bounds[0],bounds[2],bounds[4],bounds[1],bounds[3],bounds[5]);
    vtkIdType cLineNp;
    vtkIdType *pts;
    //for (int i=0; i< numOfLine; i++)
    int lineNum=0;
    for (lines->InitTraversal(); ((lineNum<numOfLine) && (lines->GetNextCell(cLineNp, pts))); lineNum++)
    {
        //double dLineNp= lines->GetComponent(0,i); //number of point in line i
        //int cLineNp = (int) dLineNp;

        if (cLineNp<=1)
            continue;

        //double dfirstP = lines->GetComponent(1,i); //first Point index of line i
        int firstP = pts[0];
        points->GetPoint(firstP, x);

        osg::Geometry* geom = new osg::Geometry;
        // Note:
        // Display Lists and draw instanced are mutually exclusive. Disable
        // display lists and use buffer objects instead.
        geom->setUseDisplayList( false );
        geom->setUseVertexBufferObjects( true );
        osg::Vec3 loc(x[0], x[1], x[2] );
        createSLPoint( *geom, cLineNp * mult, loc, osg::Vec4( .5, 1., .6, 1.) );
        geode->addDrawable( geom );
        
        // Note:
        // OSG has no idea where our vertex shader will render the points. For proper culling
        // and near/far computation, set an approximate initial bounding box.
        geom->setInitialBound( bb );
        
        int tm=0;
        int tn=0;
        
        float* pos = createPositionArray( cLineNp , mult, points, pts, tm, tn);
        //commented out scalararray as it crashes
        float* sca = createScalarArray( cLineNp , mult, pointData, pts, tm, tn, scalarName);

        osg::StateSet* ss = geom->getOrCreateStateSet();


//        osg::ref_ptr< osg::Shader > vertexShader = osg::Shader::readShaderFile(
//            osg::Shader::VERTEX, osgDB::findDataFile( "streamline.vs" ) );



        //Apply the shader code here instead of calling it from a file as above
        std::string vertexSource =

            "uniform vec2 sizes; \n"
            "uniform sampler2D texPos; \n"
            "uniform sampler2D texSca; \n"

            "uniform float osg_SimulationTime; \n"
            "uniform float totalInstances; \n"
            "uniform float fadeTime; \n"
            "uniform float repeatTime; \n"

            "void main() \n"
            "{ \n"
                // Using the instance ID, generate "texture coords" for this instance.
                "const float r = ((float)gl_InstanceID) / sizes.x; \n"
                "vec2 tC; \n"
                "tC.s = fract( r ); tC.t = floor( r ) / sizes.y; \n"

                // Get position from the texture.
                "vec4 pos = texture2D( texPos, tC ); \n"
                //"pos.x *= 2.; \n" // Huh? x seems to be half the value I expect...
                "vec4 v = gl_ModelViewMatrix * ( gl_Vertex + pos ); \n"
                "gl_Position = gl_ProjectionMatrix * v; \n"

                // TBD. Need to make this configurable from a uniform.
                "gl_PointSize = -500. / v.z; \n"

                // Compute a time offset from the InstanceID to
                // emulate motion.
                "float timeOffset = ( ((float)gl_InstanceID) / totalInstances ) * repeatTime; \n"
                "float repTimer = mod( ( osg_SimulationTime - timeOffset ), repeatTime ); \n"
                "float alpha = fadeTime - min( repTimer, fadeTime ); \n"
                //vec4 color = gl_Color;

                //color.a *= alpha;
                "vec4 color = texture2D( texSca, tC ); \n"
                "color[3]=alpha; \n"
                "gl_FrontColor = color; \n"

            "} \n";

        osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
        vertexShader->setType( osg::Shader::VERTEX );
        vertexShader->setShaderSource( vertexSource );


        osg::ref_ptr< osg::Program > program = new osg::Program();
        program->addShader( vertexShader.get() );
        ss->setAttribute( program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        // Note:
        // We will render the streamline points with depth test on and depth write disabled,
        // with an order independent blend. This means we need to draw the streamlines last
        // (so use bin # 10) but we don't need the depth sort, so use bin name "RenderBin".
        ss->setRenderBinDetails( 10, "RenderBin" );

        // Note:
        // When using a vertex shader, point size is taken from glPointSize and _not_
        // distance-attenuated. However, set the following mode ON, and then our vertex
        // shader can do its own distance attenuation and emit gl_PointSize.
        ss->setMode( GL_VERTEX_PROGRAM_POINT_SIZE, osg::StateAttribute::ON );

        // Tells the shader the dimensions of our texture: tm x tn.
        // Required to compute correct texture coordinates from the instance ID.
        osg::ref_ptr< osg::Uniform > sizesUniform =
            new osg::Uniform( "sizes", osg::Vec2( (float)tm, (float)tn ) );
        ss->addUniform( sizesUniform.get() );

        // Tell the shader the total number of instances: tm * tn.
        // Required for animation based on the instance ID.
        osg::ref_ptr< osg::Uniform > totalInstancesUniform =
            new osg::Uniform( "totalInstances", (float)(tm * tn) );
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
        osg::ref_ptr< osg::Depth > depth = new osg::Depth( osg::Depth::LESS, 0., 1., false );
        ss->setAttributeAndModes( depth.get() );

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
        osg::Image* iPos = new osg::Image;
        iPos->setImage( tm, tn, 1, GL_RGB32F_ARB, GL_RGB, GL_FLOAT,
            (unsigned char*) pos, osg::Image::USE_NEW_DELETE );
        osg::Texture2D* texPos = new osg::Texture2D( iPos );
        texPos->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
        texPos->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );
        //NOTE: Texture slot 1 is used by the splotch.png image
        ss->setTextureAttribute( 0, texPos );

        osg::ref_ptr< osg::Uniform > texPosUniform =
            new osg::Uniform( "texPos", 0 );
        ss->addUniform( texPosUniform.get() );
        
        //send down rgb using texture
        osg::Image* iSca = new osg::Image;
        iSca->setImage( tm, tn, 1, GL_RGB32F_ARB, GL_RGB, GL_FLOAT,
            (unsigned char*)sca, osg::Image::USE_NEW_DELETE );
        osg::Texture2D* texSca = new osg::Texture2D( iSca );
        texSca->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
        texSca->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );
        //NOTE: Texture slot 1 is used by the splotch.png image
        ss->setTextureAttribute( 2, texSca );

        osg::ref_ptr< osg::Uniform > texScaUniform =
            new osg::Uniform( "texSca", 2 );
        ss->addUniform( texScaUniform.get() );
    }
   
}

// Create a scene graph and state set configured to render a streamline using draw instanced.
ves::xplorer::scenegraph::Geode* OSGStreamlineStage::createInstanced( vtkPolyData* polyData, int mult, const char* scalarName)
{
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
    pointData->Update();

    vtkPoints *points = polyData->GetPoints();    
    if (points==NULL)
    {
        std::cout << " points are null " << std::endl;
        return NULL;
    }
    
    


    // Essentially a top level Group, a single Geode child, and the
    // Geode contains a single Geometry to draw a sinalg point (but
    // uses a draw instanced PrimitiveSet).
//    osg::Group* grp = new osg::Group;
//    osg::Geode* geode = new osg::Geode;
    ves::xplorer::scenegraph::Geode* geode = new ves::xplorer::scenegraph::Geode();
//    grp->addChild( geode );

    //Now needs to create streams line with the passed in polyData line data
    createStreamLines(polyData, geode, mult, scalarName);


    
    return geode;
}
/*
void cfdAnimatedStreamlineCone::Update( void )
{
    m_streamlines.clear();
    
    vtkIdType cellId;        //vtkIdType
    vtkIdType npts;          //vtkIdType
    vtkPoints * points;
    vtkPoints **pointsArray;

    vprDEBUG( vesDBG, 1 )
        << "|\tNumber of Cells : " << this->polyData->GetNumberOfCells()
        << std::endl << vprDEBUG_FLUSH;
    vprDEBUG( vesDBG, 1 )
        << "|\tNumber of Lines : " << this->polyData->GetNumberOfLines()
        << std::endl << vprDEBUG_FLUSH;
    vprDEBUG( vesDBG, 1 )
        << "|\tNumber of Points : " << this->polyData->GetNumberOfPoints()
        << std::endl << vprDEBUG_FLUSH;
    
    int numberOfStreamLines;
    numberOfStreamLines = this->polyData->GetNumberOfLines();

    if( numberOfStreamLines == 0 )
    {
        std::cout << "|\tcfdAnimatedStreamlineCone::Update : Number of streamlines is 0 " << std::endl;
        return;
    }

    // Find the maximum number of points in one streamline
    int maxNpts = 0;
    int minNpts = 1000000;

    vtkPoints* points2 = 0;
    double x2[ 3 ];
    double x1[ 3 ];
    for( cellId = 0; cellId < numberOfStreamLines; ++cellId )
    {
        points = this->polyData->GetCell( cellId )->GetPoints();
        points->GetPoint( 0, x1 );
        npts = points->GetNumberOfPoints();

        bool foundmatch = false;
        for( vtkIdType cellId2 = cellId + 1; cellId2 < numberOfStreamLines; ++cellId2 )
        {
            points2 = this->polyData->GetCell( cellId2 )->GetPoints();
            points2->GetPoint( 0, x2 );
            if( (x1[ 0 ] == x2[ 0 ]) && (x1[ 1 ] == x2[ 1 ]) && (x1[ 2 ] == x2[ 2 ]) )
            {
                m_streamlines.push_back( std::make_pair< vtkIdType, vtkIdType >( cellId, cellId2 ) );
                foundmatch = true;
                npts += points2->GetNumberOfPoints();
                break;
            }
        }

        vprDEBUG( vesDBG, 1 ) << "|\t\tNumber of points in cell " << cellId
            << " = " << npts << std::endl << vprDEBUG_FLUSH;
        if( maxNpts < npts )
            maxNpts = npts;
        
        if( minNpts > npts )
            minNpts = npts;
        
        if( !foundmatch )
        {
            bool foundstandalone = true;
            //std::cout << "did not find match " << cellId << std::endl;
            
            for( size_t i = 0; i < m_streamlines.size(); ++i )
            {
                if( (m_streamlines.at( i ).first == cellId) || (m_streamlines.at( i ).second == cellId) )
                {
                    foundstandalone = false;
                    break;
                }
            }
            
            if( !foundstandalone )
            {
                continue;
            }
            
            bool isBackwards = IsStreamlineBackwards( cellId );

            if( isBackwards )
            {
                //Is a backward integrated line
                std::cout << " Use backward" << std::endl;
                m_streamlines.push_back( std::make_pair< vtkIdType, vtkIdType >( -1, cellId ) );
            }
            else
            {
                std::cout << " Use forward" << std::endl;
                m_streamlines.push_back( std::make_pair< vtkIdType, vtkIdType >( cellId, -1 ) );
            }
        }
    }

    // Define the points at each integration time step
    pointsArray = new vtkPoints*[ maxNpts ];
    for( size_t i = 0; i < maxNpts;  i++ )
    {
        pointsArray[ i ] = vtkPoints::New();
    }

    double *x;

    for( size_t i = 0; i < m_streamlines.size(); ++i )
    {
        cellId = m_streamlines.at( i ).first;
        // For forward integrated points
        int forwardPoints = 0;
        if( cellId != -1 )
        {
            points = polyData->GetCell( cellId )->GetPoints();
            forwardPoints = points->GetNumberOfPoints();
            vprDEBUG( vesDBG, 1 )
                << "|\t\tNumber of Forward points = " << forwardPoints
                << std::endl << vprDEBUG_FLUSH;
            for( size_t j = 0; j < forwardPoints; ++j )
            {
                x = points->GetPoint( j );
                vprDEBUG( vesDBG, 3 )
                    << "|\t\tx[ " << j << " ] = " << x[ 0 ] << " : "
                    << x[ 1 ] << " : " << x[ 2 ] << std::endl << vprDEBUG_FLUSH;
                pointsArray[ j ]->InsertNextPoint( x );
            }
        }
        
        cellId = m_streamlines.at( i ).second;
        // For backward integrated points
        if( cellId != -1 )
        {
            points = polyData->GetCell( cellId )->GetPoints();
            npts = points->GetNumberOfPoints();
            vprDEBUG( vesDBG, 1 ) << "|\t\tNumber of Backward points = " << npts
                << std::endl << vprDEBUG_FLUSH;
            for( int j = npts - 1; j >= 0; j-- )
            {
                x = points->GetPoint( j );
                vprDEBUG( vesDBG, 3 )
                    << " x[ " << j << " ] = " << x[ 0 ] << " : "
                    << x[ 1 ] << " : " << x[ 2 ] << std::endl << vprDEBUG_FLUSH;
                pointsArray[( npts - 1 ) - j + forwardPoints]->InsertNextPoint( x );
            }
        }
    }

    this->sphere->SetRadius( m_streamers->GetArrowDiameter() );
    this->sphere->SetThetaResolution( 3 );
    this->sphere->SetPhiResolution( 3 );
    this->sphere->Update();

    vprDEBUG( vesDBG, 1 ) << "|\t\tmaxNpts = " 
        << maxNpts << std::endl << vprDEBUG_FLUSH;
    vprDEBUG( vesDBG, 1 ) << "|\t\tminNpts = " 
        << minNpts << std::endl << vprDEBUG_FLUSH;
    int w = maxNpts;
    double decimalRatio = ( double )w / 150.0;
    int ratio = ( int )ceil( decimalRatio );

    for( size_t i = 0; i < w; i += ratio )
    {
        //Make ploydata from the points
        vprDEBUG( vesDBG, 2 ) << "|\t\tcfdAnimatedStreamlineCone:: begin loop " << i << std::endl << vprDEBUG_FLUSH;
        this->polydata->SetPoints( pointsArray[ i ] );
        //polydata->Update();
        //polydata->Print( cout );

        //Map spheres to the polydata
        //vprDEBUG( vesDBG, 2 ) << "|\t\tcfdAnimatedStreamlineCone:: begin loop1" << std::endl << vprDEBUG_FLUSH;
        glyph->SetSource( this->sphere->GetOutput() );
        glyph->SetInput( this->polydata );
        glyph->SetScaleModeToDataScalingOff();
        //glyph->Update();

        //vprDEBUG( vesDBG, 2 ) << "|\t\tcfdAnimatedStreamlineCone:: begin loop2" << std::endl << vprDEBUG_FLUSH;
        this->mapper->SetInput( this->glyph->GetOutput() );
        //this->mapper->Update();


        //vprDEBUG( vesDBG, 2 ) << "|\t\tcfdAnimatedStreamlineCone:: begin loop3" << std::endl << vprDEBUG_FLUSH;
        vtkActor* temp = vtkActor::New();
        temp->SetMapper( this->mapper );
        temp->GetProperty()->SetSpecularPower( 20.0f );
        temp->GetProperty()->SetColor( 1.0f, 0.5f, 0.15f );
        
        geodes.push_back( new ves::xplorer::scenegraph::Geode() );
        geodes.back()->TranslateToGeode( temp );
        temp->Delete();

        //Make geodes from each polydata
        //vprDEBUG( vesDBG, 2 ) << "|\t\tcfdAnimatedStreamlineCone:: begin loop4" << std::endl << vprDEBUG_FLUSH;
        //this->_sequence->CreateGeodeVector( this->actor );

        // Reset polydata to its intial state and release all memory
        //polydata->Reset();
        this->polydata->Initialize();
        //vprDEBUG( vesDBG, 2 ) << "|\t\tcfdAnimatedStreamlineCone:: end loop" << std::endl << vprDEBUG_FLUSH;
    }

    //vprDEBUG( vesDBG, 1 ) << "|\t\tDeleting Point Array" << std::endl << vprDEBUG_FLUSH;
    for( size_t i = 0; i < maxNpts;  i++ )
    {
        pointsArray[ i ]->Delete();
    }

    delete [] pointsArray;
    //vprDEBUG( vesDBG, 1 ) << "|\t\tDeleting Point Array" << std::endl << vprDEBUG_FLUSH;

    this->updateFlag = true;
    vprDEBUG( vesDBG, 1 ) << "|\tExiting cfdStreamers Update " << std::endl << vprDEBUG_FLUSH;
}*/

