#include "OSGStage.h"
#include <string>

using namespace std;

bool PlayStateHandler::handle( const osgGA::GUIEventAdapter & event_adaptor,
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
                    case '+': // speed up
                    {
                        elapsedTime = getCurrentTime();
                        timer.setStartTick( timer.tick() );

                        // Increase speed by 33%
                        scalar *= ( 4./3. );

                        handled = true;
                    }
                    break;
                    case '-': // slow down
                    {
                        elapsedTime = getCurrentTime();
                        timer.setStartTick( timer.tick() );

                        // Decrease speed by 25%
                        scalar *= .75;

                        handled = true;
                    }
                    break;
                    case 'p': // pause
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

OSGStage::OSGStage(void)
{
	tm=tn=0;
}

OSGStage::~OSGStage(void)
{
}

int OSGStage::mylog2(unsigned x)
{
    int l = -1; // mylog2(0) will return -1
    while (x != 0u)
    {
        x = x >> 1u;
        ++l;
    }
    return l;
}

int OSGStage::mypow2(unsigned x)
{
    int l = 1; // mypow2(0) will return 1
    while (x != 0u)
    {
        l = l << 1u;
        x--;
    }
    return l;
}
osg::Group* OSGStage::createMesh(vtkPolyData* polydata, string displacement, string colorScalar)
{
	osg::ref_ptr< osg::Group > grp = new osg::Group;
    osg::Geode* geode = new osg::Geode;
    grp->addChild( geode );

    osg::Geometry* geom = new osg::Geometry();
    geode->addDrawable( geom );

    createMeshData( geom, polydata, displacement, colorScalar); 

    osg::Vec4Array* c = new osg::Vec4Array;
    c->push_back( osg::Vec4( 1., 1., 1., 1. ) );
    geom->setColorArray( c );
    geom->setColorBinding( osg::Geometry::BIND_OVERALL );

    return( grp.release() );
}

void OSGStage::createMeshData( osg::Geometry* geom, vtkPolyData* polydata, string disp, string colorScalar)
{
	int numStrips = polydata->GetNumberOfStrips();
	int numPts = polydata->GetNumberOfPoints();
	vtkPointData *pointData = polydata->GetPointData();

	if (pointData==NULL)
    {
        std::cout << " pd point data is null " << std::endl;
		return;
    }
	pointData->Update();
	vtkDataArray *vectorArray = pointData->GetVectors(disp.c_str());//("GlyphVector");
	vtkPoints *points = polydata->GetPoints();
	vtkDataArray *normals = pointData->GetNormals();
						
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

	int numCells = polydata->GetNumberOfCells();
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
	
	int cs = colors->size();
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
	
    osg::ref_ptr< osg::Uniform > texColorUniform =
        new osg::Uniform( "texColor", 1 );
    ss->addUniform( texColorUniform.get() );

    std::string vertexSource =

        "uniform sampler2D texVec; \n"
		"uniform sampler2D texColor; \n"
        "uniform float osg_SimulationTime; \n"

        "void main() \n"
        "{ \n"

			"float a = mod( osg_SimulationTime*100.0, 314.0) * 0.01; \n"
			"float scalar =  sin(a) * 100.0;\n"
            "vec4 vecOff = scalar * texture2D( texVec, gl_MultiTexCoord0.st ); \n"
			"vec4 color = texture2D( texColor, gl_MultiTexCoord0.st ); \n"
            "vec4 position = vec4( (gl_Vertex.xyz + vecOff.xyz), gl_Vertex.w ); \n"
			
            "gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * position; \n"

            "color[3]=1.0; \n"
			"gl_FrontColor = color; \n"

        "} \n";

    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    vertexShader->setType( osg::Shader::VERTEX );
    vertexShader->setShaderSource( vertexSource );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( vertexShader.get() );
    ss->setAttribute( program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
}

