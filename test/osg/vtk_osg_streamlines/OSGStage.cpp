#include "OSGStage.h"
#include <string>

using namespace std;

OSGStage::OSGStage(void)
{

}

OSGStage::~OSGStage(void)
{
}

void OSGStage::createSLPoint( osg::Geometry& geom, int nInstances, const osg::Vec3 position, const osg::Vec4 color )
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
    point->setSize( 40. );
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



float* OSGStage::createPositionArray( int numPoints , int mult, vtkPoints* points, const int* pts, int &tm, int &tn)
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

float* OSGStage::createScalarArray( int numPoints , int mult, vtkPointData* pointData, const int* pts, int &tm, int &tn, const char* scalarName)
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

void OSGStage::createStreamLines(vtkPolyData* polyData, osg::Geode* geode, int mult, const char* scalarName)
{
	int numOfLine = polyData->GetNumberOfLines();
	vtkCellArray* lines = polyData->GetLines();

	vtkPointData *pointData = polyData->GetPointData();
	vtkPoints *points = polyData->GetPoints();
	double x[3];
	double bounds[6];
	points->GetBounds(bounds);
    //VTK does bounds xmin, xmax,....
    //OSG does bounds xmin, ymin, zmin, xmax, ymax,...
	osg::BoundingBox bb(bounds[0],bounds[2],bounds[4],bounds[1],bounds[3],bounds[5]);
	int cLineNp;
	int *pts;
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
		float* sca = createScalarArray( cLineNp , mult, pointData, pts, tm, tn, scalarName);

		osg::StateSet* ss = geom->getOrCreateStateSet();

		osg::ref_ptr< osg::Shader > vertexShader = osg::Shader::readShaderFile(
			osg::Shader::VERTEX, osgDB::findDataFile( "streamline.vs" ) );

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

		ss->setTextureAttribute( 2, texSca );

		osg::ref_ptr< osg::Uniform > texScaUniform =
			new osg::Uniform( "texSca", 2 );
		ss->addUniform( texScaUniform.get() );
	}
   
}

// Create a scene graph and state set configured to render a streamline using draw instanced.
osg::Group* OSGStage::createInstanced( vtkPolyData* polyData, int mult, const char* scalarName)
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
    osg::Group* grp = new osg::Group;
    osg::Geode* geode = new osg::Geode;
    grp->addChild( geode );

	//Now needs to create streams line with the passed in polyData line data
	createStreamLines(polyData,geode, mult, scalarName);


    
    return grp;
}

/*
osg::Node* OSGStage::createInstanced(vtkPolyData* glyph, string vectorName, string scalarName)
{
    std::cout << "creating osg planes" << std::endl;
	//Now pull in the vtk data
	if (glyph==NULL)
    {
        std::cout << " glyph is null " << std::endl;
        		return NULL;
    }
	//glyph->Update();
	
	vtkPolyData *polyData = glyph;//->GetOutput();
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
        vtkDataArray *vectorArray = pointData->GetVectors(vectorName.c_str());//("GlyphVector");
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
	if ((vectorArray==NULL) && (scalarArray==NULL))
		return NULL;

	//calculate texture dimension
	int numPoints = points->GetNumberOfPoints();
    std::cout << "number of points " << numPoints << std::endl;
	int am = mylog2(numPoints)+1;
	int mm = am/2;
	int nn = am -am/2;
	tm = mypow2(mm);
	tn = mypow2(nn);
    std::cout << tm << " "<< tn << std::endl;

	//create the Geometry Node with arrows
    osg::Group* grp = new osg::Group;
    osg::Geode* geode = new osg::Geode;
    osg::Geometry* geom = new osg::Geometry;
    
	geom->setUseDisplayList( false );
    geom->setUseVertexBufferObjects( true );
    createArrow( *geom, numPoints);//tm*tn );
    geode->addDrawable( geom );
    grp->addChild( geode );

	double bounds[6];
	points->GetBounds(bounds);
    //VTK does bounds xmin, xmax,....
    //OSG does bounds xmin, ymin, zmin, xmax, ymax,...
	osg::BoundingBox bb(bounds[0],bounds[2],bounds[4],bounds[1],bounds[3],bounds[5]);
    geom->setInitialBound( bb );

	//Create the rendering shader
    std::string vertexSource =

        "uniform vec2 sizes; \n"
        "uniform sampler2D texPos; \n"
        "uniform sampler2D texAtt; \n"
		"uniform sampler2D texSca; \n"

        "void main() \n"
        "{ \n"
            // Using the instance ID, generate "texture coords" for this instance.
            "const float r = gl_InstanceID / sizes.x; \n"
            "vec2 tC; \n"
            "tC.s = fract( r ); tC.t = floor( r ) / sizes.y; \n"

            // Create orthonormal basis to position and orient this instance.
            "vec4 newZ = texture2D( texAtt, tC ); \n"
            "vec3 newX = cross( newZ.xyz, vec3( 0,0,1 ) ); \n"
            "normalize( newX ); \n"
            "vec3 newY = cross( newZ.xyz, newX ); \n"
            "normalize( newY ); \n"
            "vec4 pos = texture2D( texPos, tC ); \n"
            "mat4 mV = mat4( newX.x, newX.y, newX.z, 0., newY.x, newY.y, newY.z, 0., newZ.x, newZ.y, newZ.z, 0., pos.x, pos.y, pos.z, 1. ); \n"
            "gl_Position = (gl_ModelViewProjectionMatrix * mV * gl_Vertex); \n"

            // Use just the orientation components to transform the normal.
            "mat3 mN = mat3( newX, newY, newZ ); \n"
            "vec3 norm = normalize(gl_NormalMatrix * mN * gl_Normal); \n"

            // Diffuse lighting with light at the eyepoint.
            "gl_FrontColor = gl_Color * dot( norm, vec3( 0, 0, 1 ) ); \n"

        "} \n";

    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    vertexShader->setType( osg::Shader::VERTEX );
    vertexShader->setShaderSource( vertexSource );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( vertexShader.get() );

    osg::StateSet* ss = geode->getOrCreateStateSet();
    ss->setAttribute( program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

	//set size
    osg::ref_ptr< osg::Uniform > sizesUniform =
        new osg::Uniform( "sizes", osg::Vec2( (float)tm, (float)tn ) );
    ss->addUniform( sizesUniform.get() );

	//now set the arrays
	
	//First set the point position array
	float* pos = createPositionArray( tm, tn, points);

    osg::Image* iPos = new osg::Image;
    iPos->setImage( tm, tn, 1, GL_RGB32F_ARB, GL_RGB, GL_FLOAT,
        (unsigned char*) pos, osg::Image::USE_NEW_DELETE );
    osg::Texture2D* texPos = new osg::Texture2D( iPos );
    texPos->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
    texPos->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );

    ss->setTextureAttribute( 0, texPos );

    osg::ref_ptr< osg::Uniform > texPosUniform =
        new osg::Uniform( "texPos", 0 );
    ss->addUniform( texPosUniform.get() );

    //set attitude array
	if (vectorArray!=NULL)
	{
		float* att = createAttitudeArray( tm, tn, vectorArray);

		osg::Image* iAtt = new osg::Image;
		iAtt->setImage( tm, tn, 1, GL_RGB32F_ARB, GL_RGB, GL_FLOAT,
			(unsigned char*)att, osg::Image::USE_NEW_DELETE );
		osg::Texture2D* texAtt = new osg::Texture2D( iAtt );
		texAtt->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
		texAtt->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );

		ss->setTextureAttribute( 1, texAtt );

		osg::ref_ptr< osg::Uniform > texAttUniform =
			new osg::Uniform( "texAtt", 1 );
		ss->addUniform( texAttUniform.get() );
	}
	
	//set scalar array
	if (scalarArray!=NULL)
	{
		float* sca = createScalarArray( tm, tn, scalarArray);

		osg::Image* iSca = new osg::Image;
		iSca->setImage( tm, tn, 1, GL_RGB32F_ARB, GL_RGB, GL_FLOAT,
			(unsigned char*)sca, osg::Image::USE_NEW_DELETE );
		osg::Texture2D* texSca = new osg::Texture2D( iSca );
		texSca->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
		texSca->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );

		ss->setTextureAttribute( 2, texSca );

		osg::ref_ptr< osg::Uniform > texScaUniform =
			new osg::Uniform( "texSca", 2 );
		ss->addUniform( texScaUniform.get() );
	}

    return grp;
}*/
