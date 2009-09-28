#include "OSGStage.h"
#include "vtkLookupTable.h"
#include <string>

using namespace std;

OSGStage::OSGStage(void)
{
	tm=tn=0;
}

OSGStage::~OSGStage(void)
{
}

void OSGStage::createArrow( osg::Geometry& geom, int nInstances )
{
    const float sD( .05 ); // shaft diameter
    const float hD( .075 ); // head diameter
	const float len( 1. ); // length
    const float sh( .65 ); // length from base to start of head

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

    if( nInstances > 1 )
        geom.addPrimitiveSet( new osg::DrawArrays( GL_QUAD_STRIP, 0, 10, nInstances ) );
    else
        geom.addPrimitiveSet( new osg::DrawArrays( GL_QUAD_STRIP, 0, 10 ) );

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

    if( nInstances > 1 )
        geom.addPrimitiveSet( new osg::DrawArrays( GL_TRIANGLES, 10, 12, nInstances ) );
    else
        geom.addPrimitiveSet( new osg::DrawArrays( GL_TRIANGLES, 10, 12 ) );
}


float* OSGStage::createPositionArray( int m, int n , vtkPoints* points)
{
    float* pos = new float[ m * n * 3 ];
    float* posI = pos;

	int np = points->GetNumberOfPoints();
	cout <<"number of points is " << np << "\n";
	double x[3];
    float y[3];
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
			//	cout <<"this is " << i << "\n";
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

float* OSGStage::createAttitudeArray( int m, int n, vtkDataArray* dataArray)
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

float* OSGStage::createScalarArray( int m, int n, vtkDataArray* dataArray)
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
	//		return NULL;
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


    osg::StateSet* ss = geom->getOrCreateStateSet();
    


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
            "vec4 color = texture2D( texSca, tC ); \n"
            "color = color * dot( norm, vec3( 0, 0, 1 ) ); \n"
            "color[3]=1; \n"
			"gl_FrontColor = vec4( color ); \n"
        "} \n";

    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    vertexShader->setType( osg::Shader::VERTEX );
    vertexShader->setShaderSource( vertexSource );
   
    osg::ref_ptr< osg::Program > program = new osg::Program();

    ss->setAttribute( program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    
    program->addShader( vertexShader.get() );
    

    return grp;
}
