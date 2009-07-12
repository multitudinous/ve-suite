#include "OSGStage.h"
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
    double x[3];
	for (int i=0; i<m*n; i++)
	{
		if (i<np)
		{
			points->GetPoint(i, x);
			*posI++=(float)x[0];
			*posI++=(float)x[1];
			*posI++=(float)x[2];
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
			*attI++ =  0.;
			*attI++ = 0.;
		}
	}
    return att;
}

float* OSGStage::createScalarArray( int m, int n, vtkDataArray* dataArray)
{
    float* sca = new float[ m * n * 3 ];
    float* scaI = sca;

	int nd = dataArray->GetNumberOfTuples();

    double x;
	for (int i=0; i<m*n; i++)
	{
		if (i<nd)
		{
			dataArray->GetTuple(i,&x);
			*scaI++ = x;
            *scaI++ = 0.;
            *scaI++ = 0.;
		}
		else
		{
			*scaI++ = 0.;
			*scaI++ =  0.;
			*scaI++ = 0.;
		}
	}
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
    int l = 1; // mylog2(0) will return -1
    while (x != 0u)
    {
        l = l << 1u;
        x--;
    }
    return l;
}

osg::Node* OSGStage::createInstanced(vtkGlyph3D* glyph, string vectorName, string scalarName)
{
	//Now pull in the vtk data
	if (glyph==NULL)
		return NULL;
	glyph->Update();
	
	vtkPolyData *polyData = glyph->GetOutput();
	if (polyData==NULL)
		return NULL;
	polyData->Update();
	
	vtkPointData *pointData = polyData->GetPointData();
	if (pointData==NULL)
		return NULL;
	
	pointData->Update();

	vtkPoints *points = polyData->GetPoints();	
	if (points==NULL)
		return NULL;
	vtkDataArray *vectorArray = pointData->GetVectors(vectorName.c_str());//("GlyphVector");
	vtkDataArray *scalarArray = pointData->GetScalars(scalarName.c_str());

	if ((vectorArray==NULL) && (scalarArray==NULL))
		return NULL;

	//calculate texture dimension
	int numPoints = points->GetNumberOfPoints();
	tm = mypow2(mylog2( (int)(sqrt(float(numPoints/3))))+1);
	tn = tm;

	//create the Geometry Node with arrows
    osg::Group* grp = new osg::Group;
    osg::Geode* geode = new osg::Geode;
    osg::Geometry* geom = new osg::Geometry;
    
	geom->setUseDisplayList( false );
    geom->setUseVertexBufferObjects( true );
    createArrow( *geom, tm*tn );
    geode->addDrawable( geom );
    grp->addChild( geode );

	double bounds[6];
	points->GetBounds(bounds);
	osg::BoundingBox bb(bounds[0],bounds[1],bounds[2],bounds[3],bounds[4],bounds[5]);
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
}
