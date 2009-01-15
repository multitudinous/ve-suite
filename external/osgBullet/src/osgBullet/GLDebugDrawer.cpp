#include "osgBullet/GLDebugDrawer.h"

#include <osg/Geometry>
#include <osgText/Text>

#include <iostream>

#include <stdio.h> //printf debugging

#include "osgBullet/Utils.h"


////////////////////////////////////////////////////////////////////////////////
GLDebugDrawer::GLDebugDrawer( osg::Group* root )
    :
    mDebugMode(0)
{
    mDebugBulletGeode = new osg::Geode();
    mDebugBulletGeode->setName( "Bullet Lines" );
    mDebugBulletGeode->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

    mLinesGeom = new osg::Geometry();
    mLinesGeom->setDataVariance( osg::Object::DYNAMIC );
    mLinesGeom->setUseDisplayList( false );
    mLinesGeom->setUseVertexBufferObjects( false );

    mVertices = new osg::Vec3Array();
    mVertices->setDataVariance( osg::Object::DYNAMIC );
    mLinesGeom->setVertexArray( mVertices.get() );

    mColors = new osg::Vec4Array;
    mColors->setDataVariance( osg::Object::DYNAMIC );
    mLinesGeom->setColorArray( mColors.get() );
    mLinesGeom->setColorBinding( osg::Geometry::BIND_PER_VERTEX );

    mDA = new osg::DrawArrays( GL_LINES );
    mDA->setDataVariance( osg::Object::DYNAMIC );
    mLinesGeom->addPrimitiveSet( mDA.get() );

    mDebugBulletGeode->addDrawable( mLinesGeom.get() );

    root->addChild( mDebugBulletGeode.get() );
}
GLDebugDrawer::~GLDebugDrawer()
{
    mDebugBulletGeode->getParent( 0 )->removeChild( mDebugBulletGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
void GLDebugDrawer::drawLine(const btVector3& from,const btVector3& to,const btVector3& color)
{
    mColors->push_back( osg::Vec4( color.x(), color.y(), color.z(), 1.0f ) );
    mColors->push_back( osg::Vec4( color.x(), color.y(), color.z(), 1.0f ) );

    mVertices->push_back( osgBullet::asOsgVec3( from ) );
    mVertices->push_back( osgBullet::asOsgVec3( to ) );  

    //osg::notify( osg::ALWAYS ) << mVertices->size() << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void GLDebugDrawer::drawTriangle(const btVector3& a,const btVector3& b,const btVector3& c,const btVector3& color,btScalar alpha)
{
//	if (m_debugMode > 0)
	{
		/*const btVector3	n=cross(b-a,c-a).normalized();
		glBegin(GL_TRIANGLES);		
		glColor4f(color.getX(), color.getY(), color.getZ(),alpha);
		glNormal3d(n.getX(),n.getY(),n.getZ());
		glVertex3d(a.getX(),a.getY(),a.getZ());
		glVertex3d(b.getX(),b.getY(),b.getZ());
		glVertex3d(c.getX(),c.getY(),c.getZ());
		glEnd();*/
	}
    drawLine( a, b, color );
    drawLine( b, c, color );
    drawLine( c, a, color );
    
}
////////////////////////////////////////////////////////////////////////////////
void GLDebugDrawer::setDebugMode(int debugMode)
{
	mDebugMode = debugMode;
}
////////////////////////////////////////////////////////////////////////////////
void GLDebugDrawer::draw3dText(const btVector3& location,const char* textString)
{
    /*std::string headsUpDisplayFont( "fonts/arial.ttf" );
    osg::ref_ptr< osgText::Text > mFramerateText = new osgText::Text();
    mFramerateText->setFont( headsUpDisplayFont );
    mFramerateText->setCharacterSize( 5 );
    mFramerateText->setAxisAlignment( osgText::Text::SCREEN );
    mFramerateText->setAlignment( osgText::Text::RIGHT_BOTTOM );
    mFramerateText->setPosition( osg::Vec3( location.x(), location.y(), location.z() ) );
    mFramerateText->setText( textString );
    mDebugBulletGeode->addDrawable( mFramerateText.get() );*/
}
////////////////////////////////////////////////////////////////////////////////
void GLDebugDrawer::reportErrorWarning(const char* warningString)
{
	std::cout << warningString << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void GLDebugDrawer::drawContactPoint( const btVector3& pointOnB,
    const btVector3& normalOnB, btScalar distance, 
    int lifeTime, const btVector3& color)
{
    btVector3 to=pointOnB+normalOnB*distance;
    const btVector3&from = pointOnB;

    drawLine( from, to, color );

    char buf[12];
    sprintf(buf," %d",lifeTime);

    draw3dText( from, buf );
}
////////////////////////////////////////////////////////////////////////////////
void GLDebugDrawer::EndDraw()
{
    mDA->setFirst( 0 );
    mDA->setCount( mVertices->size() );
}
////////////////////////////////////////////////////////////////////////////////
void GLDebugDrawer::BeginDraw()
{
	mColors->clear();
    mVertices->clear();
}
////////////////////////////////////////////////////////////////////////////////

