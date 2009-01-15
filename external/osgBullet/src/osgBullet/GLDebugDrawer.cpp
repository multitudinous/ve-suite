#include "osgBullet/GLDebugDrawer.h"

#include <osgText/Text>
#include <osg/LineWidth>

#include <iostream>

#include <stdio.h> //printf debugging
////////////////////////////////////////////////////////////////////////////////
GLDebugDrawer::GLDebugDrawer( osg::Group* root )
    :
    mDebugMode(0)
{
    mDebugBulletGeode = new osg::Geode();
    mDebugBulletGeode->setName( "Bullet Lines" );
    mDebugBulletGeode->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    osg::ref_ptr<osg::LineWidth> lineWidth = new osg::LineWidth;
    lineWidth->setWidth( 3 );
    mDebugBulletGeode->getOrCreateStateSet()->setAttributeAndModes( lineWidth.get(), osg::StateAttribute::ON );
    
    //BIND_PER_PRIMITIVE,
    //BIND_PER_VERTEX
    mLinesGeom = new osg::Geometry();
    mLinesGeom->setColorBinding( osg::Geometry::BIND_PER_VERTEX );

    mDebugBulletGeode->addDrawable( mLinesGeom.get() );

	mColors = new osg::Vec4Array;
    mVertices = new osg::Vec3Array();

    mLinesGeom->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, mVertices->size() ) );

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

    mVertices->push_back( osg::Vec3( from.x(), from.y(), from.z() ) );
    mVertices->push_back( osg::Vec3( to.x(), to.y(), to.z() ) );    
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
    mFramerateText->setCharacterSize( 0.5 );
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
    //mVertices->dirty();
    //mColors->dirty();
    mLinesGeom->setVertexArray( new osg::Vec3Array( *(mVertices.get()), osg::CopyOp::DEEP_COPY_ALL ) );
    mLinesGeom->setColorArray( new osg::Vec4Array( *(mColors.get()), osg::CopyOp::DEEP_COPY_ALL ) );
    mLinesGeom->setPrimitiveSet( 0, new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, mVertices->size() ) );
    //mLinesGeom->dirtyDisplayList();
    //mLinesGeom->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void GLDebugDrawer::BeginDraw()
{
    /*osg::Group* tempGroup = mDebugBulletGeode->getParent( 0 );
    if( !tempGroup )
    return;
    tempGroup->removeChild( mDebugBulletGeode.get() );


    mDebugBulletGeode = new osg::Geode();
    mDebugBulletGeode->setName( "Bullet Lines" );
    mDebugBulletGeode->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    //BIND_PER_PRIMITIVE,
    //BIND_PER_VERTEX
    mLinesGeom = new osg::Geometry();
    mLinesGeom->setColorBinding( osg::Geometry::BIND_PER_VERTEX );
    
    mDebugBulletGeode->addDrawable( mLinesGeom.get() );
    
	//mColors = new osg::Vec4Array;
    //mVertices = new osg::Vec3Array();
    
    tempGroup->addChild( mDebugBulletGeode.get() );
*/
    /*mDebugBulletGeode->removeDrawables( 0, mDebugBulletGeode->getNumDrawables() );
    
    mLinesGeom = new osg::Geometry();
    mLinesGeom->setColorBinding( osg::Geometry::BIND_PER_VERTEX );
    
    mDebugBulletGeode->addDrawable( mLinesGeom.get() );
    
	mColors = new osg::Vec4Array;
    mVertices = new osg::Vec3Array();
    */
    //Remove all prim sets
    //if( mLinesGeom->getNumPrimitiveSets() > 0 )
    //mLinesGeom->removePrimitiveSet( 0, 1 );

    mVertices->clear();
    mColors->clear();

}
////////////////////////////////////////////////////////////////////////////////



