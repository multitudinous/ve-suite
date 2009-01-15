#ifndef GL_DEBUG_DRAWER_H
#define GL_DEBUG_DRAWER_H

#include <LinearMath/btIDebugDraw.h>

#include <osgBullet/Export.h>

#include <osg/Geometry>
#include <osg/Geode>
#include <osg/Group>
#include <osg/Array>

class OSGBULLET_EXPORT GLDebugDrawer : public btIDebugDraw
{
private:
	int mDebugMode;

    osg::ref_ptr< osg::Geode > mDebugBulletGeode;
    osg::ref_ptr< osg::Geometry > mLinesGeom;
    osg::ref_ptr< osg::Vec4Array > mColors;
    osg::ref_ptr< osg::Vec3Array > mVertices;
    osg::ref_ptr< osg::DrawArrays > mDA;
    
public:

	GLDebugDrawer( osg::Group* root );

    virtual ~GLDebugDrawer();

	virtual void	drawLine(const btVector3& from,const btVector3& to,const btVector3& color);
	
	virtual void	drawTriangle(const btVector3& a,const btVector3& b,const btVector3& c,const btVector3& color,btScalar alpha);
	
	virtual void	drawContactPoint(const btVector3& PointOnB,const btVector3& normalOnB,btScalar distance,int lifeTime,const btVector3& color);

	virtual void	reportErrorWarning(const char* warningString);

	virtual void	draw3dText(const btVector3& location,const char* textString);

	virtual void	setDebugMode(int debugMode);

	virtual int		getDebugMode() const { return mDebugMode;}
    
    void EndDraw();
    void BeginDraw();
    

};

#endif//GL_DEBUG_DRAWER_H
