// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBULLET_CHART_H__
#define __OSGBULLET_CHART_H__

#include <osg/Referenced>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Image>

#include <osgBullet/Export.h>


namespace osgBullet {


    class OSGBULLET_EXPORT Chart : public osg::Referenced
{
public:
    Chart();
    ~Chart();

    void setValue( int idx, float value );

    osg::Geode* get() const;

protected:
    float _x, _y, _w, _h;
    float _yScale;
    int _texW;

    float* _xValues;
    osg::ref_ptr< osg::Image > _image;

    osg::ref_ptr< osg::Geode > _geode;
    osg::ref_ptr< osg::Geometry > _geom;

    osg::ref_ptr< osg::Vec3Array > _verts;
    osg::ref_ptr< osg::Vec2Array > _tc;
    osg::Vec4 _bg, _fg, _overrun;
};


} // end namespace osgBullet

#endif // __OSGBULLET_UTILS_H__
