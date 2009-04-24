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
    ///Constructor
    Chart();
    ///Destructor
    ~Chart();

    ///Set the value to be plotted
    ///\param idx The index into the data array for the value storage
    ///\param value The value to be plotted
    void setValue( int idx, float value );
    ///Set the background color
    ///\param bgColor The background color
    void setBackgroundColor( osg::Vec4& bgColor );
    ///Set the foreground color
    ///\param fgColor The foreground color
    void setForegroundColor( osg::Vec4& fgColor );
    ///Set the starting location and size values
    ///\param x The x starting location
    ///\param y The y starting location
    ///\param w The width of the chart
    ///\param h The hieght of the chart
    void setChartLocationAndSize( float x, float y, float w, float h );
    ///Get the chart
    ///\return The osg::Geode for the chart
    osg::Geode* get() const;
    ///Create the chart for the data display
    ///\pre All other setter methods must be called before this
    void createChart();

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
