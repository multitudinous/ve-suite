// --- My Includes --- //
#include "Funnel.h"

// --- OSG Includes --- //
#include <osg/Geometry>

#include <osgDB/ReadFile>
//#include <osgDB/WriteFile>

//C/C++ Libraries
#include <iostream>
#include <string>

namespace demo
{

////////////////////////////////////////////////////////////////////////////////
//Funnel::Funnel()
//{
    //;
//}
////////////////////////////////////////////////////////////////////////////////
Funnel::Funnel()
{
    CreateFunnel();
}
////////////////////////////////////////////////////////////////////////////////
Funnel::Funnel( const Funnel& funnel, const osg::CopyOp& copyop )
:
osg::Geode( funnel, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Funnel::~Funnel()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Funnel::CreateFunnel()
{
    osg::ref_ptr< osg::Geode > geode = new osg::Geode;

    osg::ref_ptr< osg::Geometry > funnel = new osg::Geometry;

    osg::ref_ptr< osg::Vec3Array > funnelVertices = new osg::Vec3Array;

	double angle = 0.0;
    double height = 0.0;
    double radius =
    double angleIncrement = ( 2.0 * osg::PI ) / m_axisDivisions;
    double heightIncrement = ( m_outerRadius - m_innerRadius ) / m_heightDivisions;

    for( unsigned int i = 0; i < m_axisDivisions; ++i )
    {
        glVertex3f(x+Radius*5.0*cos(angle),(dD-Radius),z+Radius*5.0*sin(angle));
        angle += angleInc;
    }


    funnelVertices->push_back( osg::Vec3(  ) );
    funnelVertices->push_back( osg::Vec3(  ) );
    funnelVertices->push_back( osg::Vec3(  ) );
    funnelVertices->push_back( osg::Vec3(  ) );
    		

    funnel->setVertexArray( funnelVertices.get() );

	osg::ref_ptr< osg::Vec4Array > funnelColor = new osg::Vec4Array();
	funnelColor->push_back( osg::Vec4( 1.0, 1.0, 1.0, 1.0 ) );
    funnel->setColorArray( funnelColor.get() );
    funnel->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec3Array > funnelNormals = new osg::Vec3Array;

    //Left
    funnelNormals->push_back( osg::Vec3( -1.0f, 0.0f, 0.0f ) );

    funnel->setNormalArray( funnelNormals.get() );
    funnel->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    funnel->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, funnelVertices.get()->size() ) );

    addDrawable( funnel.get() );
}
////////////////////////////////////////////////////////////////////////////////

} // end demo