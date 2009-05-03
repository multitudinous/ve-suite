//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#include "Wall.h"
#include <osgDB/ReadFile>
#include <osg/Node>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/TexEnv>

#include <osgDB/FileUtils>
#include <osg/io_utils>
#include <iostream>
#include <sstream>
#include <vector>
#include <string>



Wall::Wall( std::istream& in )
{
    const int maxLineLen( 1024 );
    char line[ maxLineLen ];
    {
        in.getline( line, maxLineLen );
        std::string lineStr( line );
        while( lineStr[0] == '#' )
        {
            in.getline( line, maxLineLen );
            lineStr = line;
        }
        std::istringstream istr( lineStr );
        istr >> suffix_;
    }

    {
        in.getline( line, maxLineLen );
        std::string lineStr( line );
        while( lineStr[0] == '#' )
        {
            in.getline( line, maxLineLen );
            lineStr = line;
        }
        std::istringstream istr( lineStr );
        istr >> corner_;
    }

    {
        in.getline( line, maxLineLen );
        std::string lineStr( line );
        while( lineStr[0] == '#' )
        {
            in.getline( line, maxLineLen );
            lineStr = line;
        }
        std::istringstream istr( lineStr );
        istr >> w_;
    }

    {
        in.getline( line, maxLineLen );
        std::string lineStr( line );
        while( lineStr[0] == '#' )
        {
            in.getline( line, maxLineLen );
            lineStr = line;
        }
        std::istringstream istr( lineStr );
        istr >> h_;
    }

    {
        in.getline( line, maxLineLen );
        std::string lineStr( line );
        while( lineStr[0] == '#' )
        {
            in.getline( line, maxLineLen );
            lineStr = line;
        }
        std::istringstream istr( lineStr );
        istr >> texName_;
    }
}

void
Wall::dump( std::ostream& out )
{
    out << corner_ << std::endl;
    out << w_ << std::endl;
    out << h_ << std::endl;
}

osg::Vec4 textureProject( const osg::Vec3& v, const osg::Matrix& m )
{
    osg::Vec4 t( osg::Vec4( v[0], v[1], v[2], 1. ) * m );
    osg::Vec4 tc( t ); //(t[0] + 1.) * .5, (t[1] + 1.) * .5, 0., t[3] );
    return( tc );
}

void
Wall::createGeode( const osg::Matrix& m )
{
    geode_ = new osg::Geode();
    osg::ref_ptr< osg::Geometry > quadGeometry = new osg::Geometry();

    osg::ref_ptr< osg::Vec3Array > quadVertices = new osg::Vec3Array();
    quadVertices->resize( 4 );
    osg::ref_ptr< osg::Vec4Array > quadTexCoords = new osg::Vec4Array();
    quadTexCoords->resize( 4 );

    osg::Vec3 v( corner_ );
    (*quadVertices)[ 0 ] = v;
    (*quadTexCoords)[ 0 ] = textureProject( v, m );

    v = corner_ + w_;
    (*quadVertices)[ 1 ] = v;
    (*quadTexCoords)[ 1 ] = textureProject( v, m );

    v = corner_ + w_ + h_;
    (*quadVertices)[ 2 ] = v;
    (*quadTexCoords)[ 2 ] = textureProject( v, m );

    v = corner_ + h_;
    (*quadVertices)[ 3 ] = v;
    (*quadTexCoords)[ 3 ] = textureProject( v, m );

    quadGeometry->setVertexArray( quadVertices.get() );
    quadGeometry->setTexCoordArray( 0, quadTexCoords.get() );

    osg::ref_ptr< osg::Vec2Array > tc = new osg::Vec2Array();
    tc->resize( 4 );
    (*tc)[ 0 ].set( 0, 0 );
    (*tc)[ 1 ].set( 1, 0 );
    (*tc)[ 2 ].set( 1, 1 );
    (*tc)[ 3 ].set( 0, 1 );
    quadGeometry->setTexCoordArray( 1, tc.get() );

    quadGeometry->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, 4 ) );
    
    geode_->addDrawable( quadGeometry.get() );
    geode_->setCullingActive( false );

    std::string texName( osgDB::findDataFile( texName_ ) );
    osg::notify( osg::INFO ) << "Source texture name: " << texName_ << std::endl;
    osg::Image* image = osgDB::readImageFile( texName );
    osg::notify( osg::INFO ) << "  Texture data file path and name (from osgDB): " << texName_ << std::endl;
    if( image == NULL )
        osg::notify( osg::WARN ) << "Can't read image file " << texName << std::endl;
    else
    {
        osg::Texture2D* tex = new osg::Texture2D( image );
        geode_->getOrCreateStateSet()->setTextureAttributeAndModes( 1, tex );
        geode_->getOrCreateStateSet()->setTextureAttributeAndModes( 1, new osg::TexEnv( osg::TexEnv::REPLACE ) );
    }
}

osg::Geode*
Wall::getGeode() const
{
    return( geode_.get() );
}

osg::Matrix
Wall::getProj() const
{
    const float wHalfLen( w_.length() * .5 );
    const float hHalfLen( h_.length() * .5 );

    osg::Matrix proj( osg::Matrix::ortho( -wHalfLen, wHalfLen, -hHalfLen, hHalfLen, -1., 1. ) );
    return( proj );
}

osg::Matrix
Wall::getView() const
{
    osg::Vec3 cross( w_ ^ h_);
    cross.normalize();
    osg::Vec3 center( corner_ + (w_ * .5) + (h_ * .5) );

    osg::Matrix view( osg::Matrix::lookAt( center + cross, center, h_ ) );
    return( view );
}


WallList
readWallFile( const std::string& fName, osg::Vec3& viewPos, std::string& prefix )
{
    WallList wallList;

    std::string fullName( osgDB::findDataFile( fName ) );
    std::ifstream in( fullName.c_str() );
    if( !in.good() )
    {
        osg::notify( osg::WARN ) << "Cannot open cave config file " << fName << std::endl;
        return( wallList );
    }

    const int maxLineLen( 1024 );
    char line[ maxLineLen ];
    int nWalls;
    {
        in.getline( line, maxLineLen );
        std::string lineStr( line );
        while( lineStr[0] == '#' )
        {
            in.getline( line, maxLineLen );
            lineStr = line;
        }
        std::istringstream istr( lineStr );
        istr >> nWalls;
    }

    {
        in.getline( line, maxLineLen );
        std::string lineStr( line );
        while( lineStr[0] == '#' )
        {
            in.getline( line, maxLineLen );
            lineStr = line;
        }
        std::istringstream istr( lineStr );
        istr >> prefix;
    }

    {
        in.getline( line, maxLineLen );
        std::string lineStr( line );
        while( lineStr[0] == '#' )
        {
            in.getline( line, maxLineLen );
            lineStr = line;
        }
        std::istringstream istr( lineStr );
        istr >> viewPos;
    }

    int idx;
    for( idx=0; idx<nWalls; idx++ )
    {
        Wall wall( in );
        wallList.push_back( wall );
    }
    in.close();

    return( wallList );
}
