//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#include <osg/NodeVisitor>
#include <osg/Geode>
#include <osg/Geometry>
#include <osgViewer/Viewer>
#include <osgDB/ReadFile>
#include <osgDB/WriteFile>


class GeometryOpt : public osg::NodeVisitor
{
public:
    GeometryOpt();
    virtual ~GeometryOpt();

    //META_NodeVisitor(osgBullet,GeometryOpt)

    virtual void apply( osg::Node& node );
    virtual void apply( osg::Geode& geode );

    bool changeDLtoVBO_;
    bool changeDynamicToStatic_;

protected:
};


GeometryOpt::GeometryOpt()
  : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
    changeDLtoVBO_( true ),
    changeDynamicToStatic_( false )
{
}
GeometryOpt::~GeometryOpt()
{
}

void
GeometryOpt::apply( osg::Node& node )
{
    if( changeDynamicToStatic_ )
        node.setDataVariance( osg::Object::STATIC );
    traverse( node );
}

void
GeometryOpt::apply( osg::Geode& geode )
{
    for(unsigned int i=0;i<geode.getNumDrawables();++i)
    {
        osg::Drawable* draw = geode.getDrawable(i);
        if( changeDLtoVBO_ )
        {
            draw->setUseDisplayList( false );
            draw->setUseVertexBufferObjects( true );
        }
        if( changeDynamicToStatic_ )
        {
            draw->setDataVariance( osg::Object::STATIC );
            osg::Geometry* geometry = draw->asGeometry();
            if (geometry)
            {
                geometry->getVertexArray()->setDataVariance( osg::Object::STATIC );
            }
        }
    }
}

int main()
{
    std::string inFile( "graphicstest.ive" );
    std::string outFile( "out.ive" );

    osg::ref_ptr< osg::Node > root = osgDB::readNodeFile( inFile );
    if( !root.valid() )
        return 1;

    GeometryOpt go;
    root->accept( go );
    osgDB::writeNodeFile( *root, outFile );

    osgViewer::Viewer viewer;
    viewer.setSceneData( root.get() );
    viewer.run();
}
