#include "CreatePartTreeFileVisitor.h"

#include <osgDB/ReadFile>
#include <osgDB/Registry>
#include <osgDB/ReaderWriter>
#include <osgDB/FileUtils>
#include <osgDB/FileNameUtils>
#include <osg/Node>

#include <iostream>
#include <string>

#include <boost/filesystem/path.hpp>

#include <ves/xplorer/scenegraph/util/RemoveNodeNameVisitor.h>

int main( int argc, char* argv[] )
{
    if( argc < 2 )
    {
        std::cout << "Usage: osgPartTree <file to load> " << std::endl;
        return 1;
    }
    //read in osg file
    osg::ref_ptr< osg::Node > tempCADNode = osgDB::readNodeFile( argv[ 1 ] );
    if( !tempCADNode.valid() )
    {
        std::cout << "Invalid file loaded" << std::endl;
        return 1;
    }
    
    boost::filesystem::path p( argv[ 1 ] );

    //create stream
    std::string fileName = p.parent_path().string() + "/partTree.txt";
    //create visitor
    ves::xplorer::scenegraph::util::RemoveNodeNameVisitor polyTransCleanup( tempCADNode.get(), "", "" );
    ves::xplorer::scenegraph::CreatePartTreeFileVisitor partTreeVisitor( tempCADNode.get(), fileName );
    //put visitor on node
    //the visitor will write a file just like the graphviz visitor
    //exit
    return 0;
}
