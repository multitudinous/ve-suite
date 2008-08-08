#include "CreatePartTreeFileVisitor.h"

#include <osgDB/ReadFile>
#include <osgDB/Registry>
#include <osgDB/ReaderWriter>
#include <osgDB/FileUtils>
#include <osgDB/FileNameUtils>
#include <osg/Node>

#include <iostream>
#include <string>

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
    //create stream
    std::string fileName( "partTree.txt" );
    //create visitor
    ves::xplorer::scenegraph::CreatePartTreeFileVisitor partTreeVisitor( tempCADNode.get(), fileName );
    //put visitor on node
    //the visitor will write a file just like the graphviz visitor
    //exit
    return 0;
}
