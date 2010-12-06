#include <boost/program_options.hpp>
#include <boost/filesystem/operations.hpp> 
#include <boost/filesystem/path.hpp>

#include <boost/lexical_cast.hpp>
#include <boost/concept_check.hpp>

#include <string>
#include <vector>
#include <list>
#include <iostream>

#include <osg/Node>
#include <osg/Image>
#include <osg/Texture2D>

#include <osgUtil/Optimizer>

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgDB/Registry>
#include <osgDB/ReaderWriter>
#include <osgDB/FileNameUtils>

#include "SwapTexture.h"

std::vector<std::string> GetFilesInDirectory( std::string dir, std::string extension );


int main( int argc, char* argv[] )
{
    // At init time:
    // Set global dds options.
    osg::ref_ptr< osgDB::ReaderWriter::Options > opt = new osgDB::ReaderWriter::Options();
    opt->setOptionString( "dds_flip" );
    osgDB::Registry::instance()->setOptions( opt );
    
    std::vector< std::string > iveFiles = GetFilesInDirectory( ".", ".ive" );
    for( size_t i = 0; i < iveFiles.size(); ++i )
    {
        //Read file in with TGA textures
        std::cout << "Processing " << iveFiles.at( i ) << std::endl;
        osg::ref_ptr< osg::Node > tgaFile = osgDB::readNodeFile( iveFiles.at( i ) );
        //Run the optimizer to improve performance
        {
            osgUtil::Optimizer graphOpti;
            graphOpti.optimize( tgaFile.get(), 
                               //Had to comment out this flag because of a bug in OSG
                               osgUtil::Optimizer::FLATTEN_STATIC_TRANSFORMS |
                               osgUtil::Optimizer::REMOVE_REDUNDANT_NODES |
                               osgUtil::Optimizer::REMOVE_LOADED_PROXY_NODES |
                               osgUtil::Optimizer::COMBINE_ADJACENT_LODS |
                               //This one can cause problems with opacity settings
                               osgUtil::Optimizer::SHARE_DUPLICATE_STATE |
                               osgUtil::Optimizer::MERGE_GEOMETRY |
                               osgUtil::Optimizer::CHECK_GEOMETRY |
                               //This one causes problems when creating physics
                               //meshes for osgBullet
                               //osgUtil::Optimizer::SPATIALIZE_GROUPS |
                               osgUtil::Optimizer::OPTIMIZE_TEXTURE_SETTINGS |
                               osgUtil::Optimizer::MERGE_GEODES |
                               osgUtil::Optimizer::STATIC_OBJECT_DETECTION );
        }
        //Replace TGA textures with DDS textures
        {
            ves::xplorer::scenegraph::util::SwapTexture ddsTextureSwap( tgaFile.get() );
        }
        //write ive file back out
        //std::string olfFileName = iveFiles.at( i );
        //boost::filesystem::path oldFileName( iveFiles.at( i ), boost::filesystem::no_check );
        std::string oldFileName = osgDB::getNameLessExtension( iveFiles.at( i ) );
        oldFileName = oldFileName + "_dds.ive";
        bool success = osgDB::writeNodeFile( *(tgaFile.get()), oldFileName );
        std::cout << "New file " << oldFileName << std::endl;
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////////////////
std::vector<std::string> GetFilesInDirectory( std::string dir, std::string extension )
{
    boost::filesystem::path dir_path( dir.c_str(), boost::filesystem::no_check );
    std::list< std::string > filesInDir;
    try
    {
        if( boost::filesystem::is_directory( dir_path ) )
        {
            boost::filesystem::directory_iterator end_iter;
            for( boost::filesystem::directory_iterator dir_itr( dir_path );
                dir_itr != end_iter; ++dir_itr )
            {
                try
                {
                    if( dir_itr->path().extension() == extension )
                    {
                        std::string pathAndFileName;
                        pathAndFileName.assign( dir_path.string() );
                        pathAndFileName.append( "/" );
                        pathAndFileName.append( dir_itr->leaf() );
                        
                        filesInDir.push_back( pathAndFileName );
                    }
                }
                catch( const std::exception& ex )
                {
                    std::cout << ex.what() << std::endl;
                }
            }
        }
    }
    catch( const std::exception& ex )
    {
        std::cout << ex.what() << std::endl;
    }
    filesInDir.sort();
    
    std::vector< std::string > filesList;
    std::list< std::string >::iterator iter;
    for( iter = filesInDir.begin(); iter != filesInDir.end(); ++iter )
    {
        filesList.push_back( *iter );
    }
    
    return filesList;
}
////////////////////////////////////////////////////////////////////////////////////////////
