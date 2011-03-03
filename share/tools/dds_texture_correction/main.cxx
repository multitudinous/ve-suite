#include <boost/program_options.hpp>
#include <boost/filesystem/operations.hpp> 
#include <boost/filesystem/path.hpp>

#include <boost/lexical_cast.hpp>
#include <boost/concept_check.hpp>

#include <boost/algorithm/string/case_conv.hpp>

#include <boost/program_options.hpp>

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

namespace po = boost::program_options;

std::vector<std::string> GetFilesInDirectory( std::string dir, std::string extension );

////////////////////////////////////////////////////////////////////////////////////////////
int main( int argc, char* argv[] )
{

    po::options_description dds_desc("DDS Options");
    
    dds_desc.add_options()("write_dds_files", po::bool_switch(), 
        "Write the DDS files out after they have been flipped. This overwrites the original files.");

    dds_desc.add_options()("do_not_store_textures_in_ive", po::bool_switch(), 
                           "Do not write the textures out in the ive file.");
    
    dds_desc.add_options()("write_single_file", po::value< std::string >(), 
                               "Write single ive file and this is the file name.");
    dds_desc.add_options()("help,h", "Produce help message");
    
    // Construct a parser and do the actual parsing.
    po::command_line_parser parser(argc, argv);
    po::parsed_options parsed = 
        parser.options(dds_desc).allow_unregistered().run();
    
    // Finally store our options and use them.
    po::variables_map vm;
    po::store(parsed, vm);
    po::notify(vm);
    
    if( vm.count("help") )
    {
        std::cout << dds_desc << std::endl;
        return 0;
    }
    std::cout << vm.count("write_single_file") << std::endl;
    std::string singleIVEFile;
    bool singleFile = false;
    osg::ref_ptr< osg::Group > singleGroup = new osg::Group();
    if( vm.count("write_single_file") > 0 )
    {
        singleIVEFile = vm["write_single_file"].as< std::string >();
        singleFile = true;
    }

    bool writeDDSFiles = false;
    if( vm.count("write_dds_files") > 0 )
    {
        writeDDSFiles = true;
    }
    
    bool storeTextureRefs = false;
    if( vm.count("do_not_store_textures_in_ive") > 0 )
    {
        storeTextureRefs = true;
    }
    
    //Used to control if images are written into the ive file.
    osg::ref_ptr< osgDB::ReaderWriter::Options > noImgOpt = new osgDB::ReaderWriter::Options();
    //write ive file back out
    //but without including the image files in the ive:
    if( storeTextureRefs )
    {
        noImgOpt->setOptionString( "noTexturesInIVEFile" );
    }
    // At init time:
    // Set global dds options.
    //osg::ref_ptr< osgDB::ReaderWriter::Options > opt = new osgDB::ReaderWriter::Options();
    //opt->setOptionString( "dds_flip" );
    //osgDB::Registry::instance()->setOptions( opt );
    osg::ref_ptr< osg::Node > tgaFile;
    std::string oldFileName;
    
    std::vector< std::string > iveFiles = GetFilesInDirectory( ".", ".ive" );
    for( size_t i = 0; i < iveFiles.size(); ++i )
    {
        //Read file in with TGA textures
        std::cout << "Processing " << iveFiles.at( i ) << std::endl;
        tgaFile = osgDB::readNodeFile( iveFiles.at( i ) );
        //Run the optimizer to improve performance
        {
            osgUtil::Optimizer graphOpti;
            graphOpti.optimize( tgaFile.get(), 
                               //Had to comment out this flag because of a bug in OSG
                               osgUtil::Optimizer::FLATTEN_STATIC_TRANSFORMS |
                               osgUtil::Optimizer::REMOVE_REDUNDANT_NODES |
                               //osgUtil::Optimizer::REMOVE_LOADED_PROXY_NODES |
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
            ves::xplorer::scenegraph::util::SwapTexture ddsTextureSwap( tgaFile.get(), writeDDSFiles );
        }
        //std::string olfFileName = iveFiles.at( i );
        //boost::filesystem::path oldFileName( iveFiles.at( i ), boost::filesystem::no_check );
        if( !singleFile )
        {
            oldFileName = osgDB::getNameLessExtension( iveFiles.at( i ) );
            oldFileName = oldFileName + "_dds.ive";
            bool success = osgDB::writeNodeFile( *(tgaFile.get()), oldFileName, noImgOpt );
            std::cout << "New file " << oldFileName << " " << success << std::endl;
        }
        else
        {
            singleGroup->addChild( tgaFile.get() );
        }
    }
    
    if( singleFile )
    {
        //Run the optimizer to improve performance
        {
            osgUtil::Optimizer graphOpti;
            graphOpti.optimize( singleGroup.get(), 
                               //Had to comment out this flag because of a bug in OSG
                               osgUtil::Optimizer::FLATTEN_STATIC_TRANSFORMS |
                               osgUtil::Optimizer::REMOVE_REDUNDANT_NODES |
                               //osgUtil::Optimizer::REMOVE_LOADED_PROXY_NODES |
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
        bool success = osgDB::writeNodeFile( *(singleGroup.get()), singleIVEFile, noImgOpt );
        std::cout << "New file " << singleIVEFile << " " << success << std::endl;
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////////////////
std::vector<std::string> GetFilesInDirectory( std::string dir, std::string extension )
{
    boost::filesystem::path dir_path( dir.c_str(), boost::filesystem::no_check );
    std::list< std::string > filesInDir;
    std::string fileExt;
    std::string pathAndFileName;
    
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
                    fileExt = dir_itr->path().extension();
                    boost::algorithm::to_lower( fileExt );

                    if( fileExt == extension )
                    {
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
