/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/xplorer/environment/cfdTeacher.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/Clone.h>
#include <ves/xplorer/scenegraph/Group.h>

//#include <ves/xplorer/event/viz/cfdScalarBarActor.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/Debug.h>

#include <ves/xplorer/util/fileIO.h>

#include <ves/open/xml/Command.h>

#include <iostream>
#include <string>
#include <sstream>

#include <gmtl/MatrixOps.h>
#include <gmtl/Matrix.h>
#include <gmtl/gmtl.h>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;

#include <osgDB/WriteFile>
#include <osg/Node>
#include <osgUtil/Optimizer>
#include <osgDB/FileNameUtils>

////////////////////////////////////////////////////////////////////////////////
cfdTeacher::cfdTeacher( std::string specifiedDir, osg::Group* worldDCS )
        :
        m_currentScene( 0 ),
        directory( specifiedDir ),
        dcs( new ves::xplorer::scenegraph::DCS() ),
        mModelRoot( worldDCS )
{
    vprDEBUG( vesDBG, 1 ) << "|\tStored Scenes directory : \"" << this->directory
        << "\"" << std::endl << vprDEBUG_FLUSH;

    // initialize in case the directory is not there...
    // We use a node specifically for teacher to keep book keeping on loaded 
    // scenes a little bit easier
    dcs->SetName( "Teacher Node" );
    mModelRoot->addChild( dcs.get() );
    //Get ive, osg, and pfb filenames
    pfbFileNames = ves::xplorer::util::fileIO::GetFilesInDirectory( directory, ".pfb" );
    std::vector< std::string > tempFilenames;
    tempFilenames = ves::xplorer::util::fileIO::GetFilesInDirectory( directory, ".ive" );
    pfbFileNames.insert( pfbFileNames.end(), tempFilenames.begin(),
                         tempFilenames.end() );
    tempFilenames.clear();
    tempFilenames = ves::xplorer::util::fileIO::GetFilesInDirectory( directory, ".osg" );
    pfbFileNames.insert( pfbFileNames.end(), tempFilenames.begin(),
                         tempFilenames.end() );

    // how many performer binaries found ?
    vprDEBUG( vesDBG, 1 ) << "|\t\tNumber of stored scenes found: "
        << pfbFileNames.size() << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
cfdTeacher::~cfdTeacher( )
{
    if( m_currentScene )
    {
        dcs->removeChild( m_currentScene->GetNode() );
        delete m_currentScene;
        m_currentScene = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::CADEntityHelper* cfdTeacher::GetCurrentLoadedScene( int i )
{
    //only load the current one so clear out the list
    if( m_currentScene )
    {
        this->dcs->removeChild( m_currentScene->GetNode() );
        delete m_currentScene;
        m_currentScene = 0;
    }

    m_currentScene = new ves::xplorer::scenegraph::CADEntityHelper();
    m_currentScene->LoadFile( this->pfbFileNames[ i ], false );
    return m_currentScene;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* cfdTeacher::GetDCS()
{
    return this->dcs.get();
}
////////////////////////////////////////////////////////////////////////////////
int cfdTeacher::getNumberOfFiles()
{
    return pfbFileNames.size();
}
////////////////////////////////////////////////////////////////////////////////
std::string cfdTeacher::getFileName( int i )
{
    if( i >= 0 && i < static_cast<int>( this->pfbFileNames.size() ) )
    {
        return this->pfbFileNames[i];
    }
    else
    {
        return NULL;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::RecordScene()
{
    //check if the path to STORED_FILES exists
    boost::filesystem::path dir_path( this->directory, boost::filesystem::no_check );
    
    try
    {
        if( !boost::filesystem::is_directory( dir_path ) )
        {
            std::cout << "|\tCreated " << directory 
                << " directory." << std::endl;
            boost::filesystem::create_directory( dir_path );
        }
    }
    catch ( const std::exception& ex )
    {
        std::cout << ex.what() << std::endl;
        std::cout << "|\tCreated " << directory 
            << " directory." << std::endl;
        //Needed for backwards compatibility
        boost::filesystem::create_directory( dir_path );
    }
    
    // Generate a .pfb filename...
    std::string pfb_filename;
    std::ostringstream dirStringStream;
    dirStringStream << this->directory << "/stored_scene_"
        << pfbFileNames.size() << ".ive";

    std::string dirString = dirStringStream.str();
    pfb_filename = dirString.c_str();
    pfbFileNames.push_back( pfb_filename );

    vprDEBUG( vesDBG, 0 ) << "|\tScene stored as " << pfb_filename
        << std::endl << vprDEBUG_FLUSH;

    // store the world DCS matrix..
    if( mModelRoot.valid() )
    {
        writePFBFile( ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot(), pfb_filename );
    }
    else
    {
        writePFBFile( ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode(), pfb_filename );
    }

    vprDEBUG( vesDBG, 1 ) << "|\tStored Scene Output " << pfbFileNames.size()
        << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::LoadScene( unsigned int whichChild )
{
    if( whichChild > pfbFileNames.size() )
    {
        return;
    }

    vprDEBUG( vesDBG, 2 ) << "LOAD_PFB_FILE: addChild" << std::endl
        << vprDEBUG_FLUSH;

    this->GetDCS()->addChild( GetCurrentLoadedScene( whichChild )->GetNode() );
}
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::ClearStoredScenes()
{
    vprDEBUG( vesDBG, 2 )
        << "|\tcfdTeacher::ClearStoredScenes : CLEAR_ALL or CLEAR_PFB_FILE "
        << std::endl  << vprDEBUG_FLUSH;

    if( dcs->getNumChildren() > 0 )
    {
        dcs->removeChild( dcs->GetChild( 0 ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::UpdateCommand()
{
    std::cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::writePFBFile( osg::Node* graph, std::string fileName )
{
    osgUtil::Optimizer optimizer;
    optimizer.optimize( graph );
    bool status = osgDB::writeNodeFile( *graph, fileName );
    if( status )
    {
        std::cout << "|\tSuccessfully written " << fileName << std::endl;
    }
    else
    {
        osgDB::getNameLessExtension( fileName );
        fileName += ".osg";
        status = osgDB::writeNodeFile( *graph, fileName );
        if( status )
        {
            std::cout << "|\tSuccessfully written " << fileName << std::endl;
            return;
        }
        std::cout << "|\tThere were errors writing " << fileName << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::Reset()
{
    vprDEBUG( vesDBG, 1 ) << "|\tStored Scenes directory : \"" << this->directory
    << "\"" << std::endl << vprDEBUG_FLUSH;
    //Remove the other application's stored scenes
    ClearStoredScenes();
    //Delete the old active scene if we have one
    if( m_currentScene )
    {
        this->dcs->removeChild( m_currentScene->GetNode() );
        delete m_currentScene;
        m_currentScene = 0;
    }
    //Get ive, osg, and pfb filenames for the new application
    pfbFileNames.clear();
    pfbFileNames = ves::xplorer::util::fileIO::GetFilesInDirectory( directory, ".pfb" );
    std::vector< std::string > tempFilenames;
    tempFilenames = ves::xplorer::util::fileIO::GetFilesInDirectory( directory, ".ive" );
    pfbFileNames.insert( pfbFileNames.end(), tempFilenames.begin(),
                         tempFilenames.end() );
    tempFilenames.clear();
    tempFilenames = ves::xplorer::util::fileIO::GetFilesInDirectory( directory, ".osg" );
    pfbFileNames.insert( pfbFileNames.end(), tempFilenames.begin(),
                         tempFilenames.end() );

    // how many performer binaries found ?
    vprDEBUG( vesDBG, 1 ) << "|\t\tNumber of stored scenes found: "
        << pfbFileNames.size() << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
