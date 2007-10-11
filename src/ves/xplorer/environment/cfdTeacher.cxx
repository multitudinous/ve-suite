/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <VE_Xplorer/XplorerHandlers/cfdTeacher.h>

#include <VE_Xplorer/SceneGraph/SceneManager.h>
#include <VE_Xplorer/SceneGraph/CADEntityHelper.h>
#include <VE_Xplorer/SceneGraph/Clone.h>
#include <VE_Xplorer/SceneGraph/Group.h>

#include <VE_Xplorer/XplorerHandlers/cfdCommandArray.h>
#include <VE_Xplorer/XplorerHandlers/cfdScalarBarActor.h>
#include <VE_Xplorer/XplorerHandlers/cfdModelHandler.h>
#include <VE_Xplorer/XplorerHandlers/cfdDebug.h>

#include <VE_Xplorer/Utilities/fileIO.h>

#include <iostream>
#include <string>
#include <sstream>

#include <gmtl/MatrixOps.h>
#include <gmtl/Matrix.h>
#include <gmtl/gmtl.h>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

#ifdef _OSG
    #include <osgDB/WriteFile>
    #include <osg/Node>
    #include <osgUtil/Optimizer>
#endif

////////////////////////////////////////////////////////////////////////////////
cfdTeacher::cfdTeacher( std::string specifiedDir, VE_SceneGraph::DCS* worldDCS )
:
m_currentScene( 0 )
{
    this->directory = specifiedDir;

    vprDEBUG(vesDBG,1) << "|\tStored Scenes directory : \"" << this->directory 
        << "\"" << std::endl << vprDEBUG_FLUSH;

    // initialize in case the directory is not there...
    this->dcs = new VE_SceneGraph::DCS();
    this->dcs->SetName( "Teacher Node" );
    _worldDCS = worldDCS;
    _worldDCS->AddChild( this->dcs.get() );
    //Get ive, osg, and pfb filenames
    pfbFileNames = VE_Util::fileIO::GetFilesInDirectory( directory, ".pfb" );
    std::vector< std::string > tempFilenames;
    tempFilenames = VE_Util::fileIO::GetFilesInDirectory( directory, ".ive" );
    pfbFileNames.insert( pfbFileNames.end(), tempFilenames.begin(), 
        tempFilenames.end() );
    tempFilenames.clear();
    tempFilenames = VE_Util::fileIO::GetFilesInDirectory( directory, ".osg" );
    pfbFileNames.insert( pfbFileNames.end(), tempFilenames.begin(), 
        tempFilenames.end() );

    // how many performer binaries found ?
    vprDEBUG(vesDBG,1) << "|\t\tNumber of stored scenes found: " 
        << pfbFileNames.size() << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
cfdTeacher::~cfdTeacher( )
{
    if( m_currentScene )
    {
        this->dcs->removeChild( m_currentScene->GetNode() );
        delete m_currentScene;
        m_currentScene = 0;
    }
    
    //vprDEBUG(vesDBG,1) << "exiting cfdTeacher destructor"
    //    << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::CADEntityHelper* cfdTeacher::getpfNode( int i )
{
    //only load the current one so clear out the list
    if( m_currentScene )
    {
        this->dcs->removeChild( m_currentScene->GetNode() );
        delete m_currentScene;
        m_currentScene = 0;
    }

    m_currentScene = new VE_SceneGraph::CADEntityHelper();
    m_currentScene->LoadFile( this->pfbFileNames[ i ], false, true );
    return m_currentScene;
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* cfdTeacher::GetDCS()
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
   if ( i >= 0 && i < static_cast<int>(this->pfbFileNames.size()) )
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
    boost::filesystem::path dir_path( this->directory );
    try
    {
        boost::filesystem::is_directory( dir_path );
    }
    catch ( const std::exception& ex )
    {
        std::cout << "|\t" << ex.what() << std::endl;
        boost::filesystem::create_directory(dir_path);
        std::cout << "...so we made it for you..." << std::endl;
    }

   // Generate a .pfb filename...
   std::string pfb_filename;
   std::ostringstream dirStringStream;
   dirStringStream << this->directory << "/stored_scene_" 
#ifdef _PERFORMER
       << pfbFileNames.size() << ".pfb";
#elif _OSG
        << pfbFileNames.size() << ".ive";
#endif
   std::string dirString = dirStringStream.str();
   pfb_filename = dirString.c_str();
   pfbFileNames.push_back( pfb_filename );

   vprDEBUG(vesDBG,0) << "|\tScene stored as " << pfb_filename
       << std::endl << vprDEBUG_FLUSH;

   // store the world DCS matrix..
   if ( _worldDCS.valid() )
   {
      gmtl::Matrix44d m = this->_worldDCS->GetMat();

      //temporarily reset the world DCS matrix to the identity
      gmtl::Matrix44d I;

      // Make an identity matrix
      gmtl::identity( I );
      this->_worldDCS->SetMat( I );
		//VE_SceneGraph::Clone* graphToWrite = new VE_SceneGraph::Clone(_worldDCS.get());

      writePFBFile( this->_worldDCS.get(), pfb_filename );

		//delete graphToWrite;
      //graphToWrite = 0;
      this->_worldDCS->SetMat( m );
   }
   else
   {
      writePFBFile(VE_SceneGraph::SceneManager::instance()->GetRootNode(), pfb_filename);
   }
   
   vprDEBUG(vesDBG,1) << "|   Stored Scene Output " << pfbFileNames.size()
                             << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::LoadScene(unsigned int whichChild)
{
    if( whichChild > pfbFileNames.size() )
    {
        return;
    }
    
    vprDEBUG(vesDBG,2) << "LOAD_PFB_FILE: addChild" << std::endl 
        << vprDEBUG_FLUSH;

    this->GetDCS()->addChild( this->getpfNode( whichChild )->GetNode() );
}
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::ClearStoredScenes()
{
    vprDEBUG(vesDBG,2) 
        << "|\tcfdTeacher::CheckCommandId : CLEAR_ALL or CLEAR_PFB_FILE "
        << std::endl  << vprDEBUG_FLUSH;

    if( !this->dcs.valid() )
    {
        return;
    }

    if( this->GetDCS()->getNumChildren() > 0 )
    {
        this->GetDCS()->removeChild( this->GetDCS()->GetChild( 0 ) );
    }   
}
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::UpdateCommand()
{
    std::cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::writePFBFile( VE_SceneGraph::SceneNode* graph,std::string fileName)
{
#ifdef _OSG
    osgUtil::Optimizer optimizer;
    optimizer.optimize(dynamic_cast< osg::Node* >(graph));
    osgDB::writeNodeFile(*dynamic_cast< osg::Node* >(graph),fileName);
#endif
}
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::Reset()
{
    vprDEBUG(vesDBG,1) << "|\tStored Scenes directory : \"" << this->directory 
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
    pfbFileNames = VE_Util::fileIO::GetFilesInDirectory( directory, ".pfb" );
    std::vector< std::string > tempFilenames;
    tempFilenames = VE_Util::fileIO::GetFilesInDirectory( directory, ".ive" );
    pfbFileNames.insert( pfbFileNames.end(), tempFilenames.begin(), 
                         tempFilenames.end() );
    tempFilenames.clear();
    tempFilenames = VE_Util::fileIO::GetFilesInDirectory( directory, ".osg" );
    pfbFileNames.insert( pfbFileNames.end(), tempFilenames.begin(), 
                         tempFilenames.end() );
    
    // how many performer binaries found ?
    vprDEBUG(vesDBG,1) << "|\t\tNumber of stored scenes found: " 
        << pfbFileNames.size() << std::endl << vprDEBUG_FLUSH;    
}
