/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#ifndef _WIN32
#include <unistd.h>
#include <sys/types.h>
#include <sys/dir.h>
#endif

#include "VE_Xplorer/XplorerHandlers/cfdTeacher.h"

#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
#include "VE_Xplorer/SceneGraph/CADEntityHelper.h"
#include "VE_Xplorer/SceneGraph/Clone.h"
#include "VE_Xplorer/SceneGraph/Group.h"

#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/XplorerHandlers/cfdCommandArray.h"
#include "VE_Xplorer/XplorerHandlers/cfdRawNodeWriteTraverser.h"
#include "VE_Xplorer/XplorerHandlers/cfdScalarBarActor.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"

#include <iostream>
#include <string>
#include <sstream>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#include <gmtl/MatrixOps.h>
#include <gmtl/Matrix.h>
#include <gmtl/gmtl.h>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
cfdTeacher::cfdTeacher( std::string specifiedDir, VE_SceneGraph::DCS* worldDCS )
{
   this->directory = specifiedDir;

   vprDEBUG(vesDBG,1) << "PFB directory : \"" << this->directory << "\""
                          << std::endl << vprDEBUG_FLUSH;

   // initialize in case the directory is not there...
   _cfdWT = NULL;
   //this->dcs = NULL;
   //this->_worldDCS = 0;
   pfb_count = 0;
   _cfdWT = NULL;
   this->dcs = new VE_SceneGraph::DCS();
   this->dcs->SetName( "Teacher Node" );
   _worldDCS = worldDCS;
   _worldDCS->AddChild( this->dcs.get() );

   boost::filesystem::path dir_path( this->directory.c_str() );

   try
   {
      if ( boost::filesystem::is_directory( dir_path ) )
      {
         boost::filesystem::directory_iterator end_iter;
         for ( boost::filesystem::directory_iterator dir_itr( dir_path );
               dir_itr != end_iter; ++dir_itr )
         {
            try
            {
               if ( boost::filesystem::is_directory( *dir_itr ) )
               {
                  std::cout << dir_itr->leaf()<< " [directory]\n";
               }
               else
               {
                  if ( strstr( dir_itr->leaf().c_str(), ".pfb") || strstr( dir_itr->leaf().c_str(), ".ive") || strstr( dir_itr->leaf().c_str(), ".osg"))
                  {
                     std::string pathAndFileName;// = new char[strlen(dir_path.leaf().c_str() )+
                     //                                 strlen(dir_itr->leaf().c_str())+2];
                     pathAndFileName.assign( dir_path.leaf().c_str()); //strcpy(pathAndFileName,dir_path.leaf().c_str()); 
                     pathAndFileName.append( "/" );//strcat(pathAndFileName,"/");
                     pathAndFileName.append( dir_itr->leaf().c_str() );//strcat(pathAndFileName,dir_itr->leaf().c_str());

                     std::ostringstream filenameStream;
                     filenameStream << "./" << pathAndFileName;
                     this->pfbFileNames.push_back( filenameStream.str() );

                     vprDEBUG(vesDBG,0) << "Found performer binary : " << this->pfbFileNames.back()
                                            << std::endl << vprDEBUG_FLUSH;
                  }
               }
            }
            catch ( const std::exception& ex )
            {
	            std::cout << ex.what() << std::endl;
            }
         }
      }
   }
   catch ( const std::exception& ex )
	{
	   std::cout << ex.what() << std::endl;
	   vprDEBUG(vesDBG,1) << "The STORED_FILES directory will be made when a new scene is stored :" 
                             <<  std::endl << vprDEBUG_FLUSH;
	}

   // how many performer binaries found ?
   pfb_count = this->pfbFileNames.size();
   vprDEBUG(vesDBG,1) << "Number of performer binaries: " << pfb_count
                          << std::endl << vprDEBUG_FLUSH;

   //this->node = new VE_SceneGraph::CADEntityHelper* [ this->numFiles ];
  
   for (int i=0; i<this->pfb_count; i++)
   {
      this->node.push_back( new VE_SceneGraph::CADEntityHelper() );
	   this->node.back()->LoadFile( this->pfbFileNames[ i ] );
   }
}
////////////////////////////////////////////////////////////////////////////////
cfdTeacher::~cfdTeacher( )
{
   int i;
   for ( i = 0; i < static_cast<int>(this->node.size()); i++)
   {  
      this->dcs->removeChild( this->node[i]->GetNode() );

      delete this->node[i];
   }

   //delete this->dcs;

   node.clear();
   
   vprDEBUG(vesDBG,1) << "exiting cfdTeacher destructor"
                          << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::CADEntityHelper* cfdTeacher::getpfNode( int i )
{
   return this->node[i];
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
      std::cout << ex.what() << std::endl;
		boost::filesystem::create_directory(dir_path);
		std::cout << "...so we made it for you..." << std::endl;
	}

   // Generate a .pfb filename...
   std::string pfb_filename;
   std::ostringstream dirStringStream;
   dirStringStream << this->directory << "/stored_scene_" 
#ifdef _PERFORMER
                  << this->pfb_count << ".pfb";
#elif _OSG
         << this->pfb_count << ".osg";
#endif
   std::string dirString = dirStringStream.str();
   pfb_filename = dirString.c_str();
   pfbFileNames.push_back( pfb_filename );

   vprDEBUG(vesDBG,0) << "scene stored as " << pfb_filename
                             << std::endl << vprDEBUG_FLUSH;

   // store the world DCS matrix..
   if ( _worldDCS.valid() )
   {
     /* gmtl::Matrix44f m = this->_worldDCS->GetMat();

      //temporarily reset the world DCS matrix to the identity
      gmtl::Matrix44f I;

      // Make an identity matrix
      gmtl::identity( I );
      this->_worldDCS->SetMat( I );*/
		VE_SceneGraph::Clone* graphToWrite = new VE_SceneGraph::Clone(_worldDCS.get());

      writePFBFile( graphToWrite->GetClonedGraph() /*this->_worldDCS*/, pfb_filename );

		delete graphToWrite;
      graphToWrite = 0;
      //this->_worldDCS->SetMat( m );
   }
   else
   {
      writePFBFile(VE_SceneGraph::cfdPfSceneManagement::instance()->GetRootNode(), pfb_filename);
   }

   this->node.push_back( new VE_SceneGraph::CADEntityHelper() );
   this->node.back()->LoadFile( this->pfbFileNames.back() );
   // store the active geometry and viz objects as a pfb
   // (but not the sun, menu, laser, or text)
 
   vprDEBUG(vesDBG,1) << "|   Stored Scene Output " << pfb_count
                             << std::endl << vprDEBUG_FLUSH;
      
    // increment the counter and reset the id to -1...
    this->pfb_count ++;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::LoadScene(unsigned int whichChild)
{
   if ( this->GetDCS()->getNumChildren() == 0 )
   {
      vprDEBUG(vesDBG,2) << "LOAD_PFB_FILE: addChild" 
                         << std::endl << vprDEBUG_FLUSH;
            
      this->GetDCS()->addChild( this->getpfNode( whichChild )->GetNode() );
   }
   else
   {
      vprDEBUG(vesDBG,2) << "LOAD_PFB_FILE: replaceChild" 
                         << std::endl << vprDEBUG_FLUSH;
      this->GetDCS()->replaceChild( this->GetDCS()->GetChild( 0 ), this->getpfNode( whichChild )->GetNode() );
   }
}
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::ClearStoredScenes()
{
   vprDEBUG(vesDBG,2) << " cfdTeacher::CheckCommandId : CLEAR_ALL or CLEAR_PFB_FILE "
                             << std::endl  << vprDEBUG_FLUSH;
   if ( this->dcs != NULL )
   {
      if ( this->GetDCS()->getNumChildren() > 0 )
      {
         this->GetDCS()->removeChild( this->GetDCS()->GetChild( 0 ) );
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
bool cfdTeacher::CheckCommandId( cfdCommandArray* commandArray )
{
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == LOAD_PFB_FILE )
   {
      vprDEBUG(vesDBG,1) << "LOAD_PFB_FILE: numChildren = " 
         << this->GetDCS()->getNumChildren()
         << ", cfdTeacher_state = "
         << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << std::endl << vprDEBUG_FLUSH;

      if ( this->GetDCS()->getNumChildren() == 0 )
      {
         vprDEBUG(vesDBG,2) << "LOAD_PFB_FILE: addChild" 
                                << std::endl << vprDEBUG_FLUSH;
            
         this->GetDCS()->addChild( 
            this->getpfNode( (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) )->GetNode() );
      }
      else
      {
         vprDEBUG(vesDBG,2) << "LOAD_PFB_FILE: replaceChild" 
                                << std::endl << vprDEBUG_FLUSH;
         int child = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
         this->GetDCS()->replaceChild( this->GetDCS()->GetChild( 0 ), this->getpfNode( child )->GetNode() );
      }
      return true;
   }
   else if ( ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CLEAR_PFB_FILE ) ||
             ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CLEAR_ALL ) )
   {      
      vprDEBUG(vesDBG,2) << " cfdTeacher::CheckCommandId : CLEAR_ALL or CLEAR_PFB_FILE "
                             << std::endl  << vprDEBUG_FLUSH;
      if ( this->dcs != NULL )
      {
         if ( this->GetDCS()->getNumChildren() > 0 )
         {
            this->GetDCS()->removeChild( this->GetDCS()->GetChild( 0 ) );
         }
         return true;
      }
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == RECORD_SCENE )
   {
	   //check if the path to STORED_FILES exists
	   boost::filesystem::path dir_path( this->directory );
	   try
	   {
	      boost::filesystem::is_directory( dir_path );
	   }
	   catch ( const std::exception& ex )
	   {
		   std::cout << ex.what() << std::endl;
		   boost::filesystem::create_directory(dir_path);
		   std::cout << "...so we made it for you..." << std::endl;
	   }

      // Generate a .pfb filename...
      std::string pfb_filename;
      std::ostringstream dirStringStream;
      dirStringStream << this->directory << "/stored_scene_" 
#ifdef _PERFORMER
                        << this->pfb_count << ".pfb";
#elif _OSG
         << this->pfb_count << ".osg";
#endif
      std::string dirString = dirStringStream.str();
      pfb_filename = dirString.c_str();

      vprDEBUG(vesDBG,0) << "scene stored as " << pfb_filename
                             << std::endl << vprDEBUG_FLUSH;

      
      // store the world DCS matrix..
      if ( _worldDCS.valid() )
      {
         
			osg::ref_ptr< VE_SceneGraph::Group > tempGroup = new VE_SceneGraph::Group();
                  
         //tempGroup->AddChild(cfdModelHandler::instance()->GetScalarBar()->GetDCS());
         
         gmtl::Matrix44f m = this->_worldDCS->GetMat();

         //temporarily reset the world DCS matrix to the identity
         gmtl::Matrix44f I;

         // Make an identity matrix
         gmtl::identity( I );
         this->_worldDCS->SetMat( I );
         //float scaleUnity[ 3 ];
         //scaleUnity[ 0 ] = scaleUnity[ 1 ] = scaleUnity[ 2 ] = 1.0f;
         //this->_worldDCS->SetScaleArray( scaleUnity );
      
        
         tempGroup->AddChild( _worldDCS.get() );
         writePFBFile( this->_worldDCS.get(), (std::string)pfb_filename.c_str() );


         tempGroup->RemoveChild( _worldDCS.get() );
         //tempGroup->RemoveChild(cfdModelHandler::instance()->GetScalarBar()->GetDCS());

         /*float* scaleArray = this->_worldDCS->GetScaleArray();
         float tempScale = 1.0f / scaleArray[ 0 ];
         gmtl::Matrix44f scaleMat;
         gmtl::setScale( scaleMat, tempScale );
         gmtl::Matrix44f mTemp = scaleMat * m;*/
         // restore the world DCS matrix...
         //this->_worldDCS->SetMat( m );
         this->_worldDCS->SetMat( m );
         //this->_worldDCS->SetScaleArray( scaleArray ); 
      }
      else
      {
         writePFBFile( VE_SceneGraph::cfdPfSceneManagement::instance()->GetRootNode(), (std::string)pfb_filename );
      }
      // store the active geometry and viz objects as a pfb
      // (but not the sun, menu, laser, or text)
      int store_int = 0;

      vprDEBUG(vesDBG,1) << "|   Stored Scene Output " << store_int << std::endl << vprDEBUG_FLUSH;
      
      // increment the counter and reset the id to -1...
      this->pfb_count ++;
      return true;
   }

   return false;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::UpdateCommand()
{
   std::cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << std::endl;
}
// Need to fix later
////////////////////////////////////////////////////////////////////////////////
void cfdTeacher::writePFBFile( VE_SceneGraph::SceneNode* graph,std::string fileName)
{
   VE_Xplorer::cfdRawNodeWriteTraverser cfdWT(fileName);

   //set the graph
   cfdWT.setNode(graph);

   //set the "swapping" callback
   cfdWT.setCallback(1);

   //write out the file
   cfdWT.writeFile();
}
////////////////////////////////////////////////////////////////////////////////
