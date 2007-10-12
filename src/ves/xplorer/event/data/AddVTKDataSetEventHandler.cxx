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
#include <ves/xplorer/event/AddVTKDataSetEventHandler.h>
#include <ves/xplorer/event/viz/cfdModel.h>
#include <ves/xplorer/event/viz/cfdModelHandler.h>
#include <ves/xplorer/event/viz/cfdDataSet.h>
#include <ves/xplorer/util/fileIO.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/model/Model.h>

#include <ves/xplorer/event/viz/cfdDebug.h>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#include <iostream>
#include <sstream>

using namespace VE_EVENTS;
/////////////////////////////////////////////////////
//Constructor                                      //
/////////////////////////////////////////////////////
AddVTKDataSetEventHandler::AddVTKDataSetEventHandler()
:VE_EVENTS::EventHandler()
{
   _activeModel = 0;
}
////////////////////////////////////////////////////////////////////////////////
AddVTKDataSetEventHandler::AddVTKDataSetEventHandler(const AddVTKDataSetEventHandler& rhs)
:VE_EVENTS::EventHandler(rhs)
{
   
}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
AddVTKDataSetEventHandler::~AddVTKDataSetEventHandler()
{
   ;
}
///Equal operator
////////////////////////////////////////////////////////////////////////////////
AddVTKDataSetEventHandler& AddVTKDataSetEventHandler::operator=(const AddVTKDataSetEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::EventHandler::operator=(rhs);
   }
   return *this;
}
////////////////////////////////////////////////////////////////////////////////
void AddVTKDataSetEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model)
{
   try
   {
      if ( model )
      {
         _activeModel = dynamic_cast< VE_Xplorer::cfdModel* >( model );
      }
      else
      {
         _activeModel = VE_Xplorer::cfdModelHandler::instance()->GetActiveModel();
      }
   }
   catch(...)
   {
      _activeModel = 0;
      std::cout<<"Invalid object passed to AddVTKDataSetEventHandler::SetGlobalBaseObject!"<<std::endl;
   }
}
////////////////////////////////////////////////////////////////////////////////
void AddVTKDataSetEventHandler::Execute(VE_XML::XMLObject* xmlObject)
{
   try
   {
      VE_XML::Command* command = dynamic_cast<VE_XML::Command*>(xmlObject);
      VE_XML::DataValuePairWeakPtr veModelDVP = command->GetDataValuePair("CREATE_NEW_DATASETS");
      VE_XML::VE_Model::Model* veModel = dynamic_cast< VE_XML::VE_Model::Model* >( veModelDVP->GetDataXMLObject() );
      size_t numInfoPackets = veModel->GetNumberOfInformationPackets();
      for ( size_t i = (numInfoPackets-1); i < numInfoPackets; ++i )
      {
         VE_XML::ParameterBlock* tempInfoPacket = veModel->GetInformationPacket( i );

         if ( tempInfoPacket->GetProperty( "VTK_DATA_FILE" ) )
         {
            // Assume only one model for now
            // Flexibilty to have multiply models
            if ( !_activeModel )
            {
               return;
            }

            //Check to see if a dataset with same name has been loaded
            for( size_t k = 0; k < (numInfoPackets-1); ++k )
            {
                if( tempInfoPacket->GetName() == veModel->GetInformationPacket( k )->GetName() )
                {
                    std::cerr << "ERROR: Dataset with same name already loaded. No dataset loaded.  "
                              << "Please choose a different name for your dataset."<< std::endl;
                    veModel->RemoveInformationPacket( numInfoPackets-1 );
                    //Reset number of information packets
                    numInfoPackets = veModel->GetNumberOfInformationPackets();
                    return;
                }
            }

            //check to see if dataset is already on this particular model
            for ( size_t j = 0; j < _activeModel->GetNumberOfCfdDataSets(); ++j )
            {
               if ( tempInfoPacket->GetProperty( "VTK_DATA_FILE" )->GetID() == 
                    _activeModel->GetCfdDataSet( j )->GetUUID( "VTK_DATA_FILE" ) )
               {
                  continue;
               }
            }

            //If not already there lets create a new dataset
            _activeModel->CreateCfdDataSet();

            vprDEBUG(vesDBG,0) << "|\t************************************* "
                             << std::endl << vprDEBUG_FLUSH;

            vprDEBUG(vesDBG,0) << "|\tvtk DCS parameters:"
                             << std::endl << vprDEBUG_FLUSH;

            // Pass in -1 to GetCfdDataSet to get the last dataset added
            _activeModel->GetCfdDataSet( -1 )->GetDCS()->SetScaleArray( tempInfoPacket->GetTransform()->GetScaleArray()->GetArray() );
            _activeModel->GetCfdDataSet( -1 )->GetDCS()->SetTranslationArray( tempInfoPacket->GetTransform()->GetTranslationArray()->GetArray() );
            _activeModel->GetCfdDataSet( -1 )->GetDCS()->SetRotationArray( tempInfoPacket->GetTransform()->GetRotationArray()->GetArray() );

            //////////////////////////////////////////////////////////////
            // get vtk data set name...
            std::string vtk_filein = tempInfoPacket->GetProperty( "VTK_DATA_FILE" )->GetDataString();

            if ( VE_Util::fileIO::isFileReadable( vtk_filein ) ) 
            {
               vprDEBUG(vesDBG,0) << "|\tvtk file = " << vtk_filein 
                                << ", dcs = "  << _activeModel->GetCfdDataSet( -1 )->GetDCS()
                                << std::endl << vprDEBUG_FLUSH;
               _activeModel->GetCfdDataSet( -1 )->SetFileName( vtk_filein );
               _activeModel->GetCfdDataSet( -1 )->SetUUID( "VTK_DATA_FILE", tempInfoPacket->GetProperty( "VTK_DATA_FILE" )->GetID() );
            }
            else
            {
               std::cerr << "ERROR: unreadable vtk file = " << vtk_filein 
                           << ".  You may need to correct your param file."
                           << std::endl;
               exit(1);
            }
            //////////////////////////////////////////////////////////////
            if ( tempInfoPacket->GetProperty( "VTK_PRECOMPUTED_DIR_PATH" ) )
            {
               std::string precomputedDataSliceDir = tempInfoPacket->GetProperty( "VTK_PRECOMPUTED_DIR_PATH" )->GetDataString();
               _activeModel->GetCfdDataSet( -1 )->SetPrecomputedDataSliceDir( precomputedDataSliceDir );
               _activeModel->GetCfdDataSet( -1 )->SetUUID( "VTK_PRECOMPUTED_DIR_PATH", tempInfoPacket->GetProperty( "VTK_PRECOMPUTED_DIR_PATH" )->GetID() );
            }
            //////////////////////////////////////////////////////////////
            if ( tempInfoPacket->GetProperty( "VTK_SURFACE_DIR_PATH" ) )
            {
               std::string precomputedSurfaceDir = tempInfoPacket->GetProperty( "VTK_SURFACE_DIR_PATH" )->GetDataString();
               _activeModel->GetCfdDataSet( -1 )->SetPrecomputedSurfaceDir( precomputedSurfaceDir );
               _activeModel->GetCfdDataSet( -1 )->SetUUID( "VTK_SURFACE_DIR_PATH", tempInfoPacket->GetProperty( "VTK_SURFACE_DIR_PATH" )->GetID() );
            }

            LoadSurfaceFiles( _activeModel->GetCfdDataSet( -1 )->GetPrecomputedSurfaceDir() );
            //////////////////////////////////////////////////////////////
            //Load texture datasets
            if ( tempInfoPacket->GetProperty( "VTK_TEXTURE_DIR_PATH" ) )
            {
#ifdef _OSG
               vprDEBUG(vesDBG,0) << "|\tCreating texture dataset." << std::endl << vprDEBUG_FLUSH;
               _activeModel->CreateTextureDataSet();
               size_t numProperties = tempInfoPacket->GetNumberOfProperties();
               for ( size_t j = 0; j < numProperties; ++j )
               {
                  if ( tempInfoPacket->GetProperty( j )->GetDataName() == 
                        std::string( "VTK_TEXTURE_DIR_PATH" ) )
                  {
                     _activeModel->AddDataSetToTextureDataSet( 0, tempInfoPacket->GetProperty( j )->GetDataString() );
                     std::ostringstream textId;
                     textId << "VTK_SURFACE_DIR_PATH_" << j;
                     _activeModel->GetCfdDataSet( -1 )->SetUUID( textId.str(), tempInfoPacket->GetProperty( "VTK_TEXTURE_DIR_PATH" )->GetID() );
                  }
               }
#endif
            }
            //Now load up the dataset
            //_activeModel->GetCfdDataSet( -1 )->LoadData();
            for ( unsigned int i = 0; i < _activeModel->GetNumberOfCfdDataSets(); i++)
            {
               std::cout << "|\tLoading data for file " 
                           << _activeModel->GetCfdDataSet( i )->GetFileName()
                           << std::endl;
               _activeModel->GetCfdDataSet( i )->LoadData();
               _activeModel->GetCfdDataSet( i )->SetArrow( VE_Xplorer::cfdModelHandler::instance()->GetArrow() );
               if ( _activeModel->GetCfdDataSet( i )->GetParent() == _activeModel->GetCfdDataSet( i ) )
               {
                  VE_SceneGraph::SceneManager::instance()->GetWorldDCS()->
                        AddChild( _activeModel->GetCfdDataSet( i )->GetDCS() );
                  _activeModel->SetActiveDataSet( _activeModel->GetCfdDataSet( i ) );
               }
            }
         }
      }
   }
   catch(...)
   {
      std::cerr << "|\tSomething bad happened in AddVTKDataSetEventHandler::Execute." << std::endl;
   }
}
////////////////////////////////////////////////////////////////////////////////   
void AddVTKDataSetEventHandler::LoadSurfaceFiles( std::string precomputedSurfaceDir )
{
   if ( precomputedSurfaceDir.empty() )// == NULL )
   {
      vprDEBUG(vesDBG,1) << "precomputedSurfaceDir == NULL" 
                             << std::endl << vprDEBUG_FLUSH;
      return;
   }

   vprDEBUG(vesDBG,1) << "Loading surface files from " 
      << precomputedSurfaceDir << std::endl << vprDEBUG_FLUSH;

   boost::filesystem::path dir_path( precomputedSurfaceDir );

   if ( boost::filesystem::is_directory( dir_path ) )
   {
      std::cout << "|\tIn directory: "
              << dir_path.native_directory_string() << "\n";
      boost::filesystem::directory_iterator end_iter;
      for ( boost::filesystem::directory_iterator dir_itr( dir_path );
            dir_itr != end_iter; ++dir_itr )    
      {
         try
         {
            if ( boost::filesystem::is_directory( *dir_itr ) )
            {
               std::cout << "|\tIs a sub directory " << dir_itr->leaf() << " [directory]\n";
            }
            else
            {
               std::cout << dir_itr->leaf() << "\n";
               if ( strstr( dir_itr->leaf().c_str(), ".vtk") )
               {
                  std::string pathAndFileName;
                  pathAndFileName.assign( dir_path.leaf().c_str() );
                  pathAndFileName.append( "/" );
                  pathAndFileName.append( dir_itr->leaf().c_str() );
                  vprDEBUG(vesDBG,0) << "|\tsurface file = " << pathAndFileName
                                         << std::endl << vprDEBUG_FLUSH;

                  _activeModel->CreateCfdDataSet();
                  unsigned int numDataSets = _activeModel->GetNumberOfCfdDataSets();
                  // subtract 1 because this number was 1 base not 0 base
                  numDataSets -= 1;
                  _activeModel->GetCfdDataSet( -1 )->SetFileName( pathAndFileName );

                  // set the dcs matrix the same as the last file
                  _activeModel->GetCfdDataSet( -1 )->SetDCS( 
                                    _activeModel->GetCfdDataSet( (int)(numDataSets-1) )->GetDCS() ); 

                  // precomputed data that descends from a flowdata.vtk should
                  // automatically have the same color mapping as the "parent" 
                  _activeModel->GetCfdDataSet( -1 )->SetParent( 
                                    _activeModel->GetCfdDataSet( (int)(numDataSets-1) )->GetParent() );
               }
            }
         }
         catch ( const std::exception & ex )
         {
            std::cout << dir_itr->leaf() << " " << ex.what() << std::endl;
         }
      }
   }
   else // must be a file
   {
      std::cout << "\nFound: " << dir_path.native_file_string() << "\n";    
   }
}
////////////////////////////////////////////////////////////////////////////////   
void AddVTKDataSetEventHandler::Load3DTextureDirectories( std::string dirToLoad )
{
}
////////////////////////////////////////////////////////////////////////////////
