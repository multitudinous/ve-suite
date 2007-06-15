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
#include "cfdInteractiveGeometry.h"
#include "cfdDCS.h"
#include "cfdGroup.h"
#include "cfdModuleGeometry.h"
#include "cfdExecutive.h"
#include "cfdReadParam.h"

#include <vpr/Util/Debug.h>
#include <fstream>
#include <iostream>

cfdInteractiveGeometry::cfdInteractiveGeometry( std::string param, cfdGroup* masterNode)
{
   this->_masterNode = masterNode;
   this->_param      = param;
   this->_trainNode  = new cfdGroup();
   this->_masterNode->AddChild( this->_trainNode ); 
   this->CreateGeometryList();
}

cfdInteractiveGeometry::~cfdInteractiveGeometry( void )
{
   
}

void cfdInteractiveGeometry::Update( std::string activeScalar, cfdExecutive* executive )
{
   // Loop over all of cfdEexcutives plugins
   // We need an iterator here
   //for ( unsigned int i = 0; i < /*number of modules*/; i++ )
   /*{
      // check to see if current plugin is on the graph and in the plugin list
      if ( executive->GetPlugin( i )->OnSceneGraph() )
      {
         // then do nothing
      }
      // check to see if current plugin is NOT on the graph and in the plugin list
      else if ( !executive->GetPlugin( i )->OnSceneGraph() )
      {
         executive->GetPlugin( i )->AddSelfToSG();
      }
      // check if plugin is on the graph but not in the plugin list
      else if ( executive->GetPlugin( i )->GetID
      // check and see if 
      if ( executive-> == mapsomething )
      {
         // Need to find what needs to be removed or added to the scene graph
         
      }
   }*/
   // get some input data for module
/*   CORBA::Long mod_id = (CORBA::Long)executive->_name_map["GASI"];
   long numberOfTrains, numberOfSpares;
   executive->_it_map[mod_id].getVal("tex_idx_in_ntrains", numberOfTrains );
   executive->_it_map[mod_id].getVal("tex_idx_in_nspares", numberOfSpares );
   vprDEBUG(vprDBG_ALL,1) << " tex_idx_in_ntrains : " << mod_id << " : " << numberOfTrains << " : tex_idx_in_nspares" 
                           << numberOfSpares << std::endl << vprDEBUG_FLUSH;
   
   //   mod_id = (CORBA::Long)executive->_name_map["SRS"];
   //long numberOfSRSTrains;
   //executive->_it_map[mod_id].getVal("srs_idx_in_ntrains", numberOfSRSTrains );
   // std::cout << " srs_idx_in_trains : " << mod_id << " : " << numberOfSRSTrains << std::endl;
   
   mod_id = (CORBA::Long)executive->_name_map["SELX"];
   long numberOfSELXTrains;
   executive->_it_map[mod_id].getVal("selx_idx_in_ntowers", numberOfSELXTrains );
   vprDEBUG(vprDBG_ALL,1) << " selex_idx_in_trains : " << mod_id << " : " << numberOfSELXTrains << std::endl << vprDEBUG_FLUSH;
   if ( mod_id == 0 )
   {
      numberOfSELXTrains = 0;
   }
   else
   {
      numberOfSELXTrains = 1;
   }

   bool updateTrains = false;

///////////////////////////////////////////////////////////////////
// Add / remove WGSRs and SELXs based on SELX output
///////////////////////////////////////////////////////////////////
   if ( numberOfSELXTrains == 0 )
   {
      // Then add the pipes 
      //std::vector< std::pair < cfdModuleGeometry*, cfdModuleGeometry* > >::iterator iter;
      int iter = this->_swappableGeometry.size();
      for ( int i = 0; i < iter; i++ )
	   {
         //std::cout << "Here 1 : " << this->_swappableGeometry[ i ].second->GetModuleName() << std::endl;
         this->_swappableGeometry[ i ].second->Update();
         this->_swappableGeometry[ i ].first->Update();
	      if (!(this->_swappableGeometry[ i ].second->GetModuleName()).compare("SELX") ||
               !(this->_swappableGeometry[ i ].second->GetModuleName()).compare("WGSR") ) //see if SELX is in  
	      {
            //std::cout << "Here 2 : " << this->_swappableGeometry[ i ].second->GetModuleName() << std::endl;
	         if ( this->_trainNode->SearchChild( this->_swappableGeometry[ i ].first ) < 0 &&
		            this->_trainNode->SearchChild( this->_swappableGeometry[ i ].second ) < 0)
		      {
               //this->_swappableGeometry[ i ].first->Update();
		         this->_trainNode->AddChild(this->_swappableGeometry[ i ].first );
               updateTrains = true;
		      }
	         else if ( this->_trainNode->SearchChild( this->_swappableGeometry[ i ].first ) < 0 &&
			               this->_trainNode->SearchChild( this->_swappableGeometry[ i ].second ) >= 0)
		      {
		         this->_trainNode->ReplaceChild(this->_swappableGeometry[ i ].second, 
                                                this->_swappableGeometry[ i ].first );
               updateTrains = true;
		      }
	         else if ( this->_trainNode->SearchChild( this->_swappableGeometry[ i ].first ) >= 0 &&
			               this->_trainNode->SearchChild( this->_swappableGeometry[ i ].second ) >= 0)
		      {
               //this->_swappableGeometry[ i ].second->Update();
		         this->_trainNode->RemoveChild(this->_swappableGeometry[ i ].second );
               updateTrains = true;
		      }
	         else if ( this->_trainNode->SearchChild( this->_swappableGeometry[ i ].first ) >= 0 &&
			               this->_trainNode->SearchChild( this->_swappableGeometry[ i ].second ) < 0)
		      {
                        ; //we are good, do nothing
            }	         
            //break; //get out the loop
	      }
	   }
   }
   else
   {
       //Then add the SELXTrains
      int iter = this->_swappableGeometry.size();
      for ( int i = 0; i < iter; i++ )
	   {
	      if (!(this->_swappableGeometry[ i ].second->GetModuleName()).compare("SELX") ||
               !(this->_swappableGeometry[ i ].second->GetModuleName()).compare("WGSR") ) //see if SELX is in  
	      {

            vprDEBUG(vprDBG_ALL,2) << " Swappable geom - Is the pipe on ? " 
                                    << this->_trainNode->SearchChild( this->_swappableGeometry[ i ].first ) << std::endl << vprDEBUG_FLUSH;
            vprDEBUG(vprDBG_ALL,2) << " Swappable geom - Is the part on ? " 
                                    << this->_trainNode->SearchChild( this->_swappableGeometry[ i ].second ) << std::endl << vprDEBUG_FLUSH;

            this->_swappableGeometry[ i ].second->Update();
            this->_swappableGeometry[ i ].first->Update();
	         if ( this->_trainNode->SearchChild( this->_swappableGeometry[ i ].first ) < 0 &&
		            this->_trainNode->SearchChild( this->_swappableGeometry[ i ].second ) < 0)
		      {
               
		         this->_trainNode->AddChild(this->_swappableGeometry[ i ].second );
               updateTrains = true;
		      }
	         else if ( this->_trainNode->SearchChild( this->_swappableGeometry[ i ].first ) >= 0 &&
			               this->_trainNode->SearchChild( this->_swappableGeometry[ i ].second ) < 0)
		      {
		         //this->_trainNode->replaceChild(this->_swappableGeometry[ i ].first->GetPfDCS(), this->_swappableGeometry[ i ].second->GetPfDCS());
               this->_trainNode->AddChild(this->_swappableGeometry[ i ].second );
               this->_trainNode->RemoveChild(this->_swappableGeometry[ i ].first );

               updateTrains = true;
		      }
	         else if ( this->_trainNode->SearchChild( this->_swappableGeometry[ i ].first ) >= 0 &&
			               this->_trainNode->SearchChild( this->_swappableGeometry[ i ].second ) >= 0)
		      {
		         this->_trainNode->RemoveChild( this->_swappableGeometry[ i ].first );
               updateTrains = true;
		      }
	         else if ( this->_trainNode->SearchChild( this->_swappableGeometry[ i ].first ) < 0 &&
			               this->_trainNode->SearchChild( this->_swappableGeometry[ i ].second ) >= 0)
		      {
               ; //we are good, do nothing
	         }
            //break; //get out the loop
	      }
	   }
   }
   

///////////////////////////////////////////////////////////////////
// Add remove and create trains based on the number of gasifiers
///////////////////////////////////////////////////////////////////
   if ( numberOfTrains >= 1 && (long)this->_trainDCS.size() + 1 > numberOfTrains )
   {
      // If there are to many duplicates
      // Remove some
      //std::cout << " Remove Trains : " << std::endl;
      int counter = this->_trainDCS.size();
      for ( int iter = counter - 1; iter >= 0; iter-- )
      {
         this->_masterNode->RemoveChild( this->_trainDCS[ iter ] );
         delete this->_trainDCS[ iter ];
         this->_trainDCS.pop_back();
 
         if ( (long)this->_trainDCS.size() + 1 == numberOfTrains )
            break;
      } 
   }
   else if ( numberOfTrains > 1 && (long)this->_trainDCS.size() + 1 != numberOfTrains )
   {  
      // If there aren't enough modules
      // Add some
      int initialNumber = this->_trainDCS.size();
      for ( int j = initialNumber; j < numberOfTrains - 1; j++ )
      {
         this->_trainDCS.push_back( new cfdDCS() );
         float temp[ 3 ];
         temp[ 0 ] = 0;
         temp[ 1 ] = (j+1) * 40.0f;
         temp[ 2 ] = 0;
         this->_trainDCS.back()->SetTranslationArray( temp );
         this->_trainDCS.back()->AddChild( this->_trainNode->Clone( 0 ) );
         this->_masterNode->AddChild( this->_trainDCS.back() );    
      }
   }
   
///////////////////////////////////////////////////////////////////
// Update 
///////////////////////////////////////////////////////////////////
   int numberSwap = this->_swappableGeometry.size();
   ////////////////////////////////
   // Update Swappable geometry
   ////////////////////////////////
   for ( int i = 0; i < numberSwap; i++ )
   {
	   if ( (this->_swappableGeometry[ i ].first->GetModuleName()).compare("SELX") &&
            (this->_swappableGeometry[ i ].first->GetModuleName()).compare("WGSR") ) //see if SELX is in  
      {
      
         bool flag;
         double data;
         double totalComp = 0;
         // Get id for pipe of respective module
         if ( this->_swappableGeometry[ i ].second->GetColorFlag() == 1 && 
               ( !(this->_swappableGeometry[ i ].second->GetModuleName()).compare("WGSR") || 
                  !(this->_swappableGeometry[ i ].second->GetModuleName()).compare("SELX") ) &&
                this->_trainNode->SearchChild( this->_swappableGeometry[ i ].second ) >= 0
            )
         {           
            mod_id = (CORBA::Long)executive->_name_map[ this->_swappableGeometry[ i ].second->GetModuleName() ];
            //std::cout << this->_swappableGeometry[ i ].second->GetModuleName() << std::endl;
         }
         else
         {
            mod_id = (CORBA::Long)executive->_name_map[ this->_swappableGeometry[ i ].first->GetModuleName() ];
            //std::cout << this->_swappableGeometry[ i ].second->GetModuleName() << std::endl;
         }

         {
            std::string syngasNames[]={ "CO", "H2", "CH4", "H2S", 
                                          "COS", "NH3", "HCl", "CO2", 
                                          "H2O", "N2", "Ar", "O2", 
                                          "SO2", "SO3", "NO", "NO2"};

            for ( int k = 0; k < 16; k++ )
            {
               // get some port data for module
               totalComp += executive->_pt_map[mod_id].getDouble( (char*)syngasNames[ k ].c_str(), &flag );
               if ( !flag )
               {
                  std::cerr << " SwappableGeom :: ERROR : Sumation Map doesn't have " << (char*)syngasNames[ k ].c_str() << std::endl;
               }
            }
         }  

         data = executive->_pt_map[mod_id].getDouble( (char*)activeScalar.c_str(), &flag ) / totalComp;

         if ( flag )
         {
            double* color = executive->_3dMesh->GetLookupTable()->GetColor( data );
            vprDEBUG(vprDBG_ALL,1) << "SwappableGeom :: Module being modified : "<<this->_swappableGeometry[ i ].first->GetModuleName() << " : " << data << std::endl << vprDEBUG_FLUSH;
         
            this->_swappableGeometry[ i ].first->SetRGBAColorArray( color );
            this->_swappableGeometry[ i ].first->Update();
            if ( this->_swappableGeometry[ i ].second->GetColorFlag() == 1 )
            {
               this->_swappableGeometry[ i ].second->SetRGBAColorArray( color );
               this->_swappableGeometry[ i ].second->Update();
            }
         }
         else
         {
            double color[ 4 ] = { 0.6, 0.6, 0.6, 1.0 };
            this->_swappableGeometry[ i ].first->SetRGBAColorArray( color );
            this->_swappableGeometry[ i ].first->Update();
            if ( this->_swappableGeometry[ i ].second->GetColorFlag() == 1 )
            {
               this->_swappableGeometry[ i ].second->SetRGBAColorArray( color );
               this->_swappableGeometry[ i ].second->Update();
            }
            vprDEBUG(vprDBG_ALL,1) << " Map doesn't have " << activeScalar << std::endl << vprDEBUG_FLUSH;
         }   
      }
   }
      
   ////////////////////////////////
   // Update geometry
   ////////////////////////////////
   for ( int i = 0; i < this->_numberOfModules; i++ )
   {
      //std::cout << (this->_moduleGeometry[ i ]->GetModuleName()).compare( "BUILDING" );
      // If these aren't the active module
      if ( (this->_moduleGeometry[ i ]->GetModuleName()).compare( "BUILDING" ) &&
            (this->_moduleGeometry[ i ]->GetModuleName()).compare( "REI_Gasi" ) &&
            (this->_moduleGeometry[ i ]->GetModuleName()).compare( "GLASS" ) &&
            (this->_moduleGeometry[ i ]->GetModuleName()).compare( "SPARE_Gasi" ) )
      {
         bool flag;
         double data;
         double totalComp = 0;
         // Get id for pipe of respective module
         std::map<std::string, int>::iterator nameIter;
         nameIter = executive->_name_map.find( "REI_Gasi" );

         if ( !(this->_moduleGeometry[ i ]->GetModuleName()).compare( "GASI" ) &&
               nameIter != executive->_name_map.end() )
         {
            // If the module name is GASI but REI_Gasi is in the network
            mod_id = (CORBA::Long)executive->_name_map[ "REI_Gasi" ];
         }
         else
         {
            // If not just do everything the normal way
            mod_id = (CORBA::Long)executive->_name_map[ this->_moduleGeometry[ i ]->GetModuleName() ];
         }

         {
            std::string syngasNames[]={ "CO", "H2", "CH4", "H2S", 
                                          "COS", "NH3", "HCl", "CO2", 
                                          "H2O", "N2", "Ar", "O2", 
                                          "SO2", "SO3", "NO", "NO2"};

            for ( int k = 0; k < 16; k++ )
            {
               // get some port data for module
               totalComp += executive->_pt_map[mod_id].getDouble( (char*)syngasNames[ k ].c_str(), &flag );
               if ( !flag )
               {
                  std::cerr << " ERROR : Map doesn't have " << (char*)syngasNames[ k ].c_str() << std::endl;
               }
            }
         }  

         data = executive->_pt_map[mod_id].getDouble( (char*)activeScalar.c_str(), &flag ) / totalComp;

         if ( flag )
         {
            double* color = executive->_3dMesh->GetLookupTable()->GetColor( data );

            std::cout << " |\t Active Module Name : " << this->_moduleGeometry[ i ]->GetModuleName()
                        << "  Data Value : " << data << std::endl;
         
            this->_moduleGeometry[ i ]->SetRGBAColorArray( color );
            this->_moduleGeometry[ i ]->Update();
         }
         else
         {
            double color[ 4 ] = { 0.6, 0.6, 0.6, 1.0 };
            this->_moduleGeometry[ i ]->SetRGBAColorArray( color );
            this->_moduleGeometry[ i ]->Update();
            vprDEBUG(vprDBG_ALL,1) << " Map doesn't have " << activeScalar << std::endl << vprDEBUG_FLUSH;
         }   
      }
      // For everything but the spare gasifier
      else if ( (this->_moduleGeometry[ i ]->GetModuleName()).compare( "SPARE_Gasi" ) )
      {
         std::map<std::string, int>::iterator nameIter;
         nameIter = executive->_name_map.find( "REI_Gasi" );

         if ( (nameIter != executive->_name_map.end() && !(this->_moduleGeometry[ i ]->GetModuleName()).compare( "REI_Gasi" ) ))
         {
            vprDEBUG(vprDBG_ALL,1) << "REI Gasifier is in the network 1 " << std::endl << vprDEBUG_FLUSH;
            this->_moduleGeometry[ i ]->SetOpacity( 0.2 );
            // Remove extra trains

            int counter = this->_trainDCS.size();
            for ( int iter = counter - 1; iter >= 0; iter-- )
            {
               this->_masterNode->RemoveChild( this->_trainDCS[ iter ] );
               delete this->_trainDCS[ iter ];
               this->_trainDCS.pop_back();
            }
         }
         else if ( nameIter != executive->_name_map.end() && !(this->_moduleGeometry[ i ]->GetModuleName()).compare( "GLASS" ) )
         {
            // Drop buildings
            vprDEBUG(vprDBG_ALL,2) << " Drop the buildings " << std::endl << vprDEBUG_FLUSH;
            this->_trainNode->RemoveChild( this->_moduleGeometry[ i ] );
         }
         else
         {
            vprDEBUG(vprDBG_ALL,1) << "REI Gasifier is not in the network 2 " << std::endl << vprDEBUG_FLUSH;
            this->_moduleGeometry[ i ]->SetOpacity( 1.0f ); 
            if ( !(this->_moduleGeometry[ i ]->GetModuleName()).compare( "GLASS" ) &&
                  this->_trainNode->SearchChild( this->_moduleGeometry[ i ] ) < 0 )
            {
               // Add building back
               this->_trainNode->AddChild( this->_moduleGeometry[ i ] );
            }                         
         }

         this->_moduleGeometry[ i ]->Update();
      }
      // If it is the spare gasifier
      else if ( !(this->_moduleGeometry[ i ]->GetModuleName()).compare( "SPARE_Gasi" ) )
      {
         std::map<std::string, int>::iterator nameIter;
         nameIter = executive->_name_map.find( "REI_Gasi" );

         // Check to see if we need to add or remove it
         if ( numberOfSpares == 0 && 
               this->_masterNode->SearchChild( this->_moduleGeometry[ i ] ) >= 0
            )
         {
            // Remove the spare 
            this->_masterNode->RemoveChild( this->_moduleGeometry[ i ] );
         }
         else if ( numberOfSpares > 0 &&
                     this->_masterNode->SearchChild( this->_moduleGeometry[ i ] ) < 0
                 )
         {
            // Add it 
            this->_masterNode->AddChild( this->_moduleGeometry[ i ] );
         }
         else if ( this->_trainNode->SearchChild( this->_moduleGeometry[ i ] ) >= 0 )
         {
            this->_trainNode->RemoveChild( this->_moduleGeometry[ i ] );
         }
         else if ( nameIter != executive->_name_map.end() && 
                     this->_masterNode->SearchChild( this->_moduleGeometry[ i ] ) >= 0 
                 )
         {
            // Remove the spare 
            this->_masterNode->RemoveChild( this->_moduleGeometry[ i ] );
         }
      }
   }
////////////////////////////////
//Final Update
////////////////////////////////
   if ( updateTrains )
   {
      int counter = this->_trainDCS.size();
      for ( int iter = counter - 1; iter >= 0; iter-- )
      {
         this->_masterNode->RemoveChild( this->_trainDCS[ iter ] );
         delete this->_trainDCS[ iter ];
         this->_trainDCS.pop_back();
      }
      
      int initialNumber = this->_trainDCS.size();
      for ( int j = initialNumber; j < numberOfTrains - 1; j++ )
      {
         this->_trainDCS.push_back( new cfdDCS() );
         float temp[ 3 ];
         temp[ 0 ] = 0;
         temp[ 1 ] = (j+1) * 40.0f;
         temp[ 2 ] = 0;
         this->_trainDCS.back()->SetTranslationArray( temp );
         this->_trainDCS.back()->AddChild( this->_trainNode->Clone( 0 ) );
         this->_masterNode->AddChild( this->_trainDCS.back() );    
      }
      updateTrains = false;
   }*/
}

void cfdInteractiveGeometry::CreateGeometryList( void )
{
   int numObjects, i;
   char text[ 256 ];
   std::ifstream input;
   input.open( this->_param.c_str() );
   input >> numObjects; 
   input.getline( text, 256 );   //skip past remainder of line

   vprDEBUG(vprDBG_ALL,1) << " Number of Obejcts in Interactive Geometry : " << numObjects << std::endl  << vprDEBUG_FLUSH;
   for( i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
      vprDEBUG(vprDBG_ALL,1) << "Id of object in Interactive Geometry : " << id << std::endl << vprDEBUG_FLUSH;
      input.getline( text, 256 );   //skip past remainder of line
      if ( id == 1 )
      {
         int numberOfGauges;
         input >> numberOfGauges;
         input.getline( text, 256 );   //skip past remainder of line
         
         for ( int j = 0; j < numberOfGauges; j++ )
         {
            cfdReadParam::SkipModuleBlock( input, 12 );
         }
      }
      else if ( id == 2 )
      {
         this->_moduleGeometry.push_back( new cfdModuleGeometry( this->_trainNode ) );

         float scale[3], trans[3], rot[3];
         double color[ 4 ];
         std::string geomFilename, tagName;
         int colorFlag, transparency;

         input >> transparency;
         input.getline( text, 256 );   //skip past remainder of line
         this->_moduleGeometry.back()->SetTransparencyFlag( (bool)transparency );
         this->_moduleGeometry.back()->SetOpacity( 1.0 );
         input >> colorFlag;

         this->_moduleGeometry.back()->SetColorFlag( colorFlag );
         if ( colorFlag == 1 )
         {
            input >> color[ 0 ] >> color[ 1 ] >> color [ 2 ];
            color[ 4 ] = 1;
            input.getline( text, 256 );   //skip past remainder of line
            this->_moduleGeometry.back()->SetRGBAColorArray( color );         
         }
         else
         {
            color[ 0 ] = color[ 1 ] = color [ 2 ] = 0.6;
            color[ 4 ] = 1;
            input.getline( text, 256 );   //skip past remainder of line
            this->_moduleGeometry.back()->SetRGBAColorArray( color ); 
         }

         cfdReadParam::read_pf_DCS_parameters( input, scale, trans, rot );

         this->_moduleGeometry.back()->SetRotationArray( rot );
         this->_moduleGeometry.back()->SetTranslationArray( trans );
         this->_moduleGeometry.back()->SetScaleArray( scale );
         
         input >> geomFilename;
         input.getline( text, 256 );   //skip past remainder of line
         this->_moduleGeometry.back()->SetGeometryFilename( geomFilename );
         //std::cout << geomFilename << std::endl;

         input >> tagName;
         input.getline( text, 256 );   //skip past remainder of line
         this->_moduleGeometry.back()->SetModuleName( tagName );        
         //std::cout << tagName << std::endl;
         this->_moduleGeometry.back()->Update();
      }
      else if ( id == 3 )
      {
         //cfdReadParam::SkipModuleBlock( input, 14 );
         this->_swappableGeometry.push_back( std::make_pair( 
                           new cfdModuleGeometry( this->_trainNode ), 
                           new cfdModuleGeometry( this->_trainNode ) ) );

         
            float scale[3], trans[3], rot[3];
            double color[ 4 ];
            std::string geomFilename, tagName;
            int colorFlag, transparency;

            input >> transparency;
            input.getline( text, 256 );   //skip past remainder of line
            this->_swappableGeometry.back().first->SetTransparencyFlag( (bool)transparency );
            this->_swappableGeometry.back().first->SetOpacity( 1.0 );

            input >> colorFlag;
            this->_swappableGeometry.back().first->SetColorFlag( colorFlag );

            if ( colorFlag == 1 )
            {
               input >> color[ 0 ] >> color[ 1 ] >> color [ 2 ];
               color[ 3 ] = 1;
               input.getline( text, 256 );   //skip past remainder of line
               this->_swappableGeometry.back().first->SetRGBAColorArray( color );         
            }
            else
            {
               color[ 0 ] = color[ 1 ] = color [ 2 ] = 0.6;
               color[ 3 ] = 1;
               input.getline( text, 256 );   //skip past remainder of line
               this->_swappableGeometry.back().first->SetRGBAColorArray( color ); 
            }

            cfdReadParam::read_pf_DCS_parameters( input, scale, trans, rot );

            this->_swappableGeometry.back().first->SetRotationArray( rot );
            this->_swappableGeometry.back().first->SetTranslationArray( trans );
            this->_swappableGeometry.back().first->SetScaleArray( scale );
         
            input >> geomFilename;
            input.getline( text, 256 );   //skip past remainder of line
            this->_swappableGeometry.back().first->SetGeometryFilename( geomFilename );
            //std::cout << geomFilename << std::endl;

            input >> tagName;
            input.getline( text, 256 );   //skip past remainder of line
            this->_swappableGeometry.back().first->SetModuleName( tagName );        
            //std::cout << tagName << std::endl;

            // Start read and intialization for second object
            input >> transparency;
            this->_swappableGeometry.back().second->SetTransparencyFlag( (bool)transparency );
            this->_swappableGeometry.back().second->SetOpacity( 1.0 );
            input.getline( text, 256 );   //skip past remainder of line

            input >> colorFlag;
            this->_swappableGeometry.back().second->SetColorFlag( colorFlag );

            if ( colorFlag == 1 )
            {
               input >> color[ 0 ] >> color[ 1 ] >> color [ 2 ];
               color[ 3 ] = 1;
               input.getline( text, 256 );   //skip past remainder of line
               this->_swappableGeometry.back().second->SetRGBAColorArray( color );         
            }
            else
            {
               color[ 0 ] = color[ 1 ] = color [ 2 ] = 0.6;
               color[ 3 ] = 1;
               input.getline( text, 256 );   //skip past remainder of line
               this->_swappableGeometry.back().second->SetRGBAColorArray( color ); 
            }

            cfdReadParam::read_pf_DCS_parameters( input, scale, trans, rot );

            this->_swappableGeometry.back().second->SetRotationArray( rot );
            this->_swappableGeometry.back().second->SetTranslationArray( trans );
            this->_swappableGeometry.back().second->SetScaleArray( scale );
         
            input >> geomFilename;
            input.getline( text, 256 );   //skip past remainder of line
            this->_swappableGeometry.back().second->SetGeometryFilename( geomFilename );
            // std::cout << geomFilename << std::endl;

            input >> tagName;
            input.getline( text, 256 );   //skip past remainder of line
            this->_swappableGeometry.back().second->SetModuleName( tagName );        
            //std::cout << tagName << std::endl;
         
      }
      else if ( id == 4 )
      {
         cfdReadParam::SkipModuleBlock( input, 32 );
      }
   }
   input.close();

   this->_numberOfModules = this->_moduleGeometry.size();
}


