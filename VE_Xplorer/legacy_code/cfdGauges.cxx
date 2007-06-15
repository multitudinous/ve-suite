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
#include "cfdGauges.h"
#include "cfd1DTextInput.h"
#include "cfdDigitalAnalogGauge.h"
#include "cfdReadParam.h"
#include "cfdExecutive.h"
#include "cfdGroup.h"
#include "interface.h"

#include <fstream>
#include <sstream>
#include <iostream>
#include <map>

cfdGauges::cfdGauges( std::string param, cfdGroup* masterNode )
{
   this->param = param;
   this->_masterNode = masterNode;
   //int numGauges = param->GetNumberOfGauges();
   // Need to pass in the master node
   // Need to pass in the executive
   this->CreateGaugeList();
}

cfdGauges::~cfdGauges( void )
{
   for ( int i = 0; i < this->_numberOfGauges; i++ )
   {
      delete this->_gaugesList[ i ];
   }
   
   this->_gaugesList.clear();
}

void cfdGauges::Update( std::string activeModule, cfdExecutive* executive )
{
/*   for ( int i = 0; i < this->_numberOfGauges; i++ )
   {
      if ( activeModule == this->_gaugesList[ i ]->GetModuleName() ||
           ( !activeModule.compare( "REI_Gasi" ) && !(this->_gaugesList[ i ]->GetModuleName()).compare( "GASI" ) )
         )
      {  
         CORBA::Long mod_id = (CORBA::Long)executive->_name_map[activeModule];
         // get some port data for module
         std::ostringstream dataStream;
         std::string dataString;
         //100.0;//
         double data;
         if ( !(this->_gaugesList[ i ]->GetDataTag()).compare( "NO_EMMISION" ) )
         {
            data = executive->_pt_map[mod_id].getDouble( (char*)(this->_gaugesList[ i ]->GetDataTag().c_str()) );
            data += executive->_pt_map[mod_id].getDouble( "NO2_EMMISION" );
         }
         else
         {
            data = executive->_pt_map[mod_id].getDouble( (char*)(this->_gaugesList[ i ]->GetDataTag().c_str()) );
         }
         
         std::cout << " |\t Active Module Name : " << this->_gaugesList[ i ]->GetModuleName() 
                     << "  Active Tag Name : " << this->_gaugesList[ i ]->GetDataTag()
                     << "  Data Value : " << data << std::endl;

         dataStream << data;
         dataString = dataStream.str();
         
         this->_gaugesList[ i ]->SetDataValue( dataString );
         this->_gaugesList[ i ]->Update();
      }
   }*/
}

// Update Function Specfic to IHCC demo for Angran
// Code needs to be deleted as soon as dc trip is over 06-24-2004
/*void cfdGauges::Update( cfdExecutive* executive )
{
   for ( int i = 0; i < this->_numberOfGauges; i++ )
   {
         CORBA::Long mod_id = (CORBA::Long)executive->_name_map[activeModule];
         // get some port data for module
         std::ostringstream dataStream;
         std::string dataString;
         //100.0;//
         double data;
         if ( !(this->_gaugesList[ i ]->GetDataTag()).compare( "NO_EMMISION" ) )
         {
            data = executive->_pt_map[mod_id].getDouble( (char*)(this->_gaugesList[ i ]->GetDataTag().c_str()) );
            data += executive->_pt_map[mod_id].getDouble( "NO2_EMMISION" );
         }
         else
         {
            data = executive->_pt_map[mod_id].getDouble( (char*)(this->_gaugesList[ i ]->GetDataTag().c_str()) );
         }
         
         std::cout << " |\t Active Module Name : " << this->_gaugesList[ i ]->GetModuleName() 
                     << "  Active Tag Name : " << this->_gaugesList[ i ]->GetDataTag()
                     << "  Data Value : " << data << std::endl;

         dataStream << data;
         dataString = dataStream.str();
         
         this->_gaugesList[ i ]->SetDataValue( dataString );
         this->_gaugesList[ i ]->Update();
   }
}
*/

void cfdGauges::SetNumberOfGauges( int x )
{
   this->_numberOfGauges = x;
}

void cfdGauges::CreateGaugeNames( void )
{
   // Get list of names from the network
}

void cfdGauges::CreateGaugeList( void )
{
   int numObjects, i;
   char text[ 256 ];
   std::ifstream input;
   input.open( this->param.c_str() );
   input >> numObjects; 
   input.getline( text, 256 );   //skip past remainder of line

//std::cout << numObjects << std::endl;
   for( i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
//std::cout << id << std::endl;
      input.getline( text, 256 );   //skip past remainder of line
      if ( id == 1 )
      {
         input >> _numberOfGauges;
//std::cout << _numberOfGauges << std::endl;
         input.getline( text, 256 );   //skip past remainder of line
         
         float scale[3], trans[3], rot[3];
         std::string geomFilename, tagName;

         for ( int j = 0; j < this->_numberOfGauges; j++ )
         {
            cfdReadParam::read_pf_DCS_parameters( input, scale, trans, rot );

            this->_gaugesList.push_back( new cfdDigitalAnalogGauge( _masterNode ) );

            this->_gaugesList[ j ]->SetRotationArray( rot );
            this->_gaugesList[ j ]->SetTranslationArray( trans );
            this->_gaugesList[ j ]->SetScaleArray( scale );
            
            input >> geomFilename;
            input.getline( text, 256 );   //skip past remainder of line
            this->_gaugesList[ j ]->SetGeometryFilename( geomFilename );
//std::cout << geomFilename << std::endl;

            cfdReadParam::read_pf_DCS_parameters( input, scale, trans, rot );
            ((cfd1DTextInput*)this->_gaugesList[ j ]->_textOutput.first)->SetRotationArray( rot );
            ((cfd1DTextInput*)this->_gaugesList[ j ]->_textOutput.first)->SetTranslationArray( trans );
            ((cfd1DTextInput*)this->_gaugesList[ j ]->_textOutput.first)->SetScaleArray( scale );

            input >> tagName;
//std::cout << tagName << std::endl;
            this->_gaugesList[ j ]->SetDataTag( tagName );
            input >> tagName;
//std::cout << tagName << std::endl;
            this->_gaugesList[ j ]->SetGaugeName( tagName );
            input >> tagName;
//std::cout << tagName << std::endl;
            this->_gaugesList[ j ]->SetUnitsTag( tagName );
            input.getline( text, 256 );   //skip past remainder of line
            this->_gaugesList[ j ]->CreateGaugeName();

            cfdReadParam::read_pf_DCS_parameters( input, scale, trans, rot );
            ((cfd1DTextInput*)this->_gaugesList[ j ]->_textOutput.second)->SetRotationArray( rot );
            ((cfd1DTextInput*)this->_gaugesList[ j ]->_textOutput.second)->SetTranslationArray( trans );
            ((cfd1DTextInput*)this->_gaugesList[ j ]->_textOutput.second)->SetScaleArray( scale );

            input >> tagName;
            input.getline( text, 256 );   //skip past remainder of line
            this->_gaugesList[ j ]->SetModuleName( tagName );
         }
      }
      else if ( id == 2 )
      {
         cfdReadParam::SkipModuleBlock( input, 7 );
      }
      else if ( id == 3 )
      {
         cfdReadParam::SkipModuleBlock( input, 14 );
      }
      else if ( id == 4 )
      {
         cfdReadParam::SkipModuleBlock( input, 32 );
      }
   }
   input.close();
}
