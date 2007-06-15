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

#include "cfdDashboard.h"
#include "cfdReadParam.h"
#include "cfd1DTextInput.h"
#include "cfdDCS.h"
#include "cfdGroup.h"
#include "cfdNode.h"

#include <vpr/Util/Debug.h>

#include <fstream>

cfdDashboard::cfdDashboard( std::string param, cfdGroup *masterNode  )
{
   this->_masterNode = masterNode;
   this->_param = param;

}
/*
cfdDashboard::cfdDashboard( cfdDashboard* x )
{
}
*/

cfdDashboard::~cfdDashboard( void )
{
   vprDEBUG(vprDBG_ALL,2) << "cfdDashboard Destructor" 
                          << std::endl << vprDEBUG_FLUSH;

}

void cfdDashboard::CreateDashboard( void )
{
   int numObjects, i;
   char text[ 256 ];
   std::ifstream input;
   input.open( this->_param.c_str() );
   input >> numObjects; 

   for( i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
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
         cfdReadParam::SkipModuleBlock( input, 7 );
      }
      else if ( id == 3 )
      {
         cfdReadParam::SkipModuleBlock( input, 14 );
      }
      else if ( id == 4 )
      {
         float scale[3], trans[3], rot[3];
         std::string geomFilename, tagName;

         cfdReadParam::read_pf_DCS_parameters( input, scale, trans, rot );

         this->SetRotationArray( rot );
         this->SetTranslationArray( trans );
         this->SetScaleArray( scale );
         
         input >> _filename;
         input.getline( text, 256 );   //skip past remainder of line

         this->_node = new cfdNode();
         this->_node->LoadFile( (char*)this->_filename.c_str() );
         //this->_node->flatten( 0 );
         this->AddChild( this->_node );
         this->_masterNode->AddChild( this );

         for ( int j = 0; j < 4; j += 2 )
         {
            cfd1DTextInput displayText1;
            this->_dashDisplay[ j ] = displayText1;

            cfd1DTextInput displayText2;
            this->_dashDisplay[ j + 1 ] = displayText2;

            cfdReadParam::read_pf_DCS_parameters( input, scale, trans, rot );
            this->_dashDisplay[ j ].SetRotationArray( rot );
            this->_dashDisplay[ j ].SetTranslationArray( trans );
            this->_dashDisplay[ j ].SetScaleArray( scale );

            cfdReadParam::read_pf_DCS_parameters( input, scale, trans, rot );
            this->_dashDisplay[ j + 1 ].SetRotationArray( rot );
            this->_dashDisplay[ j + 1 ].SetTranslationArray( trans );
            this->_dashDisplay[ j + 1 ].SetScaleArray( scale );

            input >> tagName;
            input.getline( text, 256 );   //skip past remainder of line
            this->_dashDisplay[ j ].SetFilename( tagName );
            
            this->AddChild( (cfdDCS*)this->_dashDisplay[ j ].getpfDCS() );
            this->AddChild( (cfdDCS*)this->_dashDisplay[ j + 1 ].getpfDCS() );
         }
      }
   }
   input.close();
}
