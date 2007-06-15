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
#include "cfdModuleGeometry.h"
#include "cfdGroup.h"
#include "cfdNode.h"

//#include <Performer/pf/pfDCS.h>
//#include <Performer/pf/pfGroup.h>
//#include <Performer/pf/pfGeode.h>
//#include <Performer/pr/pfGeoSet.h>
//#include <Performer/pr/pfMaterial.h>
//#include <Performer/pr/pfLight.h>
//#include <Performer/pfdu.h>
#include <cassert>
#include <iostream>
#ifdef _PERFORMER
#include <Performer/pr/pfLPointState.h>
#include <Performer/pf/pfTraverser.h>
#elif _OSG
#endif

#include <vpr/Util/Debug.h>
cfdModuleGeometry::cfdModuleGeometry( cfdGroup* masterNode )
{
   this->_masterNode = masterNode;
   this->_rgba[ 0 ] = this->_rgba[ 1 ] = this->_rgba[ 2 ] = 0.6;
   this->_rgba[ 3 ]= 0;
}

cfdModuleGeometry::~cfdModuleGeometry( void )
{
   // Do nothing now
}

void cfdModuleGeometry::SetRGBAColorArray( double* color)
{
   for ( int i = 0; i < 4; i++ )
   {
      this->_rgba[ i ] = color[ i ];
   }
   vprDEBUG(vprDBG_ALL,2) << " Color ModuleGeometry: " << this->_rgba[ 0 ]  << " : " <<  this->_rgba[ 1 ]  <<  " : " << this->_rgba[ 2 ]  << std::endl << vprDEBUG_FLUSH;
}

void cfdModuleGeometry::GetColorArray( void )
{
   vprDEBUG(vprDBG_ALL,2) << " Color ModuleGeometry: " << this->_rgba[ 0 ]  << " : " <<  this->_rgba[ 1 ]  <<  " : " << this->_rgba[ 2 ]  << std::endl << vprDEBUG_FLUSH;
}

void cfdModuleGeometry::SetTransparencyFlag( bool x )
{
   this->_transparencyFlag = x;
}

void cfdModuleGeometry::SetOpacity( float x )
{
   this->_opacityLevel = x;
}

void cfdModuleGeometry::SetColorFlag( int x )
{
   this->_colorFlag = x;
}

int cfdModuleGeometry::GetColorFlag( void )
{
   return this->_colorFlag;
}

void cfdModuleGeometry::SetModuleName( std::string filename )
{
   this->_moduleName = filename;
}

std::string cfdModuleGeometry::GetModuleName( void )
{
   return this->_moduleName;
}

void cfdModuleGeometry::SetGeometryFilename( std::string filename )
{
   this->_filename = filename;
   this->_node = new cfdNode();
   this->_node->LoadFile( (char*)this->_filename.c_str() );
   //this->_node->flatten( 0 );
   this->AddChild( this->_node );
   std::cout << "cfdModuleGeometry load geometry : " << _filename << std::endl;

   this->_masterNode->AddChild( this );   
}

void cfdModuleGeometry::Update( void )
{
   std::cout << "Update Filename : " << this->_filename << std::endl;
   std::cout << "trans : " << this->_transparencyFlag << std::endl;
   std::cout << "op : " << this->_opacityLevel << std::endl;
   std::cout << "color : " << this->_colorFlag << std::endl;
   // Need to fix this call this from the cfdNode
   //SetColorOfGeometry( this->_node );
}
