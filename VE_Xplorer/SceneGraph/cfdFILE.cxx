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

#include "VE_Xplorer/SceneGraph/cfdFILE.h"
#include "VE_Xplorer/SceneGraph/cfdDCS.h"
#include "VE_Xplorer/SceneGraph/cfdNode.h"

#include <cassert>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#ifdef _PERFORMER
#include <Performer/pr/pfFog.h>
#elif _OSG
#include <osg/Fog>
#endif

using namespace VE_SceneGraph;

cfdFILE::cfdFILE( std::string geomFile, VE_SceneGraph::cfdDCS* worldDCS,bool isStream  )
{
   // Need to fix this and move some code to cfdNode
   // Leave some code here no more cfdFileInfo

   this->DCS = new VE_SceneGraph::cfdDCS();
   this->node = new VE_SceneGraph::cfdNode();  
   this->node->LoadFile( geomFile.c_str(),isStream );
   fileName.assign( geomFile );//strcpy( fileName, geomFile );
   this->DCS->AddChild( this->node );
   worldDCS->AddChild( this->DCS );
#ifdef _PERFORMER
   fog = new pfFog();
#elif _OSG
   fog = new osg::Fog();
#endif
}
cfdFILE::~cfdFILE()
{
   vprDEBUG(vesDBG,2) << "cfdFILE Destructor" 
                          << std::endl << vprDEBUG_FLUSH;

   delete this->DCS;
#ifndef _OSG
   delete this->node;
#endif
   //delete fog;
}

std::string cfdFILE::GetFilename( void )
{
   return fileName;
}

void cfdFILE::SetFILEProperties( int color, int trans, float* stlColor )
{
   this->color = color;
   this->_colorFlag = color;
   this->transparent = trans;
   this->stlColor[ 0 ] = stlColor[ 0 ];
   this->stlColor[ 1 ] = stlColor[ 1 ];
   this->stlColor[ 2 ] = stlColor[ 2 ];
}

int cfdFILE::GetTransparentFlag( void )
{
   return transparent;
}

void cfdFILE::Initialize( float op_val )
{
   this->op = op_val;
   setOpac( op_val );
}

VE_SceneGraph::cfdNode* cfdFILE::GetNode( void )
{
   return this->node;
}

VE_SceneGraph::cfdDCS* cfdFILE::GetDCS()
{
   return this->DCS;
}

float cfdFILE::getOpacity()
{
   return this->op;
}


void cfdFILE::setOpac(float op_val)
{
   this->op = op_val;
   this->node->SetNodeProperties( _colorFlag, op, stlColor );
#ifdef _PERFORMER
   this->node->pfTravNodeMaterial( this->node->GetRawNode() );
#elif _OSG
   node->TravNodeMaterial(node->GetRawNode());
#endif
}
/////////////////////////////////
void cfdFILE::setFog(double dist)
{
   
   
#ifdef _PERFORMER
   fog->setColor( 0.6f, 0.6f, 0.6f);
   fog->setRange(0, dist);
   fog->setFogType(PFFOG_PIX_EXP2);
   this->node->pfTravNodeFog( this->node->GetRawNode(), fog );
#elif _OSG
   fog->setMode( osg::Fog::EXP2 );
   fog->setDensity( 1 / ( dist / 2 ) );
   fog->setColor( osg::Vec4( 0.5f, 0.5f, 0.5f, 0.0f ) );
   //fog->setStart( 0.0f );
   //fog->setStart( dist + 100 );
   //fog->setEnd( dist + 200 );
   //fog->setFogCoordinateSource( );
   this->node->TravNodeFog( this->node->GetRawNode(), fog );
#endif
}

/// Functions taken from module geometry for future merging

void cfdFILE::SetRGBAColorArray( double* color)
{
   for ( int i = 0; i < 4; i++ )
   {
      this->_rgba[ i ] = color[ i ];
   }
   vprDEBUG(vesDBG,2) << " Color ModuleGeometry: " << this->_rgba[ 0 ]  << " : " <<  this->_rgba[ 1 ]  <<  " : " << this->_rgba[ 2 ]  << std::endl << vprDEBUG_FLUSH;
}

void cfdFILE::GetColorArray( void )
{
   vprDEBUG(vesDBG,2) << " Color ModuleGeometry: " << this->_rgba[ 0 ]  << " : " <<  this->_rgba[ 1 ]  <<  " : " << this->_rgba[ 2 ]  << std::endl << vprDEBUG_FLUSH;
}

void cfdFILE::SetTransparencyFlag( bool x )
{
   this->_transparencyFlag = x;
}

void cfdFILE::SetOpacity( float x )
{
   this->_opacityLevel = x;
}

void cfdFILE::SetColorFlag( int x )
{
   this->_colorFlag = x;
}

int cfdFILE::GetColorFlag( void )
{
   return this->_colorFlag;
}

void cfdFILE::SetModuleName( std::string filename )
{
   this->_moduleName = filename;
}

std::string cfdFILE::GetModuleName( void )
{
   return this->_moduleName;
}

void cfdFILE::SetGeometryFilename( std::string filename )
{
   this->_filename = filename;
   this->_node = new VE_SceneGraph::cfdNode();
   this->_node->LoadFile( (char*)this->_filename.c_str() );
   //this->_node->flatten( 0 );
   // Need to fix this
   //this->AddChild( (cfdSceneNode*)this->_node );
   std::cout << "cfdModuleGeometry load geometry : " << _filename << std::endl;

   // Need to fix this
   //this->_masterNode->AddChild( this );   
}

void cfdFILE::Update( void )
{
   std::cout << "Update Filename : " << this->_filename << std::endl
               << "trans : " << this->_transparencyFlag << std::endl
               << "op : " << this->_opacityLevel << std::endl
               << "color : " << this->_colorFlag << std::endl;
   // Fix this later to call traverser function
   //this->_node->SetColorOfGeometry( this->_node );
}


