/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdModuleGeometry.cxx,v $
 * Date modified: $Date: 2004/04/03 20:40:10 $
 * Version:       $Revision: 1.10 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdModuleGeometry.h"
#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfGroup.h>
#include <Performer/pf/pfGeode.h>
#include <Performer/pr/pfGeoSet.h>
#include <Performer/pr/pfMaterial.h>
#include <Performer/pr/pfLight.h>
#include <Performer/pfdu.h>
#include <assert.h>
#include <iostream>
#include <Performer/pr/pfLPointState.h>
#include <Performer/pf/pfTraverser.h>

#include <vpr/Util/Debug.h>

cfdModuleGeometry::cfdModuleGeometry( pfGroup* masterNode )
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
   this->_node = pfdLoadFile( (char*)this->_filename.c_str() );
   this->_node->flatten( 0 );
   this->GetPfDCS()->addChild( this->_node );
   std::cout << "cfdModuleGeometry load geometry : " << _filename << std::endl;

   this->_masterNode->addChild( this->GetPfDCS() );   
}

void cfdModuleGeometry::Update( void )
{
   SetColorOfGeometry( this->_node );
}

void cfdModuleGeometry::SetColorOfGeometry( pfNode* node_1 )
{
   assert( node_1 != NULL && "bad pointer passed in" );
   //assert( mat != NULL && "bad pointer passed in" );
	int i ;
	int num ;
   //int color = 1;
   float op = 1;
   //float colorone[6];
   //pfMaterial* oldmat   = NULL;
	pfGeoState* geostate = NULL;
	pfGeoSet*	geoset   = NULL;
   pfMaterial* testMat  = NULL;
   pfMaterial* bmaterial = NULL;//static int count =1;
   //int mat_s;

 	// If the node is a geode...
   if (pfIsOfType(node_1, pfGeode::getClassType()))
   {
      // Grab each of its geosets
      num = ((pfGeode*)node_1)->getNumGSets() ;
      //std::cout << "HERE IT IS " << num << std::endl;
      for (i=0; i < num; i++)
      {
         geoset = ((pfGeode*)node_1)->getGSet(i) ;
         assert( geoset != NULL && "geoset is null" );

         // Apply the material to the geostate and disable texturing
         geostate = geoset->getGState() ;
         //geoset->setDrawBin(PFSORT_TRANSP_BIN); // draw last

         if (geostate != NULL)
         {
            //geostate->setMode( PFSTATE_ANTIALIAS, PFAA_ON );
            //geostate->setMode( PFSTATE_TRANSPARENCY, PFTR_ON );
            geostate->setMode( PFSTATE_ENLIGHTING, PF_ON );
            //geostate->setMode( PFSTATE_ENHIGHLIGHTING, PF_ON );
            geostate->setMode( PFSTATE_CULLFACE, PFCF_OFF );
            //geostate->setAttr( PFSTATE_LIGHTMODEL, matLight );
            vprDEBUG(vprDBG_ALL,3) << "Done setting Transparency "
                                   << std::endl << vprDEBUG_FLUSH;

            testMat = (pfMaterial*)geostate->getAttr( PFSTATE_FRONTMTL );
            bmaterial = (pfMaterial*)geostate->getAttr( PFSTATE_BACKMTL );
            vprDEBUG(vprDBG_ALL,2) << "setting alpha to " << op 
                                   << std::endl << vprDEBUG_FLUSH;
            //std::cout << this->fmaterial << " : " << this->bmaterial << std::endl;
            if ( testMat != NULL )
            {
               //if ( this->_transparencyFlag )
               {
                  op = this->_opacityLevel;
                  //std::cout << op << " : " << _colorFlag << std::endl;
               }

               //if ( testMat->getAlpha() < 1 )
               //   std::cout << " Alpha value : " << testMat->getAlpha() << std::endl;
               //if ( bmaterial != NULL )
               //std::cout << " Back Alpha value : " <<  bmaterial->getAlpha() << std::endl;
              vprDEBUG(vprDBG_ALL,2) << "Setting Front Material : " << op 
                                      << std::endl << vprDEBUG_FLUSH;
               vprDEBUG(vprDBG_ALL,2) << " Color Flag : " << _colorFlag
                                      << std::endl << vprDEBUG_FLUSH;
               
               testMat->setAlpha( op );
               if ( op == 1 ) 
               {
                  //Turn colors on
                  if( _colorFlag == 1 )
                  {
                     //this->fmaterial->getColor( PFMTL_AMBIENT, &colorone[0], &colorone[1], &colorone[2] );
                     //this->fmaterial->getColor( PFMTL_DIFFUSE, &colorone[3], &colorone[4], &colorone[5] );
                     //vprDEBUG(vprDBG_ALL,3) 
                     //   << " Front Color 1 : " << colorone[0]<< " : " 
                     //   <<  colorone[1]<< " : " << colorone[2] << std::endl << vprDEBUG_FLUSH;
                     testMat->setColor( PFMTL_DIFFUSE , _rgba[0], _rgba[1], _rgba[2]);
                     testMat->setColor( PFMTL_AMBIENT , _rgba[0], _rgba[1], _rgba[2]);
                     //vprDEBUG(vprDBG_ALL,2) 
                     // std::cout  << " Front Color : " << _rgba[0]<< " : " 
                     //   <<  _rgba[1]<< " : " << _rgba[2]
                     //   << std::endl;// << vprDEBUG_FLUSH;
                     //this->fmaterial->setAlpha( .2 );
                     geoset->setDrawBin(PFSORT_OPAQUE_BIN);  // draw last
                     //std::cout << " Alpha value : " << testMat->getAlpha() << std::endl;
                     geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_OFF);
                  }
                  else
                  {
                     testMat->setColorMode( PFMTL_FRONT, PFMTL_CMODE_AMBIENT_AND_DIFFUSE );
                     //testMat->setColorMode( PFMTL_FRONT, PFMTL_CMODE_EMISSION );
                     //testMat->setColorMode( PFMTL_FRONT, PFMTL_CMODE_SPECULAR );
                     vprDEBUG(vprDBG_ALL,3) << "Set color Mode "
                                         << std::endl << vprDEBUG_FLUSH;
                  }
               }
               else
               {
                  //geostate->setMode( PFSTATE_TRANSPARENCY, PFTR_FAST );//PFTR_HIGH_QUALITY );// | PFTR_NO_OCCLUDE );
                  geoset->setDrawBin(PFSORT_TRANSP_BIN);  // draw last
                  //geostate->setMode(PFSTATE_CULLFACE, PFCF_OFF); // want to see backside thru
                  geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_BLEND_ALPHA | PFTR_NO_OCCLUDE);
                  if( _colorFlag == 1 )
                  {
/*
                     this->fmaterial->getColor( PFMTL_AMBIENT, &colorone[0], &colorone[1], &colorone[2] );
                     this->fmaterial->getColor( PFMTL_DIFFUSE, &colorone[3], &colorone[4], &colorone[5] );
                     vprDEBUG(vprDBG_ALL,3)
                     << " Front Color 1 : " << colorone[0] << " : " 
                     << colorone[1]<< " : " << colorone[2] 
                     << std::endl << vprDEBUG_FLUSH;
*/
                     testMat->setColor( PFMTL_DIFFUSE , 1.0f, 1.0f, 1.0f );
                     testMat->setColor( PFMTL_AMBIENT , 1.0f, 1.0f, 1.0f );
                     //testMat->setColor( PFMTL_DIFFUSE , _rgba[0], _rgba[1], _rgba[2]);
                     //testMat->setColor( PFMTL_AMBIENT , _rgba[0], _rgba[1], _rgba[2]);
                     vprDEBUG(vprDBG_ALL,2)
                        << "Front Color Transparent : " << _rgba[0] << " : " 
                        <<  _rgba[1] << " : " << _rgba[2]
                        << std::endl << vprDEBUG_FLUSH;
                     //this->fmaterial->setAlpha( 0.2 );
                  }
                  else
                  {
                     testMat->setColorMode( PFMTL_FRONT, PFMTL_CMODE_OFF );
                  }
               }
               geostate->setAttr(PFSTATE_FRONTMTL, testMat);
            }
            
            if ( bmaterial != NULL )
            {
               vprDEBUG(vprDBG_ALL, 2) << "Setting Back Material "
                                      << std::endl << vprDEBUG_FLUSH;

               bmaterial->setAlpha ( op );
               if ( op == 1 ) 
               {
                  //Turn colors on
                  vprDEBUG(vprDBG_ALL, 3) << "Set color Mode "
                                         << std::endl << vprDEBUG_FLUSH;
                  if( _colorFlag == 1)
                  {
/*
                     this->bmaterial->getColor( PFMTL_AMBIENT, &colorone[0], &colorone[1], &colorone[2] );
                     this->bmaterial->getColor( PFMTL_DIFFUSE, &colorone[3], &colorone[4], &colorone[5] );
                     vprDEBUG(vprDBG_ALL,3) 
                        << " Front Color 1 : " << colorone[0] << " : " 
                        << colorone[1] << " : " << colorone[2] 
                        << std::endl << vprDEBUG_FLUSH;
*/
                     bmaterial->setColor( PFMTL_DIFFUSE , _rgba[0], _rgba[1], _rgba[2]);
                     bmaterial->setColor( PFMTL_AMBIENT , _rgba[0], _rgba[1], _rgba[2]);
                     vprDEBUG(vprDBG_ALL,3) 
                        << "Back Color : " << _rgba[0] << " : " 
                        << _rgba[1]<< " : " << _rgba[2]
                        << std::endl << vprDEBUG_FLUSH;
                  }
                  else
                  {
                     bmaterial->setColorMode( PFMTL_BACK,  PFMTL_CMODE_AMBIENT_AND_DIFFUSE );
                  }
                  //this->bmaterial->setColorMode( PFMTL_BACK, PFMTL_CMODE_EMISSION );
                  //this->bmaterial->setColorMode( PFMTL_BACK, PFMTL_CMODE_SPECULAR );
                  geoset->setDrawBin(PFSORT_OPAQUE_BIN);  // draw last
               }
               else
               {
                  //geostate->setMode( PFSTATE_TRANSPARENCY, PFTR_FAST );//PFTR_HIGH_QUALITY );// | PFTR_NO_OCCLUDE );
                  geoset->setDrawBin(PFSORT_TRANSP_BIN);  // draw last
                  //geostate->setMode(PFSTATE_CULLFACE, PFCF_OFF); // want to see backside thru
                  geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_BLEND_ALPHA | PFTR_NO_OCCLUDE);
                  if( _colorFlag == 1 )
                  {
/*
                     this->fmaterial->getColor( PFMTL_AMBIENT, &colorone[0], &colorone[1], &colorone[2] );
                     this->fmaterial->getColor( PFMTL_DIFFUSE, &colorone[3], &colorone[4], &colorone[5] );
                     vprDEBUG(vprDBG_ALL,3) << " Front Color 1 : "
                        << colorone[0] << " : " <<  colorone[1] << " : " 
                        << colorone[2] << std::endl << vprDEBUG_FLUSH;
*/
                     bmaterial->setColor( PFMTL_DIFFUSE , 1.0f, 1.0f, 1.0f );
                     bmaterial->setColor( PFMTL_AMBIENT , 1.0f, 1.0f, 1.0f );
                     vprDEBUG(vprDBG_ALL,3)
                        << " Back Color : " << _rgba[0]<< " : " 
                        << _rgba[1] << " : " << _rgba[2]
                        << std::endl << vprDEBUG_FLUSH;
                     //this->fmaterial->setAlpha( .2 );
                  }
                  else
                  {
                     bmaterial->setColorMode( PFMTL_BACK,  PFMTL_CMODE_OFF );
                  }
               }
               geostate->setAttr(PFSTATE_BACKMTL, bmaterial);
            }
            geoset->setGState( geostate );
         }
         else
         {
            vprDEBUG(vprDBG_ALL,0) 
               << "ERROR: Tried to set transparency, but this pfGeoSet"
               << " has no pfGeoState." << std::endl << vprDEBUG_FLUSH;
         }
	   	}
	
	   }
    	else if (pfIsOfType(node_1, pfGroup::getClassType()))
	   {
	   	// Run this traverser on each of its children (recursive)
	   	num = ((pfGroup*)node_1)->getNumChildren();

	   	vprDEBUG(vprDBG_ALL,2) << num << " GROUP TYPE "
                                << std::endl << vprDEBUG_FLUSH;

         for (i=0; i < num; i++)
	   	{
	   		//if(count)
               //matList.push_back( new pfMaterial() );
            SetColorOfGeometry(((pfGroup*)node_1)->getChild(i) );
	   	}
         //count = 0;
	   }

}
