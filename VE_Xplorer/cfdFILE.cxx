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
 * File:          $RCSfile: cfdFILE.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdFILE.h"
#include "cfdFileInfo.h"
#include "cfdDCS.h"
#include "cfdNode.h"

#include <cassert>
using namespace std;

//#include <Performer/pfdu.h>
//#include <Performer/pf/pfDCS.h>
//#include <Performer/pf/pfGeode.h>
//#include <Performer/pr/pfGeoSet.h>
//#include <Performer/pr/pfMaterial.h>
//#include <Performer/pr/pfLight.h>
//#include <Performer/pr.h>
//#include <Performer/pfutil.h>
//#include <Performer/pr/pfTexture.h>
//#include <Performer/pr/pfLPointState.h>
//#include <Performer/pf/pfTraverser.h>

#include <vtkGeometryFilter.h>
#include <vtkPolyDataNormals.h>
#include <vtkSTLReader.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>

#include <vpr/Util/Debug.h>


cfdFILE::cfdFILE( fileInfo *geomFile, cfdDCS *worldDCS  )
{
   // Need to fix this and move some code to cfdNode
   // Leave some code here no more cfdFileInfo
/*// this constructor is used by cfdApp
   vprDEBUG(vprDBG_ALL,1) << " cfdFILE:geomFile->fileName = " 
                          << geomFile->fileName
                          << std::endl << vprDEBUG_FLUSH;

   // this->geode = new pfGeode;
   this->mat0 = new pfMaterial();
   this->mat1 = new pfMaterial();
   this->mat_count = 0;

   this->DCS         = geomFile->dcs;
   this->transparent = geomFile->trans;
   this->color       = geomFile->color; 

   std::cout << " Load file 1" << std::endl;
   this->node = pfdLoadFile( geomFile->fileName );  // pfNode
   std::cout << " Load file 2" << std::endl;
   //this->node->ref();
   this->node->flatten( 0 );
   this->DCS->addChild( this->node );
   worldDCS->AddChild( this->DCS );
    
   if ( this->color == 1 )
   {
      for( int i=0; i<3; i++ )
      {
         this->stlColor[i] = geomFile->stlColor[i];
      }
   }*/
}

cfdFILE::cfdFILE( char* geomFile, cfdDCS* worldDCS  )
{
   // Need to fix this and move some code to cfdNode
   // Leave some code here no more cfdFileInfo
/*// this constructor is used by cfdApp
   vprDEBUG(vprDBG_ALL,1) << " cfdFILE:geomFile->fileName = " 
                          << geomFile->fileName
                          << std::endl << vprDEBUG_FLUSH;

   // this->geode = new pfGeode;
   this->mat0 = new pfMaterial();
   this->mat1 = new pfMaterial();
   this->mat_count = 0;

   this->DCS         = geomFile->dcs;
   this->transparent = geomFile->trans;
   this->color       = geomFile->color; 

   std::cout << " Load file 1" << std::endl;
   this->node = pfdLoadFile( geomFile->fileName );  // pfNode
   std::cout << " Load file 2" << std::endl;
   //this->node->ref();
   this->node->flatten( 0 );
   this->DCS->addChild( this->node );
   worldDCS->AddChild( this->DCS );
    
   if ( this->color == 1 )
   {
      for( int i=0; i<3; i++ )
      {
         this->stlColor[i] = geomFile->stlColor[i];
      }
   }*/
}
cfdFILE::cfdFILE( float opVal, float stlColor[3], char *filename  )
{
// this constructor is used by cfdFrame
   vprDEBUG(vprDBG_ALL,1) 
      << " cfdFILE: geometry file : " << filename 
      << ", opVal = " << opVal
      << ", stlColor = " << stlColor[0] 
      << " : " << stlColor[1] << " : " << stlColor[2]
      << std::endl << vprDEBUG_FLUSH;

   //this->node = pfdLoadFile( filename );  // pfNode

   //this->mat0 = new pfMaterial();
   //this->mat1 = new pfMaterial();
   this->mat_count = 0;

   if ( stlColor[ 0 ] == -1 && stlColor[ 1 ] == -1 && stlColor[ 2 ] == -1 )
   {
      this->color = 0; 
   }
   else
   {
      this->color = 1;
   }
   vprDEBUG(vprDBG_ALL,1) << " cfdFILE: color flag = " << this->color 
                          << std::endl << vprDEBUG_FLUSH;

   if ( this->color )
   {
      for( int i=0; i<3; i++ )
      {
         this->stlColor[ i ] = stlColor[ i ];
      }
   }
   
   //if ( opVal != 1 )
   {
      Initialize ( opVal );
   }
}


cfdFILE::~cfdFILE()
{
   vprDEBUG(vprDBG_ALL,2) << "cfdFILE Destructor" 
                          << std::endl << vprDEBUG_FLUSH;
/*
   if ( this->mat0 != NULL )
   {
      pfDelete( this->mat0 );
   }

   if ( this->mat1 != NULL )
   {
      pfDelete( this->mat1 );
   }

   if ( this->node != NULL )
   {
      pfDelete( this->node );
   }
*/
}

void cfdFILE::SetFILEProperties( int color, int trans, float* stlColor )
{
   this->color = color;
   this->transparent = trans;
   this->stlColor[ 0 ] = stlColor[ 0 ];
   this->stlColor[ 1 ] = stlColor[ 1 ];
   this->stlColor[ 2 ] = stlColor[ 2 ];
}

void cfdFILE::Initialize( float op_val )
{
   this->op = op_val;
   setOpac( op_val );
}

cfdNode* cfdFILE::GetcfdNode( void )
{
   return this->node;
}

cfdDCS* cfdFILE::getpfDCS()
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
   
   //this->pfTravNodeMaterial( this->node );
}

//*****************************************************************/
//*****************************************************************/
// Traverses the given node's structure looking for geosets.
// It then changes the geostates of them all to have the same
// given material.
/* Need to fix this 
void cfdFILE::pfTravNodeMaterial( pfNode* node_1 )
{
    
   assert( node_1 != NULL && "bad pointer passed in" );
   //assert( mat != NULL && "bad pointer passed in" );
	int i ;
	int num ;
   //float colorone[6];
   //pfMaterial* oldmat   = NULL;
	pfGeoState* geostate = NULL;
	pfGeoSet*	geoset   = NULL;
   pfMaterial* testMat = NULL;
   //static int count =1;
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

         if (geostate != NULL)
         {
            geostate->setMode( PFSTATE_ANTIALIAS, PFAA_ON );
            geostate->setMode( PFSTATE_ENLIGHTING, PF_ON );
            geostate->setMode( PFSTATE_CULLFACE, PFCF_OFF );
            geostate->setMode( PFSTATE_SHADEMODEL, PFSM_GOURAUD );
            //geostate->setAttr( PFSTATE_LIGHTMODEL, matLight );
            vprDEBUG(vprDBG_ALL,3) << "Done setting Transparency "
                                   << std::endl << vprDEBUG_FLUSH;

            testMat = (pfMaterial*)geostate->getAttr( PFSTATE_FRONTMTL );
            this->bmaterial = (pfMaterial*)geostate->getAttr( PFSTATE_BACKMTL );
            vprDEBUG(vprDBG_ALL,2) << "setting alpha to " << op 
                                   << std::endl << vprDEBUG_FLUSH;
            //std::cout << this->fmaterial << " : " << this->bmaterial << std::endl;
            if ( testMat != NULL )
            {
               vprDEBUG(vprDBG_ALL,2) << "Setting Front Material : " << op 
                                      << std::endl << vprDEBUG_FLUSH;
               vprDEBUG(vprDBG_ALL,2) << " Color Flag : " << color
                                      << std::endl << vprDEBUG_FLUSH;

               testMat->setAlpha( op );
               if ( op == 1 ) 
               {
                  //Turn colors on
                     //std::cout << " Alpha value : " << testMat->getAlpha() << std::endl;
                   if( color == 1 )
                  {
                     //this->fmaterial->getColor( PFMTL_AMBIENT, &colorone[0], &colorone[1], &colorone[2] );
                     //this->fmaterial->getColor( PFMTL_DIFFUSE, &colorone[3], &colorone[4], &colorone[5] );
                     //vprDEBUG(vprDBG_ALL,3) 
                     //   << " Front Color 1 : " << colorone[0]<< " : " 
                     //   <<  colorone[1]<< " : " << colorone[2] << std::endl << vprDEBUG_FLUSH;
                     testMat->setColor( PFMTL_DIFFUSE , stlColor[0], stlColor[1], stlColor[2]);
                     testMat->setColor( PFMTL_AMBIENT , stlColor[0], stlColor[1], stlColor[2]);
                     geoset->setDrawBin(PFSORT_OPAQUE_BIN);  // draw last
                     //std::cout << " Alpha value : " << testMat->getAlpha() << std::endl;
                     geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_OFF);
                     geoset->setDrawBin(PFSORT_OPAQUE_BIN);  // draw last
                     vprDEBUG(vprDBG_ALL,2) 
                        << " Front Color : " << stlColor[0]<< " : " 
                        <<  stlColor[1]<< " : " << stlColor[2]
                        << std::endl << vprDEBUG_FLUSH;
                     //this->fmaterial->setAlpha( .2 );
                  }
                  else
                  {
                     // Do NOT turn of transparency here because textured
                     // objects may have transparent textures
                     //geoset->setDrawBin(PFSORT_OPAQUE_BIN);  // draw last
                     testMat->setColorMode( PFMTL_FRONT, PFMTL_CMODE_AMBIENT_AND_DIFFUSE );
                     //this->fmaterial->setColorMode( PFMTL_FRONT, PFMTL_CMODE_EMISSION );
                     //this->fmaterial->setColorMode( PFMTL_FRONT, PFMTL_CMODE_SPECULAR );
                     vprDEBUG(vprDBG_ALL,3) << "Set color Mode "
                                         << std::endl << vprDEBUG_FLUSH;
                  }
               }
               else
               {
                  //geostate->setMode( PFSTATE_TRANSPARENCY, PFTR_FAST );//PFTR_HIGH_QUALITY );// | PFTR_NO_OCCLUDE );
                  geoset->setDrawBin(PFSORT_TRANSP_BIN);  // draw last
                  geostate->setMode(PFSTATE_CULLFACE, PFCF_OFF); // want to see backside thru
                  geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_BLEND_ALPHA | PFTR_NO_OCCLUDE);
                  if( color == 1 )
                  {
                     testMat->setColor( PFMTL_DIFFUSE , 1.0f, 1.0f, 1.0f );
                     testMat->setColor( PFMTL_AMBIENT , 1.0f, 1.0f, 1.0f );
                     vprDEBUG(vprDBG_ALL,2)
                        << "Front Color Transparent : " << stlColor[0] << " : " 
                        <<  stlColor[1] << " : " << stlColor[2]
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
            
            if ( this->bmaterial != NULL )
            {
               vprDEBUG(vprDBG_ALL, 2) << "Setting Back Material "
                                      << std::endl << vprDEBUG_FLUSH;

               this->bmaterial->setAlpha (op );
               if ( op == 1 ) 
               {
                  //Turn colors on
                  vprDEBUG(vprDBG_ALL, 3) << "Set color Mode "
                                         << std::endl << vprDEBUG_FLUSH;
                  if( color == 1)
                  {
                     this->bmaterial->setColor( PFMTL_DIFFUSE , stlColor[0], stlColor[1], stlColor[2]);
                     this->bmaterial->setColor( PFMTL_AMBIENT , stlColor[0], stlColor[1], stlColor[2]);
                     geoset->setDrawBin(PFSORT_OPAQUE_BIN);  // draw last
                     //std::cout << " Alpha value : " << testMat->getAlpha() << std::endl;
                     geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_OFF);
                     vprDEBUG(vprDBG_ALL,3) 
                        << "Back Color : " << stlColor[0] << " : " 
                        << stlColor[1]<< " : " << stlColor[2]
                        << std::endl << vprDEBUG_FLUSH;
                  }
                  else
                  {
                     this->bmaterial->setColorMode( PFMTL_BACK,  PFMTL_CMODE_AMBIENT_AND_DIFFUSE );
                  }
                  //this->bmaterial->setColorMode( PFMTL_BACK, PFMTL_CMODE_EMISSION );
                  //this->bmaterial->setColorMode( PFMTL_BACK, PFMTL_CMODE_SPECULAR );
               }
               else
               {
                  //geostate->setMode( PFSTATE_TRANSPARENCY, PFTR_FAST );//PFTR_HIGH_QUALITY );// | PFTR_NO_OCCLUDE );
                  geoset->setDrawBin(PFSORT_TRANSP_BIN);  // draw last
                  geostate->setMode(PFSTATE_CULLFACE, PFCF_OFF); // want to see backside thru
                  geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_BLEND_ALPHA | PFTR_NO_OCCLUDE);
                  if( color == 1 )
                  {
                     this->bmaterial->setColor( PFMTL_DIFFUSE , 1.0f, 1.0f, 1.0f );
                     this->bmaterial->setColor( PFMTL_AMBIENT , 1.0f, 1.0f, 1.0f );
                     vprDEBUG(vprDBG_ALL,3)
                        << " Back Color : " << stlColor[0]<< " : " 
                        << stlColor[1] << " : " << stlColor[2]
                        << std::endl << vprDEBUG_FLUSH;
                     //this->fmaterial->setAlpha( .2 );
                  }
                  else
                  {
                     this->bmaterial->setColorMode( PFMTL_BACK,  PFMTL_CMODE_OFF );
                  }
               }
               geostate->setAttr(PFSTATE_BACKMTL, this->bmaterial);
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

	   	vprDEBUG(vprDBG_ALL,1) << num << " GROUP TYPE "
                                << std::endl << vprDEBUG_FLUSH;

         for (i=0; i < num; i++)
	   	{
	   		//if(count)
               //matList.push_back( new pfMaterial() );
            pfTravNodeMaterial( ((pfGroup*)node_1)->getChild(i) ) ;
	   	}
         //count = 0;
	   }

}
*/
