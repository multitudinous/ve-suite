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
 * File:          $RCSfile: cfdGroup.cxx,v $
 * Date modified: $Date: 2004-06-06 13:49:58 -0700 (Sun, 06 Jun 2004) $
 * Version:       $Rev: 451 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdNode.h"
#include <iostream>
#include <cstdlib>

using namespace std;

#ifdef _PERFORMER
#include <Performer/pf.h>
#include <Performer/pfdu.h>
#include <Performer/pfutil.h>
#include <Performer/pf/pfNode.h>
#include <Performer/pf/pfGeode.h>
#include <Performer/pr/pfGeoSet.h>
#include <Performer/pr/pfMaterial.h>
#include <Performer/pr/pfLight.h>
#include <Performer/pr.h>
#include <Performer/pr/pfTexture.h>
#include <Performer/pr/pfLPointState.h>
#include <Performer/pf/pfTraverser.h>
#elif _OSG
#elif _OPENSG
#endif

#include <vpr/Util/Debug.h>
 
cfdNode::cfdNode( float* scale, float* trans, float* rot ):cfdSceneNode()
{
   //this->_group = new pfNode();
}

cfdNode::cfdNode( const cfdNode& input )
{
#ifdef _PERFORMER
   this->_node = input._node;
#elif _OSG
#elif _OPENSG
#endif
}

cfdNode& cfdNode::operator=( const cfdNode& input )
{
   if ( this != &input )
   {
#ifdef _PERFORMER
   pfDelete( this->_node );
   this->_node = input._node;
#elif _OSG
#elif _OPENSG
#endif
   }
   return *this;
}

cfdNode::cfdNode( void ):cfdSceneNode()
{
   //this->_group = new pfGroup();
}

cfdNode::~cfdNode( void )
{
   // If neccesary
#ifdef _PERFORMER
   pfDelete( this->_node );
#elif _OSG
#elif _OPENSG
#endif
}

// Reimplement for other graphs
#ifdef _PERFORMER
pfNode* cfdNode::GetRawNode( void )
#elif _OSG
#elif _OPENSG
#endif
{
#ifdef _PERFORMER
   return _node;
#elif _OSG
   cout << " Error:GetRawNode !!! " << endl;
   exit( 1 );
#elif _OPENSG
   cout << " Error:GetRawNode !!! " << endl;
   exit( 1 );
#endif
}

void cfdNode::LoadFile( char* filename )
{
#ifdef _PERFORMER
   this->_node = pfdLoadFile( filename );  // pfNode
#elif _OSG
   cout << " Error:LoadFile !!! " << endl;
   exit( 1 );
#elif _OPENSG
   cout << " Error:LoadFile !!! " << endl;
   exit( 1 );
#endif
}

cfdSceneNode* cfdNode::Clone( int level )
{
#ifdef _PERFORMER
   // fix this
   //this->_node->clone( level );
   exit( 1 );
   return NULL;
#elif _OSG
   cout << " Error:Clone !!! " << endl;
   exit( 1 );
   return NULL;
#elif _OPENSG
   cout << " Error:Clone !!! " << endl;
   exit( 1 );
   return NULL;
#endif
}

void cfdNode::SetNodeProperties( int color, float trans, float* stlColor )
{
   this->color = color;
   this->op = trans;
   this->stlColor[ 0 ] = stlColor[ 0 ];
   this->stlColor[ 1 ] = stlColor[ 1 ];
   this->stlColor[ 2 ] = stlColor[ 2 ];
}

/* 
void cfdModuleGeometry::SetColorOfGeometry( cfdNode* node_1 )
{ // Needs to be fixed
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
//int attr = geoset->getAttrBind( PFGS_COLOR4 );
//if ( attr == PFGS_OFF )
//   std::cout << " attribs are off ";// << std::endl;
//else if ( attr == PFGS_OVERALL )
//   std::cout << " attribs are overall ";// << std::endl;
//else if ( attr == PFGS_PER_PRIM )
//   std::cout << " attribs are prim ";// << std::endl;
//else if ( attr == PFGS_PER_VERTEX )
//   std::cout << " attribs are vert    ";// << std::endl;
// void *alist=0; 
//   ushort *ilist=0; 

//  geoset->getAttrLists(PFGS_COORD3, &alist, &ilist); 
//   verts = (pfVec3 *) alist; 
//std::cout << geoset->getAttrBind( PFGS_COLOR4 ) << std::endl;
//int attr = geoset->getAttrBind( PFGS_COLOR4 );
//pfVec4 **colors = NULL;
// if ( ( attr == PFGS_PER_VERTEX ) || ( attr == PFGS_OVERALL ) || ( attr == PFGS_PER_PRIM ))
//{
//geoset->getAttrLists( PFGS_COLOR4, &alist, &ilist );
//pfVec4 *colors;
//colors = (pfVec4 *) alist;
// int min, max; 
//   int vertcount; 
//vertcount = geoset->getAttrRange(PFGS_COLOR4, &min, &max); 
//std::cout << " New set : " << vertcount << " : " << min << " : " << max << std::endl;
//bool alphaFlag = false;
//for ( int k = 0; k < vertcount; k++ )
//{
//if ( colors[k][3] != 1.0 )
//{
//std::cout << " alist value " <<  colors[k][0] << " : " 
//                              << colors[k][1] << " : " 
//                              << colors[k][2] << " : " 
//                              << colors[k][3] << std::endl;
//                  geoset->setDrawBin(PFSORT_TRANSP_BIN);  // draw last
//                  //geostate->setMode(PFSTATE_CULLFACE, PFCF_OFF); // want to see backside thru
//            geostate->setMode( PFSTATE_ENLIGHTING, PF_ON );
//            //geostate->setMode( PFSTATE_ENHIGHLIGHTING, PF_ON );
//            geostate->setMode( PFSTATE_CULLFACE, PFCF_OFF );
//                  geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_BLEND_ALPHA | PFTR_NO_OCCLUDE);
//   alphaFlag = true;
//   break;
//}
//}
//if ( alphaFlag )
//   continue;
//}

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
                     //geoset->setDrawBin(PFSORT_OPAQUE_BIN);  // draw last
                     //geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_OFF);
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

}*/
////////////////////////////////////////////
//*****************************************************************/
//*****************************************************************/
// Traverses the given node's structure looking for geosets.
// It then changes the geostates of them all to have the same
// given material.
void cfdNode::pfTravNodeMaterial( pfNode* node_1 )
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
   pfMaterial* bmaterial = NULL;
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
            bmaterial = (pfMaterial*)geostate->getAttr( PFSTATE_BACKMTL );
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
            
            if ( bmaterial != NULL )
            {
               vprDEBUG(vprDBG_ALL, 2) << "Setting Back Material "
                                      << std::endl << vprDEBUG_FLUSH;

               bmaterial->setAlpha (op );
               if ( op == 1 ) 
               {
                  //Turn colors on
                  vprDEBUG(vprDBG_ALL, 3) << "Set color Mode "
                                         << std::endl << vprDEBUG_FLUSH;
                  if( color == 1)
                  {
                     bmaterial->setColor( PFMTL_DIFFUSE , stlColor[0], stlColor[1], stlColor[2]);
                     bmaterial->setColor( PFMTL_AMBIENT , stlColor[0], stlColor[1], stlColor[2]);
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
                     bmaterial->setColorMode( PFMTL_BACK,  PFMTL_CMODE_AMBIENT_AND_DIFFUSE );
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
                     bmaterial->setColor( PFMTL_DIFFUSE , 1.0f, 1.0f, 1.0f );
                     bmaterial->setColor( PFMTL_AMBIENT , 1.0f, 1.0f, 1.0f );
                     vprDEBUG(vprDBG_ALL,3)
                        << " Back Color : " << stlColor[0]<< " : " 
                        << stlColor[1] << " : " << stlColor[2]
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

