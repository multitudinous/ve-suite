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
 * File:          $RCSfile: cfdNode.cxx,v $
 * Date modified: $Date: 2004-09-16 22:10:05 -0500 (Thu, 16 Sep 2004) $
 * Version:       $Rev: 993 $
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
#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfGeode.h>
#include <Performer/pr/pfGeoSet.h>
#include <Performer/pr/pfMaterial.h>
#include <Performer/pr/pfLight.h>
#include <Performer/pr.h>
#include <Performer/pr/pfTexture.h>
#include <Performer/pr/pfLPointState.h>
#include <Performer/pf/pfTraverser.h>
#include <Performer/pr/pfFog.h>
#elif _OSG
#include <osgDB/Reader>
#elif _OPENSG
#endif

#include <vpr/Util/Debug.h>
#include "cfdSequence.h"
////////////////////////////////////////////////////////// 
cfdNode::cfdNode( float* scale, float* trans, float* rot )
:cfdSceneNode(CFD_NODE)
{
   //biv--do we need to set type for scene node in here?
   //this->_group = new pfNode();
#ifdef _PERFORMER
   this->_node = 0;
#elif _OSG
   _node = 0;
#elif _OPENSG
#endif
}
/////////////////////////////////////////
cfdNode::cfdNode( const cfdNode& input )
:cfdSceneNode(CFD_NODE)
{
#ifdef _PERFORMER
   this->_node = input._node;
#elif _OSG
   _node = new osg::Node(input._node);
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////
cfdNode& cfdNode::operator=( const cfdNode& input )
{
   if ( this != &input ){
      //copy parent
      cfdSceneNode::operator=(input);
#ifdef _PERFORMER
      pfDelete( this->_node );
      this->_node = input._node;
#elif _OSG
      //recreate the node
      _node->unref();
      _node = new osg::Node(input._node);
#elif _OPENSG
#endif
      op = input.op;
      stlColor[0] = input.stlColor[0];
      stlColor[1] = input.stlColor[1];
      stlColor[2] = input.stlColor[2];
      color = input.color;
   }
   return *this;
}
//////////////////
cfdNode::cfdNode()
:cfdSceneNode(CFD_NODE)
{
}
/////////////////////////
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
osg::Node* cfdNode::GetRawNode(void)
#elif _OPENSG
#endif
{
   //return the appropriate graph node
   switch(GetCFDNodeType()){
      case CFD_GROUP:
         return _group;
         break;
      case CFD_GEODE:
         return _geode;
         break;
      case CFD_DCS:
         return _dcs;
         break;
      case CFD_SEQUENCE:
         return _sequence;
         break;
      case CFD_NODE:
      case CFD_OTHER:
      default:
         return _node;
         break;

   };
   return 0;
}
/////////////////////////////////////////
//load scene from file                 //
/////////////////////////////////////////
void cfdNode::LoadFile( char* filename )
{
#ifdef _PERFORMER
   cout << filename << endl;
   this->_node = pfdLoadFile( filename );  
#elif _OSG
   cout<< filename<<endl;
   _node = osgDB::readNodeFile(filename);
#elif _OPENSG
   cout << " Error:LoadFile !!! " << endl;
   exit( 1 );
#endif
}
////////////////////////////////////
cfdNode* cfdNode::Clone( int level )
{
#ifdef _PERFORMER
   // fix this
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
////////////////////////////////////////////
//set the properties on the node          //
////////////////////////////////////////////
void cfdNode::SetNodeProperties(int color,
                             float trans, 
                             float* stlColor )
{
   this->color = color;
   this->op = trans;
   this->stlColor[ 0 ] = stlColor[ 0 ];
   this->stlColor[ 1 ] = stlColor[ 1 ];
   this->stlColor[ 2 ] = stlColor[ 2 ];
}


////////////////////////////////////////////
//*****************************************************************/
//*****************************************************************/
// Traverses the given node's structure looking for geosets.
// It then changes the geostates of them all to have the same
// given material.
#ifdef _PERFORMER
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

         if (geostate != NULL){
            geostate->setMode( PFSTATE_ANTIALIAS, PFAA_ON );
            geostate->setMode( PFSTATE_ENLIGHTING, PF_ON );
            geostate->setMode( PFSTATE_CULLFACE, PFCF_OFF );
            geostate->setMode( PFSTATE_SHADEMODEL, PFSM_GOURAUD );
            vprDEBUG(vprDBG_ALL,3) << "Done setting Transparency "
                                   << std::endl << vprDEBUG_FLUSH;

            testMat = (pfMaterial*)geostate->getAttr( PFSTATE_FRONTMTL );
            bmaterial = (pfMaterial*)geostate->getAttr( PFSTATE_BACKMTL );
            vprDEBUG(vprDBG_ALL,2) << "setting alpha to " << op 
                                   << std::endl << vprDEBUG_FLUSH;
            if ( testMat != NULL ){
               vprDEBUG(vprDBG_ALL,2) << "Setting Front Material : " << op 
                                      << std::endl << vprDEBUG_FLUSH;
               vprDEBUG(vprDBG_ALL,2) << " Color Flag : " << color
                                      << std::endl << vprDEBUG_FLUSH;

               testMat->setAlpha( op );
               if ( op == 1 ) {
                  //Turn colors on
                   if( color == 1 ){
                     testMat->setColor( PFMTL_DIFFUSE ,
				        stlColor[0],
				       	stlColor[1],
				       	stlColor[2]);
                     testMat->setColor( PFMTL_AMBIENT ,
				        stlColor[0],
				       	stlColor[1],
				       	stlColor[2]);
                     geoset->setDrawBin(PFSORT_OPAQUE_BIN);  // draw last
                     geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_OFF);
                     geoset->setDrawBin(PFSORT_OPAQUE_BIN);  // draw last
                     geostate->setMode(PFSTATE_CULLFACE, PFCF_OFF);
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
                     testMat->setColorMode( PFMTL_FRONT,
				            PFMTL_CMODE_AMBIENT_AND_DIFFUSE );
                     vprDEBUG(vprDBG_ALL,3) << "Set color Mode "
                                         << std::endl << vprDEBUG_FLUSH;
                  }
               }
               else
               {
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
                     bmaterial->setColor( PFMTL_DIFFUSE ,
				          stlColor[0],
					  stlColor[1],
					  stlColor[2]);
                     bmaterial->setColor( PFMTL_AMBIENT ,
				          stlColor[0],
					  stlColor[1],
					  stlColor[2]);
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
               }
               else
               {
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


void cfdNode::pfTravNodeFog( pfNode* node_1, pfFog* fog )
{
   assert( node_1 != NULL && "bad pointer passed in" );
   //assert( mat != NULL && "bad pointer passed in" );
	int i ;
	int num ;
	pfGeoState* geostate = NULL;
	pfGeoSet*	geoset   = NULL;

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
            geostate->setMode( PFSTATE_ENFOG, PFFOG_ON );
            geostate->setAttr( PFSTATE_FOG, fog );
                        
            geoset->setGState( geostate );
            cout << "draw " << endl;
            //geoset->draw();
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
         pfTravNodeFog( ((pfGroup*)node_1)->getChild(i), fog ) ;
		}
      //count = 0;
	}
}
#elif _OSG
#elif _OPENSG
#endif

