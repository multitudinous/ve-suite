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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdNode.h"
#include <iostream>
#include <vpr/Util/Debug.h>
#include <cstdlib>

#ifdef _PERFORMER
#include <Performer/pf.h>
#include <Performer/pfdu.h>
#include <Performer/pfutil.h>
#include <Performer/pf/pfNode.h>
#include <Performer/pf/pfSwitch.h>
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
#include <osgDB/ReadFile>
#include <osg/Node>
#include <osg/Fog>
#include <osg/Group>
#include <osg/MatrixTransform>
#include <osg/Geode>
#include <osg/Sequence>
#include <osg/Material>
#include <osg/State>
#include <osg/StateSet>
#include <osg/StateAttribute>
#include <osg/ShadeModel>
#include <osgDB/Registry>
#include <osgDB/FileUtils>
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
   _node = input._node;
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
      //_node->unref();
      _node = input._node;
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
   //biv--do we need to set type for scene node in here?
   //this->_group = new pfNode();
#ifdef _PERFORMER
   this->_node = 0;
#elif _OSG
   _node = 0;
#elif _OPENSG
#endif
}
/////////////////////////
cfdNode::~cfdNode( void )
{
   // If neccesary
#ifdef _PERFORMER
   if ( this->_node != NULL )
   {
      vprDEBUG(vprDBG_ALL,3) << "destructor for cfdNode " 
                              << std::endl << vprDEBUG_FLUSH;
      pfDelete( this->_node );
   }
#elif _OSG
   //_node->unref();
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
         //std::cout<<"ERROR!!!"<<std::endl;
         //std::cout<<"cfdSequence doesn't contain a raw node!!!"<<std::endl;
         //std::cout<<"cfdNode::GetRawNode()"<<std::endl;
	      return _sequence;
         break;
      case CFD_SWITCH:
         return _switch;
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
   this->_node = pfdLoadFile( filename );  
#elif _OSG
   std::cout<< filename<<std::endl;

   //osgDB::RegisterReaderWriterProxy<ReaderWriterPFB> rw;
  
   //osgDB::Registry::instance()->addReaderWriter(rw);
   //osgDB::setLibraryFilePathList("C:/OSG_OP_OT-0.9.7-3/OpenSceneGraph/bin");
   _node = osgDB::readNodeFile(filename);
#elif _OPENSG
   std::cout << " Error:LoadFile !!! " << std::endl;
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
   std::cout << " Error:Clone !!! " << std::endl;
   exit( 1 );
   return NULL;
#elif _OPENSG
   std::cout << " Error:Clone !!! " << std::endl;
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

            testMat = (pfMaterial*)geostate->getAttr( PFSTATE_FRONTMTL );
            bmaterial = (pfMaterial*)geostate->getAttr( PFSTATE_BACKMTL );
            vprDEBUG(vprDBG_ALL,3) << "setting alpha to " << op 
                                   << std::endl << vprDEBUG_FLUSH;
            if ( testMat != NULL ){
               vprDEBUG(vprDBG_ALL,3) << "Setting Front Material : " << op 
                                      << std::endl << vprDEBUG_FLUSH;
               vprDEBUG(vprDBG_ALL,3) << " Color Flag : " << color
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
                     vprDEBUG(vprDBG_ALL,3) 
                        << " Front Color : " << stlColor[0]<< " : " 
                        <<  stlColor[1]<< " : " << stlColor[2]
                        << std::endl << vprDEBUG_FLUSH;
                     //this->fmaterial->setAlpha( .2 );
                  }
                  else
                  {
                     // Do NOT turn of transparency here because textured
                     // objects may have transparent textures
                     
                     // This may cause color issues
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
                  geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_BLEND_ALPHA );//| PFTR_NO_OCCLUDE);
                  if( color == 1 )
                  {
                     testMat->setColor( PFMTL_DIFFUSE , 1.0f, 1.0f, 1.0f );
                     testMat->setColor( PFMTL_AMBIENT , 1.0f, 1.0f, 1.0f );
                     vprDEBUG(vprDBG_ALL,3)
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

	   	vprDEBUG(vprDBG_ALL,2) << num << " GROUP TYPE "
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
            std::cout << "draw " << std::endl;
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
///////////////////////////////////////////////
void cfdNode::TravNodeMaterial(cfdNode* node)
{
   
	int i  = 0;
	int num = 0;

 	// If the node is a geode...
   if (node->GetCFDNodeType()== cfdNode::CFD_GEODE){
      osg::Drawable* geoset = NULL;
      osg::Geode* geode = (osg::Geode*)node->GetRawNode();
      osg::StateSet* geostate = NULL;
      
      osg::Material* material = NULL;
      osg::Material* front = NULL;
      osg::Material* back = NULL;

      // Grab each of its geosets
      num = geode->getNumDrawables();

      //std::cout << "HERE IT IS " << num << std::endl;
      for (i=0; i < num; i++){
         geoset = geode->getDrawable(i) ;
         assert( geoset != NULL && "geoset is null" );

         // Apply the material to the geostate and disable texturing
         geostate = geoset->getStateSet();

         if (geostate != NULL){
            //lighting
            geostate->setMode(GL_LIGHTING,osg::StateAttribute::ON);
            
            //culling
            geostate->setMode(GL_CULL_FACE,osg::StateAttribute::OFF);
            
            //gourand shading
            geostate->setMode(osg::StateAttribute::SHADEMODEL,
                            osg::ShadeModel::SMOOTH);

            vprDEBUG(vprDBG_ALL,3) << "Done setting Transparency "
                                   << std::endl << vprDEBUG_FLUSH;

            vprDEBUG(vprDBG_ALL,2) << "setting alpha to " << op 
                                   << std::endl << vprDEBUG_FLUSH;
            //if ( ssattrib != NULL ){
               vprDEBUG(vprDBG_ALL,2) << "Setting Material : " << op 
                                      << std::endl << vprDEBUG_FLUSH;
               vprDEBUG(vprDBG_ALL,2) << " Color Flag : " << color
                                      << std::endl << vprDEBUG_FLUSH;
               //create the material
               material = new osg::Material();
               
               //set the opacity
               material->setAlpha(osg::Material::FRONT_AND_BACK,
                                op);
               //Turn colors on
               if( color == 1 ){
                  osg::Vec4f lColor(stlColor[0],
				       	               stlColor[1],
				       	               stlColor[2],
                                         op);
                  //set up the material properties
                  material->setDiffuse(osg::Material::FRONT_AND_BACK ,
				                            lColor);
                  material->setAmbient( osg::Material::FRONT_AND_BACK ,
				                            lColor);
                   
                  vprDEBUG(vprDBG_ALL,2) 
                         << " Front Color : " << stlColor[0]<< " : " 
                         <<  stlColor[1]<< " : " << stlColor[2]
                        << std::endl << vprDEBUG_FLUSH;
               }else{
                  // Do NOT turn of transparency here because textured
                  // objects may have transparent textures
                  material->setColorMode( osg::Material::AMBIENT_AND_DIFFUSE );
                  vprDEBUG(vprDBG_ALL,3) << "Set color Mode "
                                         << std::endl << vprDEBUG_FLUSH;
               }
               //put in the appropriate bin
               if ( op == 1 ) {
                  geostate->setRenderingHint(osg::StateSet::OPAQUE_BIN);
               }else{
                  geostate->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
               }
               geostate->setAttribute(material,osg::StateAttribute::ON);            
               geoset->setStateSet(geostate);

         }else{
            vprDEBUG(vprDBG_ALL,0) 
               << "ERROR: Tried to set transparency, but this pfGeoSet"
               << " has no pfGeoState." << std::endl << vprDEBUG_FLUSH;
         }
      }
	
   }else if (node->GetCFDNodeType()== cfdNode::CFD_GROUP){
	   	// Run this traverser on each of its children (recursive)
	   	num = ((cfdGroup*)node)->GetNumChildren();

	   	vprDEBUG(vprDBG_ALL,1) << num << " GROUP TYPE "
                                << std::endl << vprDEBUG_FLUSH;

         for (i=0; i < num; i++){
	   		
            TravNodeMaterial( ((cfdGroup*)node)->GetChild(i) ) ;
         }
         //count = 0;
	   }
 }
 ///////////////////////////////////////////////////////////
 void cfdNode::TravNodeFog(osg::Node* node_1, osg::Fog* fog)
 {
 }
#elif _OPENSG
#endif

