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
#include "VE_Xplorer/SceneGraph/CADEntityHelper.h"
#include "VE_Xplorer/SceneGraph/Group.h"
#include "VE_Xplorer/SceneGraph/Switch.h"
#include "VE_Xplorer/SceneGraph/Geode.h"

#include <iostream>
#include <sstream>
#include <string>
#include <fstream>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
//#include <cstdlib>

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
#include <osg/Switch>
#include <osg/Group>
#include <osg/ShadeModel>
#include <osg/Geometry>
#include <osg/BlendFunc>
#include <osg/Array>
#include <osg/Depth>
#include <osg/LOD>
#include <osg/ShadeModel>
#include <osg/LightModel>
#include <osgDB/ReaderWriter>
#elif _OPENSG
#endif

#include <string>
#include <istream>
#include <sstream>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

namespace VE_SceneGraph{

////////////////////////////////////////////////////////////////////////////////
CADEntityHelper::CADEntityHelper()
{
   //biv--do we need to set type for scene node in here?
   //this->_group = new pfNode();
   op = 1.0f;
   stlColor[ 1 ] = stlColor[ 1 ] = stlColor[ 0 ] = -1;
   color = 0;
   twosidedlighting = false;

#ifdef _PERFORMER
   this->cadNode = 0;
   this->lightModel = 0;
#elif _OSG
   //cadNode = 0;//new osg::Node();
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
CADEntityHelper::CADEntityHelper( const CADEntityHelper& input )
{
   #ifdef _PERFORMER
   this->cadNode = input.cadNode;
   #elif _OSG
   if( cadNode.valid() )
   {
      cadNode = input.cadNode;
   }

   op = input.op;
   stlColor[0] = input.stlColor[0];
   stlColor[1] = input.stlColor[1];
   stlColor[2] = input.stlColor[2];
   color = input.color;

#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
CADEntityHelper& CADEntityHelper::operator=( const CADEntityHelper& input )
{
   if( this != &input ){

      #ifdef _PERFORMER
      pfDelete( this->cadNode );
      this->cadNode = input.cadNode;
      #elif _OSG
      //Recreate the node
      //cadNode->unref();
      cadNode = input.cadNode;
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

////////////////////////////////////////////////////////////////////////////////
// Code that can be used at a later date
// there are issues to be reolved on wether == should be defined
// as below 
/*bool Node::operator== ( const Node& node1 ) const
{
   if ( guid == node1.guid )
   {
      return true;
   }
   else
   {
      return false;
   }
}*/
////////////////////////////////////////////////////////////////////////////////
CADEntityHelper::~CADEntityHelper( void )
{
   // If neccesary
#ifdef _PERFORMER
   if ( this->cadNode != NULL )
   {
      vprDEBUG(vesDBG,3) << "destructor for Node " 
                              << std::endl << vprDEBUG_FLUSH;
      pfDelete( this->cadNode );
   }
#elif _OSG
   //cadNode->unref();
#elif _OPENSG
#endif
}
// Reimplement for other graphs
#ifdef _PERFORMER
pfNode* CADEntityHelper::GetNode( void )
#elif _OSG
osg::Node* CADEntityHelper::GetNode(void)
#elif _OPENSG
#endif
{
#ifdef _PERFORMER
   return cadNode;
#elif _OSG
   return cadNode.get();
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::SetName(std::string name)
{
   if ( GetNode() )
      GetNode()->setName(name.c_str());
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::ToggleDisplay(bool onOff)
{
   std::string value = (onOff==true)?"ON":"OFF";
   ToggleDisplay(value);
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::ToggleDisplay(std::string onOff)
{
   if ( !GetNode() )
      return;
      
   if(onOff == "ON")
   {
#ifdef _OSG
      GetNode()->setNodeMask(1);
#elif _OPENSG
#elif _PERFORMER
#endif
   }
   else if(onOff == "OFF")
   {
#ifdef _OSG
      GetNode()->setNodeMask(0);
#elif _OPENSG
#elif _PERFORMER
#endif
   }
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::LoadFile( std::string filename
#ifdef _OSG
                       ,bool isStream
#endif
                       )
{
   //std::ostringstream filestring;
   //filestring << filename;
   //if ( strstr( filestring.str().c_str(), ".stl") || strstr( filestring.str().c_str(), ".stla") )
   if ( strstr( filename.c_str(), ".stl") || strstr( filename.c_str(), ".stla") )

   {
      twosidedlighting = true;
   }

#ifdef _PERFORMER
   this->cadNode = pfdLoadFile( filename.c_str() ); 
   if ( twosidedlighting )
   {
      lightModel = new pfLightModel();
      lightModel->setLocal( PF_ON );
      lightModel->setTwoSide( PF_ON );
   }

#elif _OSG
   if(!isStream)
   {
      cadNode = osgDB::readNodeFile(filename);
   }
   else
   {
      std::istringstream textNodeStream(filename);
      cadNode = osgDB::Registry::instance()->getReaderWriterForExtension("osg")->readNode(textNodeStream).getNode();
   }
   if ( twosidedlighting )
   {
      lightModel = new osg::LightModel;
      lightModel->setTwoSided( true );
   }
      
      
#elif _OPENSG
   std::cout << " Error:LoadFile !!! " << std::endl;
   exit( 1 );
#endif
#ifdef _PERFORMER
   if(cadNode){
#elif _OSG
   if(cadNode.valid()){
#endif
      cadNode->setName(filename.c_str());
   }
}
////////////////////////////////////////////////////////////////////////////////
/*CADEntityHelper* CADEntityHelper::Clone( int level )
{
#ifdef _PERFORMER
   std::cout << " Error:Clone !!! " << level << std::endl;
   exit( 1 );
   return NULL;
#elif _OSG
   std::cout << " Error:Clone !!! " << level << std::endl;
   exit( 1 );
   return NULL;
#elif _OPENSG
   std::cout << " Error:Clone !!! " << level << std::endl;
   exit( 1 );
   return NULL;
#endif
}*/
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::SetNodeProperties(int color,
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
void CADEntityHelper::pfTravNodeMaterial( pfNode* node_1 )
{
#ifdef _DEBUG
   assert( node_1 != NULL && "bad pointer passed in" );
#endif
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
#ifdef _DEBUG
         assert( geoset != NULL && "geoset is null" );
#endif
         // Apply the material to the geostate and disable texturing
         geostate = geoset->getGState() ;

/////////////////////////////////////////


int attr = geoset->getAttrBind( PFGS_COLOR4 );
/*if ( attr == PFGS_OFF )
   std::cout << " attribs are off ";// << std::endl;
else if ( attr == PFGS_OVERALL )
   std::cout << " attribs are overall ";// << std::endl;
else if ( attr == PFGS_PER_PRIM )
   std::cout << " attribs are prim ";// << std::endl;
else if ( attr == PFGS_PER_VERTEX )
   std::cout << " attribs are vert    ";// << std::endl;
*/
void *alist=0; 
ushort *ilist=0; 

   if ( ( attr == PFGS_PER_VERTEX ) || ( attr == PFGS_OVERALL ) || ( attr == PFGS_PER_PRIM ))
   {
      geoset->getAttrLists( PFGS_COLOR4, &alist, &ilist );
      pfVec4 *colors;
      colors = (pfVec4 *) alist;
      int min, max; 
      int vertcount; 
      //int numVerts = geoset->getAttrRange( PFGS_COORD3, NULL, &max );
      //pfVec4* new_colors = new pfVec4[ numVerts ];
      //std::cout << " New set : " << max << std::endl;
      vertcount = geoset->getAttrRange( PFGS_COLOR4, &min, &max ); 
      //std::cout << " New set : " << numVerts << " : " << vertcount << " : " << min << " : " << max << " : " << ilist << std::endl;
      //float temp[ 4 ];


      for ( int k = 0; k < vertcount; ++k )
      {
         colors[ min ][ 3 ] = op;
         //std::cout << colors[ k ][ 0 ] << " : " << colors[ k ][ 1 ] << " : " <<
         //         colors[ k ][ 2 ] << " : " << colors[ k ][ 3 ] << std::endl;
      }

      if ( op != 1.0 )
      {
         geoset->setDrawBin(PFSORT_TRANSP_BIN);  // draw last
         geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_BLEND_ALPHA );//| PFTR_NO_OCCLUDE);
      }
      else
      {
         geoset->setDrawBin(PFSORT_OPAQUE_BIN);  // draw first
         geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_OFF);
      }

      geoset->setAttr( PFGS_COLOR4, PFGS_OVERALL, colors, ilist );
   }
///////////////////////////////////////////
///////////////////////////////////////////
///////////////////////////////////////////

         if (geostate != NULL)
         {
            geostate->setMode( PFSTATE_ANTIALIAS, PFAA_ON );
            geostate->setMode( PFSTATE_ENLIGHTING, PF_ON );
            geostate->setMode( PFSTATE_CULLFACE, PFCF_OFF );
            geostate->setMode( PFSTATE_SHADEMODEL, PFSM_GOURAUD );

            testMat = (pfMaterial*)geostate->getAttr( PFSTATE_FRONTMTL );
            bmaterial = (pfMaterial*)geostate->getAttr( PFSTATE_BACKMTL );
            vprDEBUG(vesDBG,3) << "setting alpha to " << op 
                                   << std::endl << vprDEBUG_FLUSH;
            if ( testMat != NULL )
            {
               if ( twosidedlighting )
               {
                  testMat->setSide( PFMTL_BOTH );
                  geostate->setAttr( PFSTATE_FRONTMTL, testMat );				
                  geostate->setAttr( PFSTATE_BACKMTL, testMat );
                  geostate->setAttr( PFSTATE_LIGHTMODEL, lightModel );
                  vprDEBUG(vesDBG,3) << "Two-Sided Lighting Has Been Turned ON : " <<  
                                       std::endl << vprDEBUG_FLUSH;
               }
               vprDEBUG(vesDBG,3) << "Setting Front Material : " << op 
                                      << std::endl << vprDEBUG_FLUSH;
               vprDEBUG(vesDBG,3) << " Color Flag : " << color
                                      << std::endl << vprDEBUG_FLUSH;

               testMat->setAlpha( op );
               if ( op == 1 ) 
               {
                  //Turn colors on
                  if( color == 1 )
                  {
                     testMat->setColor( PFMTL_DIFFUSE ,
                           stlColor[0], stlColor[1], stlColor[2]);
                     testMat->setColor( PFMTL_AMBIENT ,
                           stlColor[0],	stlColor[1], stlColor[2]);
                     geoset->setDrawBin(PFSORT_OPAQUE_BIN);  // draw last
                     geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_OFF);
                     //geoset->setDrawBin(PFSORT_OPAQUE_BIN);  // draw last
                     geostate->setMode(PFSTATE_CULLFACE, PFCF_OFF);
                     vprDEBUG(vesDBG,3) 
                           << " Front Color : " << stlColor[0]<< " : " 
                           <<  stlColor[1]<< " : " << stlColor[2]
                           << std::endl << vprDEBUG_FLUSH;
                     //this->fmaterial->setAlpha( .2 );
                  }
                  else
                  {
                       testMat->setColorMode( PFMTL_FRONT, PFMTL_CMODE_AMBIENT_AND_DIFFUSE );
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
                     vprDEBUG(vesDBG,3)
                        << "Front Color Transparent : " << stlColor[0] << " : " 
                        <<  stlColor[1] << " : " << stlColor[2]
                        << std::endl << vprDEBUG_FLUSH;
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
                  vprDEBUG(vesDBG, 3) << "Set color Mode "
                                         << std::endl << vprDEBUG_FLUSH;
                  if( color == 1)
                  {
                     bmaterial->setColor( PFMTL_DIFFUSE,
				          stlColor[0], stlColor[1], stlColor[2]);
                     bmaterial->setColor( PFMTL_AMBIENT,
				          stlColor[0], stlColor[1], stlColor[2]);
                     geoset->setDrawBin(PFSORT_OPAQUE_BIN);  // draw last
                     //std::cout << " Alpha value : " << testMat->getAlpha() << std::endl;
                     geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_OFF);
                     vprDEBUG(vesDBG,3) 
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
                  geostate->setMode(PFSTATE_TRANSPARENCY, PFTR_BLEND_ALPHA );//| PFTR_NO_OCCLUDE);
                  if( color == 1 )
                  {
                     bmaterial->setColor( PFMTL_DIFFUSE , 1.0f, 1.0f, 1.0f );
                     bmaterial->setColor( PFMTL_AMBIENT , 1.0f, 1.0f, 1.0f );
                     vprDEBUG(vesDBG,3)
                        << " Back Color : " << stlColor[0]<< " : " 
                        << stlColor[1] << " : " << stlColor[2]
                        << std::endl << vprDEBUG_FLUSH;
                     //this->fmaterial->setAlpha( .2 );
                  }
                  else
                  {
                     //bmaterial->setColorMode( PFMTL_BACK,  PFMTL_CMODE_OFF );
                  }
               }
               geostate->setAttr(PFSTATE_BACKMTL, bmaterial);
            }
            geoset->setGState( geostate );
         }
         else
         {
            vprDEBUG(vesDBG,0) 
               << "ERROR: Tried to set transparency, but this pfGeoSet"
               << " has no pfGeoState." << std::endl << vprDEBUG_FLUSH;
         }
	   	}
	
	   }
    	else if (pfIsOfType(node_1, pfGroup::getClassType()))
	   {
	   	// Run this traverser on each of its children (recursive)
	   	num = ((pfGroup*)node_1)->getNumChildren();

	   	vprDEBUG(vesDBG,2) << num << " GROUP TYPE "
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

////////////////////////////////////////////////////////////////////////////////
void Node::pfTravNodeFog( pfNode* node_1, pfFog* fog )
{
#ifdef _DEBUG
   assert( node_1 != NULL && "bad pointer passed in" );
#endif
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
#ifdef _DEBUG
         assert( geoset != NULL && "geoset is null" );
#endif
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
            vprDEBUG(vesDBG,0) 
               << "ERROR: Tried to set transparency, but this pfGeoSet"
               << " has no pfGeoState." << std::endl << vprDEBUG_FLUSH;
         }
      }	
	}
   else if (pfIsOfType(node_1, pfGroup::getClassType()))
	{
		// Run this traverser on each of its children (recursive)
		num = ((pfGroup*)node_1)->getNumChildren();

		vprDEBUG(vesDBG,1) << num << " GROUP TYPE "
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
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::TravNodeMaterial(osg::Node* node)
{
   if(!node)return;
	int i  = 0;
	int num = 0;
   osg::ref_ptr<osg::Group> tempGroup = new osg::Group;

 	// If the node is a geode...
   if(!strcmp(node->className(),"Geode")){
      osg::ref_ptr<osg::Drawable> geoset = NULL;
      osg::ref_ptr<osg::Geode> geode = dynamic_cast<osg::Geode*>(node);
      osg::ref_ptr<osg::StateSet> geostate = NULL;
      
      osg::ref_ptr<osg::Material> material = NULL;
      osg::ref_ptr<osg::Material> front = NULL;
      osg::ref_ptr<osg::Material> back = NULL;

      
      // Grab each of its geosets
      num = geode->getNumDrawables();

      //std::cout << "HERE IT IS " << num << std::endl;
      for (i=0; i < num; i++){
         geoset = geode->getDrawable(i) ;
#ifdef _DEBUG
         assert( geoset.get() != NULL && "geoset is null" );
#endif
         // Apply the material to the geostate and disable texturing
         geostate = geoset->getOrCreateStateSet();
         material = dynamic_cast<osg::Material*>(geostate->getAttribute(osg::StateAttribute::MATERIAL));
         if(material.valid())
         {
            material->setAlpha(osg::Material::FRONT_AND_BACK,op);
         }

         if ( twosidedlighting )
         {
            geostate->setAttributeAndModes(lightModel.get(), osg::StateAttribute::ON);
            vprDEBUG(vesDBG,3) << "Two-Sided Lighting Has Been Turned ON : " <<  
                                 std::endl << vprDEBUG_FLUSH;
         }
         osg::Vec4Array* curColors = 0;
         
         if(color == 1){
            curColors = new osg::Vec4Array(1);
            geoset->asGeometry()->setColorBinding(osg::Geometry::BIND_OVERALL);
         }else{
            curColors = dynamic_cast<osg::Vec4Array*>(geoset->asGeometry()->getColorArray());
         }
         if(curColors){
            //update the opacity
            unsigned int nColors = curColors->getNumElements();
            for(int i = nColors-1; i ==0; i--){
               //handle stl
               if(color == 1){
                  (*curColors)[i][0] = stlColor[0];
                  (*curColors)[i][1] = stlColor[1];
                  (*curColors)[i][2] = stlColor[2];
               }
               //just update the opacity
               (*curColors)[i][3] = op;
            }
            geoset->asGeometry()->setColorArray(curColors);
         }
         //set up blending for opacity
         osg::ref_ptr<osg::BlendFunc> bf = new osg::BlendFunc;
         osg::ref_ptr<osg::ShadeModel> shade = new osg::ShadeModel;
         shade->setMode(osg::ShadeModel::SMOOTH);
         bf->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA);
		    //osg::ref_ptr<osg::Depth> depth = new osg::Depth;	 
         //put in the appropriate bin
         if ( op == 1 ) {
             //depth->setWriteMask(true);
             geostate->setRenderingHint(osg::StateSet::OPAQUE_BIN);
             geostate->setMode(GL_BLEND,osg::StateAttribute::ON);
         }else{
             //depth->setWriteMask(false);
             geostate->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
             geostate->setRenderBinDetails(99,std::string("DepthSortedBin"));
             geostate->setMode(GL_BLEND,osg::StateAttribute::ON);
         }

         geostate->setAttributeAndModes(bf.get(),osg::StateAttribute::ON);
         geostate->setAttributeAndModes(shade.get(),osg::StateAttribute::ON);
         //geostate->setAttributeAndModes(depth.get(),osg::StateAttribute::ON);
         geostate->setMode(GL_CULL_FACE,osg::StateAttribute::OFF);
         osg::Vec4Array* curNormals = 0;
         curNormals  = dynamic_cast<osg::Vec4Array*>(geoset->asGeometry()->getNormalArray());
         if(curNormals){
            unsigned int nNormals = curNormals->getNumElements();
         }else{
            geostate->setMode(GL_NORMALIZE, osg::StateAttribute::ON);
         }
         //reset the state
         geoset->setStateSet(geostate.get());
      }

   }else  if(node->isSameKindAs(tempGroup.get())){
      num = ((osg::Group*)node)->getNumChildren();
      vprDEBUG(vesDBG,1) << num << " GROUP TYPE "
                                << std::endl << vprDEBUG_FLUSH;
      for (i = 0; i < num; i++){
         this->TravNodeMaterial(((osg::Group*)node)->getChild(i)) ;
           
      }
   }else if(!strcmp(node->className(),"LOD")){
      num = ((osg::LOD*)node)->getNumChildren();
      vprDEBUG(vesDBG,1) << num << " GROUP TYPE "
                                << std::endl << vprDEBUG_FLUSH;
      for (i = 0; i < num; i++){
         this->TravNodeMaterial(((osg::LOD*)node)->getChild(i)) ;
           
      }
   }else if(!strcmp(node->className(),"PositionAttitudeTransform")){
       num = ((osg::MatrixTransform*)node)->getNumChildren();
      vprDEBUG(vesDBG,1) << num << " GROUP TYPE "
                                << std::endl << vprDEBUG_FLUSH;
      for (i = 0; i < num; i++){
         this->TravNodeMaterial(((osg::MatrixTransform*)node)->getChild(i)) ;
           
      }
   }
 }
 ////////////////////////////////////////////////////////////////////////////////
 void CADEntityHelper::TravNodeFog(osg::Node* node_1, osg::Fog* fog)
 {
   if(!node_1)return;
	int i  = 0;
	int num = 0;
   osg::ref_ptr<osg::Group> tempGroup = new osg::Group;

 	// If the node_1 is a geode...
   if(!strcmp(node_1->className(),"Geode")){
      osg::ref_ptr<osg::Drawable> geoset = NULL;
      osg::ref_ptr<osg::Geode> geode = dynamic_cast<osg::Geode*>(node_1);
      osg::ref_ptr<osg::StateSet> geostate = NULL;

      // Grab each of its geosets
      num = geode->getNumDrawables();

      //std::cout << "HERE IT IS " << num << std::endl;
      for (i=0; i < num; i++){
         geoset = geode->getDrawable(i) ;
#ifdef _DEBUG
         assert( geoset.get() != NULL && "geoset is null" );
#endif
         // Apply the material to the geostate and disable texturing
         geostate = geoset->getOrCreateStateSet();

         geostate->setAttribute( fog, osg::StateAttribute::ON );
         geostate->setMode( GL_FOG, osg::StateAttribute::ON );

         geoset->setStateSet(geostate.get());
      }
   }else  if(node_1->isSameKindAs(tempGroup.get())){
      num = ((osg::Group*)node_1)->getNumChildren();
      vprDEBUG(vesDBG,1) << num << " GROUP TYPE "
                                << std::endl << vprDEBUG_FLUSH;
      for (i = 0; i < num; i++){
         this->TravNodeFog(((osg::Group*)node_1)->getChild(i), fog) ;     
      }
   }else if(!strcmp(node_1->className(),"LOD")){
      num = ((osg::LOD*)node_1)->getNumChildren();
      vprDEBUG(vesDBG,1) << num << " GROUP TYPE "
                                << std::endl << vprDEBUG_FLUSH;
      for (i = 0; i < num; i++){
         this->TravNodeFog(((osg::LOD*)node_1)->getChild(i), fog) ;
           
      }
   }else if(!strcmp(node_1->className(),"MatrixTransform")){
       num = ((osg::MatrixTransform*)node_1)->getNumChildren();
      vprDEBUG(vesDBG,1) << num << " GROUP TYPE "
                                << std::endl << vprDEBUG_FLUSH;
      for (i = 0; i < num; i++){
         this->TravNodeFog(((osg::MatrixTransform*)node_1)->getChild(i), fog) ;
           
      }
   }
 }
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
