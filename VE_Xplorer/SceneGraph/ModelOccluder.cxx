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
 * Date modified: $Date: 2006-11-28 14:11:49 -0600 (Tue, 28 Nov 2006) $
 * Version:       $Rev: 6082 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/SceneGraph/ModelOccluder.h"
#include "VE_Xplorer/SceneGraph/cfdGroup.h"
#include "VE_Xplorer/SceneGraph/cfdSwitch.h"
#include "VE_Xplorer/SceneGraph/cfdGeode.h"
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
#include "VE_Xplorer/SceneGraph/cfdSequence.h"

namespace VE_SceneGraph{

//////////////////
ModelOccluder::ModelOccluder()
:cfdSceneNode(CFD_NODE)
{
   //biv--do we need to set type for scene node in here?
   //this->_group = new pfNode();
   op = 1.0f;
   stlColor[ 1 ] = stlColor[ 1 ] = stlColor[ 0 ] = -1;
   color = 0;
   twosidedlighting = false;

#ifdef _PERFORMER
   this->_node = 0;
   this->lightModel = 0;
#elif _OSG
   //_node = 0;//new osg::Node();
#elif _OPENSG
#endif
}
/////////////////////////////////////////
ModelOccluder::ModelOccluder( const ModelOccluder& input )
:cfdSceneNode(input)
{
#ifdef _PERFORMER
   this->_node = input._node;
#elif _OSG
   if ( _node.valid() )
   {
      _node = input._node;
   }
#elif _OPENSG
#endif
}

////////////////////////////////////////////////////
ModelOccluder& ModelOccluder::operator=( const ModelOccluder& input )
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

////////////////////////////////////////////////////
// Code that can be used at a later date
// there are issues to be reolved on wether == should be defined
// as below 
/*bool ModelOccluder::operator== ( const ModelOccluder& node1 ) const
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
ModelOccluder::~ModelOccluder( void )
{
   // If neccesary
#ifdef _PERFORMER
   if ( this->_node != NULL )
   {
      vprDEBUG(vesDBG,3) << "destructor for ModelOccluder " 
                              << std::endl << vprDEBUG_FLUSH;
      pfDelete( this->_node );
   }
#elif _OSG
   //_node->unref();
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
#ifdef _PERFORMER
#elif _OSG
////////////////////////////////////////////////////////////////////////////////
void ModelOccluder::TravNodeMaterial(osg::Node* node)
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
   }else if(!strcmp(node->className(),"MatrixTransform")){
       num = ((osg::MatrixTransform*)node)->getNumChildren();
      vprDEBUG(vesDBG,1) << num << " GROUP TYPE "
                                << std::endl << vprDEBUG_FLUSH;
      for (i = 0; i < num; i++){
         this->TravNodeMaterial(((osg::MatrixTransform*)node)->getChild(i)) ;
           
      }
   }
 }
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* createOccluder(const osg::Vec3& v1,const osg::Vec3& v2,const osg::Vec3& v3,const osg::Vec3& v4,float holeRatio=-1.0f)
{
   // create and occluder which will site along side the loadmodel model.
   osg::OccluderNode* occluderNode = new osg::OccluderNode;
   
   // create the convex planer occluder 
   osg::ConvexPlanarOccluder* cpo = new osg::ConvexPlanarOccluder;
   
   // attach it to the occluder node.
   occluderNode->setOccluder(cpo);
   occluderNode->setName("occluder");
   
   // set the occluder up for the front face of the bounding box.
   osg::ConvexPlanarPolygon& occluder = cpo->getOccluder();
   occluder.add(v1);
   occluder.add(v2);
   occluder.add(v3);
   occluder.add(v4);
   
   // create a whole at the center of the occluder if needed.
   if (holeRatio>0.0f)
   {
      // create hole.
      float ratio = holeRatio;
      float one_minus_ratio = 1-ratio;
      osg::Vec3 center = (v1+v2+v3+v4)*0.25f;
      osg::Vec3 v1dash = v1*ratio + center*one_minus_ratio;
      osg::Vec3 v2dash = v2*ratio + center*one_minus_ratio;
      osg::Vec3 v3dash = v3*ratio + center*one_minus_ratio;
      osg::Vec3 v4dash = v4*ratio + center*one_minus_ratio;
      
      osg::ConvexPlanarPolygon hole;
      hole.add(v1dash);
      hole.add(v2dash);
      hole.add(v3dash);
      hole.add(v4dash);
      
      cpo->addHole(hole);
   }    
   
   
   // create a drawable for occluder.
   osg::Geometry* geom = new osg::Geometry;
   
   osg::Vec3Array* coords = new osg::Vec3Array(occluder.getVertexList().begin(),occluder.getVertexList().end());
   geom->setVertexArray(coords);
   
   osg::Vec4Array* colors = new osg::Vec4Array(1);
   (*colors)[0].set(1.0f,1.0f,1.0f,0.5f);
   geom->setColorArray(colors);
   geom->setColorBinding(osg::Geometry::BIND_OVERALL);
   
   geom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS,0,4));
   
   osg::Geode* geode = new osg::Geode;
   geode->addDrawable(geom);
   
   osg::StateSet* stateset = new osg::StateSet;
   stateset->setMode(GL_LIGHTING,osg::StateAttribute::OFF);
   stateset->setMode(GL_BLEND,osg::StateAttribute::ON);
   stateset->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
   
   geom->setStateSet(stateset);
   
   // add the occluder geode as a child of the occluder,
   // as the occluder can't self occlude its subgraph the
   // geode will never be occluder by this occluder.
   occluderNode->addChild(geode);    
   
   return occluderNode;
   
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* createOccludersAroundModel(osg::Node* model)
{
   osg::Group* scene = new osg::Group;
   scene->setName("rootgroup");
   
   
   // add the loaded model into a the scene group.
   scene->addChild(model);
   model->setName("model");
   
   // get the bounding volume of the model.
   const osg::BoundingSphere bs = model->getBound();
   
   // create a bounding box around the sphere.
   osg::BoundingBox bb;
   bb.expandBy(bs);
   
   // front
   scene->addChild(createOccluder(bb.corner(0),
                                  bb.corner(1),
                                  bb.corner(5),
                                  bb.corner(4)));
   
   // right side
   scene->addChild(createOccluder(bb.corner(1),
                                  bb.corner(3),
                                  bb.corner(7),
                                  bb.corner(5)));
   
   // left side
   scene->addChild(createOccluder(bb.corner(2),
                                  bb.corner(0),
                                  bb.corner(4),
                                  bb.corner(6)));
   
   // back side
   scene->addChild(createOccluder(bb.corner(3),
                                  bb.corner(2),
                                  bb.corner(6),
                                  bb.corner(7),
                                  0.5f)); // create a hole half the size of the occluder.
   
   return scene;
} 

