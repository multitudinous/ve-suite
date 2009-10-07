/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- My Includes --- //
#include "VESensorDisplayGraphicalPlugin.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/shader/Shader.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/device/KeyboardMouse.h>

#include <gadget/Type/KeyboardMouse/KeyEvent.h>
#include <gadget/Type/KeyboardMouse/MouseEvent.h>
#include <gadget/Type/KeyboardMouseInterface.h>

// --- OSG Includes --- //
#include <osg/MatrixTransform>
#include <osg/AnimationPath>
#include <osg/ShapeDrawable>
#include <osg/Sequence>

#include <osgText/Text>

#include <osgDB/ReadFile>

#include <osgSim/ColorRange>
#include <osg/Vec3d>

#include <osg/Billboard>

#include <osg/BlendFunc>

// --- C/C++ Libraries --- //


////////////////////////////////////////////////////////////////////////////////
VESensorDisplayGraphicalPlugin::VESensorDisplayGraphicalPlugin()
    :
    PluginBase(),
    m_keyboard( 0 )
{
   // mObjectName = "sensor_overlayUI";
	mObjectName="DefaultPlugin";
	std::cout<<"sensor display graphical plugin was called"<<std::endl;
}
////////////////////////////////////////////////////////////////////////////////
VESensorDisplayGraphicalPlugin::~VESensorDisplayGraphicalPlugin()
{

}
////////////////////////////////////////////////////////////////////////////////
void VESensorDisplayGraphicalPlugin::InitializeNode( osg::Group* veworldDCS )//gets called on submit.  Only do once for creation.  Don't put anything in constructor, rather put here.  call function to create quads here.
{
    PluginBase::InitializeNode( veworldDCS );
	
	std::cout<<"initialize node was just called"<<std::endl;
	
	osg::Group* root = ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode();

	osg::Vec3* quadTrans = new osg::Vec3;
	osg::Vec3* quadTrans1=new osg::Vec3;
	osg::Vec3* quadTrans2=new osg::Vec3;
	osg::Vec3* quadTrans3=new osg::Vec3;
	
	quadTrans->set(87.5,112,-645.5);  //above my desk
	quadTrans1->set(  191.5, 32, -700 ); // Chad's desk
	quadTrans2->set(400, 69, -1173); //Angie's desk
	quadTrans3->set(620,18,-863); //wall
	
	//billboarding
	osg::Billboard* quadBillBoard = new osg::Billboard();
	root->addChild(quadBillBoard);
	
	quadBillBoard->setMode(osg::Billboard::POINT_ROT_EYE);
	quadBillBoard->setNormal(osg::Vec3(0.0f,-1.0f,0.0f));
	
	//set texture
	texture=new osg::Texture2D;
	texture1=new osg::Texture2D;
	texture2=new osg::Texture2D;
	texture3=new osg::Texture2D;
	
	texture->setDataVariance(osg::Object::DYNAMIC); // protect from being optimized away as static state.
	texture1->setDataVariance(osg::Object::DYNAMIC); // protect from being optimized away as static state.
	texture2->setDataVariance(osg::Object::DYNAMIC); // protect from being optimized away as static state.
	texture3->setDataVariance(osg::Object::DYNAMIC); // protect from being optimized away as static state.
	

	setTexture(); //set the initial texture images to the images in the dir
	
	billBoardStateSet = new osg::StateSet;
	billBoardStateSet->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    billBoardStateSet->setTextureAttributeAndModes(0,texture,osg::StateAttribute::ON);
	billBoardStateSet->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    billBoardStateSet->setAttributeAndModes
	(new osg::BlendFunc, osg::StateAttribute::ON );
	
	billBoardStateSet1 = new osg::StateSet;
	billBoardStateSet1->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    billBoardStateSet1->setTextureAttributeAndModes(0,texture1,osg::StateAttribute::ON);
	billBoardStateSet1->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    billBoardStateSet1->setAttributeAndModes
	(new osg::BlendFunc, osg::StateAttribute::ON );
	
	billBoardStateSet2 = new osg::StateSet;
	billBoardStateSet2->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    billBoardStateSet2->setTextureAttributeAndModes(0,texture2,osg::StateAttribute::ON);
	billBoardStateSet2->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    billBoardStateSet2->setAttributeAndModes
	(new osg::BlendFunc, osg::StateAttribute::ON );
	
	billBoardStateSet3 = new osg::StateSet;
	billBoardStateSet3->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    billBoardStateSet3->setTextureAttributeAndModes(0,texture3,osg::StateAttribute::ON);
	billBoardStateSet3->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    billBoardStateSet3->setAttributeAndModes
	(new osg::BlendFunc, osg::StateAttribute::ON );
	

	std::cout<<"just before create quad in init"<<std::endl;
	
	quadDrawable = createQuad(  billBoardStateSet);
    quad1Drawable = createQuad(  billBoardStateSet1);
	quad2Drawable = createQuad( billBoardStateSet2);
    quad3Drawable = createQuad(  billBoardStateSet3);
	
    // Add these drawables to our billboard at various positions 
    quadBillBoard->addDrawable( quadDrawable, *quadTrans  );
    quadBillBoard->addDrawable( quad1Drawable, *quadTrans1 );
	quadBillBoard->addDrawable( quad2Drawable,*quadTrans2 );
    quadBillBoard->addDrawable( quad3Drawable, *quadTrans3 );
	

}
////////////////////////////////////////////////////////////////////////////////
void VESensorDisplayGraphicalPlugin::PreFrameUpdate()//  Called every frame.  Render plot to quad here.  Think there will be another function to get the results from CE (Unit plugin).
{
	if (iterator<1600)
	{
		iterator++;
		return;
	}
	else
	{
	setTexture();
	}
	
}
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
osg::Drawable* VESensorDisplayGraphicalPlugin::createQuad(osg::StateSet* bbState)  //make it a node without bb.
{
	//osg::Group* group=new osg::Group;
	//osg::Geode* quadGeode = new osg::Geode();
	osg::Geometry* quadGeometry = new osg::Geometry();

	
	osg::Vec3Array* quadVertices = new osg::Vec3Array;
	quadVertices->push_back( osg::Vec3( 0, 0, 0) ); // front left
	quadVertices->push_back( osg::Vec3(20, 0, 0) ); // front right
	quadVertices->push_back( osg::Vec3(20,0, 20) ); // back right 
	quadVertices->push_back( osg::Vec3( 0,0, 20) ); // back left 
	    
	quadGeometry->setVertexArray( quadVertices );
	
	
	osg::Vec2Array* texcoords = new osg::Vec2Array(4);
	(*texcoords)[0].set(0.00f,0.0f); // tex coord for vertex 0 
	(*texcoords)[1].set(1.0f,0.0f); // tex coord for vertex 1 
	(*texcoords)[2].set(1.0f,1.0f); //  ""
	(*texcoords)[3].set(0.0f,1.0f); //  "" 
	quadGeometry->setTexCoordArray(0,texcoords);
	
	quadGeometry->addPrimitiveSet(new osg::DrawArrays (osg::PrimitiveSet::QUADS,0,4));
	
	osg::Vec4Array* colors = new osg::Vec4Array(1);
    (*colors)[0].set(1.0f,1.0f,1.0f,1.0f);
    quadGeometry->setColorArray(colors);
    quadGeometry->setColorBinding(osg::Geometry::BIND_OVERALL);

	

	
	//billboard
	quadGeometry->setStateSet(bbState);
	
	std::cout<<"just ended create quad"<<std::cout;
	
	
	//return group; //use without billboarding
	return quadGeometry;  //use with billboarding
	
	
}
////////////////////////////////////////////////////////////////////////////////

void VESensorDisplayGraphicalPlugin::setTexture()
{
	
	
	
	std::cout<<"trying to load snet39 texture"<<std::endl;
	texture->setImage(osgDB::readImageFile("snet39.png"));  //above Adam's desk
	std::cout<<"trying to load snet35 texture"<<std::endl;
	texture1->setImage(osgDB::readImageFile("snet35.png"));  //Chad'd desk
	std::cout<<"trying to load snet37 texture"<<std::endl;
	texture2->setImage(osgDB::readImageFile("snet37.png"));  //Angie's Desk
	std::cout<<"trying to load snet38 texture"<<std::endl;
	texture3->setImage(osgDB::readImageFile("snet38.png"));  //wall
	
	std::cout<<"new texture loaded"<<std::endl;
	
	iterator=0;

}

/*
void VESensorDisplayGraphicalPlugin::FileExists(std::string strFilename)
{
	struct stat stFileInfo;
	int intStat
	
	//try to get the file attributes
	intStat=stat(strFilename.c_str(), &stFileInfo);
	if (intStat==0)
	{
		setTexture(strFilename);
	}
	else
	{
		sleep(1);
		setTexture(strFilename);
	}
	
}
 */
