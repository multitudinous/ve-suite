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
* Date modified: $Date: 2006-12-19 22:02:50 -0600 (Tue, 19 Dec 2006) $
* Version:       $Rev: 6352 $
* Author:        $Author: mccdo $
* Id:            $Id: cfdFILE.cxx 6352 2006-12-20 04:02:50Z mccdo $
* -----------------------------------------------------------------
*
*************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerNetwork/NetworkSystemView.h"
#include "temp/bkpparser.h"
#include <osgDB/ReadFile>
#include <osgUtil/Optimizer>
#include <osgProducer/Viewer>
#include <osg/CoordinateSystemNode>
#include <osg/MatrixTransform>
#include <osgText/Text>
#include <osg/Vec3>
#include <osg/Vec4>
#include <osg/AutoTransform>
#include <osg/Group>

using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////////
NetworkSystemView::NetworkSystemView()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
NetworkSystemView::NetworkSystemView( const NetworkSystemView& input )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
NetworkSystemView::~NetworkSystemView( void )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
NetworkSystemView& NetworkSystemView::operator=( const NetworkSystemView& input )
{
   if ( this != &input )
   {
      ;
   }
   return *this;
}
////////////////////////////////////////////////////////////////////////////////
void NetworkSystemView::SetNetwork( std::string network )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr< osg::Group > NetworkSystemView::DrawNetwork( void )
{
	osg::ref_ptr<osg::Group> loadedModels = new osg::Group();
	
	//instatiate the parser
	BKPParser bkpp;
//	bkpp.openFile(argv[1]);
	bkpp.openFile("Hyper.bkp");
	//bkpp.openFile("igcc.bkp");
	// read the scene from the list of file specified commandline args.
	//read blocks
	int count = 0;

	//if(bkpp.getNumComponents()<1)
	//	std::cout<<"No Blocks available."<<std::endl;
	
	//add blocks and id to node
	while(count<bkpp.getNumComponents())
	{
		//Add id text
		osg::ref_ptr<osg::Geode> textGeode = new osg::Geode();
		osg::ref_ptr<osgText::Text> text = new  osgText::Text;
		textGeode.get()->addDrawable(text.get());
		text.get()->setColor(osg::Vec4(1.0, 1.0, 0.0, 1.0));
		text.get()->setCharacterSize(6.0);
		text.get()->setAlignment(osgText::Text::CENTER_CENTER);
		text.get()->setText(bkpp.getBlockID(count));
		textGeode.get()->setName(bkpp.getBlockID(count));

		osg::ref_ptr<osg::AutoTransform> at = new osg::AutoTransform;
		at.get()->setPosition(osg::Vec3(bkpp.getXCoord(count)*30, bkpp.getYCoord(count)*30, 15.0f));
		at.get()->setAutoRotateMode(osg::AutoTransform::ROTATE_TO_SCREEN);
		at.get()->addChild(textGeode.get());
		loadedModels.get()->addChild(at.get());

		//add 3d blocks
		//osg::ref_ptr<osg::Node> loadedModel = osgDB::readNodeFile("osg/"+bkpp.getBlockType(count)+".ive");
		osg::ref_ptr<osg::Node> loadedModel = osgDB::readNodeFile("3DIcons/"+bkpp.getBlockType(count)+"/"+bkpp.getBlockType(count)+"."+bkpp.getBlockIcon(count)+".obj");
		
		//add red block id if block .ive file is not found
		if(loadedModel.get() == NULL)
		{
			//std::cout<<"Unsupported Component: "<<count<<" : "<<bkpp.getBlockType(count)<<std::endl;
			loadedModel = osgDB::readNodeFile("3DIcons/UnsupportedComponent.ive");
			//std::cout<<"Rendering Aborted!"<<std::endl;
			//return 1;
		}
		
		//set the blocks name
		loadedModel.get()->setName(bkpp.getBlockID(count));
		
		//move the block to its correct location
		osg::ref_ptr<osg::MatrixTransform> mModelTrans = new osg::MatrixTransform();
		mModelTrans.get()->addChild(loadedModel.get());
		mModelTrans.get()->preMult(osg::Matrix::translate(bkpp.getXCoord(count)*30, bkpp.getYCoord(count)*30, 0.0));
		mModelTrans.get()->setName(bkpp.getBlockID(count));
		loadedModels.get()->addChild(mModelTrans.get());
		
		count++;
	}

	//add streams to node
	int streamCount=0;
	count=0;
	//create colors for each type of stream
	osg::ref_ptr<osg::Vec4Array> colorBlack = new osg::Vec4Array;
	colorBlack->push_back(osg::Vec4(0.0f,0.0f,0.0f,1.0f));
	osg::ref_ptr<osg::Vec4Array> colorRed = new osg::Vec4Array;
	colorRed->push_back(osg::Vec4(1.0f,0.0f,0.0f,1.0f));
	osg::ref_ptr<osg::Vec4Array> colorGreen = new osg::Vec4Array;
	colorGreen->push_back(osg::Vec4(0.0f,1.0f,0.0f,1.0f));

	// same trick for shared normal.
	osg::ref_ptr<osg::Vec3Array> shared_normals = new osg::Vec3Array;
	shared_normals->push_back(osg::Vec3(0.0f,-1.0f,0.0f));
	
	osg::Geode* geode = new osg::Geode();

	//if there are no streams
	//if(bkpp.getNumStream()<1)
	//	std::cout<<"No Streams available."<<std::endl;
	
	
	while(streamCount < bkpp.getNumStream())
	{
		osg::Vec3Array* vertices = new osg::Vec3Array(bkpp.getStreamSize(streamCount));
        osg::Geometry* linesGeom = new osg::Geometry();
		while(count < bkpp.getStreamSize(streamCount))
		{	
        	(*vertices)[count].set(bkpp.getStreamXCoord(streamCount, count)*30.0, bkpp.getStreamYCoord(streamCount, count)*30.0, 0.0);
			count++;
		}
		linesGeom->setVertexArray(vertices);
		
		// apply correct color to line
		// Material - Type 0 - black
		// Heat - Type 1 - Red
		// Work - Type 2 - Green
		if(bkpp.getStreamType(streamCount)==0)
			linesGeom->setColorArray(colorBlack.get());
		else if(bkpp.getStreamType(streamCount)==1)
			linesGeom->setColorArray(colorRed.get());
		else if(bkpp.getStreamType(streamCount)==2)
			linesGeom->setColorArray(colorGreen.get());
		//else
		//	std::cout<<"Stream Type Error: Stream Type - "<<bkpp.getStreamType(streamCount)<<std::endl;
		
		linesGeom->setColorBinding(osg::Geometry::BIND_OVERALL);
		// use the shared normal array.
		linesGeom->setNormalArray(shared_normals.get());
		linesGeom->setNormalBinding(osg::Geometry::BIND_OVERALL);

	        linesGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_STRIP,0,bkpp.getStreamSize(streamCount)));
		
		geode->addDrawable(linesGeom);

		//Add id text
		osg::Geode* textGeode = new osg::Geode();
		osg::ref_ptr<osgText::Text> text = new  osgText::Text;
		textGeode->addDrawable(text.get());
		text.get()->setColor(osg::Vec4(1.0, 1.0, 0.0, 1.0));
		text.get()->setCharacterSize(6.0);
		text.get()->setAlignment(osgText::Text::CENTER_CENTER);
		text.get()->setText(bkpp.getStreamId(streamCount));
		textGeode->setName(bkpp.getStreamId(streamCount));

		osg::AutoTransform* at = new osg::AutoTransform;
		at->setPosition(osg::Vec3(bkpp.getStreamXCoord(streamCount, 0)*30.0, bkpp.getStreamYCoord(streamCount, 0)*30.0, 3.0f));
		at->setAutoRotateMode(osg::AutoTransform::ROTATE_TO_SCREEN);
		at->addChild(textGeode);
		loadedModels.get()->addChild(at);

		streamCount++;
		count = 0;
	}
	loadedModels.get()->addChild(geode);
	return loadedModels.get();
}
