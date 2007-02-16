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
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/Model/Point.h"
#include "VE_Open/XML/Model/Network.h"
#include "VE_Open/XML/Model/Link.h"
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
#include "VE_Open/XML/Model/Model.h"
#include "VE_Xplorer/SceneGraph/TextTexture.h"

using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////////
NetworkSystemView::NetworkSystemView()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
NetworkSystemView::NetworkSystemView(std::string network)
{
	this->network = network;
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
/*osg::ref_ptr< osg::Group > NetworkSystemView::DrawNetwork( void )
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
		//osg::ref_ptr<osg::Geode> textGeode = new osg::Geode();
		//osg::ref_ptr<osgText::Text> text = new  osgText::Text;
		//textGeode.get()->addDrawable(text.get());
		//text.get()->setColor(osg::Vec4(1.0, 1.0, 0.0, 1.0));
		//text.get()->setCharacterSize(6.0);
		//text.get()->setAlignment(osgText::Text::CENTER_CENTER);
		//text.get()->setText(bkpp.getBlockID(count));
		//textGeode.get()->setName(bkpp.getBlockID(count));

		osg::ref_ptr<osg::AutoTransform> at = new osg::AutoTransform;
		at.get()->setPosition(osg::Vec3(bkpp.getXCoord(count)*30, bkpp.getYCoord(count)*30, 15.0f));
		at.get()->setAutoRotateMode(osg::AutoTransform::ROTATE_TO_SCREEN);
		//at.get()->addChild(textGeode.get());
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
		
		//Scale up 3D comps
		osg::ref_ptr<osg::AutoTransform> scale = new osg::AutoTransform;
		scale.get()->addChild(loadedModel.get());
		scale.get()->setScale(5.0f);

		//move the block to its correct location
		osg::ref_ptr<osg::MatrixTransform> mModelTrans = new osg::MatrixTransform();
		//mModelTrans.get()->addChild(loadedModel.get());
		mModelTrans.get()->addChild(scale.get());
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
		//osg::Geode* textGeode = new osg::Geode();
		//osg::ref_ptr<osgText::Text> text = new  osgText::Text;
		//textGeode->addDrawable(text.get());
		//text.get()->setColor(osg::Vec4(1.0, 1.0, 0.0, 1.0));
		//text.get()->setCharacterSize(6.0);
		//text.get()->setAlignment(osgText::Text::CENTER_CENTER);
		//text.get()->setText(bkpp.getStreamId(streamCount));
		//textGeode->setName(bkpp.getStreamId(streamCount));

		osg::AutoTransform* at = new osg::AutoTransform;
		at->setPosition(osg::Vec3(bkpp.getStreamXCoord(streamCount, 0)*30.0, bkpp.getStreamYCoord(streamCount, 0)*30.0, 3.0f));
		at->setAutoRotateMode(osg::AutoTransform::ROTATE_TO_SCREEN);
		//at->addChild(textGeode);
		loadedModels.get()->addChild(at);

		streamCount++;
		count = 0;
	}
	loadedModels.get()->addChild(geode);
	return loadedModels.get();
}*/

////////////////////////////////////////////////////////
osg::ref_ptr< osg::Group > NetworkSystemView::DrawNetwork( void )
{
	osg::ref_ptr<osg::Group> loadedModels = new osg::Group(); 
	osg::ref_ptr<osg::Vec4Array> colorBlack = new osg::Vec4Array;
	colorBlack->push_back(osg::Vec4(0.0f,0.0f,0.0f,1.0f));
	osg::ref_ptr<osg::Vec3Array> shared_normals = new osg::Vec3Array;
	shared_normals->push_back(osg::Vec3(0.0f,-1.0f,0.0f));
	VE_XML::XMLReaderWriter networkWriter;
	networkWriter.UseStandaloneDOMDocumentManager();

	std::vector< VE_XML::XMLObject* > objectVector;

	// do this for models
	networkWriter.ReadXMLData( network, "Model", "veModel" );
	objectVector = networkWriter.GetLoadedXMLObjects();

	// now lets create a list of them
	for ( size_t i = 0; i < objectVector.size(); ++i )
	{
		//_fileProgress->Update( 75 + (i*timeCalc), _("Loading data") );
		VE_XML::VE_Model::Model* model = dynamic_cast< VE_XML::VE_Model::Model* >( objectVector.at( i ) );
		std::cout<<"FILENAME: "<< model->GetIconFilename() <<std::endl;
		VE_XML::VE_Model::Point * iconLocation = model->GetIconLocation();
		std::pair<unsigned int, unsigned int> xyPair = iconLocation->GetPoint();
		std::cout<<"X: "<< xyPair.first <<" Y: " << xyPair.second <<std::endl;

		//add 3d blocks
		osg::ref_ptr<osg::Node> loadedModel = osgDB::readNodeFile("3DIcons/"+model->GetIconFilename()+".obj");
		osg::ref_ptr<VE_SceneGraph::TextTexture> text = new VE_SceneGraph::TextTexture();
		text->UpdateText(model->GetModelName());

		//add red block id if block .ive file is not found
		if(loadedModel.get() == NULL)
			loadedModel = osgDB::readNodeFile("3DIcons/UnsupportedComponent.ive");

		//set the blocks name
		loadedModel.get()->setName(model->GetModelName());

		//Rotate the 3d comps 90 degrees around X axis
		osg::ref_ptr<osg::AutoTransform> rotatedComp = new osg::AutoTransform();
		rotatedComp.get()->addChild(loadedModel.get());
		rotatedComp.get()->setRotation(osg::Quat(osg::DegreesToRadians(90.0), osg::Vec3d(1.0, 0.0, 0.0)));

		//move the text to the -y
		//osg::ref_ptr<osg::MatrixTransform> textTrans = new osg::MatrixTransform();
		//textTrans.get()->addChild(text.get());
		//textTrans->preMult(osg::Matrix::translate(0.0, -loadedModel.get()->getBound().radius(), 0.0));

		//move the text to the -y
		osg::ref_ptr<osg::AutoTransform> textTrans = new osg::AutoTransform();
		textTrans.get()->addChild(text.get());
		textTrans.get()->setPosition(osg::Vec3d(0.0, -loadedModel.get()->getBound().radius(), 0.0));

		//Scale up 3D comps & text
		osg::ref_ptr<osg::AutoTransform> scale = new osg::AutoTransform;
		scale.get()->addChild(rotatedComp.get());
		scale.get()->addChild(textTrans.get());
		scale.get()->setScale(5.0f);

		//translate to comp with name to correct location
		//osg::ref_ptr<osg::MatrixTransform> mModelTrans = new osg::MatrixTransform();
		//mModelTrans.get()->addChild(scale.get());
		////mModelTrans.get()->addChild(loadedModel.get());
		//mModelTrans.get()->preMult(osg::Matrix::translate(xyPair.first*1.02, xyPair.second*1.05, 0.0));
		//mModelTrans.get()->setName(model->GetModelName());
		//loadedModels.get()->addChild(mModelTrans.get());
		
		//translate to comp with name to correct location
		osg::ref_ptr<osg::AutoTransform> mModelTrans = new osg::AutoTransform();
		mModelTrans.get()->addChild(scale.get());
		mModelTrans.get()->setPosition(osg::Vec3d(xyPair.first*1.02, xyPair.second*1.05, 0.0));
		mModelTrans.get()->setName(model->GetModelName());
		loadedModels.get()->addChild(mModelTrans.get());
	}	
	
	//Streams	
	networkWriter.ReadXMLData( network, "Model", "veNetwork" );
	objectVector = networkWriter.GetLoadedXMLObjects();
	VE_XML::VE_Model::Network* veNetwork = dynamic_cast< VE_XML::VE_Model::Network* >( objectVector.at( 0 ) );
	std::cout << veNetwork->GetNumberOfLinks() << std::endl;
	osg::Geode* geode = new osg::Geode();
	for ( size_t i = 0; i < veNetwork->GetNumberOfLinks(); ++i )
	{
		size_t numberOfPoints = veNetwork->GetLink( i )->GetNumberOfLinkPoints();
		osg::Vec3Array* vertices = new osg::Vec3Array(numberOfPoints);
		osg::Geometry* linesGeom = new osg::Geometry();
		std::cout<<"NP: "<< numberOfPoints<<std::endl;
		for ( size_t j = 0; j < numberOfPoints; j++ )
		{
			std::pair< unsigned int, unsigned int > rawPoint = veNetwork->GetLink( i )->GetLinkPoint( j )->GetPoint();
			std::cout << "X: " << rawPoint.first << " Y: " << rawPoint.second << std::endl;
			(*vertices)[j].set(rawPoint.first, rawPoint.second, 0.0);
		}
		linesGeom->setVertexArray(vertices);
		linesGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_STRIP, 0, numberOfPoints));		linesGeom->setColorBinding(osg::Geometry::BIND_OVERALL);
		linesGeom->setNormalArray(shared_normals.get());
		linesGeom->setNormalBinding(osg::Geometry::BIND_OVERALL);
		linesGeom->setColorArray(colorBlack.get());
		geode->addDrawable(linesGeom);
	}
	loadedModels->addChild(geode);
	return loadedModels;
}