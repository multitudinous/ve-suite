/*************** <auto-copyright.pl BEGIN do not edit this line> **************
*
* VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#if defined(WIN32)
    #define WIN32_LEAN_AND_MEAN
#endif
#include <ves/xplorer/network/NetworkSystemView.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/Link.h>

#include <osgDB/ReadFile>
#include <osgUtil/Optimizer>
#include <osg/CoordinateSystemNode>
#include <osg/MatrixTransform>
#include <osgText/Text>
#include <osg/Vec3>
#include <osg/Vec4>
#include <osg/AutoTransform>
#include <osg/Group>
#include <osg/Image>
#include <osg/TextureRectangle>
#include <osg/TexMat>
#include <osg/StateSet>

#include <ves/open/xml/model/Model.h>
#include <ves/xplorer/scenegraph/TextTexture.h>
#include <ves/xplorer/network/UnsupportedComponent.h>

#include <ves/xplorer/scenegraph/util/PhongLoader.h>
#include <ves/xplorer/scenegraph/util/ComputeBoundsVisitor.h>
#include <iostream>
#include <fstream>

using namespace VE_Xplorer;
using namespace ves::open::xml;
using namespace ves::open::xml::model;
using namespace ves::xplorer::network;

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

////////////////////////////////////////////////////////
osg::ref_ptr< osg::Group > NetworkSystemView::DrawNetwork( void )
{
	XMLReaderWriter networkWriter;
	networkWriter.UseStandaloneDOMDocumentManager();
	std::vector< XMLObject* > objectVector;
	// do this for models
	//networkWriter.ReadXMLData( network, "Model", "veModel" );
	networkWriter.ReadXMLData( network, "System", "veSystem" );
	objectVector = networkWriter.GetLoadedXMLObjects();
    if( objectVector.empty() )
    {
        return 0;
    }
    
	osg::ref_ptr<osg::Group> loadedModels = new osg::Group(); 
	osg::ref_ptr<osg::Vec4Array> colorBlack = new osg::Vec4Array;
	colorBlack->push_back(osg::Vec4(0.0f,0.0f,0.0f,1.0f));
	osg::ref_ptr<osg::Vec3Array> shared_normals = new osg::Vec3Array;
	shared_normals->push_back(osg::Vec3(0.0f,-1.0f,0.0f));
	std::ofstream output ("scale.txt");
	System * mainSystem = dynamic_cast < System * > ( objectVector.at( 0 ) );
	// now lets create a list of them
	//for ( size_t i = 0; i < objectVector.size(); ++i )
	for ( size_t i = 0; i < mainSystem->GetModels().size(); ++i )
	{
		//_fileProgress->Update( 75 + (i*timeCalc), _("Loading data") );
		//Model* model = dynamic_cast< Model* >( objectVector.at( i ) );
		ModelWeakPtr model = mainSystem->GetModel(i);

        //add 3d blocks
		osg::ref_ptr<osg::Node> loadedModel = osgDB::readNodeFile("3DIcons/"+model->GetIconFilename()+".obj");
		
		//osg::ref_ptr<ves::xplorer::scenegraph::TextTexture> text = new ves::xplorer::scenegraph::TextTexture();
		//text->UpdateText(model->GetModelName());
		
		//add red block id if block .ive file is not found
		if( !loadedModel.valid() )
		{	
            std::istringstream textNodeStream( GetVESuite_UnsupportedComponent() );
            loadedModel = osgDB::Registry::instance()->
                getReaderWriterForExtension( "osg" )->
                readNode( textNodeStream ).getNode();        
        }
        
        if( !loadedModel.valid() )
        {
            return 0;
        }

		//set the blocks name
		loadedModel->setName(model->GetModelName());

		//calculate the original size of the icon
		ves::xplorer::scenegraph::util::ComputeBoundsVisitor visitor;
		loadedModel->accept(visitor);
		osg::BoundingBox bounds = visitor.getBoundingBox();
		float dx = bounds.xMax() - bounds.xMin();
		float dy = bounds.yMax() - bounds.yMin();
		float dz = bounds.zMax() - bounds.zMin();
		//std::cout <<model->GetModelName()<<std::endl;
		//std::cout <<"dx: "<<dx<<"dy: "<<dy<<"dz: "<<dz<<std::endl;

		//scale icon to 2d worksheet size
		osg::ref_ptr<osg::Image> image = osgDB::readImageFile("2DIcons/"+model->GetIconFilename()+".jpg");
		//osg::ref_ptr<osg::AutoTransform> worksheetScaledModel = new osg::AutoTransform();
        if( image.valid() )
        {
            output<<"width: "<<image->s()<<"height: "<<image->t()<<std::endl;
            //output<<"nx: "<<image->s()/dx<<"ny: "<<image->s()/dy<<"nz: "<<image->t()/dz<<std::endl;
        }
		
		//scales
		//osg::Vec3 vec;
		//vec.set(image->s()/dx, image->s()/dy, image->t()/dz);
		//worksheetScaledModel->addChild(loadedModel.get());
		//worksheetScaledModel->setScale(vec);
		
		//problem with coords - wx has upper left origin
		//worksheetScaledModel->setPosition(osg::Vec3d(
		//	worksheetScaledModel->getPosition().x() + (0.5*image->s()),
		//	worksheetScaledModel->getPosition().y() ,
		//	worksheetScaledModel->getPosition().z() + (0.5*image->t())));

		//Put origin in the center of the model
		//osg::ref_ptr<osg::AutoTransform> loadedModelNormalized = new osg::AutoTransform();
		//loadedModelNormalized->addChild(loadedModel.get());
		//float radius = loadedModel.get()->getBound().radius();
		//loadedModelNormalized->setPosition(osg::Vec3(0, 0, 0-radius));

		//Result Pane
		/*
		osg::ref_ptr<osg::Group> resultPane = new osg::Group();

		osg::ref_ptr<osgText::Text> text = new osgText::Text();
		osg::Vec4 layoutColor(0.0f,0.0f,0.0f,1.0f);
        text->setColor(layoutColor);
		text->setText(model->GetModelName());
		//text->setAutoRotateToScreen(true);
		text->setRotation(osg::Quat(osg::DegreesToRadians(90.0), osg::Vec3d(1.0, 0.0, 0.0)));
		text->setCharacterSize(0.5);

		osg::ref_ptr<osg::Geode> textGeode = new osg::Geode();
		textGeode->addDrawable(text.get());

		osg::ref_ptr<osg::AutoTransform> transTextGeode = new osg::AutoTransform();
		transTextGeode->addChild(textGeode.get());
		transTextGeode->setPosition(osg::Vec3d(-1.0, -0.1, 1.0));

		osg::ref_ptr<osg::Geometry> pane = new osg::Geometry;
		osg::ref_ptr<osg::Vec3Array> coords = new osg::Vec3Array(4);
		(*coords)[0] = osg::Vec3f(-1.5, 0.0, -2.0);//original png size is 300X400 => 3X4
		(*coords)[1] = osg::Vec3f(1.5, 0.0, -2.0);
		(*coords)[2] = osg::Vec3f(1.5, 0.0, 2.0);
		(*coords)[3] = osg::Vec3f(-1.5, 0.0, 2.0);

		pane->setVertexArray(coords.get());
		pane->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS,0,4));

		osg::Vec2Array* texcoords = new osg::Vec2Array(4);
		(*texcoords)[0].set(0.0f, 0.0f);
		(*texcoords)[1].set(1.0f, 0.0f);
		(*texcoords)[2].set(1.0f, 1.0f);
		(*texcoords)[3].set(0.0f, 1.0f);
		pane->setTexCoordArray(0,texcoords);

		osg::Vec3Array* normals = new osg::Vec3Array(1);
		(*normals)[0].set(1.0f,0.0f,0.0f);
		pane->setNormalArray(normals);
		pane->setNormalBinding(osg::Geometry::BIND_OVERALL);
		
		// load image
		osg::Image* img = osgDB::readImageFile("F:/suite/share/dashboard.png");

		// setup texture
		osg::TextureRectangle* texture = new osg::TextureRectangle(img);
		osg::TexMat* texmat = new osg::TexMat;
		texmat->setScaleByTextureRectangleSize(true);

		// setup state
		osg::StateSet* state = pane->getOrCreateStateSet();
		state->setTextureAttributeAndModes(0, texture, osg::StateAttribute::ON);
		state->setTextureAttributeAndModes(0, texmat, osg::StateAttribute::ON);

		osg::ref_ptr<osg::Geode> paneGeode = new osg::Geode();
		paneGeode->addDrawable(pane.get());

		resultPane->addChild(transTextGeode.get());
		resultPane->addChild(paneGeode.get());
		*/

		//Rotate the 3d comps 180 degrees around X axis
		//corrects issue with initial model location
		osg::ref_ptr<osg::AutoTransform> rotatedComp = new osg::AutoTransform();
		rotatedComp->addChild(loadedModel.get());
		//rotatedComp->addChild(worksheetScaledModel.get());
		rotatedComp->setRotation(osg::Quat(osg::DegreesToRadians(180.0), osg::Vec3d(1.0, 0.0, 0.0)));

		//rotate/scale/mirror component
		int mirror = model->GetIconMirror();
		float rotation = model->GetIconRotation();
		float iconScale = model->GetIconScale();

		//rotate according to iconMirror value
		osg::ref_ptr<osg::AutoTransform> mirrorComp = new osg::AutoTransform();
		mirrorComp->addChild(rotatedComp.get());
		if(mirror > 0 && mirror < 3)
		{
			//horizontally
			if(mirror == 1)
				mirrorComp->setRotation(osg::Quat(osg::DegreesToRadians(180.0), osg::Vec3d(0.0, 0.0, 1.0)));
			//vertically
			else
				mirrorComp->setRotation(osg::Quat(osg::DegreesToRadians(180.0), osg::Vec3d(1.0, 0.0, 0.0)));
		}

		//rotate according to iconRotation value
		osg::ref_ptr<osg::AutoTransform> reRotatedComp = new osg::AutoTransform();
		reRotatedComp->addChild(mirrorComp.get());
		reRotatedComp->setRotation(osg::Quat(osg::DegreesToRadians(rotation), osg::Vec3d(0.0, 1.0, 0.0)));

		//move the text to the -y
		//osg::ref_ptr<osg::AutoTransform> textTrans = new osg::AutoTransform();
		//textTrans->addChild(resultPane.get());
		//textTrans->setPosition(osg::Vec3d(0.0, 0.0, loadedModel.get()->getBound().radius()));
		//textTrans->setRotation(osg::Quat(osg::DegreesToRadians(180.0), osg::Vec3d(1.0, 0.0, 0.0)));

		//Scale up 3D comps & text
		//osg::ref_ptr<osg::AutoTransform> scale = new osg::AutoTransform;
		//scale->addChild(reRotatedComp.get());
		//scale.get()->addChild(rotatedComp.get());
		//scale->addChild(textTrans.get());
		//scale->setScale(6.0f * iconScale);
		//scale->setScale(100);

		//translate to comp with name to correct location
		Point * iconLocation = model->GetIconLocation();
		std::pair<unsigned int, unsigned int> xyPair = iconLocation->GetPoint();
		//std::cout<<"X: "<< xyPair.first <<" Y: " << xyPair.second <<std::endl;
		osg::ref_ptr<osg::AutoTransform> mModelTrans = new osg::AutoTransform();
		osg::Vec3 center = mModelTrans.get()->getBound().center();
		//20 and 28 should be replaced with conductor icon width and height respectively
		//osg::Vec3 centerTrans = osg::Vec3((xyPair.first + 20) - center.x(), (xyPair.second + 28) - center.y(), 0 - center.z());
		//osg::Vec3 centerTrans = osg::Vec3((xyPair.first + 20) - center.x(), 0 - center.z(), (xyPair.second + 22) - center.y());
		//osg::Vec3 centerTrans = osg::Vec3(xyPair.first + 20, xyPair.second + 25, 0 );
        //std::cout << " center " << center.x() << " " << center.z() << " " << center.y() << std::endl;
		osg::Vec3 centerTrans = osg::Vec3(xyPair.first - center.x(), 0 - center.z(), xyPair.second - center.y());
		//mModelTrans->addChild(scale.get());
		mModelTrans->addChild(reRotatedComp.get());
		mModelTrans->setPosition(centerTrans);
		mModelTrans->setName(model->GetModelName());
		loadedModels->addChild(mModelTrans.get());	
        //normalize the normals so that lighting works better
        loadedModels->getOrCreateStateSet()->setMode( GL_NORMALIZE, osg::StateAttribute::ON );
	}	
	
	//Streams	
	//networkWriter.ReadXMLData( network, "Model", "veNetwork" );
	//objectVector = networkWriter.GetLoadedXMLObjects();
	//Network* veNetwork = dynamic_cast< Network* >( objectVector.at( 0 ) );
	NetworkWeakPtr veNetwork = mainSystem->GetNetwork();
	//std::cout << "num links " <<  veNetwork->GetNumberOfLinks() << std::endl;
	osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    geode->setName( "Network Lines" );
	for ( size_t i = 0; i < veNetwork->GetNumberOfLinks(); ++i )
	{
		size_t numberOfPoints = veNetwork->GetLink( i )->GetNumberOfLinkPoints();
		osg::Vec3Array* vertices = new osg::Vec3Array(numberOfPoints);
		osg::Geometry* linesGeom = new osg::Geometry();
		//std::cout<<"NP: "<< numberOfPoints<<std::endl;
		for ( size_t j = 0; j < numberOfPoints; j++ )
		{
			std::pair< unsigned int, unsigned int > rawPoint = veNetwork->GetLink( i )->GetLinkPoint( j )->GetPoint();
			//std::cout << "links X: " << rawPoint.first << " Y: " << rawPoint.second << std::endl;
			//(*vertices)[j].set(rawPoint.first, rawPoint.second, 0.0);
			(*vertices)[j].set(rawPoint.first, 0.0, rawPoint.second);
		}
		linesGeom->setVertexArray(vertices);
		linesGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_STRIP, 0, numberOfPoints));		
        linesGeom->setColorBinding(osg::Geometry::BIND_OVERALL);
		linesGeom->setNormalArray(shared_normals.get());
		linesGeom->setNormalBinding(osg::Geometry::BIND_OVERALL);
		linesGeom->setColorArray(colorBlack.get());
		geode->addDrawable(linesGeom);
	}
	loadedModels->addChild(geode.get());
	
	//rotate the world about the X to normalize the flowsheet
	osg::ref_ptr<osg::AutoTransform> worldRotate = new osg::AutoTransform();
	worldRotate->addChild(loadedModels.get());
	worldRotate->setRotation(osg::Quat(osg::DegreesToRadians(180.0), osg::Vec3d(1.0, 0.0, 0.0)));

	//center the world
	worldTranslate = new osg::AutoTransform();
	worldTranslate->addChild(worldRotate.get());
	//osg::Vec3 worldCenter = worldRotate.get()->getBound().center();
	//osg::Vec3 worldTrans = osg::Vec3(0 - worldCenter.x(), 0 - worldCenter.y(), 0 - worldCenter.z());
	//osg::Vec3 worldTrans = osg::Vec3(0 - worldCenter.x(), 0, 0 - worldCenter.z());
    //std::cout << worldCenter.x() << " " << worldCenter.y() << std::endl;
	//worldTranslate->setPosition(worldTrans);

    //Add phong shading to the geodes
    //osg::ref_ptr< osg::StateSet > geodeProperties = worldTranslate->getOrCreateStateSet();
    //ves::xplorer::scenegraph::util::PhongLoader phongShader;
    //phongShader.SetStateSet( geodeProperties.get() );
    //phongShader.SyncShaderAndStateSet();

	return worldTranslate.get();
}