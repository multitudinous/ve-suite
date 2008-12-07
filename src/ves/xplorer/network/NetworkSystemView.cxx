/*************** <auto-copyright.rb BEGIN do not edit this line> **************
*
* VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/xplorer/network/NetworkSystemView.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/User.h>
#include <ves/open/xml/UserPtr.h>
#include <ves/open/xml/StateInfo.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/System.h>
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
#include <osg/LightModel>

#include <ves/open/xml/model/Model.h>
#include <ves/xplorer/scenegraph/TextTexture.h>
#include <ves/xplorer/network/UnsupportedComponent.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/util/PhongLoader.h>
#include <ves/xplorer/scenegraph/util/ComputeBoundsVisitor.h>

#include <ves/xplorer/Debug.h>

#include <vpr/System.h>

#include <iostream>
#include <fstream>

using namespace ves::xplorer;
using namespace ves::open::xml;
using namespace ves::open::xml::model;
using namespace ves::xplorer::network;

////////////////////////////////////////////////////////////////////////////////
NetworkSystemView::NetworkSystemView()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
NetworkSystemView::NetworkSystemView( std::string network )
{
	LoadVESData( network );
}
////////////////////////////////////////////////////////////////////////////////
NetworkSystemView::NetworkSystemView( const NetworkSystemView& input )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
NetworkSystemView::~NetworkSystemView( void )
{
    ves::xplorer::scenegraph::SceneManager::instance()->GetNetworkDCS()->
        removeChildren( 0, ves::xplorer::scenegraph::SceneManager::instance()->
        GetNetworkDCS()->getNumChildren() );
}
////////////////////////////////////////////////////////////////////////////////
NetworkSystemView& NetworkSystemView::operator=( const NetworkSystemView& input )
{
    if( this != &input )
    {
        ;
    }
    return *this;
}

////////////////////////////////////////////////////////
osg::ref_ptr< osg::Group > NetworkSystemView::DrawNetwork( std::string netId )
{
    osg::ref_ptr<osg::Group> loadedModels = new osg::Group();
    SystemPtr mainSystem = systems[netId];
    // now lets create a list of them
    for( size_t i = 0; i < mainSystem->GetModels().size(); ++i )
    {
        ModelPtr model = mainSystem->GetModel( i );

        if( model->GetIconHiddenFlag() == 0 )
        {
            //add 3d blocks
            std::string dataPrefix;
            vpr::System::getenv( "XPLORER_DATA_DIR", dataPrefix );
            //try default location first
            osg::ref_ptr<osg::Node> loadedModel = 
                osgDB::readNodeFile( dataPrefix + "/3DIcons/" + 
                    model->GetIconFilename() + ".obj.ive" );

            //add red block id if block .ive file is not found
            if( !loadedModel.valid() )
            {
                std::istringstream textNodeStream( 
                    GetVESuite_UnsupportedComponent() );
                loadedModel = osgDB::Registry::instance()->
                              getReaderWriterForExtension( "osg" )->
                              readNode( textNodeStream ).getNode();
            }

            if( !loadedModel.valid() )
            {
                return 0;
            }

            //set the blocks name
            loadedModel->setName( model->GetModelName() );
            //normalize the normals so that lighting works better
            loadedModel->getOrCreateStateSet()->setMode( GL_NORMALIZE, 
                osg::StateAttribute::ON );
		    //setup two sided lighting to account for poor modeling
            osg::ref_ptr< osg::LightModel > lightModel;
            lightModel = new osg::LightModel;
            lightModel->setTwoSided( true );
            loadedModel->getOrCreateStateSet()->setAttributeAndModes(
                lightModel.get(), osg::StateAttribute::ON );     

            //Rotate the 3d comps 180 degrees around X axis
            //corrects issue with initial model location
            osg::ref_ptr<osg::AutoTransform> rotatedComp = new osg::AutoTransform();
		    rotatedComp->addChild( loadedModel.get() );
    		
		    //move pivot point to center
		    ves::xplorer::scenegraph::util::ComputeBoundsVisitor visitor2;
            rotatedComp->accept( visitor2 );
            osg::BoundingBox bounds2 = visitor2.getBoundingBox();
		    rotatedComp->setPivotPoint(bounds2.center());
    		
		    //rotate
            rotatedComp->setRotation( osg::Quat( osg::DegreesToRadians( 180.0 ), 
                osg::Vec3d( 1.0, 0.0, 0.0 ) ) );

            //rotate/scale/mirror component
            int mirror = model->GetIconMirror();
            float rotation = model->GetIconRotation();
            float iconScale = model->GetIconScale();

            //rotate according to iconMirror value
            osg::ref_ptr<osg::AutoTransform> mirrorComp = new osg::AutoTransform();
		    vprDEBUG( vesDBG, 2 ) << "PP: x="<<mirrorComp->getPivotPoint().x()
                << " y="<<mirrorComp->getPivotPoint().y()
                << " z="<<mirrorComp->getPivotPoint().z()
                << std::endl << vprDEBUG_FLUSH;
            mirrorComp->addChild( rotatedComp.get() );
            
		    //move pivot point to center
		    ves::xplorer::scenegraph::util::ComputeBoundsVisitor visitor3;
            mirrorComp->accept( visitor3 );
            osg::BoundingBox bounds3 = visitor3.getBoundingBox();
		    mirrorComp->setPivotPoint(bounds3.center());
    		
		    if( mirror > 0 && mirror < 3 )
            {
                //horizontally
                if( mirror == 1 )
                    mirrorComp->setRotation( osg::Quat( 
                        osg::DegreesToRadians( 180.0 ), 
                        osg::Vec3d( 0.0, 0.0, 1.0 ) ) );
                //vertically
                else
                    mirrorComp->setRotation( osg::Quat( 
                        osg::DegreesToRadians( 180.0 ), 
                        osg::Vec3d( 1.0, 0.0, 0.0 ) ) );
            }

            //rotate according to iconRotation value
            osg::ref_ptr<osg::AutoTransform> reRotatedComp = new osg::AutoTransform();
            reRotatedComp->addChild( mirrorComp.get() );
            
		    //move pivot point to center
		    ves::xplorer::scenegraph::util::ComputeBoundsVisitor visitor4;
            reRotatedComp->accept( visitor4 );
            osg::BoundingBox bounds4 = visitor4.getBoundingBox();
		    reRotatedComp->setPivotPoint(bounds4.center());
    		
		    //rotate
		    reRotatedComp->setRotation( osg::Quat( 
                    osg::DegreesToRadians( rotation ), 
                    osg::Vec3d( 0.0, 1.0, 0.0 ) ) );

            //translate to comp with name to correct location
            PointPtr iconLocation = model->GetIconLocation();
            std::pair<unsigned int, unsigned int> xyPair = iconLocation->GetPoint();
            osg::ref_ptr<osg::AutoTransform> mModelTrans = new osg::AutoTransform();
            mModelTrans->addChild( reRotatedComp.get() );
    		
		    //find offset from center to upper left corner
		    ves::xplorer::scenegraph::util::ComputeBoundsVisitor visitor5;
            mModelTrans->accept( visitor5 );
            osg::BoundingBox bounds5 = visitor5.getBoundingBox();
		    osg::Vec3 centerTrans = osg::Vec3( xyPair.first + 
                ((bounds5.xMax()-bounds5.xMin())/2), 0, xyPair.second + 
                ((bounds5.zMax()-bounds5.zMin())/2));
            mModelTrans->setPosition( centerTrans );
            mModelTrans->setName( model->GetModelName() );
            loadedModels->addChild( mModelTrans.get() );
        }
    }

    //Streams
    NetworkPtr veNetwork = mainSystem->GetNetwork();
    //different typed of streams material, heat, & work
	osg::ref_ptr<osg::Vec4Array> colorBlack = new osg::Vec4Array;
    osg::ref_ptr<osg::Vec4Array> colorGreen = new osg::Vec4Array;
    osg::ref_ptr<osg::Vec4Array> colorRed = new osg::Vec4Array;
    colorBlack->push_back( osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
    colorGreen->push_back( osg::Vec4( 0.0f, 1.0f, 0.0f, 1.0f ) );
    colorRed->push_back( osg::Vec4( 1.0f, 0.0f, 0.0f, 1.0f ) );
    osg::ref_ptr<osg::Vec3Array> shared_normals = new osg::Vec3Array;
    shared_normals->push_back( osg::Vec3( 0.0f, -1.0f, 0.0f ) );
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    geode->setName( "Network Lines" );
    for( size_t i = 0; i < veNetwork->GetNumberOfLinks(); ++i )
    {
        size_t numberOfPoints = veNetwork->GetLink( i )->
            GetNumberOfLinkPoints();
        osg::Vec3Array* vertices = new osg::Vec3Array( numberOfPoints );
        osg::Geometry* linesGeom = new osg::Geometry();
        //std::cout<<"NP: "<< numberOfPoints<<std::endl;
        for( size_t j = 0; j < numberOfPoints; j++ )
        {
            std::pair< unsigned int, unsigned int > rawPoint = 
                veNetwork->GetLink( i )->GetLinkPoint( j )->GetPoint();
            ( *vertices )[j].set( rawPoint.first, 0.0, rawPoint.second );
        }
        linesGeom->setVertexArray( vertices );
        linesGeom->addPrimitiveSet( new osg::DrawArrays(
            osg::PrimitiveSet::LINE_STRIP, 0, numberOfPoints ) );
        linesGeom->setColorBinding( osg::Geometry::BIND_OVERALL );
        linesGeom->setNormalArray( shared_normals.get() );
        linesGeom->setNormalBinding( osg::Geometry::BIND_OVERALL );
		if( veNetwork->GetLink( i )->GetLinkType() == 0 )
		{
            linesGeom->setColorArray( colorBlack.get() );
		}
		else if( veNetwork->GetLink( i )->GetLinkType() == 1 )
		{
            linesGeom->setColorArray( colorRed.get() );
		}
		else if( veNetwork->GetLink( i )->GetLinkType() == 2 )
		{
            linesGeom->setColorArray( colorGreen.get() );
		}
		else
		{
            linesGeom->setColorArray( colorBlack.get() );
		}
        geode->addDrawable( linesGeom );
    }
    geode->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    loadedModels->addChild( geode.get() );

    //rotate the world about the X to normalize the flowsheet
    osg::ref_ptr<osg::AutoTransform> worldRotate = new osg::AutoTransform();
    worldRotate->addChild( loadedModels.get() );
    worldRotate->setRotation( osg::Quat( osg::DegreesToRadians( 180.0 ),
        osg::Vec3d( 1.0, 0.0, 0.0 ) ) );

    //center the world
    worldTranslate = new osg::AutoTransform();
    worldTranslate->addChild( worldRotate.get() );

    return worldTranslate.get();
}
////////////////////////////////////////////////////////////////////////////////
void NetworkSystemView::LoadVESData( std::string xmlNetwork )
{

    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();

	std::string topId;
	std::map< std::string, ves::open::xml::model::NetworkPtr > m_networkMap;
    std::map< std::string, std::vector< std::string > > m_networkModelMap;
    std::map< std::string, ves::open::xml::model::ModelPtr > m_modelMap;
    std::map< std::string, ves::open::xml::UserPtr > m_userMap;
    std::vector< std::pair< std::string, std::string > > dataToObtain;
    std::vector< std::pair< std::string, std::string > >::iterator dataIter;
    dataToObtain.push_back( std::make_pair( "Model", "veSystem" ) );
    dataToObtain.push_back( std::make_pair( "Model", "veNetwork" ) );
    dataToObtain.push_back( std::make_pair( "Model", "veModel" ) );
    dataToObtain.push_back( std::make_pair( "XML", "User" ) );
    networkWriter.ReadXMLData( xmlNetwork, dataToObtain );
    std::vector< ves::open::xml::XMLObjectPtr >::iterator objectIter;
    std::vector< ves::open::xml::XMLObjectPtr > objectVector =
        networkWriter.GetLoadedXMLObjects();
    ves::open::xml::model::SystemPtr tempSystem;

    // we are expecting that a network will be found
    if( !objectVector.empty() )
    {
        //typeid
        //If the file is a new xml file with a system element
        if( objectVector.at( 0 )->GetObjectType() == "System" )
        //if( tempSystem )
        {
            tempSystem = 
                boost::dynamic_pointer_cast<ves::open::xml::model::System>(
                objectVector.at( 0 ) );
            systems[tempSystem->GetID()] = tempSystem;
            //get the main systems id
            
			topId = tempSystem->GetID();
            m_networkMap[ "Network" ] = tempSystem->GetNetwork();
            objectIter = objectVector.begin();
            objectIter = objectVector.erase( objectIter );
        }
    }
    else
    {
        std::cerr << "Improperly formated ves file."
            << "VES File Read Error" << std::endl;
    }

    std::vector< std::string > networkModelVector;
    std::vector< std::string >::iterator stringIter;
    long moduleID = 0;
    std::ostringstream fromID;
    std::ostringstream toID;
    ves::open::xml::model::NetworkPtr tempNetwork = m_networkMap[ "Network" ];
    for( size_t i = 0; i < tempNetwork->GetNumberOfLinks(); ++i )
    {
        tempNetwork->GetLink( i )->GetFromModule()->GetData( moduleID );
        fromID << moduleID;
        tempNetwork->GetLink( i )->GetToModule()->GetData( moduleID );
        toID << moduleID;

        stringIter = std::find( networkModelVector.begin(),
            networkModelVector.end(), fromID.str() );
        if( stringIter == networkModelVector.end() )
        {
            networkModelVector.push_back( fromID.str() );
        }

        stringIter = std::find( networkModelVector.begin(),
            networkModelVector.end(), toID.str() );
        if( stringIter == networkModelVector.end() )
        {
            networkModelVector.push_back( toID.str() );
        }
        fromID.str( "" );
        toID.str( "" );
    }

    if( !tempSystem )
    {
        // now lets create a list of them
        std::ostringstream modelID;
        for( objectIter = objectVector.begin();
            objectIter != objectVector.end(); )
        {
            if( (*objectIter)->GetObjectType() != "Model" )
            {
                //if this object is not a model continue
                ++objectIter;
                continue;
            }
            ves::open::xml::model::ModelPtr model =
                boost::dynamic_pointer_cast<Model>( *objectIter );
            objectIter = objectVector.erase( objectIter );
            modelID << model->GetModelID();
            m_modelMap[ modelID.str()] = model;
            systems[ topId ]->AddModel( model );
            modelID.str( "" );
        }
    }
    else
    {
        // now lets create a list of them
        std::ostringstream modelID;
        std::vector< ves::open::xml::model::ModelPtr >::iterator modelIter;
        std::vector< ves::open::xml::model::ModelPtr > modelVector;
        modelVector = tempSystem->GetModels();

        for( modelIter = modelVector.begin(); modelIter != modelVector.end(); )
        {
            ves::open::xml::model::ModelPtr model = *modelIter;

            modelIter = modelVector.erase( modelIter );
            modelID << model->GetModelID();
            m_modelMap[ modelID.str()] = model;
            stringIter = std::find( networkModelVector.begin(),
                                    networkModelVector.end(), modelID.str() );
            if( stringIter == networkModelVector.end() )
            {
                networkModelVector.push_back( modelID.str() );
            }
            modelID.str( "" );
        }
    }
   //For the case where there are no links between models
    //Just grab all the models in the ves file
    //this is somewhat of a hack but the schema does not support anything else
    if( tempNetwork->GetNumberOfLinks() == 0 )
    {
        std::ostringstream modelID;
        for( std::map< std::string, ves::open::xml::model::ModelPtr >::iterator
                modelIter = m_modelMap.begin(); modelIter != m_modelMap.end();
                ++modelIter )
        {
            modelID << modelIter->second->GetModelID();
            networkModelVector.push_back( modelID.str() );
            modelID.str( "" );
        }
    }
    m_networkModelMap[ "Network" ] = networkModelVector;

    if( !objectVector.empty() )
    {
        m_userMap[ "Network" ] = 
            boost::dynamic_pointer_cast<ves::open::xml::User>
            ( objectVector.at( 0 ) );
        //Set user preferences
        std::vector< ves::open::xml::CommandPtr > tempStates =
            m_userMap[ "Network" ]->GetUserStateInfo()->GetStateVector();
        std::map< std::string, ves::open::xml::CommandPtr > tempMap;
        for( size_t i = 0; i < tempStates.size(); ++i )
        {
            tempMap[ tempStates.at( i )->GetCommandName()] =
                tempStates.at( i );
        }
    }
    else
    {
        m_userMap[ "Network" ] =
            ves::open::xml::UserPtr( new ves::open::xml::User() );
    }

    m_userMap[ "Network" ]->SetUserId( "User" );
    m_userMap[ "Network" ]->SetControlStatus(
        ves::open::xml::User::VEControlStatus( "MASTER" ) );

    if( tempSystem )
    {
        //Parse out the remaining subsystems
        int modelCount = tempSystem->GetNumberOfModels();
        for( size_t j = 0; j < modelCount; j++ )
        {
            if( tempSystem->GetModel( j )->GetSubSystem() )
            {
                ParseSystem( tempSystem->GetModel( j )->GetSubSystem() );
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
void NetworkSystemView::ParseSystem( ves::open::xml::model::SystemPtr system )
{
    //add the system to the map
    systems[system->GetID()] = system;

    //Parse out the subsystems
    int modelCount = system->GetNumberOfModels();
    for( size_t j = 0; j < modelCount; j++ )
    {
        if( system->GetModel( j )->GetSubSystem() )
        {
            ParseSystem( system->GetModel( j )->GetSubSystem() );
        }
    }
}
///////////////////////////////////////////////////////////////////////////////
std::map< std::string, ves::open::xml::model::SystemPtr > 
    NetworkSystemView::GetSystemsMap( void )
{
   return systems;
}