#include "VE_Xplorer/XplorerHandlers/DisplayInformation.h"

#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"

#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
#include "VE_Xplorer/SceneGraph/CADEntity.h"
#include "VE_Xplorer/SceneGraph/CADEntityHelper.h"

#include "VE_Xplorer/XplorerHandlers/WCS.h"

#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Projection>
#include <osg/MatrixTransform>
#include <osg/Light>
#include <osg/LightSource>

//C/C++ libraries
#include <sstream>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
DisplayInformation::DisplayInformation()
{
   display_switch = new VE_SceneGraph::Switch;
   display_switch->SetName( "Display Information Switch Node" );
	VE_SceneGraph::cfdPfSceneManagement::instance()->GetRootNode()->AddChild( display_switch.get() );

	framerate = new osg::CameraNode;
   framerate->setName( "Framerate Node" );
	wcs = new osg::CameraNode;
   wcs->setName( "World Coordinate System Node" );

	framerate_text = new osgText::Text;
	wcs_x_text = new osgText::Text;
	wcs_y_text = new osgText::Text;
	wcs_z_text = new osgText::Text;

	//The physical model for the world coordinate system display
	osg::ref_ptr< VE_SceneGraph::DCS > dcs = new VE_SceneGraph::DCS();
	wcs_model = new VE_SceneGraph::CADEntity( GetVESuite_WCS(), dcs.get(), true );

	display_switch->addChild( framerate.get() );
	display_switch->addChild( wcs.get() );

	display_switch->setChildValue( framerate.get(), false );
	display_switch->setChildValue( wcs.get(), false );
}
////////////////////////////////////////////////////////////////////////////////
DisplayInformation::~DisplayInformation()
{
	if( wcs_model )
	{
		delete wcs_model;
	}
}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::InitFrameRateDisplay()
{
	osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    
	std::string framerate_font( "fonts/arial.ttf" );

   //Turn lighting off for the text and disable depth test to ensure its always ontop
	osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet;
	//framerate->setStateSet( stateset.get() );

	{
      geode->addDrawable( framerate_text.get() );
      framerate_text->setFont( framerate_font );
		framerate_text->setCharacterSize( 20 );
		framerate_text->setAxisAlignment( osgText::Text::SCREEN );
		framerate_text->setAlignment( osgText::Text::RIGHT_BOTTOM );
	}

   //Set the view matrix    
   framerate->setReferenceFrame( osg::Transform::ABSOLUTE_RF );
   framerate->setViewMatrix( osg::Matrix::identity() );

   //Only clear the depth buffer
   framerate->setClearMask( GL_DEPTH_BUFFER_BIT );

   //Draw subgraph after main camera view
   framerate->setRenderOrder( osg::CameraNode::POST_RENDER );

   framerate->addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::InitCoordSysDisplay()
{
	osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    
	std::string wcs_font( "fonts/arial.ttf" );

	//Turn lighting off for the text and disable depth test to ensure its always ontop
	osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet;
	//wcs->setStateSet( stateset.get() );

   {
		geode->addDrawable( wcs_x_text.get() );
		geode->addDrawable( wcs_y_text.get() );
		geode->addDrawable( wcs_z_text.get() );

      wcs_x_text->setFont( wcs_font );
      wcs_x_text->setText( "x" );
		wcs_x_text->setCharacterSize( 15 );
		wcs_x_text->setAxisAlignment( osgText::Text::SCREEN );
		wcs_x_text->setAlignment( osgText::Text::CENTER_CENTER );

		wcs_y_text->setFont( wcs_font );
      wcs_y_text->setText( "y" );
		wcs_y_text->setCharacterSize( 15 );
		wcs_y_text->setAxisAlignment( osgText::Text::SCREEN );
		wcs_y_text->setAlignment( osgText::Text::CENTER_CENTER );

		wcs_z_text->setFont( wcs_font );
      wcs_z_text->setText( "z" );
		wcs_z_text->setCharacterSize( 15 );
		wcs_z_text->setAxisAlignment( osgText::Text::SCREEN );
		wcs_z_text->setAlignment( osgText::Text::CENTER_CENTER );
	}

	//Set the view matrix    
	wcs->setReferenceFrame( osg::Transform::ABSOLUTE_RF );

	wcs->setViewMatrix( osg::Matrix::translate( osg::Vec3( 0.0, 0.0, -40.0 ) ) );

   //Only clear the depth buffer
   wcs->setClearMask( GL_DEPTH_BUFFER_BIT );

   //Draw subgraph after main camera view
   wcs->setRenderOrder( osg::CameraNode::POST_RENDER );

	wcs->addChild( wcs_model->GetDCS() );
	wcs_model->GetDCS()->addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::LatePreFrame()
{
   VE_SceneGraph::DCS* activeNodeDCS = VE_SceneGraph::cfdPfSceneManagement::instance()->GetActiveSwitchNode();
   VE_SceneGraph::DCS* worldDCS = VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS();

   if( activeNodeDCS != worldDCS )
   {
      display_switch->setNodeMask( 0 );
   }

   else
   {
      display_switch->setNodeMask( 1 );
   }

	if( display_switch->getChildValue( framerate.get() ) )
	{
		std::stringstream ss;
		ss << VE_Xplorer::cfdEnvironmentHandler::instance()->GetFrameRate();
		ss << " fps";

		framerate_text->setText( ss.str() );
	}

	if( display_switch->getChildValue( wcs.get() ) )
	{
		osg::Quat temp = VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->getAttitude();
		osg::Quat quat( temp.x(), temp.z(), -temp.y(), temp.w() );

		wcs_model->GetDCS()->setAttitude( quat );
	}
}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::SetFrameRateFlag( bool val )
{
	if( val )
	{
		display_switch->setChildValue( framerate.get(), true );
	}

	else
	{
		display_switch->setChildValue( framerate.get(), false );
	}
}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::SetCoordSysFlag( bool val )
{
	if( val )
	{
		display_switch->setChildValue( wcs.get(), true );
	}

	else
	{
		display_switch->setChildValue( wcs.get(), false );
	}
}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::SetTextColor( std::vector< double > color )
{
	if( ( color[0] + color[1] + color[2] ) > 1.1 && ( color[0] + color[1] + color[2] ) < 2.0 )
	{
		framerate_text->setColor( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
		wcs_x_text->setColor( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
		wcs_y_text->setColor( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
		wcs_z_text->setColor( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );;
	}

	else
	{
		framerate_text->setColor( osg::Vec4( ( 1 - color[0] ), ( 1 - color[1] ), ( 1 - color[2] ), 1.0 ) );
		wcs_x_text->setColor( osg::Vec4( ( 1 - color[0] ), ( 1 - color[1] ), ( 1 - color[2] ), 1.0 ) );
		wcs_y_text->setColor( osg::Vec4( ( 1 - color[0] ), ( 1 - color[1] ), ( 1 - color[2] ), 1.0 ) );
		wcs_z_text->setColor( osg::Vec4( ( 1 - color[0] ), ( 1 - color[1] ), ( 1 - color[2] ), 1.0 ) );
	}
}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::SetDisplayPositions( unsigned int width, unsigned int height )
{
		//Set the projection matrix
		framerate->setProjectionMatrix( osg::Matrix::ortho2D( 0, width, 0, height ) );
		wcs->setProjectionMatrix( osg::Matrix::ortho2D( 0, width, 0, height ) );

		framerate_text->setPosition( osg::Vec3( width - 10, 5, 0 ) );
		wcs_x_text->setPosition( osg::Vec3( 50, 0, 0 ) );
		wcs_y_text->setPosition( osg::Vec3( 0, 0, -50 ) );
		wcs_z_text->setPosition( osg::Vec3( 0, 50, 0 ) );

		wcs_model->GetDCS()->setScale( osg::Vec3( 0.8, 0.8, 0.8 ) );
		wcs_model->GetDCS()->setPosition( osg::Vec3( 50, height - 50, 0 ) );
		
		this->InitFrameRateDisplay();
		this->InitCoordSysDisplay();
}
////////////////////////////////////////////////////////////////////////////////