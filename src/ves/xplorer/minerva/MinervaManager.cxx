/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

///////////////////////////////////////////////////////////////////////////////
//
//  Wrapper around Minerva library.
//
///////////////////////////////////////////////////////////////////////////////
#include <ves/xplorer/minerva/MinervaManager.h>
#include <ves/xplorer/minerva/EventHandler.h>
#include <ves/xplorer/minerva/AddEarthHandler.h>
#include <ves/xplorer/minerva/AddElevationLayerHandler.h>
#include <ves/xplorer/minerva/AddElevationGroupHandler.h>
#include <ves/xplorer/minerva/AddRasterLayerHandler.h>
#include <ves/xplorer/minerva/AddRasterGroupHandler.h>
#include <ves/xplorer/minerva/RemoveEarthHandler.h>
#include <ves/xplorer/minerva/RemoveElevationLayerHandler.h>
#include <ves/xplorer/minerva/RemoveRasterLayerHandler.h>
#include <ves/xplorer/minerva/PropertiesHandler.h>
#include <ves/xplorer/minerva/TransformHandler.h>
#include <ves/xplorer/minerva/NavigateToLayer.h>
#include <ves/xplorer/minerva/NavigateToModel.h>
#include <ves/xplorer/minerva/ModelWrapper.h>
#include <ves/xplorer/minerva/Log.h>

#include <Minerva/Config.h>
#include <Minerva/Version.h>
#include <Minerva/Common/Extents.h>
#include <Minerva/Core/Data/Camera.h>
#include <Minerva/Core/TileEngine/Body.h>
#include <Minerva/Core/Functions/MakeBody.h>
#include <Minerva/Core/Layers/RasterLayerWms.h>
#include <Minerva/Core/Visitor.h>

#include <Usul/App/Application.h>
#include <Usul/Components/Manager.h>
#include <Usul/Errors/Assert.h>
#include <Usul/Functions/SafeCall.h>
#include <Usul/Pointers/Functions.h>
#include <Usul/Jobs/Manager.h>
#include <Usul/System/Environment.h>

#include <ves/util/commands/Minerva.h>
#include <ves/open/xml/Command.h>
#include <ves/xplorer/Debug.h>
#include <ves/xplorer/command/CommandManager.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#include <osg/CoordinateSystemNode>
#include <osg/Program>
#include <osgDB/ReadFile>
#include <osgDB/FileUtils>

#include <gmtl/Generate.h>

#include <boost/bind.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>

using namespace ves::xplorer::minerva;

vprSingletonImp ( MinervaManager );

namespace Detail
{
  void setenv ( const std::string& variable, const std::string& value )
  {
#ifdef _MSC_VER
    ::_putenv_s ( variable.c_str(), value.c_str() );
#else
    ::setenv ( variable.c_str(), value.c_str(), 0 );
#endif
  }
}

typedef Minerva::Common::IPlanetCoordinates IPlanetCoordinates;
typedef Minerva::Common::IElevationDatabase IElevationDatabase;

///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

MinervaManager::MinervaManager()
    : 
    _eventHandlers(),
    _currentCommand( CommandPtr() ),
    _body( 0x0 ),
    _manager( 0x0 ),
    _scene( 0x0 ),
    _models()
{
    std::string xplorerDataDir = Usul::System::Environment::get ( "XPLORER_DATA_DIR" );
    std::string minervaDir = xplorerDataDir + std::string( "/minerva" );
    std::cout << "|\tMinerva data directory = " << minervaDir << std::endl;
    bool vesuiteHomeDefined = false;
    try
    {
        boost::filesystem::path minervaDirPath( 
            minervaDir, boost::filesystem::no_check );
        if( boost::filesystem::is_directory( minervaDirPath ) )
        {
            vesuiteHomeDefined = true;
            std::cout << "|\tDirectory for minerva exists" << std::endl;
        }
    }
    catch( const std::exception& ex )
    {
        vprDEBUG( vesDBG, 1 ) << ex.what()
            << std::endl
            << vprDEBUG_FLUSH;
    }    
    Detail::setenv ( MINERVA_DATA_DIR_VARIABLE, minervaDir );

    // Set the program name since the cache is based on this.
    Usul::App::Application::instance().program ( "Minerva" );

    _eventHandlers[ves::util::commands::ADD_EARTH_COMMAND_NAME] = new AddEarthHandler;
    _eventHandlers[ves::util::commands::REMOVE_EARTH_COMMAND_NAME] = new RemoveEarthHandler;
    _eventHandlers[ves::util::commands::SET_GEOGRAPHIC_PROPERTIERS] = new PropertiesHandler;
    _eventHandlers["CAD_TRANSFORM_UPDATE"] = new TransformHandler;
    //_eventHandlers["CAD_DELETE_NODE"] = new DeleteHandler;
    _eventHandlers["Move to cad"] = new NavigateToModel;
    _eventHandlers[ves::util::commands::NAVIGATE_TO_LAYER] = new NavigateToLayer;
    _eventHandlers[ves::util::commands::ADD_ELEVATION_LAYER] = new AddElevationLayerHandler;
    _eventHandlers[ves::util::commands::ADD_ELEVATION_GROUP] = new AddElevationGroupHandler;
    _eventHandlers[ves::util::commands::ADD_RASTER_LAYER] = new AddRasterLayerHandler;
    _eventHandlers[ves::util::commands::ADD_RASTER_GROUP] = new AddRasterGroupHandler;
    _eventHandlers[ves::util::commands::REMOVE_ELEVATION_LAYER] = new RemoveElevationLayerHandler;
    _eventHandlers[ves::util::commands::REMOVE_RASTER_LAYER] = new RemoveRasterLayerHandler;

    Usul::Functions::safeCall( boost::bind( &MinervaManager::_loadPlugins, this ) );
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

MinervaManager::~MinervaManager()
{
  Usul::Functions::safeCall ( boost::bind ( &MinervaManager::_unloadPlugins, this ) );

  // Delete the event handlers.
  EventHandlers::iterator iter ( _eventHandlers.begin() );
  while ( iter != _eventHandlers.end() )
  {
      delete iter->second;
      _eventHandlers.erase( iter++ );
  }

  for ( Models::iterator iter = _models.begin(); iter != _models.end(); ++iter )
  {
    Usul::Pointers::unreference ( iter->second );
    iter->second = 0x0;
  }
  _models.clear();

  this->Clear();
}


///////////////////////////////////////////////////////////////////////////////
//
//  Handle updates.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::PreFrameUpdate()
{
    vprDEBUG( vesDBG, 3 ) << "|\tMinervaManager::LatePreFrameUpdate" 
        << std::endl << vprDEBUG_FLUSH;
    const ves::open::xml::CommandPtr tempCommand =
        ves::xplorer::command::CommandManager::instance()->GetXMLCommand();
    if ( tempCommand )
    {
        const std::string name ( tempCommand->GetCommandName() );

        EventHandlers::iterator iter ( _eventHandlers.find ( name ) );
        if ( iter != _eventHandlers.end() )
        {
            if( !_manager && ( name != ves::util::commands::ADD_EARTH_COMMAND_NAME ) )
            {
                return;
            }

            EventHandler *handler ( iter->second );
            if ( 0x0 != handler )
            {
                vprDEBUG( vesDBG, 0 ) << "|\tMinerva manager executing: " << name << std::endl << vprDEBUG_FLUSH;
                try
                {
                    handler->Execute ( tempCommand, *this );
                }
                catch ( ... )
                {
                    ;
                }
            }
        }
        // Clear the command.
        //_currentCommand = CommandPtr();
    }

    if( _body )
    {
        // Remove all tiles that are ready for deletion.
        _body->purgeTiles();
    }
    vprDEBUG( vesDBG, 3 ) << "|\tMinervaManager::LatePreFrameUpdate End" 
        << std::endl << vprDEBUG_FLUSH;
}
///////////////////////////////////////////////////////////////////////////////
//
//  Add Earth to the scene.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::AddEarthToScene()
{
  // Make sure.
  this->Clear();

  vprDEBUG( vesDBG, 0 ) << "|\tMinerva manager adding earth to the scene." << std::endl << vprDEBUG_FLUSH;

  _manager = new Usul::Jobs::Manager ( "VE-Suite Minerva Job Manager", 4 );

  using Minerva::Core::TileEngine::MeshSize;
  using Minerva::Core::TileEngine::ImageSize;

  const MeshSize meshSize ( 32, 32 );
  const ImageSize imageSize ( 256, 256 );
  //const ImageSize imageSize ( 512, 512 );
  const double splitDistance ( osg::WGS_84_RADIUS_EQUATOR * 3.0 );

  _body = Minerva::Core::Functions::makeEarth ( _manager, meshSize, imageSize, splitDistance );

  Usul::Pointers::reference ( _body );

  // Set the log to print to vprDBG.
  _body->logSet ( new Log );

  _scene = _body->scene();

    osg::ref_ptr< osg::StateSet > tempStateSet = _scene->getOrCreateStateSet();
    //tempStateSet->setRenderBinDetails ( 0, "RenderBin" );

    std::string shaderName = osgDB::findDataFile( "null_glow.fs" );
    osg::ref_ptr< osg::Shader > fragShader = 
        osg::Shader::readShaderFile( osg::Shader::FRAGMENT, shaderName );
        
    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( fragShader.get() );
        
    tempStateSet->setAttributeAndModes( program.get(),
                                    osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        
    //drawable_stateset->addUniform( new osg::Uniform( "opacityVal", 1.0f ) );  
    //drawable_stateset->addUniform( new osg::Uniform( "tex", 0 ) );

  osg::ref_ptr<osg::Group> root ( ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot() );
  if ( root.valid() )
  {
    root->addChild ( _scene.get() );
  }

  for ( Models::iterator iter = _models.begin(); iter != _models.end(); ++iter )
  {
    ModelWrapper::RefPtr model ( iter->second );
    if ( model.valid() )
    {
      Minerva::Core::Data::DataObject::RefPtr dataObject ( new Minerva::Core::Data::DataObject );
      model->SetParent ( dataObject.get() );

      _body->vectorData()->add ( dataObject.get() );
      this->UpdateModel ( model );
    }
  }
}


///////////////////////////////////////////////////////////////////////////////
//
//  Clear.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::Clear()
{
    osg::ref_ptr<osg::Group> root( ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot() );
    if( root.valid() )
    {
        vprDEBUG( vesDBG, 0 ) << "|\tMinerva manager removing earth from the scene." << std::endl << vprDEBUG_FLUSH;
        root->removeChild( _scene.get() );
    }
    _scene = 0x0;

    if( 0x0 != _body )
    {
        _body->clear();
        Usul::Pointers::unreference( _body );
        _body = 0x0;
    }

    // Clean up job manager.
    if( 0x0 != _manager )
    {
        vprDEBUG( vesDBG, 0 ) << "|\tMinerva manager canceling all threads." << std::endl << vprDEBUG_FLUSH;
        
        // Remove all queued jobs and cancel running jobs.
        _manager->cancel();

        vprDEBUG( vesDBG, 0 ) << "|\tMinerva manager waiting for threads to finish." << std::endl << vprDEBUG_FLUSH;

        // Wait for remaining jobs to finish.
        _manager->wait();

        vprDEBUG( vesDBG, 0 ) << "|\tMinerva manager all threads finished." << std::endl << vprDEBUG_FLUSH;

        // Delete the manager.
        delete _manager;
        _manager = 0x0;
    }
}

void MinervaManager::ClearModels()
{
    for ( Models::iterator iter = _models.begin(); iter != _models.end(); ++iter )
    {
        Usul::Pointers::unreference ( iter->second );
        iter->second = 0x0;
    }
    _models.clear();
}

///////////////////////////////////////////////////////////////////////////////
//
//  Add the model.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::AddModel ( const std::string& guid, ModelWrapper* model )
{
  // Remove anything we may have.
  this->RemoveModel ( guid );
  
  // Add the model to our map, even though the body may be null.
  // When the body is created, add the models.
  Usul::Pointers::reference ( model );
  _models[guid] = model;

  if ( 0x0 != _body )
  {
    Minerva::Core::Data::DataObject::RefPtr dataObject ( new Minerva::Core::Data::DataObject );
    model->SetParent ( dataObject.get() );

    _body->vectorData()->add ( dataObject.get() );

    this->UpdateModel ( model );
  }
}


///////////////////////////////////////////////////////////////////////////////
//
//  Get the model wrapper for the guid.
//
///////////////////////////////////////////////////////////////////////////////

ModelWrapper* MinervaManager::GetModel ( const std::string& guid ) const
{
  Models::const_iterator iter ( _models.find ( guid ) );
  return ( iter != _models.end() ? iter->second : 0x0 );
}


///////////////////////////////////////////////////////////////////////////////
//
//  Do we have a model already for the guid?
//
///////////////////////////////////////////////////////////////////////////////

bool MinervaManager::HasModel ( const std::string& guid ) const
{
  Models::const_iterator iter ( _models.find ( guid ) );
  return iter != _models.end();
}


///////////////////////////////////////////////////////////////////////////////
//
//  Update the matrix for the model.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::UpdateModel ( ModelWrapper* model )
{
  if ( 0x0 != model && 0x0 != _body )
  {
    IPlanetCoordinates::QueryPtr planet ( _body );
    IElevationDatabase::QueryPtr elevation ( _body );
    model->UpdateMatrix ( planet.get(), elevation.get() );
  }
}


///////////////////////////////////////////////////////////////////////////////
//
//  Delete the model.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::RemoveModel ( const std::string& guid )
{
  ModelWrapper::RefPtr wrapper ( this->GetModel ( guid ) );
  if ( wrapper.valid() )
  {
    if ( 0x0 != _body )
    {
      _body->vectorData()->remove ( wrapper->GetParent() );
    }

    Usul::Pointers::unreference ( _models[guid] );
    _models.erase ( guid );
  }
}


///////////////////////////////////////////////////////////////////////////////
//
//  Create the view matrix for the given camera.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::GetViewMatrix ( Minerva::Core::Data::Camera* camera, gmtl::Matrix44d& matrix ) const
{
  if ( 0x0 == _body || 0x0 == camera )
    return;

  Minerva::Core::TileEngine::LandModel::RefPtr landModel ( _body->landModel() );
  if ( !landModel.valid() )
    return;

  typedef Minerva::Core::Data::Camera::Matrix Matrix;
  Matrix m ( camera->viewMatrix ( landModel.get() ) );

  matrix.set ( &m[0] );

  gmtl::AxisAngled axisAngle ( -osg::PI_2, 1, 0, 0 );
  gmtl::Matrix44d R;
  gmtl::setRot( R, gmtl::make< gmtl::Quatd >( axisAngle ) );

  gmtl::postMult( matrix, R );
  gmtl::invert( matrix );
}


///////////////////////////////////////////////////////////////////////////////
//
//  Add an elevation layer.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::AddElevationLayer ( Minerva::Core::Layers::RasterLayer* layer )
{
  if ( 0x0 == _body || 0x0 == layer )
    return;


  _body->elevationAppend ( layer );
}


///////////////////////////////////////////////////////////////////////////////
//
//  Add a raster layer.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::AddRasterLayer ( Minerva::Core::Layers::RasterLayer* layer )
{
  if ( 0x0 == _body || 0x0 == layer )
    return;

  _body->rasterAppend ( layer );
}


///////////////////////////////////////////////////////////////////////////////
//
//  Remove an elevation layer.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::RemoveElevationLayer ( const std::string& guid )
{
  if ( 0x0 == _body )
    return;

  typedef Minerva::Core::Data::Container Container;
  Container::RefPtr group ( _body->elevationData() );
  Extents extents;
  this->_removeLayer ( group.get(), guid, extents );
}


///////////////////////////////////////////////////////////////////////////////
//
//  Remove a raster layer.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::RemoveRasterLayer ( const std::string& guid )
{
  if ( 0x0 == _body )
    return;

  // This dynamic cast should always be true.
  typedef Minerva::Core::Data::Container Container;
  Container::RefPtr group ( _body->rasterData() );

  Extents extents;
  if ( this->_removeLayer ( group.get(), guid, extents ) )
  {
    _body->dirtyTextures ( extents );
  }
}


///////////////////////////////////////////////////////////////////////////////
//
//  Remove a layer from the group.
//
///////////////////////////////////////////////////////////////////////////////

bool MinervaManager::_removeLayer ( Minerva::Core::Data::Container *group, const std::string& guid, Extents& extents )
{
    typedef Minerva::Core::Data::Container Container;

    if( 0x0 != group )
    {
        Minerva::Core::Data::Feature::RefPtr feature ( group->find ( guid ) );
        
        // If we found a layer, remove it.
        if ( feature.valid() )
        {
            group->remove ( feature.get() );
            extents = feature->extents();

            return true;
        }
    }

    return false;
}


///////////////////////////////////////////////////////////////////////////////
//
//  Load the plugins.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::_loadPlugins()
{
//#ifdef __APPLE__
  Usul::Components::Manager::instance().load( std::string("GDALReadImage.plug") );
//#endif
  // this causes huge problems because the output is never flushed.
  //This can probably be corrected but I am not sure how.
  if( vpr::Debug::instance()->isDebugEnabled() && vpr::Debug::instance()->isCategoryAllowed( vesDBG ) )
  {
      //Usul::Components::Manager::instance().print ( vpr::Debug::instance()->getStream( vesDBG, 0, true ) );
      Usul::Components::Manager::instance().print( std::cout );
  }

#ifdef _MSC_VER
  const std::string minervaGdalName ( "MinervaGDAL.dll" );
#elif __APPLE__
  const std::string minervaGdalName ( "libMinervaGDAL.bundle" );
#else
  const std::string minervaGdalName ( "libMinervaGDAL.so" );
#endif
  
  Usul::Components::Manager::instance().load ( minervaGdalName );
}

///////////////////////////////////////////////////////////////////////////////
//
//  Unload the plugins.
//
///////////////////////////////////////////////////////////////////////////////

void MinervaManager::_unloadPlugins()
{
  Usul::Components::Manager::instance().clear ( 0x0 );
}
///////////////////////////////////////////////////////////////////////////////
namespace {
  struct FindLayer : public Minerva::Core::Visitor
  {
    FindLayer ( const std::string& guid ) : Minerva::Core::Visitor(), guid ( guid ), layer ( 0x0 ){}
    virtual ~FindLayer() {}
    virtual void visit ( Minerva::Core::Layers::RasterLayer & currentLayer )
    {
      if ( guid == currentLayer.objectId() )
      {
        layer = &currentLayer;
      }
    }
    std::string guid;
    Minerva::Core::Layers::RasterLayer::RefPtr layer;
  };
}
///////////////////////////////////////////////////////////////////////////////
Minerva::Core::Layers::RasterLayer* MinervaManager::GetLayer( const std::string& guid ) const
{
  if ( 0x0 == _body )
    return 0x0;

  ::FindLayer visitor ( guid );
  _body->accept ( visitor );
  return visitor.layer.get();
}
////////////////////////////////////////////////////////////////////////////////
Minerva::Core::TileEngine::Body* MinervaManager::GetTileEngineBody()
{
    return _body;
}
